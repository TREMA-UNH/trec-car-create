{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module CAR.FilterPred where

import Data.Maybe
import Data.Void
import Control.Applicative
import Control.Monad (void, when)
import Data.Foldable
import Data.Hashable
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Prelude hiding (pred)

import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style

import CAR.Types
import CAR.Utils
import CAR.KnowledgeBase (InlinkInfo(redirectPages))

-- | Salt used by @hashable-1.2.5.0@
defaultSalt :: Int
defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4

data Pred a = NameContains T.Text
            | NameHasPrefix T.Text
            | NameHasSuffix T.Text
            | NameInSet (HS.HashSet PageName)
            | NameOrRedirectInSet (HS.HashSet PageName)
            | PageIdInSet (HS.HashSet PageId)
            | HasCategoryContaining T.Text
            | PageHashMod Int Int Int -- ^ for training/test split
            | IsRedirect
            | IsDisambiguation
            | IsCategory
            | HasWikidataQid (HS.HashSet WikiDataId)
            | HasPageTag (HS.HashSet T.Text)

            | Any [Pred a]
            | All [Pred a]
            | Negate (Pred a)
            | TruePred
            | Pure a
            deriving (Show, Functor, Foldable, Traversable)

runPred :: Applicative m => (a -> m (Pred b)) -> Pred a -> m (Pred b)
runPred _ (NameContains x)     = pure $ NameContains x
runPred _ (NameHasPrefix x)    = pure $ NameHasPrefix x
runPred _ (NameHasSuffix x)    = pure $ NameHasSuffix x
runPred _ (NameInSet x)        = pure $ NameInSet x
runPred _ (NameOrRedirectInSet x)  = pure $ NameOrRedirectInSet x
runPred _ (PageIdInSet x)        = pure $ PageIdInSet x
runPred _ (HasCategoryContaining x)  = pure $ HasCategoryContaining x
runPred _ (PageHashMod s x y)  = pure $ PageHashMod s x y
runPred _ IsRedirect           = pure IsRedirect
runPred _ IsDisambiguation     = pure IsDisambiguation
runPred _ IsCategory           = pure IsCategory
runPred _ (HasWikidataQid qids)       = pure $ HasWikidataQid qids
runPred _ (HasPageTag tags)       = pure $ HasPageTag tags
runPred f (Any x)     = Any <$> traverse (runPred f) x
runPred f (All x)     = All <$> traverse (runPred f) x
runPred f (Negate x)  = Negate <$> runPred f x
runPred _ TruePred    = pure TruePred
runPred f (Pure x)    = f x
runPred _ _ = undefined

parsePred :: Parser a -> Parser (Pred a)
parsePred inj = term
  where
    opTable :: OperatorTable Parser (Pred a)
    opTable = [ [ prefix "!" Negate ]
              , [ binary "&"   (\x y -> All [x,y]) AssocLeft
                , binary "and" (\x y -> All [x,y]) AssocLeft
                , binary "|"   (\x y -> Any [x,y]) AssocLeft
                , binary "or"  (\x y -> Any [x,y]) AssocLeft
                ]
              ]
      where
        binary  name fun assoc = Infix (fun <$ reservedOp name) assoc
        prefix  name fun       = Prefix (fun <$ reservedOp name)
        reservedOp name = reserve emptyOps name

    expr = buildExpressionParser opTable term <?> "job match expression"

    term = parens expr <|> simple

    simple =
        asum [ nameContains, nameHasPrefix, nameHasSuffix
             , nameInSet, nameOrRedirectInSet, hasCategoryContaining, pageHashMod
             , testSet, trainSet, foldPred, isRedirect, isDisambiguation, isCategory
             , hasWikidataQid, hasPageTag
             , truePred
             , Pure <$> inj
             ]

    truePred = textSymbol "true" >> pure TruePred
    trainSet = textSymbol "train-set" >> pure (PageHashMod defaultSalt 2 0)
    testSet  = textSymbol "test-set"  >> pure (PageHashMod defaultSalt 2 1)
    foldPred = do void $ textSymbol "fold"
                  k <- fmap fromIntegral natural
                  pure $ Any [ PageHashMod defaultSalt 10 (2*k)
                             , PageHashMod defaultSalt 10 (2*k+1) ]
    isRedirect = textSymbol "is-redirect" >> pure IsRedirect
    isDisambiguation = textSymbol "is-disambiguation" >> pure IsDisambiguation
    isCategory = textSymbol "is-category" >> pure IsCategory


    nameContains = do
        void $ textSymbol "name-contains"
        NameContains . T.toCaseFold <$> string'

    nameHasPrefix = do
        void $ textSymbol "name-has-prefix"
        NameHasPrefix <$> string'

    nameHasSuffix = do
        void $ textSymbol "name-has-suffix"
        NameHasSuffix <$> string'

    nameInSet = do
        void $ textSymbol "name-in-set"
        NameInSet . HS.fromList . map PageName <$> listOf string'

    nameOrRedirectInSet = do
        void $ textSymbol "name-or-redirect-in-set"
        NameOrRedirectInSet . HS.fromList . map PageName <$> listOf string'

    hasWikidataQid = do
        void $ textSymbol "qid-in-set"
        let wikiDataId = do
                s <- string'
                case readWikiDataId s of
                    Nothing -> fail $ "mal-formed WikiDataId "++T.unpack s
                    Just wdid -> return wdid
        HasWikidataQid . HS.fromList <$> listOf wikiDataId

    hasPageTag = do 
        void $ textSymbol "has-page-tag"
        HasPageTag . HS.fromList <$> listOf string'

    pageIdInSet = do
        void $ textSymbol "pageid-in-set"
        PageIdInSet . HS.fromList . map (packPageId . T.unpack) <$> listOf string'

    hasCategoryContaining = do
        void $ textSymbol "category-contains"
        HasCategoryContaining . T.toCaseFold <$> string'

    pageHashMod = do
        void $ textSymbol "page-hash-mod"
        let natural' = fmap fromIntegral natural
        n <- natural'
        k <- natural'
        when (k >= n) $ fail "pageHashMod: k must be less than n"
        salt <- natural' <|> pure defaultSalt
        return $ PageHashMod salt n k

    listOf :: Parser a -> Parser [a]
    listOf element = brackets $ commaSep element

    string' :: Parser T.Text
    string' = fmap T.pack $ stringLiteral <|> (many alphaNum <* whiteSpace)

-- todo fix this - currently not called, and does not apply recursively
normalize :: Pred a -> Pred a
normalize (Any [xs, Any ys]) = Any (xs:ys)
normalize (Any [Any xs, ys]) = Any (ys:xs)
normalize (All [xs, All ys]) = All (xs:ys)
normalize (All [All xs, ys]) = All (ys:xs)
normalize x                  = x

interpret :: (PageName -> PageName) -> Pred Void -> Page -> Bool
interpret pageNameTranslate pred page =
    case pred of
      NameContains t                -> t `T.isInfixOf` (T.toCaseFold $ getPageName $ pageNameTranslate $ pageName page)
      NameHasPrefix prefix          -> prefix `T.isPrefixOf` (getPageName $ pageNameTranslate $ pageName page)
      NameHasSuffix suffix          -> suffix `T.isSuffixOf` (getPageName $ pageNameTranslate $ pageName page)
      NameInSet names               -> (pageNameTranslate $ pageName page) `HS.member` names
      NameOrRedirectInSet names     -> (pageNameTranslate $ pageName page) `HS.member` names 
                                    || hasRedirectName page names
      PageIdInSet pageIds           -> (pageId page) `HS.member` pageIds
      HasCategoryContaining s       -> any (s `T.isInfixOf`) $ map (getPageName . pageNameTranslate) $ fromMaybe [] $ getMetadata _CategoryNames (pageMetadata page)
      PageHashMod salt n k          -> let h = hashWithSalt salt $ pageNameTranslate $ pageName page
                                       in h `mod` n == k
      IsRedirect                    -> isJust $ pageRedirect page
      IsDisambiguation              -> pageIsDisambiguation page
      IsCategory                    -> pageIsCategory page
      HasWikidataQid qids           -> pageHasWikidataQid page qids
      HasPageTag tags               -> pageHasPageTag page tags
      Any preds                     -> any (\pred' -> interpret pageNameTranslate pred' page) preds
      All preds                     -> all (\pred' -> interpret pageNameTranslate pred' page) preds
      Negate p                      -> not $ interpret pageNameTranslate p page
      TruePred                      -> True
      Pure _                        -> error "Impossible"

  where pageHasWikidataQid :: Page -> HS.HashSet WikiDataId -> Bool
        pageHasWikidataQid page@Page{pageMetadata = meta}  targetQids =
            case getMetadata _WikiDataQID meta of
                Just qid -> qid `HS.member` targetQids
                _ -> error $ "WikidataQid information is not available for page "<> show page

        pageHasPageTag :: Page -> HS.HashSet (T.Text) -> Bool
        pageHasPageTag page@Page{pageMetadata = meta} targetTags  =
            case getMetadata _PageTags meta of
                Just tags -> any  (`HS.member` targetTags)  tags
                _         -> False

        hasRedirectName :: Page -> HS.HashSet (PageName) -> Bool
        hasRedirectName page@Page{pageMetadata = meta} targetNames  =
            case getMetadata _RedirectNames meta of
                Just redirects -> any  (`HS.member` targetNames)  redirects
                _              -> False

