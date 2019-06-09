-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String hiding (concatMap, filter, length, zip)
import Data.Aeson
import GHC.Generics
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import JavaScript.Web.XMLHttpRequest

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy
-- import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Data.Time
import Data.Char
import Data.String
import Language.Porter
import qualified System.FilePath

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types
import qualified Codec.Serialise as CBOR
import qualified CAR.Types.CborList as CBOR
import qualified CAR.Types.Files as CAR

import qualified Debug.Trace as Debug

mss :: Show a => a -> MisoString
mss = ms . show


data TqaModel =
    Initialize
    | TqaModel { pages :: [Page]
               , headings :: M.Map SectionPath T.Text
              }
    | ErrorMessageModel { errorMessage :: MisoString
                        }
  deriving (Eq, Show)

instance Eq Page where
    p1 == p2 = (pageId p1) == (pageId p2)

emptyModel = Initialize


-- | Sum type for application events
data Action
  = Annotate
  | LoadCbor JSString
  | SetTqaPages [Page]
  | ReportError MisoString
--   | Initialize
  deriving (Show)


-- | Type synonym for an application model
type Model = TqaModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = LoadCbor "./data/tqa2.cbor"  -- -- initial action to be executed on application load
    model  = emptyModel -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

updateModel (LoadCbor filename) m = m <# do
    cbor <- fetchCbor filename
            :: IO (Either FetchJSONError [Page])
--     now' <- getCurrentTime
    return $ case cbor of
      Right pages -> SetTqaPages pages
      Left e  -> ReportError $ mss e

updateModel (SetTqaPages pages) _ = noEff $ TqaModel pages mempty


updateModel (ReportError e) m = noEff $ ErrorMessageModel e

fetchCbor :: forall a. CBOR.Serialise a => JSString -> IO (Either FetchJSONError [a])
fetchCbor url = do
    result <- fetchByteString url
    case result of
        Left err        -> pure $ Left err
        Right byteStr   -> let hdr ::  CAR.Header
                               (hdr, pages) = CBOR.decodeCborList $ Data.ByteString.Lazy.fromStrict byteStr
                           in pure $ Right pages


data FetchJSONError = XHRFailed XHRError
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)


fetchByteString:: JSString -> IO (Either FetchJSONError BS.ByteString)
fetchByteString url = do
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200
        , Just c <- contents -> pure $ Right c
      Right resp             -> pure $ Left $ BadResponse (status resp) (contents resp)
      Left err               -> pure $ Left $ XHRFailed err
  where
    onError :: XHRError -> IO (Either XHRError (Response BS.ByteString))
    onError = pure . Left

    req = Request { reqMethod = GET
                  , reqURI = url
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }

decodeByteString :: BS.ByteString -> T.Text
decodeByteString = Data.Text.Encoding.decodeUtf8

-- ------------- Presentation ----------------------

type SectionPathId = [Either PageName HeadingId]
unpackSectionPathId :: SectionPathId -> T.Text
unpackSectionPathId sp = T.intercalate "/" $ fmap unpackElem sp
  where unpackElem :: Either PageName HeadingId -> T.Text
        unpackElem (Left pageName ) =  T.pack $ unpackPageName pageName
        unpackElem (Right headingId) =  T.pack $ unpackHeadingId headingId

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@TqaModel{..} =
    div_ []
       [ h1_ [] [text $ "TQA Pages"]
       , ol_ [] $ fmap renderPage pages
       ]

  where renderPage :: Page -> View Action
        renderPage Page{..} =
            li_ [] -- liKeyed_  (Key $ ms $ unpackPageId pageId)
                ([ renderIncludePageCheckbox pageName
                 , h1_ [][text $ ms $ unpackPageName pageName]
                 ] <>
                 (foldMap (renderSkel [Left pageName]) pageSkeleton)
                )
        renderSkel :: SectionPathId -> PageSkeleton -> [View Action]
        renderSkel sp (Section sectionHeading headingid skeleton) =
            ([ h2_ [] [text $ ms $ getSectionHeading sectionHeading]
            , p_ [] [text $ ms $ unpackHeadingId headingid]
            , renderIncludeSectionCheckbox sp'
            , input_ []
            ] <> foldMap (renderSkel sp') skeleton
            )
          where sp' = sp <> [Right headingid]
        renderSkel sp (Para p) =
            [p_ [] [text $ ms $ getText p]]
        renderSkel sp (Image txt _ ) =
            [p_ [] [text $ ms txt]]
        renderSkel sp (Infobox _ _) = mempty
        renderSkel sp (List _ para) =
            [br_ [], text $ ms $ getText para]

        getText (Paragraph {..}) = foldMap getParaBodyTxt paraBody
        getParaBodyTxt (ParaText txt) = txt
        getParaBodyTxt (ParaLink Link{..}) = linkAnchor

        renderIncludePageCheckbox :: PageName -> View Action
        renderIncludePageCheckbox pageName =
            div_ [class_ "custom-control custom-checkbox"] [
                  input_ [type_ "checkbox", class_ "custom-control-input", id_ "defaultUnchecked"]
                , label_ [class_ "custom-control-label", for_ "defaultUnchecked"] [text $  ms ( "Include page? "<> (unpackPageName pageName))]
            ]

        renderIncludeSectionCheckbox :: SectionPathId -> View Action
        renderIncludeSectionCheckbox sp =
            div_ [class_ "custom-control custom-checkbox"] [
                  input_ [type_ "checkbox", class_ "custom-control-input", id_ "defaultUnchecked"]
                , label_ [class_ "custom-control-label", for_ "defaultUnchecked"] [text $  ms ( "Include section? "<> (unpackSectionPathId sp))]
            ]


viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage
viewModel Initialize = viewErrorMessage $ "Initializing..."



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

