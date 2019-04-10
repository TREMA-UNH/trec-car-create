-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import JavaScript.Web.XMLHttpRequest

import GHC.Generics
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Hashable

import qualified Data.Text as T

import JSDOM.URLSearchParams
import JavaScript.Web.Location

import CAR.Types
import Types

import qualified Debug.Trace as Debug



data AssessmentLabel = MustLabel | ShouldLabel | CanLabel | TopicLabel | NonRelLabel | TrashLabel  |DuplicateLabel |UnsetLabel
    deriving (Eq, FromJSON, ToJSON, Generic, Show)

prettyLabel :: AssessmentLabel -> MisoString
prettyLabel MustLabel = "Must"
prettyLabel ShouldLabel = "Should"
prettyLabel CanLabel = "Can"
prettyLabel TopicLabel = "Topic"
prettyLabel NonRelLabel = "No"
prettyLabel TrashLabel = "Trash"
prettyLabel DuplicateLabel = "Duplicate"
prettyLabel UnsetLabel = "x"


data AssessmentTransitionLabel = RedundantTransition | SameTransition | AppropriateTransition | SwitchTransition | OfftopicTransition | ToNonRelTransition | UnsetTransition
    deriving (Eq, FromJSON, ToJSON, Generic, Show)

prettyTransition :: AssessmentTransitionLabel -> MisoString
prettyTransition RedundantTransition = "Redundant"
prettyTransition SameTransition = "Same"
prettyTransition AppropriateTransition = "Good"
prettyTransition SwitchTransition = "Switch"
prettyTransition OfftopicTransition = "ToOfftopic"
prettyTransition ToNonRelTransition = "ToNonRel"
prettyTransition UnsetTransition = "x"




data AssessmentKey = AssessmentKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic)

data AssessmentTransitionKey = AssessmentTransitionKey {
        userId :: UserId
        , queryId :: QueryId
        , paragraphId1 :: ParagraphId
        , paragraphId2 :: ParagraphId
    }
  deriving (Eq, Hashable, Ord, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Generic)



data AssessmentState = AssessmentState {
                    labelState :: M.Map AssessmentKey AssessmentLabel
                    , notesState :: M.Map AssessmentKey T.Text
                    , facetState :: M.Map AssessmentKey AssessmentFacet
                    , transitionLabelState :: M.Map AssessmentTransitionKey AssessmentTransitionLabel
                    , transitionNotesState :: M.Map AssessmentTransitionKey T.Text
                    , hiddenState :: M.Map AssessmentKey Bool
    }
  deriving (Eq, FromJSON, ToJSON, Generic)

emptyAssessmentState = AssessmentState { labelState = mempty
                                       , notesState = mempty
                                       , facetState = mempty
                                       , transitionLabelState = mempty
                                       , transitionNotesState = mempty
                                       , hiddenState = mempty
                                       }

data SavedAssessments = SavedAssessments {
        savedData :: AssessmentState
    }
  deriving (Eq, FromJSON, ToJSON, Generic)



data DisplayConfig = DisplayConfig { displayAssessments :: Bool}
  deriving (Eq)

defaultDisplayConfig = DisplayConfig {displayAssessments = False}


data AssessmentModel =
    AssessmentModel { page :: AssessmentPage
                    , state :: AssessmentState
                    , config :: DisplayConfig
                    }
    | FileNotFoundErrorModel { filename :: MisoString }
    | ErrorMessageModel { errorMessage :: MisoString }
    | LoadingPageModel
  deriving (Eq)


type UserId = T.Text
defaultUser :: UserId
defaultUser = "defaultuser"


uploadUrl :: JSString
uploadUrl = "/assessment"

-- | Sum type for application events
data Action
  = FetchAssessmentPage String
  | SetAssessmentPage AssessmentPage
  | ReportError MisoString
  | Initialize
  | SetAssessment UserId QueryId ParagraphId AssessmentLabel
  | SetFacet UserId QueryId ParagraphId MisoString
  | SetNotes UserId QueryId ParagraphId MisoString
  | ToggleHidden UserId QueryId ParagraphId
  | SetTransitionAssessment UserId QueryId ParagraphId ParagraphId AssessmentTransitionLabel
  | Noop
  | FlagSaveSuccess
  | SaveAssessments
  | DisplayAssessments
  deriving (Show)



emptyAssessmentModel :: AssessmentModel
emptyAssessmentModel = LoadingPageModel




-- | Type synonym for an application model
type Model = AssessmentModel


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Initialize -- FetchAssessmentPage "water-distribution" -- initial action to be executed on application load
    model  = LoadingPageModel  -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (FetchAssessmentPage pageName) m = m <# do
    page <- fetchJson $ getAssessmentPageFilePath pageName

    return $ case page of 
      Right p -> SetAssessmentPage p
      Left e  -> ReportError $ ms $ show e
updateModel (Initialize) m = m <# do
    loc <- getWindowLocation >>= getSearch
    params <- newURLSearchParams loc
    maybeQ <- get params ("q" :: MisoString)
    let query = fromMaybe "default" maybeQ
    return $ FetchAssessmentPage query

updateModel (SetAssessmentPage page) _ = noEff $ (AssessmentModel page' emptyAssessmentState defaultDisplayConfig)  -- todo load from storage
    where page' = page{apQueryFacets = facets'}
          facets' = (apQueryFacets $ page)
                  <> [ AssessmentFacet{apHeading=(SectionHeading "NONE OF THESE")
                     , apHeadingId=packHeadingId "NONE_OF_THESE" }
                     ]
updateModel (ReportError e) _ = noEff $ ErrorMessageModel e

updateModel (SetAssessment userId queryId paraId label) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    let key = storageKey "label" userId queryId paraId
        value = label
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value
    return Noop
  where newModel =
            let labelState' = M.insert (AssessmentKey userId queryId paraId) label labelState
            in m {state = state{labelState = labelState'}}


updateModel (SetFacet userId queryId paraId headingIdStr) m@AssessmentModel {state=state@AssessmentState{..}, page=AssessmentPage{apQueryFacets =facetList}} =  newModel <# do
    let key = storageKey "facet" userId queryId paraId
        value = headingIdStr
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value
    putStrLn $ "SetFacet "<> show queryId <> " - " <> unpackParagraphId paraId <> " - " <> (fromMisoString headingIdStr)
    return Noop
  where newModel =
            let facetState' =
                    case [ f
                         | f@AssessmentFacet {apHeadingId=hid} <- facetList
                         , (unpackHeadingId hid) == (fromMisoString headingIdStr)
                         ] of
                    (facet:_) -> M.insert (AssessmentKey userId queryId paraId) facet facetState
                    otherwise -> M.delete (AssessmentKey userId queryId paraId) facetState
            in m {state = state{ facetState = facetState'}}

updateModel (SetNotes userId queryId paraId txt) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    let key = storageKey "notes" userId queryId paraId
        value = txt
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value
    return Noop
  where newModel =
            let notesState' = M.insert (AssessmentKey userId queryId paraId) (T.pack $ fromMisoString txt) notesState
            in m {state = state{notesState = notesState'}}

updateModel (ToggleHidden userId queryId paraId) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    let key = storageKey "hidden" userId queryId paraId
        value = show $ newState
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value
    return Noop
  where newModel = m {state = state{hiddenState = hiddenState'}}
        oldState = fromMaybe False $ M.lookup  (AssessmentKey userId queryId paraId) hiddenState
        newState = not oldState
        hiddenState' = M.insert (AssessmentKey userId queryId paraId) (newState) hiddenState

updateModel (SetTransitionAssessment userId queryId paraId1 paraId2 label) m@AssessmentModel {state=state@AssessmentState{..}} =  newModel <# do
    let key = storageKeyTransition "transition" userId queryId paraId1 paraId2
        value = label
    putStrLn $ show key <> "   " <> show value
    setLocalStorage key value
    return Noop
  where newModel =
            let transitionLabelState' = M.insert (AssessmentTransitionKey userId queryId paraId1 paraId2) label transitionLabelState
            in m {state = state{transitionLabelState = transitionLabelState'}}


updateModel Noop m = noEff m

updateModel SaveAssessments m = m <# do
    res <- uploadAssessments $ state m
    return $ case res of
      Right () -> FlagSaveSuccess
      Left e  -> ReportError $ ms $ show e

updateModel FlagSaveSuccess m@AssessmentModel{page=AssessmentPage{apSquid=queryId}} = m <# do
    alert $ "Uploaded annotations for page " <> (ms $ unQueryId queryId)
    return Noop

updateModel DisplayAssessments m@AssessmentModel{ config=c@DisplayConfig {displayAssessments=display}} =
    noEff $ m { config =
                    c {
                        displayAssessments = not display
                      }
              }



storageKey :: String -> UserId -> QueryId -> ParagraphId -> MisoString
storageKey category userId queryId paraId =
    (ms category) <> "-" <> (ms $ userId) <> "-" <> (ms $ unQueryId queryId) <> "-" <> (ms $ unpackParagraphId paraId)

storageKeyTransition :: String -> UserId -> QueryId -> ParagraphId -> ParagraphId -> MisoString
storageKeyTransition category userId queryId paraId1 paraId2 =
    (ms category) <> "-"
    <> (ms $ userId) <> "-"
    <> (ms $ unQueryId queryId) <> "-"
    <> (ms $ unpackParagraphId paraId1)  <> "-"
    <> (ms $ unpackParagraphId paraId2)

-- updateModel Home m = m <# do
--                h <- windowInnerHeight
--                return $ UpdateModelWithWindow h
-- updateModel (UpdateModelWithWindow h) m = noEff (h)
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp




getAssessmentPageFilePath :: String -> MisoString
getAssessmentPageFilePath pageName =
    ms
    $ "/data/" <> pageName <> ".json"

--     $ "http://trec-car2.cs.unh.edu:8080/data/" <> pageName <> ".json"
--     $ "http://localhost:8000/data/" <> pageName <> ".json"

uploadAssessments ::  AssessmentState -> IO (Either FetchJSONError ())
uploadAssessments assessmentState = do
    putStrLn $ "uploadURL " <> (show uploadUrl)
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200      -> pure $ Right ()
      Right resp             -> pure $ Left $ BadResponse (status resp) (contents resp)
      Left err               -> pure $ Left $ XHRFailed err
  where
    onError :: XHRError -> IO (Either XHRError (Response BS.ByteString))
    onError = pure . Left

    req = Request { reqMethod = POST
                  , reqURI = uploadUrl
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = StringData $ ms$  Data.Aeson.encode $  SavedAssessments assessmentState
                  }

data FetchJSONError = XHRFailed XHRError
                    | InvalidJSON String
                    | BadResponse { badRespStatus :: Int, badRespContents :: Maybe BS.ByteString }
                    deriving (Eq, Show)


fetchJson :: forall a. FromJSON a => JSString -> IO (Either FetchJSONError a)
fetchJson url = do
    resp <- handle onError $ fmap Right $ xhrByteString req
    case resp of
      Right (Response{..})
        | status == 200
        , Just d <- contents -> pure $ either (Left . InvalidJSON) Right $ eitherDecodeStrict d
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

-- ------------- Presentation ----------------------

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel AssessmentModel{
            page= AssessmentPage{..}
            , state = s@AssessmentState { labelState = labelState
                                      , transitionLabelState = transitionState
                                      , hiddenState = hiddenState
                                      }
            , config = c@DisplayConfig {..}
          } =

    div_ []
       [ h1_ [] [text $ ms apTitle]
       , p_ [] [text "Query Id: ", text $ ms $ unQueryId apSquid]
       , p_ [] [text "Run: ", text $ ms apRunId]
       , button_ [onClick SaveAssessments] [text "Upload"]
       , button_ [onClick DisplayAssessments, class_ hiddenDisplayBtn] [text "Display"]
       , textarea_ [class_ ("assessment-display "<> hiddenDisplay), id_ "assessment-display"] [
            text $  ms$  AesonPretty.encodePretty $  SavedAssessments s
       ]
       , hr_ []
       , ol_ [] $
           let interleave :: (a -> b) -> (a -> a -> b) -> [a] -> [b]
               interleave f g = go
                 where
                   go (x:y:rest) = f x : g x y : go (y:rest)
                   go [x]        = [f x]
                   go []         = []
           in interleave renderParagraph renderTransition apParagraphs
       ]
  where
        hiddenDisplay = if displayAssessments then "active-display" else "hidden-display"
        hiddenDisplayBtn = if displayAssessments then "active-display-btn" else "display-btn"
        queryId = apSquid

        facetMap = M.fromList [ (hid, facet)  |  facet@AssessmentFacet{apHeadingId=hid} <- apQueryFacets]

        mkHidable className key paraId =
            div_[] [
                button_ [class_ ("hider "<> className), onClick (ToggleHidden defaultUser queryId paraId)] [text "Hide from Article"]
            ]

        mkButtons key paraId =
            div_[] [
            label_ [for_ "relevance"] [text ""] -- Relevance Assessments:"
            ,span_ [class_ "btn-group", id_ "relevance"] [         --  , role_ "toolbar"
                    mkButton paraId MustLabel
                  , mkButton paraId ShouldLabel
                  , mkButton paraId CanLabel
                  , mkButton paraId TopicLabel
                  , mkButton paraId NonRelLabel
                  , mkButton paraId TrashLabel
                  , mkButton paraId DuplicateLabel
                  , mkButton paraId UnsetLabel
                ]
            ]
          where
            current = fromMaybe UnsetLabel $ key `M.lookup` labelState
            mkButton paraId label =
              let active = if label==current then "active" else ""
              in button_ [ class_ ("btn btn-sm "<> active)
                         , onClick (SetAssessment defaultUser queryId paraId label) ]
                         [text $ prettyLabel label]
        mkNotesField key paraId =
            div_ [class_ "notes-div"] [
                label_ [] [text "Notes:"
                    , input_ [ class_ "notes-field"
                           , type_ "text"
                           , size_ "50"
                           , maxlength_ "100"
                           , onInput (\str -> SetNotes defaultUser queryId paraId str)
                          ]
                ]
            ]

        mkQueryFacetField key paraId =
            let idStr = storageKey "transition" defaultUser queryId paraId
                facetList :: [AssessmentFacet]
                facetList = apQueryFacets
            in div_ [] [
                label_ [for_ idStr ] [text "Best fitting query Facet:"
                    , (select_ [ class_ "facet-select"
                               , id_ idStr
                               , onChange (\str -> SetFacet defaultUser queryId paraId str)
                               ]
                        $ [ option_ [ class_ "facet-option", value_ $ ""] [text $ "Please select"]]
                        <> (fmap renderFacet facetList)
                       )
                    ]
                ]

          where renderFacet :: AssessmentFacet -> View Action
                renderFacet AssessmentFacet{..}=
                    let headingId = unpackHeadingId $  apHeadingId
                        headingText = getSectionHeading $ apHeading
                    in option_ [value_ $ ms $ headingId] [text $ ms $ headingText ]




        mkTransitionButtons key paraId1 paraId2 =
                    label_ [] [text "Transition Quality:"
                        ,div_ [class_ "trans-group"] [
                            mkButton paraId1 paraId2 RedundantTransition
                          , mkButton paraId1 paraId2 SameTransition
                          , mkButton paraId1 paraId2 AppropriateTransition
                          , mkButton paraId1 paraId2 SwitchTransition
                          , mkButton paraId1 paraId2 OfftopicTransition
                          , mkButton paraId1 paraId2 ToNonRelTransition
                          , mkButton paraId1 paraId2 UnsetTransition
                        ]
                    ]
                  where
                    current = fromMaybe UnsetTransition $ key `M.lookup` transitionState       -- todo fetch state
                    mkButton paraId1 paraId2 label =
                      let active = if label==current then "active" else ""
                      in button_ [ class_ ("btn btn-sm "<> active)
                                 , onClick (SetTransitionAssessment defaultUser queryId paraId1 paraId2 label) ]
                                 [text $ prettyTransition label]


        renderTransition:: Paragraph -> Paragraph -> View Action
        renderTransition p1@Paragraph{paraId = paraId1} p2@Paragraph{paraId=paraId2} =
            let assessmentKey = (AssessmentTransitionKey defaultUser queryId paraId1 paraId2)
                idStr = storageKey "transition" defaultUser queryId paraId1


-- Trying to hide transition when paragraph is hidden ---
--  ...however this does not work, as then the next transition has the wrong "para1"
--                 psgAssessmentKey = (AssessmentKey defaultUser queryId paraId1)
--                 isHidden = fromMaybe False $ assessmentKey `M.lookup` hiddenState
--                 hiddenStateClass = if isHidden then "hidden-panel" else "shown-panel"

            in  span_ [class_ "annotation"] [
                    div_ [class_ ("btn-toolbar annotate") ] [
                        mkTransitionButtons assessmentKey paraId1 paraId2
                    ]
                ]
        renderParagraph :: Paragraph -> View Action
        renderParagraph Paragraph{..} =
            let assessmentKey = (AssessmentKey defaultUser queryId paraId)
                isHidden = fromMaybe False $ assessmentKey `M.lookup` hiddenState
                hiddenStateClass = if isHidden then "hidden-panel" else "shown-panel"
                hidableClass = if isHidden then "hidable-hidden" else "hidable-shown"
            in li_ [class_ ("entity-snippet-li")] [
                p_ [] [
                      mkHidable hidableClass assessmentKey paraId
                    , div_ [class_ hiddenStateClass] [
                        mkNotesField assessmentKey paraId
                        , mkQueryFacetField assessmentKey paraId
                        ,  span_ [class_ "annotation"][ -- data-item  data-query
                            div_ [class_ "btn-toolbar annotate" ] [ -- data_ann role_ "toolbar"
                                mkButtons assessmentKey paraId
                            ]
                        ]
                        , p_ [class_ "paragraph-id"] [text $ ms $ unpackParagraphId paraId]
                        , p_ [class_ "entity-snippet-li-text"] $ fmap renderParaBody paraBody
                    ]
                ]
            ]
        renderParaBody :: ParaBody -> View action
        renderParaBody (ParaText txt) = text $ ms txt
        renderParaBody (ParaLink Link{..}) = a_ [ href_ $ ms $ unpackPageName linkTarget] [text $ ms linkAnchor]

viewModel LoadingPageModel = viewErrorMessage $ "Loading Page"
viewModel FileNotFoundErrorModel { .. }= viewErrorMessage $ "File not Found " <> ms filename
viewModel ErrorMessageModel { .. }= viewErrorMessage $ ms errorMessage



viewErrorMessage :: MisoString -> View Action
viewErrorMessage msg = div_ []
    [ h1_[] [text $ msg ],
      p_[] [ a_ [ href_ "/"] [text "Back to start..."]]
    ]

