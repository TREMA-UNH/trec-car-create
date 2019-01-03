module LookupWrapper where

import Data.Maybe
import CAR.Types hiding (Entity)
import CAR.TocFile as Toc
import qualified Data.HashMap.Strict as HM

-- ---------------------------------------------
-- Fetch pages from cbor
-- ---------------------------------------------
type PagesLookup =  ([PageId] -> [Page])

readPagesToc :: Toc.IndexedCborPath PageId Page -> IO PagesLookup
readPagesToc pagesFileWithToc = do
    toc <- Toc.open pagesFileWithToc
    return $ \pageIds -> mapMaybe ( `Toc.lookup` toc) pageIds

wrapPagesTocs :: HM.HashMap PageId Page
                 -> PagesLookup
wrapPagesTocs pageId2Page =
    \pageIds -> catMaybes $ fmap (`HM.lookup` pageId2Page) pageIds