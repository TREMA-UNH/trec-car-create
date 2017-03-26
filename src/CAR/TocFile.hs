{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module CAR.TocFile
    ( IndexedCborPath(..)
    , IndexedCbor
      -- * Building index
    , buildIndex
      -- * Opening index
    , open
      -- * Queries
    , lookup
    , toList
    , keys
    ) where

import Data.Foldable hiding (toList)
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import System.IO.MMap
import CAR.Types
import System.FilePath
import Prelude hiding (lookup)

type Offset = Int

readValuesWithOffsets :: forall a. CBOR.Serialise a
                      => BSL.ByteString -> [(Offset, a)]
readValuesWithOffsets = start 0 . BSL.toChunks
  where
    start :: Offset  -- ^ current offset
          -> [BS.ByteString]
          -> [(Offset, a)]
    start _offset []  = []
    start offset  bss =
        go offset offset CBOR.deserialiseIncremental bss

    go :: Offset          -- ^ offset of beginning of current chunk
       -> Offset          -- ^ start offset of thing currently being decoded
       -> CBOR.IDecode a
       -> [BS.ByteString] -- ^ remaining chunks
       -> [(Offset, a)]
    go !currOff !startOff (CBOR.Partial f)   [] =
        go currOff startOff (f Nothing) []

    go currOff  startOff (CBOR.Partial f)   (bs:bss) =
        go currOff startOff (f (Just bs)) bss

    go currOff  startOff (CBOR.Done bs off x) bss =
        let !currOff' = currOff + fromIntegral off
            bss' | BS.null bs = bss
                 | otherwise  = bs : bss
        in (startOff, x) : start currOff' bss'

    go _currOff _startOff (CBOR.Fail _rest _ err) _ =
        error $ show err

data IndexedCborPath i a = IndexedCborPath FilePath
                         deriving (Show)

buildIndex :: (Hashable i, Eq i, CBOR.Serialise i, CBOR.Serialise a)
           => (a -> i) -> FilePath -> IO (IndexedCborPath i a)
buildIndex toIndex path = do
    xs <- readValuesWithOffsets <$> BSL.readFile path
    let addElem acc (offset, x) = HM.insert (toIndex x) offset acc
        toc = foldl' addElem mempty xs
        tocPath = path <.> "toc"
    BSL.writeFile tocPath $ CBOR.serialise toc
    return $ IndexedCborPath path

data IndexedCbor i a = IndexedCbor (HM.HashMap i Offset) BS.ByteString

open :: (Hashable i, Eq i, CBOR.Serialise i)
     => IndexedCborPath i a -> IO (IndexedCbor i a)
open (IndexedCborPath fname) = do
    cbor <- mmapFileByteString fname Nothing
    toc <- CBOR.deserialise <$> BSL.readFile (fname <.> "toc")
    return $ IndexedCbor toc cbor

lookup :: (Hashable i, Eq i, CBOR.Serialise a)
       => i -> IndexedCbor i a -> Maybe a
lookup i (IndexedCbor toc bs) = deser <$> HM.lookup i toc
  where deser offset = CBOR.deserialise $ BSL.fromStrict $ BS.drop offset bs

toList :: (CBOR.Serialise a) => IndexedCbor i a -> [a]
toList (IndexedCbor _ bs) = decodeCborList $ BSL.fromStrict bs

keys :: IndexedCbor i a -> [i]
keys (IndexedCbor toc _) = HM.keys toc