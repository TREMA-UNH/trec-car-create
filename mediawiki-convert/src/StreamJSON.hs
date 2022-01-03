module StreamJSON where

import qualified Data.JsonStream.Parser as JS
import qualified Data.ByteString as BS
import           Pipes
import           Pipes.Internal as P
import qualified Pipes.Safe
import           Pipes.Safe (MonadSafe)
import           Pipes.Safe.Prelude (withFile)

parseJsonP
    :: (MonadSafe m, MonadFail m)
    => JS.Parser a
    -> Producer BS.ByteString m ()
    -> Producer a m (Producer BS.ByteString m ())
parseJsonP parser = go (JS.runParser parser)
  where
      go (JS.ParseYield x next) prod              = yield x >> go next prod
      go (JS.ParseFailed err)   prod              = fail $ "parsePipesBS: " ++ err
      go (JS.ParseDone rest)    prod              = return prod
      go (JS.ParseNeedData k)   (P.Respond bs k') = go (k bs) (k' ())
      go (JS.ParseNeedData k)   (P.M k')          = lift k' >>= go (JS.ParseNeedData k)
      go (JS.ParseNeedData k)   (P.Pure ())       = fail "parsePipesBS: end of stream"
      go (JS.ParseNeedData k)   (P.Request _ _)   = fail "parsePipesBS: impossible"

