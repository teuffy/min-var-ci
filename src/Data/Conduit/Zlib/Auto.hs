-- | Auto decompression.

module Data.Conduit.Zlib.Auto where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.Zlib (ungzip)

-- | Decompress the stream as gzip if the gzip header is detected.
autoUngzip :: (MonadUnsafeIO m, MonadThrow m)
           => Conduit ByteString m ByteString
autoUngzip =
  do mchunk <- await
     case mchunk of
       Nothing -> return ()
       Just chunk ->
         do leftover chunk
            if hasGzipHeader chunk
               then ungzip
               else awaitForever yield
  where hasGzipHeader x =
          case S.uncons x of
            Just (31,y) ->
              case S.uncons y of
                Just (139,_) -> True
                _ -> False
            _ -> False
