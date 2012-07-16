-- |
-- Module      : Network.Metrics.Internal
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metrics.Internal (
    -- * Exported types
      Handle(..)

    -- * Socket Handle operations
    , open
    , close
    , emit
    ) where

import Control.Monad                  (unless)
import Network.Socket                 hiding (send)
import Network.Socket.ByteString.Lazy (send)

import qualified Data.ByteString.Lazy.Char8 as BL

-- | Socket handle
data Handle = Handle Socket SockAddr deriving (Show)

--
-- API
--

-- | Create a new unconnected socket handle for UDP communication
open :: SocketType -> String -> String -> IO Handle
open typ host port = do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
    sock     <- socket (addrFamily addr) typ defaultProtocol
    return $ Handle sock (addrAddress addr)

-- | Close a socket handle
close :: Handle -> IO ()
close (Handle sock _) = sClose sock

-- | Push an encoded metric to the specified socket handle
emit :: BL.ByteString -> Handle -> IO ()
emit bstr (Handle sock addr) | BL.null bstr = return ()
                             | otherwise    = do
    sIsConnected sock >>= \b -> unless b $ connect sock addr
    _ <- send sock bstr
    return ()
