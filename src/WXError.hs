{- This file is part of PhoneDirectory.
   Copyright (C) 2009 Michael Steele

   PhoneDirectory is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   PhoneDirectory is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with PhoneDirectory.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WXError
    ( WXError ()
    , fromEither
    , fromMaybe
    , wxerror
    , module Control.Monad.Trans
    ) where

import Control.Monad.Error
import Control.Monad.Trans
import System.IO.Error (try)

newtype WXError a = WXError { runWXError :: ErrorT String IO a }
    deriving (Functor, Monad)

instance MonadError String WXError where
    throwError = WXError . throwError
    m `catchError` h = WXError $ ErrorT $ do
        a <- runErrorT $ runWXError m
        case a of
            Left l  -> runErrorT (runWXError $ h l)
            Right r -> return (Right r)

-- |Redirect all IO exceptions into the normal error stream.
instance MonadIO WXError where
    liftIO a = WXError $ liftIO (try a) >>= either (throwError . show) return

-- |Convenience function for getting out of the WXError monad
wxerror :: WXError a -> IO (Either String a)
wxerror = runErrorT . runWXError

fromMaybe
    :: String
    -> Maybe a
    -> WXError a
fromMaybe msg = maybe (throwError msg) return

fromEither
    :: Either String b
    -> WXError b
fromEither = either throwError return
