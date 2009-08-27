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

module WXError where

import Control.Monad.Error
import Text.JSON
import System.IO.Error (try)

newtype WXError a = WXError { runWXError :: ErrorT String IO a }
    deriving (Functor, Monad, MonadIO)

instance MonadError String WXError where
    throwError = WXError . throwError
    m `catchError` h = WXError $ ErrorT $ do
        a <- runErrorT $ runWXError m
        case a of
            Left l  -> runErrorT (runWXError $ h l)
            Right r -> return (Right r)

wxerror :: WXError a -> IO (Either String a)
wxerror = runErrorT . runWXError

fromJSONResult :: Result a -> WXError a
fromJSONResult = either throwError return . resultToEither

-- |Execute an IO computation, trapping any IOError exceptions in the
-- ErrorT String monad.
fromIO 
    :: Maybe String -- ^Error message to display. Nothing causes the
                    -- underlying error to be used.  Just x causes x to be
                    -- used.
                    -- TODO: provide a hook to use a different error message
                    -- depending on which IOError gets thrown.
    -> IO a   -- ^Computation to execute
    -> WXError a -- ^Result wrapped into the ErrorT String monad.
fromIO msg = liftIO . try >=> either errorOp return
  where
    errorOp = case msg of
        Nothing -> throwError . show
        Just x -> throwError . ((x ++ ": ") ++) . show

fromMaybe
    :: String
    -> Maybe a
    -> WXError a
fromMaybe msg = maybe (throwError msg) return

fromEither
    :: Either String b
    -> WXError b
fromEither = either throwError return
