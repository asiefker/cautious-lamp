{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Lens.TH
import           Control.Monad.State.Class (gets)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import           Snap.Snaplet
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

data App = App {
    _companyName :: IORef B.ByteString
}

-- makeLenses ''App

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

companyHandler :: Handler App App ()
companyHandler = method GET getter <|> method POST setter
  where
    getter = do
        nameRef <- gets _companyName
        name <- liftIO $ readIORef nameRef
        writeBS name
    setter = do
        mname <- getParam "name"
        nameRef <- gets _companyName
        liftIO $ maybe (return ()) (writeIORef nameRef) mname
        getter
