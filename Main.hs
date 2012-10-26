{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified App
import qualified Config
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Scotty

main :: IO ()
main = do
    scotty 3000 $ do
      middleware logStdoutDev
      middleware $ staticPolicy $ addBase "static"

      App.app
