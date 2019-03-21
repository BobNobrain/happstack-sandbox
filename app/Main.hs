{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, Conf (..))
import qualified Data.ByteString as B
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson

data ServerConf = ServerConf Int

instance FromJSON ServerConf where
    parseJSON (Object root) = ServerConf <$> root .: "port"
    parseJSON _ = mzero

main :: IO ()
main = do
    rawJSON <- B.readFile "config.json"
    let result = decodeStrict rawJSON
    case result of
        Nothing -> putStrLn "Error parsing config"
        Just (ServerConf port) -> do
            let conf = nullConf { port = port }
            putStrLn ("Listening on port " ++ (show port))
            simpleHTTP conf $ ok ("Hello, world!" :: String)
