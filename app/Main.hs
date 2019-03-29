{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Happstack.Server (nullConf
                        , simpleHTTP
                        , toResponse
                        , dir
                        , path
                        , ok
                        , Conf (..)
                        , ToMessage (..)
                        , FromReqURI (..)
                        , ServerPart
                        )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad (mzero, msum)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson

data ServerConf = ServerConf Int

instance FromJSON ServerConf where
    parseJSON (Object root) = ServerConf <$> root .: "port"
    parseJSON _ = mzero

data Account = Account  { accountId :: Int
                        , accountBalance :: Int
                        }

instance ToJSON Account where
    toJSON acc = object [ "id" .= (accountId acc)
                        , "balance" .= (accountBalance acc)
                        ]

instance (ToJSON a) => ToMessage a where
    toMessage = encode
    toContentType _ = "application/json; charset=utf-8"

john :: Account
john = Account { accountId = 2, accountBalance = 100 }

oleg :: Account
oleg = Account { accountId = 3, accountBalance = 15 }


instance FromReqURI Account where
    fromReqURI "2" = Just john
    fromReqURI "3" = Just oleg
    fromReqURI _ = Nothing

accountIdentity200 :: Account -> ServerPart Account
accountIdentity200 = ok


main :: IO ()
main = do
    LC.putStrLn $ encode john
    rawJSON <- B.readFile "config.json"
    let result = decodeStrict rawJSON
    case result of
        Nothing -> putStrLn "Error parsing config"
        Just (ServerConf port) -> do
            let conf = nullConf { port = port }
            putStrLn ("Listening on port " ++ (show port))
            simpleHTTP conf $ msum  [ dir "user" $ path accountIdentity200
                                    , dir "oleg" $ ok john
                                    , dir "john" $ ok john
                                    ]

