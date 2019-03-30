{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Happstack.Server
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad (mzero, msum, MonadPlus)
import Control.Monad.Trans (lift)
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

john :: Account
john = Account { accountId = 2, accountBalance = 100 }

oleg :: Account
oleg = Account { accountId = 3, accountBalance = 15 }

accountIdentity200 :: Account -> ServerPart Account
accountIdentity200 = ok


data ApiResponse a
    = ApiResponseError String
    | ApiResponseResult a
    deriving (Show, Eq)


instance (ToJSON a) => ToJSON (ApiResponse a) where
    toJSON (ApiResponseError errmsg) = object [ "success" .= False, "error" .= object [ "message" .= errmsg ] ]
    toJSON (ApiResponseResult value) = object [ "success" .= True, "data" .= (toJSON value) ]

instance (ToJSON a) => ToMessage (ApiResponse a) where
    toMessage = encode
    toContentType _ = "application/json; charset=utf-8"


testRqHandler :: ServerPartT IO (ApiResponse Account)
testRqHandler = do
    -- guard route
    dir "user" $ anyPath nullDir
    noTrailingSlash
    rq <- askRq
    putStrLn' $ show $ rqPaths rq
    userId <- dir "user" $ path (\(i :: Int) -> return i)
    -- nullDir
    -- putStrLn' "Let's check for nullDir..."
    -- nullDir
    -- putStrLn' "yup"
    case userId of 2 -> ok $ ApiResponseResult john
                   3 -> ok $ ApiResponseResult oleg
                   _ -> notFound $ ApiResponseError "No such user"
    where
        putStrLn' :: String -> ServerPartT IO ()
        putStrLn' = (lift . putStrLn)

        waste _ = return ()


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
            simpleHTTP conf $ msum  [ testRqHandler
                                    ]

