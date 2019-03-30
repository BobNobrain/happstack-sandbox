{-# LANGUAGE TupleSections #-}
module WebTools.Routing ( RouteParser
                        , runRouteParser
                        , literal
                        , parseValue
                        ) where

import Text.Read (Read, readMaybe)
import Control.Applicative (Applicative (..))
import Control.Monad (liftM, ap)


type Route = [String]

data RouteParser a = RouteParser { execParser :: (Route -> Maybe (a, Route)) }

runRouteParser :: RouteParser a -> Route -> Maybe a
runRouteParser p route = ((execParser p) route) >>= Just . fst

instance Functor RouteParser where
    fmap = liftM

instance Applicative RouteParser where
    pure v = RouteParser (\r -> Just (v, r))
    (<*>) = ap

instance Monad RouteParser where
    return = pure
    (>>=) = bindRouteParser
    fail _ = RouteParser (\_ -> Nothing)


bindRouteParser :: RouteParser a -> (a -> RouteParser b) -> RouteParser b
bindRouteParser (RouteParser m) k = RouteParser result where
    result r = case m r of  Just (a, r') -> (execParser (k a)) r'
                            Nothing -> Nothing


literal :: String -> RouteParser ()
literal value = RouteParser (extractLiteral) where
    extractLiteral :: Route -> Maybe ((), Route)
    extractLiteral (r:rs) = Just ((), rs)
    extractLiteral [] = Nothing

parseValue :: (Read a) => RouteParser a
parseValue = RouteParser (extractAndParse) where
    extractAndParse :: (Read a) => Route -> Maybe (a, Route)
    extractAndParse (r:rs) = readMaybe r >>= Just . (,rs)
    extractAndParse [] = Nothing
