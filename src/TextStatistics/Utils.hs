module TextStatistics.Utils where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.List (foldl', dropWhile, dropWhileEnd)
import Data.Char
import Text.Regex
import TextStatistics.Pluralise


strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

replaceAndCount :: String -> (String, Int) -> Regex -> (String, Int)
replaceAndCount restr (str, n) re = (a, c) where
    (a, b, c) = go ("", str, n)
    go :: (String, String, Int) -> (String, String, Int)
    go (pre, str, n) =
        case matchRegexAll re str of
            Nothing -> (pre ++ str, "", n)
            Just (pre2, _, str2, _) -> go (pre ++ pre2 ++ restr, str2, n + 1)

matchAndCount :: (String, Int) -> Regex -> (String, Int)
matchAndCount (str, n) re = 
        case matchRegexAll re str of
            Nothing -> ("", n)
            Just (_, _, str2, _) -> matchAndCount (str2, n + 1) re
