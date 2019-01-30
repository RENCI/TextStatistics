module TextStatistics.Pluralise where

import Prelude hiding (lookup)
-- import Data.Map.Strict (Map, fromList, lookup)
import Data.List (find, findIndex, isPrefixOf, tails)
import Control.Arrow
import Text.Regex
import Data.Maybe (fromJust)
import Data.Char (toLower)

{- | 
   Singularising and Pluralising functions from following URL, released
   under an MIT license and used with thanks:
   http://kuwamoto.org/2007/12/17mproved-pluralizing-in-php-actionscript-and-ror/
-}
plural :: [(Regex, String)]
plural = map (first mkRegex) [
        ("(quiz)$", "$1zes"),
        ("^(ox)$", "$1en"),
        ("([m|l])ouse$", "$1ice"),
        ("(matrix|vertex|index)$", "$1ices"),
        ("(x|ch|ss|sh)$", "$1es"),
        ("([^aeiouy]|qu)y$", "$1ies"),
        ("(hive)$", "$1s"),
        ("(?:([^f])fe|([lr])f)$", "$1$2ves"),
        ("(shea|lea|loa|thie)f$", "$1ves"),
        ("sis$", "ses"),
        ("([ti])um$", "$1a"),
        ("(tomat|potat|ech|her|vet)o$", "$1oes"),
        ("(bu)s$", "$1ses"),
        ("(alias)$", "$1es"),
        ("(octop)us$", "$1i"),
        ("(ax|test)is$", "$1es"),
        ("(us)$", "$1es"),
        ("s$", "s")
    ]

singular :: [(Regex, String)]
singular = map (first mkRegex) [
        ("(quiz)zes$", "$1"),
        ("(matr)ices$", "$1ix"),
        ("(vert|ind)ices$", "$1ex"),
        ("^(ox)en$", "$1"),
        ("(alias)es$", "$1"),
        ("(octop|vir)i$", "$1us"),
        ("(cris|ax|test)es$", "$1is"),
        ("(shoe)s$", "$1"),
        ("(o)es$", "$1"),
        ("(bus)es$", "$1"),
        ("([m|l])ice$", "$1ouse"),
        ("(x|ch|ss|sh)es$", "$1"),
        ("(m)ovies$", "$1ovie"),
        ("(s)eries$", "$1eries"),
        ("([^aeiouy]|qu)ies$", "$1y"),
        ("([lr])ves$", "$1f"),
        ("(tive)s$", "$1"),
        ("(hive)s$", "$1"),
        ("(li|wi|kni)ves$", "$1fe"),
        ("(shea|loa|lea|thie)ves$", "$1f"),
        ("(^analy)ses$", "$1sis"),
        ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$", "$1$2sis"),
        ("([ti])a$", "$1um"),
        ("(n)ews$", "$1ews"),
        ("(h|bl)ouses$", "$1ouse"),
        ("(corpse)s$", "$1"),
        ("(us)es$", "$1"),
        ("s$", "")
    ]

irregular :: [(String, String)]
irregular = [
        ("child", "children"),
        ("foot", "feet"),
        ("goose", "geese"),
        ("man", "men"),
        ("mouse", "mice"),
        ("move", "moves"),
        ("person", "people"),
        ("sex", "sexes"),
        ("tooth", "teeth")
    ]

{- |
   Some words are only uncountable sometimes. For example, "blues" can be
   uncountable when referring to music, but countable when referring to
   multiple colours.
   -}
uncountable = [
        "beef",
        "bison",
        "buffalo",
        "carbon",
        "chemistry",
        "copper",
        "geometry",
        "gold",
        "cs",
        "css",
        "deer",
        "equipment",
        "fish",
        "furniture",
        "information",
        "mathematics",
        "money",
        "moose",
        "nitrogen",
        "oxygen",
        "rice",
        "series",
        "sheep",
        "species",
        "surgery",
        "traffic",
        "water"
    ]

findSubstring :: Eq a => [a] -> [a] -> Maybe Int
findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

matchAndReplace :: [(Regex, String)] -> String -> Maybe String
matchAndReplace [] str = Nothing
matchAndReplace ((re, restr) : t) str =
    case matchRegexAll re str of
        Nothing -> matchAndReplace t str
        Just (pre, _, post, (sub : _)) -> 
            let i = fromJust (findSubstring "$1" restr) in
                Just (pre ++ take i restr ++ sub ++ drop (i + 2) restr ++ post)

{- |
   Get the plural of the word passed in.
   -}
getPlural :: String -- ^ Word to pluralise
          -> String -- ^ Pluralised word
getPlural string =
    -- save some time in the case that singular and plural are the same
    let lowercaseWord = map toLower string in
        if lowercaseWord `elem` uncountable
            then string
            else case find (\a -> snd a == lowercaseWord) irregular of
                Just _ -> lowercaseWord
                Nothing -> case find (\a -> fst a == lowercaseWord) irregular of
                    Just (_, plural) -> plural
                    Nothing -> case matchAndReplace plural lowercaseWord of
                        Just plural -> plural
                        Nothing -> lowercaseWord ++ "s"

{- | 
   Get the singular of the word passed in.
   -}
getSingular :: String -- ^ Word to singularise
            -> String -- ^ Singularised word
getSingular string =
    -- save some time in the case that singular and plural are the same
    let lowercaseWord = map toLower string in
        if lowercaseWord `elem` uncountable
            then string
            else case find (\a -> fst a == lowercaseWord) irregular of
                Just _ -> lowercaseWord
                Nothing -> case find (\a -> snd a == lowercaseWord) irregular of
                    Just (_, singular) -> singular
                    Nothing -> case matchAndReplace singular lowercaseWord of
                        Just singular -> singular
                        Nothing -> string
