module TextStatistics.Syllables where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.List (foldl', dropWhile, dropWhileEnd)
import Data.Char
import Text.Regex
import TextStatistics.Pluralise
import TextStatistics.Utils

{- |
   Specific common exceptions that don't follow the rule set below are handled individually
   array of problem words (with word as key, syllable count as value).
   Common reasons we need to override some words:
     - Trailing "e" is pronounced
     - Portmanteaus
    -}
arrProblemWords:: Map String Int
arrProblemWords = fromList [
    ("abalone", 4),
    ("abare", 3),
    ("abed", 2),
    ("abruzzese", 4),
    ("abbruzzese", 4),
    ("aborigine", 5),
    ("acreage", 3),
    ("adame", 3),
    ("adieu", 2),
    ("adobe", 3),
    ("anemone", 4),
    ("apache", 3),
    ("aphrodite", 4),
    ("apostrophe", 4),
    ("ariadne", 4),
    ("cafe", 2),
    ("calliope", 4),
    ("catastrophe", 4),
    ("chile", 2),
    ("chloe", 2),
    ("circe", 2),
    ("coyote", 3),
    ("epitome", 4),
    ("forever", 3),
    ("gethsemane", 4),
    ("guacamole", 4),
    ("hyperbole", 4),
    ("jesse", 2),
    ("jukebox", 2),
    ("karate", 3),
    ("machete", 3),
    ("maybe", 2),
    ("people", 2),
    ("recipe", 3),
    ("sesame", 3),
    ("shoreline", 2),
    ("simile", 3),
    ("syncope", 3),
    ("tamale", 3),
    ("yosemite", 4),
    ("daphne", 2),
    ("eurydice", 4),
    ("euterpe", 3),
    ("hermione", 4),
    ("penelope", 4),
    ("persephone", 4),
    ("phoebe", 2),
    ("zoe", 2)
    ]

-- | These syllables would be counted as two but should be one
arrSubSyllables :: [Regex]
arrSubSyllables = map mkRegex [
         "cia(l|$)" -- glacial, acacia
        ,"tia"
        ,"cius"
        ,"cious"
        ,"[^aeiou]giu"
        ,"[aeiouy][^aeiouy]ion"
        ,"iou"
        ,"sia$"
        ,"eous$"
        ,"[oa]gue$"
        ,".[^aeiuoycgltdb]{2,}ed$"
        ,".ely$"
        -- ,"[cg]h?ed?$"
        -- ,"rved?$"
        -- ,"[aeiouy][dt]es?$"
        -- ,"^[dr]e[aeiou][^aeiou]+$" // Sorts out deal, deign etc
        -- ,"[aeiouy]rse$" // Purse, hearse
        ,"^jua"
        -- ,"nne[ds]?$" // canadienne
        ,"uai" -- acquainted
        ,"eau" -- champeau
        -- ,"pagne[ds]?$" // champagne
        -- ,"[aeiouy][^aeiuoytdbcgrnzs]h?e[rsd]?$"
        {- The following detects words ending with a soft e ending. Don't
           mess with it unless you absolutely have to! The following
           is a list of words you can use to test a new version of
           this rule (add "r", "s" and "d" where possible to test
           fully):
             - absolve
             - acquiesce
             - audience
             - ache
             - acquire
             - brunelle
             - byrne
             - canadienne
             - coughed
             - curved
             - champagne
             - designate
             - force
             - lace
             - late
             - lathe
             - make
             - relayed
             - scrounge
             - side
             - sideline
             - some
             - wide
             - taste -}
        ,"[aeiouy](b|c|ch|d|dg|f|g|gh|gn|k|l|ll|lv|m|mm|n|nc|ng|nn|p|r|rc|rn|rs|rv|s|sc|sk|sl|squ|ss|st|t|th|v|y|z)e$"
        {- For soft e endings with a "d". Test words:
             - crunched
             - forced
             - hated
             - sided
             - sidelined
             - unexploded
             - unexplored
             - scrounged
             - squelched
             - forced -}
        ,"[aeiouy](b|c|ch|dg|f|g|gh|gn|k|l|lch|ll|lv|m|mm|n|nc|ng|nch|nn|p|r|rc|rn|rs|rv|s|sc|sk|sl|squ|ss|th|v|y|z)ed$"
        {- For soft e endings with a "s". Test words:
             - absences
             - accomplices
             - acknowledges
             - advantages
             - byrnes
             - crunches
             - forces
             - scrounges
             - squelches -}
        ,"[aeiouy](b|ch|d|f|gh|gn|k|l|lch|ll|lv|m|mm|n|nch|nn|p|r|rn|rs|rv|s|sc|sk|sl|squ|ss|st|t|th|v|y)es$"
        ,"^busi$"
    ]   

-- | These syllables would be counted as one but should be two
arrAddSyllables :: [Regex]
arrAddSyllables = map mkRegex [
         "([^s]|^)ia"
        ,"riet"
        ,"dien" -- audience
        ,"iu"
        ,"io"
        ,"eo($|[b-df-hj-np-tv-z])"
        ,"ii"
        ,"[ou]a$"
        ,"[aeiouym]bl$"
        ,"[aeiou]{3}"
        ,"[aeiou]y[aeiou]"
        ,"^mc"
        ,"ism$"
        ,"asm$"
        ,"thm$"
        ,"([^aeiouy])\1l$"
        ,"[^l]lien"
        ,"^coa[dglx]."
        ,"[^gq]ua[^auieo]"
        ,"dnt$"
        ,"uity$"
        ,"[^aeiouy]ie(r|st|t)$"
        ,"eings?$"
        ,"[aeiouy]sh?e[rsd]$"
        ,"iell"
        ,"dea$"
        ,"real" -- real, cereal
        ,"[^aeiou]y[ae]" -- bryan, byerley
        ,"gean$" -- aegean
        ,"uen" -- influence, affluence
    ]

-- | Single syllable prefixes and suffixes
arrAffix :: [Regex]
arrAffix = map mkRegex [
         "^un"
        ,"^fore"
        ,"^ware"
        ,"^none?"
        ,"^out"
        ,"^post"
        ,"^sub"
        ,"^pre"
        ,"^pro"
        ,"^dis"
        ,"^side"
        ,"ly$"
        ,"less$"
        ,"some$"
        ,"ful$"
        ,"ers?$"
        ,"ness$"
        ,"cians?$"
        ,"ments?$"
        ,"ettes?$"
        ,"villes?$"
        ,"ships?$"
        ,"sides?$"
        ,"ports?$"
        ,"shires?$"
        ,"tion(ed)?$"
    ]

-- | Double syllable prefixes and suffixes
arrDoubleAffix :: [Regex]
arrDoubleAffix = map mkRegex [
         "^above"
        ,"^ant[ie]"
        ,"^counter"
        ,"^hyper"
        ,"^afore"
        ,"^agri"
        ,"^in[ft]ra"
        ,"^inter"
        ,"^over"
        ,"^semi"
        ,"^ultra"
        ,"^under"
        ,"^extra"
        ,"^dia"
        ,"^micro"
        ,"^mega"
        ,"^kilo"
        ,"^pico"
        ,"^nano"
        ,"^macro"
        ,"berry$"
        ,"woman$"
        ,"women$"
    ]

-- | Triple syllable prefixes and suffixes
arrTripleAffix :: [Regex]
arrTripleAffix = map mkRegex [
         "ology$"
        ,"ologist$"
        ,"onomy$"
        ,"onomist$"
    ]

{- | 
   Returns the number of syllables in the word.
   Based in part on Greg Fast's Perl module Lingua::EN::Syllables
-}
syllableCount :: String -- ^ Word to be measured
              -> Int
syllableCount strWord = 
    let strippedWord = strip strWord in
        if length strippedWord == 0
            then 0
            else 
                let strippedWord2 = subRegex (mkRegex "[^A-Za-z]") strippedWord ""
                    lowercaseStrippedWord = map toLower strippedWord2 in
                        case lookup lowercaseStrippedWord arrProblemWords of
                            Just sylln -> sylln
                            Nothing -> 
                                let singularWord = getSingular lowercaseStrippedWord in
                                    case lookup singularWord arrProblemWords of
                                        Just sylln -> sylln
                                        Nothing -> 
                                            let (singularWord2, intAffixCount) = foldl' (replaceAndCount "") (singularWord, 0) arrAffix
                                                (singularWord3, intDoubleAffixCount) = foldl' (replaceAndCount "") (singularWord2, 0) arrDoubleAffix
                                                (singularWord4, intTripleAffixCount) = foldl' (replaceAndCount "") (singularWord3, 0) arrTripleAffix
                                                arrWordParts = splitRegex (mkRegex "[^aeiouy]+") singularWord4
                                                intWordPartCount = length (filter (/= "") arrWordParts)
                                                -- Some syllables do not follow normal rules - check for them
                                                -- Thanks to Joe Kovar for correcting a bug in the following lines
                                                intSyllableCount = intWordPartCount + intAffixCount + 2 * intDoubleAffixCount + 3 * intTripleAffixCount - snd (foldl' matchAndCount (singularWord4, 0) arrSubSyllables) + snd (foldl' matchAndCount (singularWord4, 0) arrAddSyllables) in
                                                if intSyllableCount == 0 then 1 else intSyllableCount

                                                

{- | Returns total syllable count for text.
-}
totalSyllables :: String -- ^ Text to be measured
               -> Int
totalSyllables strText = 
    let arrWords = words strText in
        sum (map syllableCount arrWords)

{- | Returns average syllables per word for text.
-}
averageSyllablesPerWord :: String -- ^ Text to be measured
                        -> Double
averageSyllablesPerWord strText =
    let arrWords = words strText
        intWordCount = length arrWords
        intSyllableCount = totalSyllables strText in
        fromIntegral intSyllableCount / fromIntegral intWordCount

{- | Returns the number of words with more than three syllables
-}
wordsWithThreeSyllables :: String -- ^ Text to be measured
                        -> Bool -- ^ should proper nouns be included in words count
                        -> Int
wordsWithThreeSyllables strText blnCountProperNouns =
    let arrWords = words strText in
        length (filter (> 2) (map syllableCount (filter (\strWord -> blnCountProperNouns || isLower (head strWord)) arrWords)))

{- | Returns the percentage of words with more than three syllables
-}    
percentageWordsWithThreeSyllables :: String -- ^ Text to be measured
                                  -> Bool -- ^ should proper nouns be included in words count
                                  -> Double
percentageWordsWithThreeSyllables strText blnCountProperNouns =
    let arrWords = words strText
        intWordCount = length arrWords
        intLongWordCount = wordsWithThreeSyllables strText blnCountProperNouns in
        fromIntegral intLongWordCount / fromIntegral intWordCount
