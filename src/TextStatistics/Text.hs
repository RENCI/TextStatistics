module TextStatistics.Text where

import Text.Regex
import Data.List (dropWhileEnd)
import TextStatistics.Utils
import Data.Char (toLower)

lowerCaseWordsFollowingTerminators :: String -> String
lowerCaseWordsFollowingTerminators strText =
    case matchRegexAll (mkRegex "\\. [^. ]") strText of
        Nothing -> strText
        Just (pre, str, post, _) -> pre ++ map toLower str ++ lowerCaseWordsFollowingTerminators post

{- | 
   Trims, removes line breaks, multiple spaces and generally cleans text
   before processing.
   -}
cleanText :: String -- ^ Text to be transformed
          -> String
cleanText strText =    
    -- Replace periods within numbers
    let strText2 = subRegex (mkRegex "([^0-9][0-9]+)\\.([0-9]+[^0-9])") strText "\\10\\2"

        -- Assume blank lines (i.e., paragraph breaks) end sentences (useful
        -- for titles in plain text documents) and replace remaining new
        -- lines with spaces
        strText3 = subRegex (mkRegex "(\\r\\n|\\n\\r)") strText2 "\n"
        strText4 = subRegex (mkRegex "(\\r|\\n){2,}") strText3 ".\n\n"
        strText5 = subRegex (mkRegex "[ ]*(\\n|\\r\\n|\\r)[ ]*") strText4 " "

        -- Replace commas, hyphens, quotes etc (count as spaces)
        strText6 = subRegex (mkRegex "[\",:;()/`-]") strText5 " "

        -- Unify terminators and spaces
        strText7 = dropWhileEnd (\c -> c == '.' || c == ' ') strText6 ++ "." -- Add final terminator.
        strText8 = subRegex (mkRegex "[.!?]") strText7 "." -- Unify terminators
        strText9 = subRegex (mkRegex "([.\\s]*\\.[.\\s]*)") strText8 ". " -- Merge terminators separated by whitespace.
        strText10 = subRegex (mkRegex "[ ]+") strText9 " " -- Remove multiple spaces
        strText11 = subRegex (mkRegex "([.])[. ]+") strText10 "\\1" -- Check for duplicated terminators
        strText12 = strip (subRegex (mkRegex "[ ]*([.])") strText11 "\\1 ") -- Pad sentence terminators

        -- Lower case all words following terminators (for gunning fog score)
        strText13 = lowerCaseWordsFollowingTerminators strText12

        strText14 = strip strText13 in
        strText14

{- |
   Gives letter count (ignores all non-letters). Tries mb_strlen and if
   that fails uses regular strlen.
-}
letterCount :: String -- ^ Text to be measured
            -> Int
letterCount strText =
    let strText3 = subRegex (mkRegex "[^A-Za-z]+") strText "" in
        length strText3

{- |
   Returns word count for text.
   -}
wordCount :: String -- ^ Text to be measured
          -> Int
wordCount strText =
        length (words strText)

{- |
   Returns sentence count for text.
    -}
sentenceCount :: String -- ^ Text to be measured
              -> Int
sentenceCount strText =
    -- Will be tripped up by "Mr." or "U.K.". Not a major concern at this point.
    let strText3 = subRegex (mkRegex "[^.!?]") strText "" in
        length strText3

{- |
   Returns average words per sentence for text.
-}    
averageWordsPerSentence :: String -- ^ Text to be measured
                        -> Double
averageWordsPerSentence strText =
    let intSentenceCount = sentenceCount strText
        intWordCount = wordCount strText

        averageWords = fromIntegral intWordCount / fromIntegral intSentenceCount in
        averageWords
