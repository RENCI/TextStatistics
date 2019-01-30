{-|

    TextStatistics Project
    https://github.com/DaveChild/Text-Statistics

    Released under New BSD license
    http://www.opensource.org/licenses/bsd-license.php

    Calculates following readability scores (formulae can be found in Wikipedia):
      * Flesch Kincaid Reading Ease
      * Flesch Kincaid Grade Level
      * Gunning Fog Score
      * Coleman Liau Index
      * SMOG Index
      * Automated Reability Index
      * Dale-Chall Readability Score
      * Spache Readability Score

    Will also give:
      * String length
      * Letter count
      * Syllable count
      * Sentence count
      * Average words per sentence
      * Average syllables per word

    Sample Code
    ----------------
    $statistics = new DaveChild\TextStatistics\TextStatistics;
    $text = 'The quick brown fox jumped over the lazy dog.';
    echo 'Flesch-Kincaid Reading Ease: ' . $statistics->flesch_kincaid_reading_ease($text);

-}


module TextStatistics.TextStatistics where

import Data.List (nub)
import Text.Regex (subRegex, mkRegex)
import TextStatistics.DaleChallWordList
import TextStatistics.SpacheWordList
import TextStatistics.Pluralise
import TextStatistics.Syllables
import TextStatistics.Text

{- | 
   Gives the Flesch-Kincaid Reading Ease of text entered
-}
fleschKincaidReadingEase :: String -- ^ Text to be checked
                         -> Double
fleschKincaidReadingEase strText =
    206.835 - 1.015 * averageWordsPerSentence strText - 84.6 * averageSyllablesPerWord strText


{- |
   Gives the Flesch-Kincaid Grade level of text entered
-}
fleschKincaidGradeLevel :: String -- ^ Text to be checked
                        -> Double
fleschKincaidGradeLevel strText =
  0.39 * averageWordsPerSentence strText + 11.8 * averageSyllablesPerWord strText - 15.59

{- |
   Gives the Gunning-Fog score of text entered
-}
gunningFogScore :: String -- ^ Text to be checked
                -> Double
gunningFogScore strText =
    (averageWordsPerSentence strText + percentageWordsWithThreeSyllables strText False) * 0.4

{- |
   Gives the Coleman-Liau Index of text entered
   -}
colemanLiauIndex :: String -- ^ Text to be checked
                 -> Double
colemanLiauIndex strText =
    0.0588 * fromIntegral (letterCount strText) / fromIntegral (wordCount strText) * 100 - 0.296 * fromIntegral (sentenceCount strText) / fromIntegral (wordCount strText) * 100 - 15.8

{- |
   Gives the SMOG Index of text entered
   -}
smogIndex :: String -- ^ Text to be checked
          -> Double
smogIndex strText =
    1.043 * (sqrt (fromIntegral (wordsWithThreeSyllables strText True) * 30 / fromIntegral (sentenceCount strText)) + 3.1291)

{- |
   Gives the Automated Readability Index of text entered
   -}
automatedReadabilityIndex :: String -- ^ Text to be checked
                          -> Double
automatedReadabilityIndex strText =
    4.71 * fromIntegral (letterCount strText) / fromIntegral (wordCount strText) + 0.5 * fromIntegral (wordCount strText) / fromIntegral (sentenceCount strText) - 21.43

{- |
   Gives the Dale-Chall readability score of text entered
   -}
daleChallReadabilityScore :: String -- ^ Text to be checked
                          -> Double
daleChallReadabilityScore strText =
    0.1579 * 100 * fromIntegral (daleChallDifficultWordCount strText) / fromIntegral (wordCount strText) + 0.0496 * fromIntegral (wordCount strText) / fromIntegral (sentenceCount strText)

{- |
   Gives the Spache readability score of text entered
-}
spacheReadabilityScore :: String -- ^ Text to be checked
                       -> Double
spacheReadabilityScore strText =
    0.121 * fromIntegral (wordCount strText) / fromIntegral (sentenceCount strText) + 0.082 * fromIntegral (spacheDifficultWordCount strText) + 0.659

{- |
   Returns the number of words NOT on the Dale-Chall easy word list
   -}
daleChallDifficultWordCount :: String -- ^ Text to be measured
                            -> Int
daleChallDifficultWordCount strText =
    let arrWords = words (subRegex (mkRegex "[^A-za-z' ]") strText "") in
        length (filter (\word ->
            let singular = getSingular word
                plural = getPlural word in
                length word >=2 && not (singular `elem` arrDaleChallWordList) && not (plural `elem` arrDaleChallWordList)) arrWords)

{- |
   Returns the number of unique words NOT on the Spache easy word list
   -}
spacheDifficultWordCount :: String -- ^ Text to be measured
                         -> Int
spacheDifficultWordCount strText =
    let arrWords = words (subRegex (mkRegex "[^A-za-z' ]") strText "") in
        length (nub (filter (\word ->
            let plural = getPlural word in
                length word >=2 && not (word `elem` arrSpacheWordList) && not (plural `elem` arrSpacheWordList)) (map getSingular arrWords)))

