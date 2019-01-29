module TextStatistics.Maths where

{-|
  Normalises score according to min & max allowed. If score larger
  than max, max is returned. If score less than min, min is returned.
  Also rounds result to specified precision.
  Thanks to github.com/lvil.
-}
normaliseScore :: Ord a 
               => a -- ^ Initial score
               -> a -- ^ Minimum score allowed
               -> a -- ^ Maximum score allowed
               -> a
normaliseScore score min max =
    if score > max
        then max
        else if score < min
            then min
            else score
