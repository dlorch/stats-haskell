module Main where

import NSFG
import Data.List (group, sort, genericLength)

frequencies xs = map countGroups (group (sort xs))
    where countGroups x = (head x, length x)

mode xs = findMax (frequencies xs)
    where findMax ((val, freq):[]) = val
          findMax ((val1, freq1):(val2, freq2):xs) = if freq1 > freq2
                                                     then findMax ((val1, freq1):xs)
                                                     else findMax ((val2, freq2):xs)

mean xs = realToFrac (sum xs) / genericLength xs

var xs = sum (map squared (map (deviationFromMean xs) xs)) / genericLength xs
    where squared x = x * x
          meanValue = mean xs
          deviationFromMean xs = \x -> (realToFrac x) - meanValue

std xs = sqrt (var xs)

sampleSize ((val, freq):[]) = freq
sampleSize ((val, freq):xs) = freq + (sampleSize xs)

normalize xs = map (divideBySampleSize (sampleSize xs)) xs
    where divideBySampleSize sampleSize = \(val, freq) -> (val, (realToFrac freq) / (realToFrac sampleSize))

pmfMean xs = sum (map probTimesVal xs)
    where probTimesVal (val, prob) = (realToFrac val) * prob

pmfVar xs = sum (map sumBody xs)
    where meanValue = pmfMean xs
          squared x = x * x
          sumBody (val, prob) = prob * (squared (val - meanValue))

main :: IO ()
main = do
    -- http://www.icpsr.umich.edu/nsfg6/Controller?displayPage=labelDetails&fileCode=PREG&section=&subSec=8016&srtLabel=611932
    print (frequencies fempreg_outcome)
    -- http://www.icpsr.umich.edu/nsfg6/Controller?displayPage=labelDetails&fileCode=PREG&section=&subSec=8014&srtLabel=611802
    print (frequencies fempreg_birthwgt_lb)
    -- most common birth weight is 7 pounds
    print (mode fempreg_birthwgt_lb)
    -- for all live births, mean pregnancy length is 38.6
    print (mean fempreg_live_prglngth)
    -- for all live births, variance of pregnagncy length is 7.3
    print (var fempreg_live_prglngth)
    -- for all live births, standard deviation is 2.7 weeks
    print (std fempreg_live_prglngth)

    let freqs = frequencies [1, 2, 2, 3, 5]
    -- PMF is {1: 0.2, 2: 0.4, 3: 0.2, 5: 0.2}
    let pmf = normalize freqs
    print pmf
    print (pmfMean pmf)
    print (pmfVar pmf)
