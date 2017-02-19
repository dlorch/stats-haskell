module Main where

import NSFG
import BabyBoom
import Data.List (group, sort, genericLength)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)

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

percentileRank scores yourScore = 100 * count / numScores
    where numScores = genericLength scores
          count = genericLength (filter (<= yourScore) scores)

percentile scores percentileRank = sortedScores !! index
    where sortedScores = sort scores
          index = truncate(percentileRank * ((genericLength sortedScores) - 1) / 100)

cdf sample x = count / numSamples
    where numSamples = genericLength sample
          count = genericLength (filter (<= x) sample)

toDoublePair :: (Integer, Int) -> (Double, Double)
toDoublePair (a, b) = (realToFrac a, realToFrac b)

exponentialDistributionCDF :: Double -> [Double] -> [(Double, Double)]
exponentialDistributionCDF lambda xs = [ (x, 1 - exp(-lambda * x)) | x <- xs ]

cdfDistribution :: [Double] -> [Double] -> [(Double, Double)]
cdfDistribution sample xs = [ (x, cdf sample x) | x <- xs ]

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
    -- draw histogram of live pregnancy lengths
    let fempreg_prglngth_freqs = map toDoublePair (frequencies fempreg_live_prglngth)
    toFile def "charts/fempreg_prglngth.svg" $ do
        layout_title .= "Histogram of pregnancy length in weeks"
        setColors [opaque blue]
        plot (line "prglngth" [fempreg_prglngth_freqs])

    let freqs = frequencies [1, 2, 2, 3, 5]
    -- PMF is {1: 0.2, 2: 0.4, 3: 0.2, 5: 0.2}
    let pmf = normalize freqs
    print pmf
    print (pmfMean pmf)
    print (pmfVar pmf)

    let scores = [55, 66, 77, 88, 99]
    -- the percentile rank is the fraction of people who scored lower than you (or the same) (here: 80)
    print (percentileRank scores 88)
    -- the 50th percentile is the value with percentile rank 50 (here: 77)
    print (percentile scores 50)
    -- the interquartile range is a measure of the spread of a distribution
    let iqr = (percentile scores 75) - (percentile scores 25)
    print iqr
    
    let sample = [1, 2, 2, 3, 5]
    -- the cumulative distribution function maps from a value to its percentile rank
    print (cdf sample 0)
    print (cdf sample 1)
    print (cdf sample 2)
    print (cdf sample 3)
    print (cdf sample 4)
    print (cdf sample 5)

    toFile def "charts/exponential_distribution_cdf.svg" $ do
        layout_title .= "CDFs of exponential distributions with various parameters"
        setColors [opaque darkblue, opaque blue, opaque cyan]
        plot (line "lambda = 2"   [exponentialDistributionCDF 2   [0,(0.01)..3.0]])
        plot (line "lambda = 1"   [exponentialDistributionCDF 1   [0,(0.01)..3.0]])
        plot (line "lambda = 0.5" [exponentialDistributionCDF 0.5 [0,(0.01)..3.0]])
    
    toFile def "charts/babyboom_birthtimes_cdf.svg" $ do
        layout_title .= "CDF of birth interarrival times"
        setColors [opaque darkblue]
        plot (line "CDF" [cdfDistribution babyboom_minutes_diff [0,(0.5)..160]])