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
          deviationFromMean xs = \x -> (realToFrac x) - (mean xs)

std xs = sqrt (var xs)

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