-- K-Nearest Neighbors implementation in Haskell
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Data
input    = [1,2,3]
training = [(1, [1,2,3]), (1, [4,5,6]), (2, [1,0,3])]

-- Maximum of neighbors to consider
kn = 3

-- Utils
getFirst (x,_)  = x
getSecond (_,x) = x
absSubtract x y = abs (subtract x y)

-- pairwise distance of list elements
pairwiseDistance :: (Num a, Eq a) => [a] -> [a] -> [a]
pairwiseDistance x y = zipWith absSubtract x y

-- replace labeled data (_,yd) with the sum of its pairwise distance to x
distanceToLabeledData :: (Num a, Eq a) => [a] -> (b, [a]) -> (b, a)
distanceToLabeledData x (yl,yd) = (yl, sum (pairwiseDistance x yd))

-- Partial to always measure DistanceToLabeledData using defined input variable
distanceToLabeledPartial = distanceToLabeledData input

-- Convert all labeled data pairs, to (label, sum of pairwise distance to input) pairs
allLabelsWithDistances   = map distanceToLabeledPartial training

-- Sort list of labeled data by sum of pairwise distance to input data
quicksort :: (Ord a) => [(b, a)] -> [(b, a)]
quicksort []     = []
quicksort (x:xs) =
    let smaller  = quicksort [y | y <- xs, (getSecond y) <= (getSecond x)]
        bigger   = quicksort [y | y <- xs, (getSecond y) > (getSecond x)]
    in smaller ++ [x] ++ bigger  

-- make sorted list of training data by input
sortedLabeledData = quicksort allLabelsWithDistances

-- Count most prevalent label in k closest training data examples
countLabels       = Map.unionsWith (+) . map (\(k, v) -> Map.singleton k 1)
labelCounts k     = countLabels (take k sortedLabeledData)
sortedLabelCounts = reverse (quicksort (Map.toList (labelCounts kn)))

-- Get Prediction by getting the label of the first element in the sorted and transformed training data
prediction = getFirst (head sortedLabelCounts)

