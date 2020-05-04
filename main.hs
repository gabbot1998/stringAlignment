
scoreMatch = 1
scoreMisMatch = -1
scoreSpace = -2
minimumInteger = -99999

type ALignmentType = (String, String)

similarityScore :: String -> String -> Int
similarityScore _ [] = 0
similarityScore [] _ = 0
similarityScore (s1:ss1) (s2:ss2)
  | s1 == s2 = scoreMatch + similarityScore ss1 ss2
  | s1 == '-' || s2 == '-' = scoreSpace + similarityScore ss1 ss2
  | otherwise = scoreMisMatch + similarityScore ss1 ss2

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: (a -> Int) -> [a] -> [a]
maximaBy valueFcn xs = maximaByHelp valueFcn xs [] minimumInteger

--Varför fungerar inte typparametrarna som vi tror att de ska göra! :O
maximaByHelp :: (a -> Int) -> [a] -> [a] -> Int -> [a]
maximaByHelp _ [] ys _ = ys
maximaByHelp valueFcn (x:xs) ys max_n
  | val < max_n = maximaByHelp valueFcn xs ys max_n
  | val > max_n = maximaByHelp valueFcn xs [x] val
  | otherwise = maximaByHelp valueFcn xs (x:ys) val
  where val = valueFcn x

-- optAlignments :: String -> String -> [AlignmentType]
-- optAlignments string1 string2 =

lieToMe = show("Gabbe made this code alone")
truth = show("Mahir made this code alone")
