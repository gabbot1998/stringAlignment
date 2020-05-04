import Debug.Trace

main = do print (sim "hej" "hej")

scoreMatch = 1
scoreMisMatch = -1
scoreSpace = -2
minimumInteger = -99999

similarityScore :: String -> String -> Int
similarityScore _ [] = 0
similarityScore [] _ = 0
similarityScore (s1:ss1) (s2:ss2)
  | s1 == s2 = scoreMatch + similarityScore ss1 ss2
  | s1 == '-' || s2 == '-' = scoreSpace + similarityScore ss1 ss2
  | otherwise = scoreMisMatch + similarityScore ss1 ss2

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: (Integral b) => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = maximaByHelp valueFcn xs 0 -- Calls with largest n = 0

maximaByHelp :: (Ord b) => (a -> b) -> [a] -> b -> [a]
maximaByHelp _ [] _ = []
maximaByHelp valueFcn (x:xs) max_n
  | val < max_n = maximaByHelp valueFcn xs max_n
  | otherwise = x : maximaByHelp valueFcn xs val
  where val = valueFcn x

type AlignmentType = (String,String)
-- optAlignments :: String -> String -> [AlignmentType]

sim :: String -> String -> Int
sim [] [] = 0
sim (x:xs) [] = sim xs [] + score [x] "-"
sim [] (y:ys) = sim [] ys + score [y] "-"
sim (x:xs) (y:ys) = max (max (sim xs ys + score [x] [y]) (sim xs (y:ys) + score [x] "-"))
                          (sim (x:xs) ys + score "-" [y] )
score = similarityScore

--



mcsLength :: String -> String -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> Int
    mcsEntry 0 0 = 0
    mcsEntry _ 0 = scoreSpace
    mcsEntry 0 _ = scoreSpace
    mcsEntry i j = max (max (score [x] "-" + mcsLen (i-1) j)
                        (score "-" [y] + mcsLen i (j-1)))
                        (mcsLen (i-1) (j-1) + score [x] [y])
      where
         x = xs!!(i-1)
         y = ys!!(j-1)


lieToMe = show("Gabbe was involved in writing this code")
truth = show("Mahir made this code alone")
