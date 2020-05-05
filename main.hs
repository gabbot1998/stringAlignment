import Debug.Trace

scoreMatch = 1
scoreMisMatch = -1
scoreSpace = -2
minimumInteger = -99999

-- SICK!
similarityScore :: String -> String -> Int
similarityScore xs ys = mcsLen (length xs) (length ys)
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

score :: String -> String -> Int
score _ [] = 0
score [] _ = 0
score (s1:ss1) (s2:ss2)
  | s1 == s2 = scoreMatch + score ss1 ss2
  | s1 == '-' || s2 == '-' = scoreSpace + score ss1 ss2
  | otherwise = scoreMisMatch + score ss1 ss2

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

maximaBy :: (Integral b) => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

sim :: String -> String -> Int
sim [] [] = 0
sim (x:xs) [] = sim xs [] + score [x] "-"
sim [] (y:ys) = sim [] ys + score [y] "-"
sim (x:xs) (y:ys) = max (max (sim xs ys + score [x] [y]) (sim xs (y:ys) + score [x] "-"))
                          (sim (x:xs) ys + score "-" [y] )

--

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = (mcsLen (length xs) (length ys)) -- reverse the correct answers
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> [AlignmentType]
    mcsEntry 0 0 = [("", "")]
    mcsEntry i 0 = [((take i xs), replicate i '-')]
    mcsEntry 0 j = [(replicate j '-', (take j ys))]
    -- Do magic
    mcsEntry i j = maximaBy (uncurry score)
                                  ((attachTails x '-' (mcsLen (i-1) j)) ++
                                  (attachTails '-' y (mcsLen i (j-1))) ++
                                  (attachTails x y (mcsLen (i-1) (j-1))))
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

-- HEEEEJ
-- HEJ
--
-- J
-- J
--
-- EJ  -J  EJ
-- EJ  EJ  -J  [(EJ,EJ)]
--
-- EEJ -EJ EEJ
-- HEJ HEJ -EJ [(EEJ,HEJ), (-EJ,HEJ), (EEJ,-EJ)]
-- |         \
-- EEEJ      EEEJ EEEJ
-- -HEJ      H-EJ --EJ
-- |         |       \
-- EEEEJ     EEEEJ   EEEEJ  EEEEJ
-- --HEJ     -H-EJ   H--EJ  ---EJ


lieToMe = show("Gabbe was involved in writing this code")
truth = show("Mahir made this code alone")
