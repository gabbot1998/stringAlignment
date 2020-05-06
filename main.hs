import Debug.Trace

scoreMatch = 0
scoreMisMatch = -1
scoreSpace = -1
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
optAlignments xs ys = snd (optLen (length xs) (length ys))
  where
    optLen i j = optTable!!i!!j
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [("", "")]) -- End of String
    optEntry i 0 = (scoreSpace * i, [((take i xs), replicate i '-')]) -- If last character on one string, rest should be spaces
    optEntry 0 j = (scoreSpace * j, [(replicate j '-', (take j ys))]) -- Same same, but different
    -- Do magic --
    optEntry i j = foldr (\(a,b) (c,d) -> (a, b ++ d)) (0,[]) (maximaBy fst -- Appends AlignmentType's of the max options
                                            [(handleOptEntry x '-' (optLen (i-1) j)),
                                            (handleOptEntry '-' y (optLen i (j-1))),
                                            (handleOptEntry x y (optLen (i-1) (j-1))) ])
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

handleOptEntry :: Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
handleOptEntry x y optEntry
  | x == '-' || y == '-'  = ( (fst optEntry + scoreSpace)    ,  attachTails x y (snd (optEntry)) ) -- Add scoreSpance and attach Space
  | x == y                = ( (fst optEntry + scoreMatch)    ,  attachTails x y (snd (optEntry)) ) -- Add scoreMatch  and attach x & y
  | otherwise             = ( (fst optEntry + scoreMisMatch) ,  attachTails x y (snd (optEntry)) ) -- Add scoreMissMatch  and attach x & y

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments x y = do
  let a = optAlignments x y
  putStrLn ("There are " ++ show (length a) ++ " optimal alignments")
  mapM_ (putStrLn . f) a
    where
      f (a, b) = "\n" ++ g a ++ "\n" ++ g b
      g = unwords . map return

lieToMe = show("Gabbe was involved in writing this code")
truth = show("Mahir made this code alone")
