module Main where

import qualified Data.Map as Map

{-
Alternatively, to avoid the lambda.
insertEdge m (x:y:_) = Map.insert x (y: Map.findWithDefault [] x m) (Map.insert y (x: Map.findWithDefault [] y m) m)
    gg = foldl insertEdge Map.empty edges
-}
countSubTrees :: Int -> [[Int]] -> String -> [Int]
countSubTrees n edges labels =
    let gg = foldl (\m (x:y:_) -> Map.insert x (y: Map.findWithDefault [] x m) (Map.insert y (x: Map.findWithDefault [] y m) m)) Map.empty edges
        ctr = foldl (\m k -> Map.insert k (Map.singleton (labels !! k) 1) m) Map.empty (Map.keys gg)
        dq = [k | k <- Map.keys gg, length (gg Map.! k) == 1, k /= 0]
    in go dq ctr gg
  where go dq ctr gg
            | null dq = map (\i -> Map.findWithDefault 0 (labels !! i) (ctr Map.! i)) [0..n-1]
            | otherwise = let cur = head dq
                              dq' = tail dq
                              parent = head (gg Map.! cur)
                              gg' = Map.adjust (filter (/= cur)) parent gg
                              ctr' = Map.adjust (Map.unionWith (+) (ctr Map.! cur)) parent ctr
                              dq'' = if parent /= 0 && length (gg' Map.! parent) == 1 then parent : dq' else dq'
                          in go dq'' ctr' gg'


testCountSubTrees :: IO ()
testCountSubTrees =
  let edges = [[0,1],[0,2],[1,3],[0,4]]
      labels = "aabab"
      n = 5
      expectedOutput = [3,2,1,1,1]
      result = countSubTrees n edges labels
  in if result == expectedOutput
      then putStrLn "test passed"
      else putStrLn $ "test failed, expected: " ++ show expectedOutput ++ " but got: " ++ show result

main :: IO()
main = do
  let edges = [[0,1],[0,2],[1,4],[1,5],[2,3],[2,6]]
      labels = "abaedcd"
      n = 7
      output = countSubTrees n edges labels
  -- [2,1,1,1,1,1,1] expected
  print output

  testCountSubTrees
