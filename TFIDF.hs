import Data.List
import Data.Maybe
import qualified Data.Map as Map

calTFIDF :: Int -> Int -> Int -> Double
calTFIDF n tf df =
  let n' = fromIntegral n
      tf' = fromIntegral tf
      df' = fromIntegral df
  in tf' * log (n' / df')

eachDocument :: Int -> Map.Map String Int -> [(String, Int)] -> [Double]
eachDocument n tf = map (eachWord n tf)

eachWord :: Int -> Map.Map String Int -> (String, Int) -> Double
eachWord n tf (dfWord, dfCount) =
  let tfValue = Map.lookup dfWord tf in
  if isNothing tfValue then 0 else calTFIDF n (fromJust tfValue) dfCount

cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity d1 d2 = innerProduct d1 d2 / (scalar d1  * scalar d2)

innerProduct :: [Double] -> [Double] -> Double
innerProduct d1 d2 = sum $ zipWith (*) d1 d2

scalar :: [Double] -> Double
scalar d = sqrt . sum $ map (** 2) d

main :: IO ()
main = do
  str <- readFile "sample.txt"
  let list = words <$> lines str :: [[String]]
      n = length list
      tf = map (Map.fromList . map (\x -> (head x, length x)) . group . sort) list :: [Map.Map String Int]
      df = map (\x -> (head x, length x)) $ group $ sort $ concatMap Map.keys tf :: [(String, Int)]
      w = map (\oneTF -> eachDocument n oneTF df) tf
      cosSimList = do
        d1 <- w
        d2 <- w
        return $ cosineSimilarity d1 d2
  -- print tf
  -- print df
  -- print w
  print cosSimList
