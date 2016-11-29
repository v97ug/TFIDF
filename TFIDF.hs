import Control.Monad
import qualified Data.Map as Map

import Type
import CosineSimilarity
import CalTFIDF

printDFWords :: [(String, Int)] -> IO ()
printDFWords df = do
  putStrLn "== Word List =="
  putStrLn . unwords $ map fst df
  putStrLn ""

printDocsAndFeatureV :: [Document] -> [Vector] -> IO ()
printDocsAndFeatureV = zipWithM_ printEach
  where
    printEach :: Document -> Vector -> IO ()
    printEach doc fv = do
      putStrLn $ "--- Documents ---\n" ++ doc
      putStrLn "--- Feature Vector ---"
      print fv
      putStrLn ""

printSimilarity :: [(Document, Document, Double)] -> IO ()
printSimilarity = mapM_ $ \(d1, d2, cosSim) -> do
  putStrLn $ "D1 : " ++ d1
  putStrLn $ "D2 : " ++ d2
  putStr "similatiry : "
  print cosSim
  putStrLn ""

main :: IO ()
main = do
  str <- readFile "input/sample.txt"
  let documents = lines str :: [Document]
      n = length documents
      splitWords = map words documents
      tf = calTF splitWords :: [Map.Map String Int]
      df = calDF tf :: [(String, Int)]
      featureVectors = map (\oneTF -> eachDocument n oneTF df) tf :: [Vector]
      docAndVectors = zip documents featureVectors :: [(Document, Vector)]
      cosSimList = do
        d1 <- docAndVectors
        d2 <- docAndVectors
        return (fst d1, fst d2, cosineSimilarity (snd d1) (snd d2))

  -- 表示
  printDFWords df
  printDocsAndFeatureV documents featureVectors
  putStrLn "--- cosineSimilatity ---\n"
  printSimilarity cosSimList
