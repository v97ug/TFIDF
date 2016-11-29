module CosineSimilarity where

import Type

cosineSimilarity :: Vector -> Vector -> Double
cosineSimilarity d1 d2 = innerProduct d1 d2 / (scalar d1  * scalar d2)

innerProduct :: Vector -> Vector -> Double
innerProduct d1 d2 = sum $ zipWith (*) d1 d2

scalar :: Vector -> Double
scalar d = sqrt . sum $ map (** 2) d
