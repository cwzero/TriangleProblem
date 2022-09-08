module TriangleTypes(Graph, countTriangles) where

import qualified Data.List as List
import Data.Maybe

-- Type synonyms
type Vertex = Char
type Line = [Vertex]
type Graph = [Line]

-- Custom Data Type for Result
data Triangle = Triangle (Vertex, Vertex, Vertex)
instance Show Triangle where
    show (Triangle (v1, v2, v3)) = List.sort [v1, v2, v3]
instance Eq Triangle where
    Triangle (v1, v2, v3) == Triangle (v4, v5, v6) =
        List.sort [v1, v2, v3] == List.sort [v4, v5, v6]

-- State Type
data LineTrace = Empty | Single Line | Double (Line, Line) | Triple (Line, Line, Line)

-- Basic Helper Function
flatMap :: (t -> [a]) -> [t] -> [a]  
flatMap _ [] = []  
flatMap f (x:xs) = f x ++ flatMap f xs

-- Do the 2 given lines intersect?
intersect :: Line -> Line -> Bool
intersect l1 l2 = not $ List.null $ List.intersect l1 l2

-- Where do the 2 given lines intersect (if they do)?
findIntersection :: Line -> Line -> Maybe Vertex
findIntersection l1 l2
    | intersect l1 l2 = Just (head (filter (\e -> elem e l2) l1))
    | otherwise = Nothing

-- The payload - count the triangles in a given graph
countTriangles :: Graph -> Int
countTriangles g = length $ findTriangles g

-- Get a List of Triangles in the Given Graph
-- Use recursive go from https://kowainik.github.io/posts/haskell-mini-patterns
findTriangles :: Graph -> [Triangle]
findTriangles g = List.nub $ flatMap (go Empty) g
    where
        go :: LineTrace -> Line -> [Triangle]
        go (Triple _) _ = []
        go (Double (l1, l2)) l3 = map fromJust $ filter isJust [makeTriangle (Triple (l1, l2, l3))]
        go (Single l1) l2
            | validate (Double (l1, l2)) = flatMap (go (Double (l1, l2))) g
            | otherwise = []
        go Empty l1 = flatMap (go (Single l1)) g

-- Make a Triangle from a Triple (or nothing from a lesser trace)
makeTriangle :: LineTrace -> Maybe Triangle
makeTriangle (Triple (l1, l2, l3))
    | validate (Triple (l1, l2, l3)) = Just (Triangle (fromJust (findIntersection l1 l2), fromJust (findIntersection l1 l3), fromJust (findIntersection l2 l3)))
    | otherwise = Nothing
makeTriangle _ = Nothing

-- Is a given trace valid
validate :: LineTrace -> Bool
validate Empty = True
validate (Single _) = True
validate (Double (l1, l2)) = (l1 /= l2) && (intersect l1 l2)
validate (Triple (l1, l2, l3)) = (l1 /= l3) && (l2 /= l3) && (intersect l1 l3) && (intersect l2 l3) && (validateTriangle l1 l2 l3)

-- Is a given triple a triangle
validateTriangle :: Line -> Line -> Line -> Bool
validateTriangle l1 l2 l3 = (findIntersection l1 l2) /= (findIntersection l1 l3) && (findIntersection l1 l2) /= (findIntersection l2 l3) && (findIntersection l1 l3) /= (findIntersection l2 l3)