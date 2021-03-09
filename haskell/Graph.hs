module Graph where

import qualified Data.Map as M
import Data.List (partition)

data Graph = G [Vertex] [Arc]
-- graph(α, β) = set(α) × set(α =β=> a)
type Vertex = Int
type Label = Int
data Arc = Arc Vertex Label Vertex deriving (Eq)

vertices :: Graph -> [Vertex]
vertices (G vs _) = vs

arcs :: Graph -> [Arc]
arcs (G _ cs) = cs

sourceOf, destinationOf :: Arc -> Vertex
sourceOf (Arc x _ _) = x
destinationOf (Arc _ _ x) = x

labelOf :: Arc -> Label
labelOf (Arc _ x _) = x

emptyG :: Graph
emptyG = G [] []

put :: Eq a => a -> [a] -> [a]
put x ys = if x `elem` ys then ys else (x:ys)

addVertex :: Vertex -> Graph -> Graph
addVertex v (G vs cs) = G (put v vs) cs

addArc :: Arc -> Graph -> Graph
addArc c@(Arc t _ h) (G vs cs) = if (h `elem` vs && t `elem` vs) then G vs (put c cs) else error "Vertex not in Graph"

addArcF :: Arc -> Graph -> Graph
-- no safe guard
addArcF c (G vs cs) = G vs (put c cs)

deleteArc :: Vertex -> Vertex -> Graph -> Graph
deleteArc t h (G vs cs) = G vs $ filter (\(Arc tail _ head) -> tail /= t || head /= h) cs

replaceArc :: Arc -> Graph -> Graph
replaceArc c@(Arc t _ h) graph = addArcF c $ deleteArc t h graph

deleteVertex :: Vertex -> Graph -> Graph
deleteVertex v (G vs cs) = G (filter (/= v) vs) (filter (\(Arc t _ h) -> t /= v && h /= v) cs)





type Weight = Maybe Label

type AdjMatrix = M.Map (Vertex,Vertex) Weight

-- Nothing => 节点间不联通

selectL :: Weight -> Weight -> Weight
-- 选择较短路径
selectL Nothing x = x
selectL x Nothing = x
selectL (Just x) (Just y) | x < y = (Just x)
                          | otherwise = (Just y)

updateL :: (Vertex,Vertex) -> Weight -> AdjMatrix -> AdjMatrix
updateL p w m = case M.lookup p m of {
                Nothing -> M.insert p w m;
                (Just x) -> M.insert p (selectL w x) m }

graphT :: Graph -> AdjMatrix
graphT (G vs cs) = fst $ foldr go (M.empty,cs) [(x,y) | x <- vs , y <- vs] where
    go :: (Vertex,Vertex) -> (AdjMatrix,[Arc]) -> (AdjMatrix,[Arc])
    go (t,h) (m,l) = let {(mono,mix) = partition (\(Arc t' _ h') -> t == t' && h == h') l} in ((foldr (walk (t,h)) (M.insert (t,h) (newContent (t,h)) m) mono), mix)
    walk :: (Vertex, Vertex) -> Arc -> AdjMatrix -> AdjMatrix
    walk p (Arc _ w _) m = updateL p (Just w) m
    newContent :: (Vertex,Vertex) -> Weight
    newContent (x,y) | x == y = Just 0
                     | otherwise = Nothing


