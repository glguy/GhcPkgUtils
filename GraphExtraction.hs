module GraphExtraction where

import Data.Graph
import Data.Array
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

type VertexSet = IntSet

opGraph :: Graph -> Graph
opGraph g = buildG (bounds g) [ (y,x) | (x,y) <- edges g]

reachableSet :: Graph -> Vertex -> VertexSet
reachableSet g v = IntSet.fromList (reachable g v)

extract :: Graph -> [Vertex] -> [Vertex]
extract g vs = filter (`IntSet.member` extractSet) sortedVs
  where
    g' = opGraph g
    sortedVs = topSort g
    extractSet = foldMap (reachableSet g') vs
