module Data.Coinductive.Graph where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- | A type representing directed graphs where every two vertices determine
-- a unique edge and the edges are labeled.
-- Fundamentally, a graph can be viewed as a function which associates each
-- vertex in the space of vertices to a (possibly empty) collection of vertices
-- from that same space. In this representation, each association is also labeled.
-- Graphs are therefore _observed_ through this relation given by the function
-- 'successors', instead of _constructed_ from a set of nodes and labels. Therefore,
-- the 'Graph' type can be considered to be _coinductive_ instead of _inductive_.
newtype Graph l a = Graph { successors :: a -> Map a l }

-- | The empty graph is represented by the function which does not associate any vertex
-- to any other vertex.
empty :: Graph l a
empty = Graph $ const Map.empty

-- | Adding an edge means extending the successors of a vertex. Note that if the
-- edge already existed, the label will be overwritten with the new label.
addEdge :: Ord a => a -> a -> l -> Graph l a -> Graph l a 
addEdge v1 v2 lbl graph = Graph $ \current ->
  if current == v1
    then Map.insert v2 lbl (successors graph v1)
    else successors graph current

-- Removing an edge means restricting the successors of a vertex.
removeEdge :: Ord a => a -> a -> Graph l a -> Graph l a
removeEdge v1 v2 graph = Graph $ \current ->
  if current == v1
    then Map.delete v2 (successors graph v1)
    else successors graph current

getEdgeLabel :: Ord a => a -> a -> Graph l a -> Maybe l
getEdgeLabel v1 v2 graph = 
  Map.lookup v2 (successors graph v1)

-- | Unfolds the graph in a breadth-first order. Produces a potentially
-- infinite list of edges, such as in the case of cyclic graphs.
unfoldBreadthFirst :: Ord a => a -> Graph l a -> [(a, l, a)]
unfoldBreadthFirst start graph = go (Seq.singleton start)
  where
    go Seq.Empty = []
    go (current Seq.:<| rest) = 
      let succs = successors graph current
          vertices = Map.keys succs
          edges = [ (current, l, next) | (next, l) <- Map.toList succs ]
          queue = rest <> Seq.fromList vertices
       in edges <> go queue

-- | Unfolds the graph in a depth-first order. Produces a potentially infinite
-- list of edges, such as in the case of cyclic graphs.
unfoldDepthFirst :: Ord a => a -> Graph l a -> [(a, l, a)]
unfoldDepthFirst start graph =
  let nexts = Map.toList $ successors graph start
   in
      foldMap
        (\(vertex, label) ->
          (start, label, vertex) : unfoldDepthFirst vertex graph
        )
        nexts

-- TODO: shortest path extraction from the stream produced by unfoldBreadthFirst