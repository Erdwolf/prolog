{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GraphViz where

import Control.Applicative ((<$>), Applicative(..))
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.List (intercalate, intersperse)

import qualified Data.Graph.Inductive as Graph
import Data.HashTable (hashString)

import Data.GraphViz (preview, runGraphviz, setDirectedness, graphToDot, GraphvizOutput(Png), toLabel, nonClusteredParams, GraphvizParams(..))
import Data.GraphViz.Attributes.Colors (Color(X11Color), X11Color(..))
import Data.GraphViz.Attributes.HTML

import Prolog


-- Graphical output of derivation tree
resolveTree p q = preview =<< execGraphGenT (resolve_ p q)

resolveTreeToFile path p q = do
 graph <- execGraphGenT (resolve_ p q)
 runGraphviz (setDirectedness graphToDot params graph) Png path
   where
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                }


type Graph = Graph.Gr HtmlText HtmlText

newtype GraphGenT m a = GraphGenT (StateT Graph m a) deriving (Monad, Functor, MonadFix, MonadPlus, Applicative, MonadError e, MonadState Graph, MonadTrans)
runGraphGenT (GraphGenT st) = runStateT st Graph.empty
execGraphGenT (GraphGenT st) = execStateT st Graph.empty


instance Monad m => MonadGraphGen (GraphGenT m) where

   createConnections usf gs branches = do
      let current = hash (usf, gs)
      let label = makeNodeLabel usf gs
      ensureNode current label
      forM_ branches $ \x@(u',gs') -> do
         connect current u' gs'
      when (null branches) $ do
         modifyLabel current (colorize Red)

   markSolution usf = do
      let current = hash (usf,[])
      modifyLabel current (colorize Green)

   markCutBranches stackPrefix = do
      forM_ stackPrefix $ \(u_,gs_,alts_) -> do
         forM_ alts_ $ \x -> do
            let child = hash x
            modifyLabel child (colorize Gray)


ensureNode node label =
   modify $ \graph ->
      if Graph.gelem node graph
         then graph
         else Graph.insNode (node, label) graph

makeNodeLabel _ [] = [HtmlStr "[]"]
makeNodeLabel _ gs = [HtmlStr $ intercalate "," $ map show gs]

makeEdgeLabel [] _ = [HtmlFont [HtmlPointSize 8] [HtmlStr "{}"]]
makeEdgeLabel u  _ = [HtmlFont [HtmlPointSize 8] $ intersperse (HtmlNewline []) [HtmlStr $ show v ++ " = " ++ show t | (v,t) <- u]]

modifyLabel node f = do
   modify $ Graph.gmap $ \cxt@(in_,node',label,out) ->
      if node == node'
         then (in_, node', f label, out)
         else cxt

colorize color label = [HtmlFont [HtmlColor (X11Color color)] label]

connect current u gs = do
   let new = hash (u, gs)
   modify $ Graph.insNode (new, makeNodeLabel u gs)
   modify $ Graph.insEdge (current, new, makeEdgeLabel u gs)

hash = fromEnum . hashString . show
