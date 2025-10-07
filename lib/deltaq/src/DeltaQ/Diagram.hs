{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-|
Copyright   : PLWORKZ R&D, 2025
License     : BSD-3-Clause
Description : Render outcome diagrams.

Render outcome expressions as outcome diagrams.
-}
module DeltaQ.Diagram
    ( -- * Outcome diagrams
      renderOutcomeDiagram

    -- * Outcome expressions
    , Expr (..)
    , var
    , maxParallel
    ) where

import Prelude hiding (last, seq)

import Diagrams.Prelude hiding (First, Last, op)
import Diagrams.Backend.SVG

out :: Expr String -> IO ()
out =
    renderSVG "xoutcomes.svg" (mkWidth 700)
    . renderOutcomeDiagram

-- | Render an outcome expression as an outcome diagram.
renderOutcomeDiagram :: Expr String -> Diagram SVG
renderOutcomeDiagram = renderTiles . layout

{-----------------------------------------------------------------------------
    Diagram rendering
------------------------------------------------------------------------------}
type X = Int
type Y = Int

data Op = OFirst | OLast
    deriving (Eq, Ord, Show)

-- | Data attached to a 'Tile'.
data Token
    = VarT String
    | Horizontal
    | Close [Y]
    | Open Op [Y]
    deriving (Eq, Ord, Show)

data Tile = Tile X Y Token
    deriving (Eq, Ord, Show)

-- | Render a collection of tiles.
renderTiles :: [Tile] -> Diagram SVG
renderTiles = frame 0.1 . position . map renderTile
  where
    renderTile (Tile x y token) =
        ( p2 (fromIntegral x, negate $ fromIntegral y)
        , renderToken token
        )

-- | Render a single 'Token' associated with a 'Tile'.
renderToken :: Token -> Diagram SVG
renderToken (VarT s)     =
    scale 0.3 (text s)
    <> (circle 0.44 & lc orange & lw 4 & fc white)
    <> hrule 1
renderToken Horizontal   = hrule 1
renderToken (Close ds)   =
    mconcat (map (renderLine . fromIntegral . negate) ds)
    <> hrule 1
  where
    renderLine d = fromVertices [p2 (0, 0), p2 (-0.5, d)] & strokeLine
renderToken (Open op ds) =
    scale 0.4 (renderOp op <> (square 1 & fc white))
    <> mconcat (map (renderLine . fromIntegral . negate) ds)
    <> hrule 1
  where
    renderOp :: Op -> Diagram SVG
    renderOp OFirst = text "∀"
    renderOp OLast  = text "∃"

    renderLine d = fromVertices [p2 (0, 0), p2 (0.5, d)] & strokeLine

{-----------------------------------------------------------------------------
    Diagram Layout
------------------------------------------------------------------------------}
{- Note [LayoutComputation]

In order to compute the layout for an outcome diagram,
we use a data structure called 'Shrub', which is a tree with labels
on the edges (as opposed to on the leaves).
The 'Shrub' represents the data that we still need to layout,
we think of the leaves as being located to the left,
and the root to the right, like this:

 leaf
 o----
      \
       o----o----o root
      /
 o----

-}

type EdgeData = (Y, Maybe (Expr String))

-- | Layout an outcome diagram.
layout :: Expr String -> [Tile]
layout t = go 0 $ twig (0, Just t)
  where
    go !x0 s0
        | isRoot s0 = []
        | otherwise = tiles <> go x1 s1
      where
        (tiles, s1) = emitColumn x0 s0
        x1 = if null tiles then x0 else x0 + 1

-- | Emit the next column, possible empty.
emitColumn :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitColumn x s
    | any (onExpr isSeq)      (foliage s) = ([], expandSeq s)
    | hasIsolatedTwigs s                  = ([], dropIsolatedTwigs s)
    | hasGroupClose s                     = emitGroupClose x s
    | any (onExpr isParallel) (foliage s) = emitParallel x s
    | any (onExpr isVar)      (foliage s) = emitVar x s
  where
    onExpr p (_, Just t) = p t
    onExpr _ _ = False

-- | Check that a given 'EdgeData' does not contain a Expr.
isSilent :: EdgeData -> Bool
isSilent (_, Nothing) = True
isSilent _ = False

-- | Expand all 'Seq' in the 'foliage' as a sequence of twigs.
expandSeq :: Shrub EdgeData -> Shrub EdgeData
expandSeq = updateFoliage expand
  where
    expand (y, Just (Seq exprs)) =
        foldl (\s expr -> Branch [((y, Just expr), s)]) root exprs
        -- Note: The Seq is left-to-right, but the Shrub is right-to-left.
        -- foldl reorders the sequence appropriately.
    expand edge = twig edge

-- | Emit a column with the next named items.
emitVar :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitVar x s =
    (map emit $ foliage s, updateFoliage dropit s)
  where
    dropit (y, Nothing          ) = twig (y, Nothing)
    dropit (y, Just (Var _)     ) = twig (y, Nothing)
    dropit _ = error "emitSeq does not expect non-Var Tiles"

    emit (y, Nothing     ) = Tile x y Horizontal
    emit (y, Just (Var v)) = Tile x y (VarT v)
    emit _ = error "emitSeq does not expect non-Var Tiles"

-- | Emit a column with the next parallel items.
emitParallel :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitParallel x s =
    (map emit $ foliage s, updateFoliage dropit s)
  where
    dropit (y, Nothing           ) = twig (y, Nothing)
    dropit (y, Just (Last  exprs)) = close y exprs
    dropit (y, Just (First exprs)) = close y exprs
    dropit (y, Just t            ) = twig (y, Just t)

    close y exprs =
        Branch [((y,Nothing), Branch [(e, root) | e <- verticals y exprs])]
    verticals y exprs =
        zipWith (\z t -> (y + z, Just t)) (distances exprs) exprs 
    distances = init . scanl (+) 0 . map maxParallel

    emit (y, Just (Last  exprs)) = Tile x y $ Open OLast  (distances exprs)
    emit (y, Just (First exprs)) = Tile x y $ Open OFirst (distances exprs)
    emit (y, _                 ) = Tile x y Horizontal

-- | Check whether there is a group of silent foliage that should
-- be closed.
hasGroupClose :: Shrub EdgeData -> Bool
hasGroupClose = any isGroupClose . foliageBushes
  where
    isGroupClose (Branch ts) =
        length ts > 1 && all isSilent (map fst ts)

-- | Check whether there are any foliage edges that
-- have no expressions and no siblings.
hasIsolatedTwigs :: Shrub EdgeData -> Bool
hasIsolatedTwigs = any isIsolatedTwig . foliageBushes

-- | 'foliageBushes' returns the collection of all subtrees
-- that consist only of foliage. (These trees have height 1).
--
-- Trees where the immediate children of a root contain both
-- foliage and non-foliage are not returned here!
foliageBushes :: Shrub a -> [Shrub a]
foliageBushes (Branch []) = []
foliageBushes (Branch ts)
    | all isRoot (map snd ts) = [ Branch ts ]
    | otherwise               = concatMap (foliageBushes . snd) ts

-- | Emit a column that closes all groups that can be closed
-- at the moment.
emitGroupClose :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitGroupClose _ (Branch []) = ([], Branch [])
emitGroupClose x (Branch ts)
    | all isRoot children && all isSilent labels && length children > 1 =
        ([Tile x y $ Close $ map (subtract y) ys], twig (y, Nothing))
    | otherwise =
        let (tiles, children') = unzip $ map (emitGroupClose x) children
            tiles2 = [ Tile x y2 Horizontal | ((y2, _), Branch []) <- ts ]
        in  (concat tiles <> tiles2, Branch (zip labels children'))
  where
    y        = head ys
    ys       = map fst labels
    labels   = map fst ts
    children = map snd ts

-- | Drop all foliage edges that have no expressions and have no siblings.
dropIsolatedTwigs :: Shrub EdgeData -> Shrub EdgeData
dropIsolatedTwigs (Branch []) = Branch []
dropIsolatedTwigs (Branch ts)
    | isIsolatedTwig (Branch ts) = Branch []
    | otherwise = Branch $ zip labels $ map dropIsolatedTwigs children
  where
    labels   = map fst ts
    children = map snd ts

-- | Check whether a 'Shrub' is a twig without expression.
isIsolatedTwig :: Shrub EdgeData -> Bool
isIsolatedTwig (Branch [((_,Nothing), Branch [])]) = True
isIsolatedTwig _ = False

{-----------------------------------------------------------------------------
    Shrub
    data structure
------------------------------------------------------------------------------}
-- | A 'Shrub' is a rooted tree that has labels (only) on the edges
-- (as opposed to on the leaves).
newtype Shrub a = Branch { unBranch :: [(a, Shrub a)] }
    deriving (Eq, Ord, Show)

instance Functor Shrub where
    fmap f (Branch bs) = Branch [ (f x, fmap f b) | (x, b) <- bs ]

-- | Test whether a 'Shrub' is an isolated root.
isRoot :: Shrub a -> Bool
isRoot (Branch []) = True
isRoot _ = False

-- | The 'Shrub' that is an isolated root.
root :: Shrub a
root = Branch []

-- | Test whether a 'Shrub' is a single edge.
isTwig :: Shrub a -> Bool
isTwig (Branch [(_, b)]) = isRoot b
isTwig _ = False

-- | Construct a 'Shrub' with a single edge.
twig :: a -> Shrub a
twig x = Branch [(x, root)]

-- | The 'foliage' of a 'Shrub' is the collection of
-- edges that end in a leaf.
--
-- This function returns the labels of the foliage of the given 'Shrub'.
foliage :: Shrub a -> [a]
foliage (Branch []) = []
foliage (Branch bs) = concatMap go bs
  where
    go (x, Branch []) = [x]
    go (_, Branch cs) = concatMap go cs

-- | Replace the foliage by news 'Shrub's.
--
-- Each edge in the foliage is replaced by the new 'Shrub'
-- whose root is attached to the end of the edge that is not a leaf.
--
-- Special cases:
--
-- * If the new 'Shrub' is 'root', the foliage edge will be removed.
-- * If the new 'Shrub' is a 'twig', the foliage edge will get a new label.
updateFoliage :: (a -> Shrub a) -> Shrub a -> Shrub a
updateFoliage _ (Branch []) = Branch []
updateFoliage f (Branch bs) = Branch $ concatMap update bs
  where
    update (x, Branch []) = unBranch $ f x
    update (x, Branch cs) = [(x, Branch $ concatMap update cs)]

{-----------------------------------------------------------------------------
    Exprs
------------------------------------------------------------------------------}
-- | Outcome expressions.
data Expr v
    = Var v
--    | Never
--    | Wait Rational
    | Seq [Expr v]
        -- invariant: list nonempty
        -- invariant: items of the list are not seqs.
    | Last [Expr v]
    | First [Expr v]
--  | Choice loc [Expr v]
    deriving (Show, Eq, Ord)

-- | Smart constructor for 'Var'.
var :: v -> Expr v
var = Var

-- | Smart constructor for sequential composition.
seq :: [Expr v] -> Expr v
seq = Seq . concatMap pop
  where
    pop (Seq ts) = ts
    pop t = [t]

-- | Smart constructor for last-to-finish.
last :: [Expr v] -> Expr v
last = Last . concatMap pop
  where
    pop (Last ts) = ts
    pop t = [t]

-- | Smart constructor for first-to-finish.
first :: [Expr v] -> Expr v
first = First . concatMap pop
  where
    pop (Last ts) = ts
    pop t = [t]

-- | Check whether a 'Expr' is a 'Seq'.
isSeq :: Expr v -> Bool
isSeq (Seq _) = True
isSeq _       = False

-- | Check whether a 'Expr' is a 'Var'.
isVar :: Expr v -> Bool
isVar (Var _) = True
isVar _       = False

-- | Check whether a 'Expr' is a parallel operation.
isParallel :: Expr v -> Bool
isParallel (First ts) = not (null ts)
isParallel (Last  ts) = not (null ts)
isParallel _          = False

-- | Maximal number of outcomes that run in parallel.
maxParallel :: Expr v -> Int
maxParallel (Var   _) = 1
maxParallel (Seq   ts) = maximum (map maxParallel ts)
maxParallel (Last  ts) = sum (map maxParallel ts)
maxParallel (First ts) = sum (map maxParallel ts)

example1 :: Expr String
example1 =
    last
        [ var "AZ"
        , seq [var "AB", last [var "BZ", seq [var "BC", var "CZ"]]]
        ]

example2 :: Expr String
example2 =
    seq
        [ var "AB"
        , first [var "BX", seq [ var "BC", var "CX" ] ]
        , var "XY"
        , last [var "YZ", seq [ var "YQ", var "QZ" ] ]
        ]

exampleS :: Expr String
exampleS = seq [ first [var "S1", var "S2"], var "S3" ]

example3 :: Expr String
example3 = last [seq [exampleS, exampleS], exampleS, seq [example1, var "ZQ"]]
