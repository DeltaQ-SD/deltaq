{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module DeltaQ.Diagram where

import Prelude hiding (last, seq)

import Control.Monad (ap)
import Diagrams.Prelude hiding (First, Last)
import Diagrams.Backend.SVG

-- main = renderSVG "test.svg" (mkWidth 250) (circle 1 :: Dia)

main = out example3

out :: Term String -> IO ()
out =
    renderSVG "xoutcomes.svg" (mkWidth 700)
    . renderTiles
    . layout

{-----------------------------------------------------------------------------
    Diagram rendering
------------------------------------------------------------------------------}
type X = Int
type Y = Int

data Token
    = VarT String
    | Horizontal
    | Close
    | OpenFirst
    | OpenLast
    deriving (Eq, Ord, Show)

data Tile = Tile X Y Token
    deriving (Eq, Ord, Show)

type Dia = Diagram B

renderTiles :: [Tile] -> Dia
renderTiles = position . map renderTile
  where
    renderTile (Tile x y token) =
        ( mkP2 (fromIntegral x) (negate $ fromIntegral y)
        , renderToken token <> (square 1 & lc gray & lw 1)
        )

renderToken :: Token -> Dia
renderToken (VarT s)   =
    (circle 0.44 & lc orange & lw 4)
    <> scale 0.3 (text s)
renderToken Horizontal = hrule 1
renderToken Close      = circle 0.05 & fc black
renderToken OpenFirst  = scale 0.4 (text "∃" <> square 1)
renderToken OpenLast   = scale 0.4 (text "∀" <> square 1)

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

type EdgeData = (Y, Maybe (Term String))

-- | Layout an outcome diagram.
layout :: Term String -> [Tile]
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
    | any (onTerm isSeq)      (foliage s) = ([], expandSeq s)
    | hasIsolatedTwigs s                  = ([], dropIsolatedTwigs s)
    | hasGroupClose s                     = emitGroupClose x s
    | any (onTerm isParallel) (foliage s) = emitParallel x s
    | any (onTerm isVar)      (foliage s) = emitVar x s
  where
    onTerm p (_, Just t) = p t
    onTerm p _ = False

-- | Check that a given 'EdgeData' does not contain a term.
isSilent :: EdgeData -> Bool
isSilent (_, Nothing) = True
isSilent _ = False

-- | Expand all 'Seq' in the 'foliage' as a sequence of twigs.
expandSeq :: Shrub EdgeData -> Shrub EdgeData
expandSeq = updateFoliage expand
  where
    expand (y, Just (Seq terms)) =
        foldl (\s term -> Branch [((y, Just term), s)]) root terms
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
    dropit (y, Just (Last  terms)) = close y terms
    dropit (y, Just (First terms)) = close y terms
    dropit (y, Just t            ) = twig (y, Just t)

    close y terms =
        Branch [((y,Nothing), Branch [(e, root) | e <- verticals y terms])]
    verticals y terms = zipWith (\y t -> (y, Just t)) ys terms
      where
        -- increase vertical distances
        ds = map maxParallel terms
        ys = scanl (+) y ds

    emit (y, Just (Last  _)) = Tile x y OpenLast
    emit (y, Just (First _)) = Tile x y OpenFirst
    emit (y, _             ) = Tile x y Horizontal

-- | Check whether there is a group of silent foliage that should
-- be closed.
hasGroupClose :: Shrub EdgeData -> Bool
hasGroupClose = any isGroupClose . foliageBushes
  where
    isGroupClose (Branch ts) =
        length ts > 1 && all isSilent (map fst ts)

-- | Check whether there are any foliage edges that
-- have no terms and no siblings.
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
emitGroupClose x (Branch []) = ([], Branch [])
emitGroupClose x (Branch ts)
    | all isRoot children && all isSilent labels && length children > 1 =
        ([Tile x y Close], twig (y, Nothing))
    | otherwise =
        let (tiles, children') = unzip $ map (emitGroupClose x) children
            tiles2 = [ Tile x y Horizontal | ((y, _), Branch []) <- ts ]
        in  (concat tiles <> tiles2, Branch (zip labels children'))
  where
    y        = fst $ head labels
    labels   = map fst ts
    children = map snd ts

-- | Drop all foliage edges that have no term and have no siblings.
dropIsolatedTwigs :: Shrub EdgeData -> Shrub EdgeData
dropIsolatedTwigs (Branch []) = Branch []
dropIsolatedTwigs (Branch ts)
    | isIsolatedTwig (Branch ts) = Branch []
    | otherwise = Branch $ zip labels $ map dropIsolatedTwigs children
  where
    labels   = map fst ts
    children = map snd ts

-- | Check whether a 'Shrub' is a twig without term
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
updateFoliage f (Branch []) = Branch []
updateFoliage f (Branch bs) = Branch $ concatMap update bs
  where
    update (x, Branch []) = unBranch $ f x
    update (x, Branch cs) = [(x, Branch $ concatMap update cs)]

{-----------------------------------------------------------------------------
    Terms
------------------------------------------------------------------------------}
-- | Outcome terms, with location annotations.
data Term v
    = Var v
--    | Never
--    | Wait Rational
    | Seq [Term v]
        -- invariant: list nonempty
        -- invariant: items of the list are not seqs.
    | Last [Term v]
    | First [Term v]
--  | Choice loc [Term v]
    deriving (Show, Eq, Ord)

-- | Smart constructor for 'Var'.
var :: v -> Term v
var = Var

-- | Smart constructor for sequential composition.
seq :: [Term v] -> Term v
seq = Seq . concatMap pop
  where
    pop (Seq ts) = ts
    pop t = [t]

-- | Smart constructor for last-to-finish.
last :: [Term v] -> Term v
last = Last . concatMap pop
  where
    pop (Last ts) = ts
    pop t = [t]

-- | Smart constructor for first-to-finish.
first :: [Term v] -> Term v
first = First . concatMap pop
  where
    pop (Last ts) = ts
    pop t = [t]

-- | Check whether a 'Term' is a 'Seq'.
isSeq :: Term v -> Bool
isSeq (Seq _) = True
isSeq _       = False

-- | Check whether a 'Term' is a 'Var'.
isVar :: Term v -> Bool
isVar (Var _) = True
isVar _       = False

-- | Check whether a 'Term' is a parallel operation.
isParallel :: Term v -> Bool
isParallel (First ts) = not (null ts)
isParallel (Last  ts) = not (null ts)
isParallel _          = False

-- | Maximal number of outcomes that run in parallel.
-- Needed to determine vertical position.
maxParallel :: Term v -> Int
maxParallel (Var   _) = 1
maxParallel (Seq   ts) = maximum (map maxParallel ts)
maxParallel (Last  ts) = sum (map maxParallel ts)
maxParallel (First ts) = sum (map maxParallel ts)

example1 :: Term String
example1 =
    last
        [ var "AZ"
        , seq [var "AB", last [var "BZ", seq [var "BC", var "CZ"]]]
        ]

example2 :: Term String
example2 =
    seq
        [ var "AB"
        , first [var "BX", seq [ var "BC", var "CX" ] ]
        , var "XY"
        , last [var "YZ", seq [ var "YQ", var "QZ" ] ]
        ]

exampleS :: Term String
exampleS = seq [ first [var "S1", var "S2"], var "S3" ]

example3 :: Term String
example3 = last [seq [exampleS, exampleS], exampleS, seq [example1, var "ZQ"]]
