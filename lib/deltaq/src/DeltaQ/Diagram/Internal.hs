{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

{-|
Copyright   : PLWORKZ R&D, 2025
License     : BSD-3-Clause
Description : Render outcome diagrams.

INTERNAL MODULE. USE "DeltqQ.Diagram" instead!

Render outcome expressions as outcome diagrams.
-}
module DeltaQ.Diagram.Internal where

import DeltaQ.Class
    ( Outcome (..)
    , ProbabilisticOutcome (..)
    )
import DeltaQ.Expr
    ( O
    , Term (..)
    , isLoc
    , isSeq
    , loc
    , termFromOutcome
    , var
    )
import Diagrams.Prelude hiding (Empty, First, Last, Loc, op, loc)
import Diagrams.Backend.SVG
import Text.Printf
    ( printf
    )
out :: O -> IO ()
out =
    renderSVG "xoutcomes.svg" (mkWidth 700)
    . renderOutcomeDiagram

-- | Render an outcome expression as an outcome diagram.
renderOutcomeDiagram :: O -> Diagram SVG
renderOutcomeDiagram = renderTiles . layout . termFromOutcome

{-----------------------------------------------------------------------------
    Diagram rendering
------------------------------------------------------------------------------}
type X = Int
type Y = Int
type Prob = Rational
type Description = String

data Op
    = OFirst
    | OLast
    | OChoices [Prob]
    deriving (Eq, Ord, Show)

-- | Tiles representing predefined outcomes
data Op0
    = ONever
    | OWait0
    | OWait Rational
    deriving (Eq, Ord, Show)

-- | Data attached to a 'Tile'.
data Token
    = VarT String
    | Outcome Op0
    | Location Description
    | Horizontal
    | Close (Maybe Description) [Y]
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
renderToken (VarT s) =
    scale 0.3 (text s)
    <> (circle 0.44 & lc orange & lw 4 & fc white)
    <> hrule 1
renderToken (Outcome OWait0) =
    hrule 1
renderToken (Outcome o) =
    scale 0.3 (renderOp0Symbol o)
    <> (circle 0.44 & lc orange & lw 4 & fc white)
    <> hrule 1
renderToken (Location s) =
    (scale 0.2 (text s) & fc teal & translate (r2 (0,0.2)))
    <> (square 0.2 & fc white)
    <> hrule 1
renderToken Horizontal = hrule 1
renderToken (Close mlocation ds) =
    maybe mempty (renderToken . Location) mlocation
    <> mconcat (map (renderLine . fromIntegral . negate) ds)
    <> hrule 1
  where
    renderLine d = fromVertices [p2 (0, 0), p2 (-0.5, d)] & strokeLine
renderToken (Open op ds) =
    scale 0.4 ((renderOpSymbol op & lw 1.7) <> (square 1 & fc white))
    <> mconcat (map renderLine ys)
    <> mconcat (renderLineAnnotations op ys)
    <> hrule 1
  where
    ys = map (fromIntegral . negate) ds

    renderLine d = fromVertices [p2 (0, 0), p2 (0.5, d)] & strokeLine

    renderLineAnnotations (OChoices ws) =
        zipWith renderLineAnnotation ps
      where
        ps = map (/ sum ws) ws
    renderLineAnnotations _ = const []

    posLineAnnotation _ 0 = p2 (0.2, 0.3)
    posLineAnnotation _ d = p2 (0.2, 0.1 + d)
    renderLineAnnotation p d =
        position
            [(posLineAnnotation p d
            , scale 0.2 (text $ showProb p) & fc teal
            )]

-- | Render the symbol that represents a known outcome.
renderOp0Symbol :: Op0 -> Diagram SVG
renderOp0Symbol ONever    = text "⊥"
renderOp0Symbol OWait0    = mempty
renderOp0Symbol (OWait t) =
    text $ "wait " <> printf "%.2f" (fromRational t :: Double)

-- | Render the symbol that represents an operation with multiple arguments
renderOpSymbol :: Op -> Diagram SVG
renderOpSymbol OFirst =
    (fromVertices [p2 (-0.35,0.25), p2 (0,0.25), p2 (0,-0.25), p2 (-0.35,-0.25)]
        & strokeLine & translate (r2 (-0.35/2, 0.25)))
    <> (fromOffsets [r2 (-0.35,0)] & strokeLine & translate (r2 (0.35/2, 0)))
renderOpSymbol OLast  =
    (fromOffsets [r2 (-0.16, 0), r2 (2*0.16, 0)] & strokeLine)
    <>
    (((fromOffsets [r2 (-0.25, 0.6)] & strokeLine)
        <> (fromOffsets [r2 (0.25, 0.6)] & strokeLine))
        & translate (r2 (0,-0.35))
    ) & translate (r2 (0, 0.03))
renderOpSymbol (OChoices _) =
    (fromOffsets [r2 (0.33, 0), r2 (-0.6, 0), r2 (0.2, 0.15)]
        & strokeLine & translate (r2 (0,0.1)))
    <>
    (fromOffsets [r2 (-0.33, 0), r2 (0.6, 0), r2 (-0.2, -0.15)]
        & strokeLine & translate (r2 (0,-0.1)))

-- | Show a probability in scientific notation with two digits of precision.
showProb :: Rational -> String
showProb r
    | r >= 0.01 = printf "%.2f" x
    | otherwise = printf "%.2e\n" x
  where
    x = fromRational r :: Double

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

-- | Outcome-related data that we associate with an edge.
data EdgeOutcome
    = Empty
        -- ^ No outcome is associated with this edge.
    | Term (Term String) (Maybe Description)
        -- ^ Term with maybe a description of its ending observation location.
    deriving (Eq, Ord, Show)

type EdgeData = (Y, EdgeOutcome)

-- | Layout an outcome diagram.
layout :: Term String -> [Tile]
layout t = go 0 $ twig (0, Term t Nothing)
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
    | any (onExpr isSeq)        (foliage s) = ([], expandSeq s)
    | hasIsolatedTwigs s                    = ([], dropIsolatedTwigs s)
    | hasGroupClose s                       = emitGroupClose x s
    | any (onExpr isVertical)   (foliage s) = emitVertical x s
    | any (onExpr isLoc)        (foliage s) = emitLoc x s
    | any (onExpr isVarOrKnown) (foliage s) = emitVar x s
    | otherwise = error "emitColumn: unreachable"
  where
    onExpr p (_, Term t _) = p t
    onExpr _ _ = False

-- | Check that a given Term is a 'Var' or known constant.
isVarOrKnown :: Term v -> Bool
isVarOrKnown (Var _)  = True
isVarOrKnown Never    = True
isVarOrKnown Wait0    = True
isVarOrKnown (Wait _) = True
isVarOrKnown _ = False

-- | Check that a given 'EdgeData' contains no outcome-related data.
isEmpty :: EdgeData -> Bool
isEmpty (_, Empty) = True
isEmpty _ = False

-- | Expand all 'Seq' in the 'foliage' as a sequence of twigs.
--
-- Merge observation locations that follow after a vertical term
-- into the term annotation.
expandSeq :: Shrub EdgeData -> Shrub EdgeData
expandSeq = updateFoliage expand
  where
    expand (y, Term (Seq exprs) _) = mkBranches y root exprs
    expand edge = twig edge

    mkBranches _ acc [] = acc
    mkBranches y acc (t:Loc description:ts)
        | isVertical t =
            mkBranches y (Branch [((y, Term t (Just description)), acc)]) ts
    mkBranches y acc (t:ts) =
        mkBranches y (Branch [((y, Term t Nothing), acc)]) ts

-- | Emit a column with the next observation locations.
emitLoc :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitLoc x s =
    (concatMap emit $ foliage s, updateFoliage dropit s)
  where
    dropit (y, Term (Loc _) _) = twig (y, Empty)
    dropit edge                = twig edge

    emit (y, Term (Loc l) _) = [Tile x y $ Location l]
    emit (y, _             ) = [Tile x y Horizontal]

-- | Emit a column with the next named items.
emitVar :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitVar x s =
    (map emit $ foliage s, updateFoliage dropit s)
  where
    dropit (y, Term t _) | isVarOrKnown t = twig (y, Empty)
    dropit edge                           = twig edge

    emit (y, Term (Var v)  _) = Tile x y $ VarT v
    emit (y, Term Never    _) = Tile x y $ Outcome ONever
    emit (y, Term Wait0    _) = Tile x y $ Outcome OWait0
    emit (y, Term (Wait t) _) = Tile x y $ Outcome $ OWait t
    emit (y, _              ) = Tile x y Horizontal

-- | Emit a column with the next vertical items.
emitVertical :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitVertical x s =
    (map emit $ foliage s, updateFoliage dropit s)
  where
    dropit (y, Term (Last    exprs ) ml) = close y ml exprs
    dropit (y, Term (First   exprs ) ml) = close y ml exprs
    dropit (y, Term (Choices wexprs) ml) = close y ml (map snd wexprs)
    dropit edge                         = twig edge

    mkLocation Nothing  = Empty
    mkLocation (Just l) = Term (Loc l) Nothing
    close y ml exprs =
        Branch [
          ( (y, mkLocation ml)
          , Branch [(e, root) | e <- verticals y exprs])
        ]
    verticals y exprs =
        zipWith (\z t -> (y + z, Term t Nothing)) (distances exprs) exprs 
    distances = init . scanl (+) 0 . map maxVertical

    emit (y, Term (Last    exprs ) _) =
        Tile x y $ Open OLast $ distances exprs
    emit (y, Term (First   exprs ) _) =
        Tile x y $ Open OFirst $ distances exprs
    emit (y, Term (Choices wexprs) _) =
        Tile x y $ Open (OChoices ws) $ distances exprs
      where (ws, exprs) = unzip wexprs
    emit (y, _) = Tile x y Horizontal

-- | Outcomes that open a new vertical.
isVertical :: Term v -> Bool
isVertical (Last    _) = True
isVertical (First   _) = True
isVertical (Choices _) = True
isVertical _           = False

-- | Maximal number of outcomes that are shown vertically.
-- Parallel operations are shown vertically, but also 'Choices'.
maxVertical :: Term v -> Int
maxVertical (Seq   ts)    = maximum $ map maxVertical ts
maxVertical (Last  ts)    = sum $ map maxVertical ts
maxVertical (First ts)    = sum $ map maxVertical ts
maxVertical (Choices wts) = sum $ map (maxVertical . snd) wts
maxVertical _             = 1


-- | Check whether there is a group of silent foliage that should
-- be closed.
hasGroupClose :: Shrub EdgeData -> Bool
hasGroupClose = any isGroupClose . foliageBushes
  where
    isGroupClose (Branch ts) =
        length ts > 1 && all isEmpty (map fst ts)

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
--
-- Special case: If the next-higher branch is an observation
-- location with a label, we merge that location into the close.
emitGroupClose :: X -> Shrub EdgeData -> ([Tile], Shrub EdgeData)
emitGroupClose x shrub = goBranch shrub
  where
    goBranch (Branch []) = ([], Branch [])
    goBranch (Branch ts) = (concat tiless, Branch edges)
      where (tiless, edges) = unzip $ map (uncurry goEdge) ts

    goEdge (y, term) (Branch []) =
        ( [Tile x y Horizontal]
        , ((y, term), Branch [])
        )
    goEdge (y, term) (Branch ts)
        | isGroupClose ts =
            ( [Tile x y $ Close mlocation $ map (subtract y) ys]
            , ((y, Empty), root)
            )
        | otherwise =
            let (tiles, branch') = goBranch (Branch ts)
            in  ( tiles
                , ((y, term), branch')
                )
      where
        mlocation = case term of
            Term (Loc s) _ -> Just s
            _ -> Nothing
        ys = map (fst . fst) ts

    isGroupClose ts
        = all isRoot (map snd ts) && all isEmpty (map fst ts) && length ts > 1

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
isIsolatedTwig (Branch [((_,Empty), Branch [])]) = True
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
    Example expressions
------------------------------------------------------------------------------}
example1 :: O
example1 =
    var "AZ"
    ./\. (var "AB" .>>. (var "BZ" .\/. (var "BC" .>>. var "CZ")))

example2 :: O
example2 =
    var "AB"
    .>>. (var "BX" .\/. (var "BC" .>>. var "CX"))
    .>>. var "XY"
    .>>. (var "YZ" .\/. (var "YQ" .>>. var "QZ"))

exampleS :: O
exampleS = (var "S1" .\/. var "S2") .>>. var "S3"

example3 :: O
example3 =
    (exampleS .>>. exampleS)
    .\/. exampleS
    .\/. (example1 .>>. var "ZQ")

exampleCache :: O
exampleCache =
    loc "read"
    .>>. choices
        [ (95, var "c-hit" .>>. loc "hit")
        , ( 5, var "c-miss" .>>. loc "miss" .>>. (net .\/. timeout) .>>. loc "")
        ]
    .>>. loc "return"
  where
    net = var "net"
        .>>. loc "mread"
        .>>. var "main"
        .>>. loc "mreturn"
        .>>. var "net"
    timeout = var "t-out"
