F certain algorithmic puzzle website has a problem that goes like this...

```
Given a grid of integers, find the largest product of n numbers which are
adjacent in the same direction (left, right, up, down, or diagonally)
```

In this diagram, `A`, `B`, and `C` are diagonally adjacent:
```
0 0 0 0
0 0 0 C
0 0 B 0
0 A 0 0 
```

And in this one, `A`, `B`, and `C` are vertically adjacent:
```
0 A 0 0
0 B 0 0
0 C 0 0
0 0 0 0 
```

I initially solved the problem with this data structure and operations:
```haskell
data Grid a
  = Grid 
  { width :: !Int
  , height :: !Int
  , xPos :: !Int
  , yPos :: !Int
  , content :: [[a]]
  }

focus :: Grid a -> a
focus (Grid _ _ x y g) = (g !! y) !! x

-- These operations return Nothing if we are at the edge of the grid,
-- otherwise increment/decrement xPos/yPos accordingly
up, left, down, right :: Grid a -> Maybe (Grid a)
```

The idea being to walk through the grid, and for each position calculate the
product of the adjacent elements. For example, the product of the focus and
the two neighbours to its right would be:

```haskell
example1 :: Num a => Grid a -> Maybe a
example1 grid =
  (\b c -> focus grid * b * c) <$> 
  fmap pos (right grid) <*>
  fmap pos (right <=< right $ grid) <*>
```

`Grid` can be given a `Comonad` instance, and this process of per-position
calulation can be expressed using comonadic operations. If we plug the
`example1` function into `extend`, we get the function
`extend example1 :: Grid a -> Grid (Maybe a)`. This function walks through
the grid, and replaces each cell with the result of running `example1` on it
and its neighbours.

This is cool in and of itself, but implementing `duplicate` or `extend` for
`Grid` is tedious. `Grid` can actually be implemented as the composition of
two comonads:
[Env](https://hackage.haskell.org/package/comonad/docs/Control-Comonad-Env.html)
and
[Store](https://hackage.haskell.org/package/comonad/docs/Control-Comonad-Store.html), which gives us the correct comonadic behaviour for free.

***

[Literal Haskell source]({{ "/files/grid_comonad.lhs" | absolute_url }})

\begin{code}
import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Control.Comonad ((=>>), extract)
import Control.Comonad.Env (EnvT(..), ask)
import Control.Comonad.Store (Store, store, peek, pos, seek)
import Data.List (maximum)

type Dimensions = (Int, Int)
type Position = (Int, Int)
type Grid a = EnvT Dimensions (Store Position) a
\end{code}

`EnvT e w a` is an environment of type `e` paired with an underlying comonad
`w a`. We can inspect the environment with
`ask :: ComonadEnv e w => w a -> e`. `extract`ing from an `EnvT` just
extracts from the underlying comonad, and ignores the environment. The
dimensions of the grid are the environment because they remain static
throughout the program.

`Store s a` consists of some state `s`, and an "accessor" function of type
`s -> a`. `extract`ing a `Store` feeds its state into the accessor function.
For `Grid`, the focus position is the state, and the accessor is a function
that pulls out the corresponding element from some list of lists.

Three important functions on `Store` are:

* `pos :: ComonadStore s w => w a -> s`
* `seek :: ComonadStore s w => s -> w a -> w a`
* `peek :: ComonadStore s w => s -> w a -> a`

`pos` returns the current state, `seek` replaces the state, and `peek` runs
the accessor function on a different piece of state, leaving the actual state
unchanged.

Here's how we make a grid. Notice that the accessor function passed to
`store` behaves like `focus`.

\begin{code}
mkGrid :: [[a]] -> Maybe (Grid a)
mkGrid [] = Nothing
mkGrid g@(r:rs)
  | rl <- length r
  , all ((==rl) . length) rs =
      Just $
      EnvT
        (rl, length g)
        (store (\(x, y) -> (g !! y) !! x) (0, 0))
  | otherwise = Nothing
\end{code}

If the grid has no rows, or has rows of different
lengths, return nothing. Otherwise, calculate the dimensions of the grid,
and initialise the store pointing to the top-left cell in the grid.

Now we can implement `up, down, left, right`:
\begin{code}
up :: Grid a -> Maybe (Grid a)
up g =
  let
    (w, h) = ask g
    (x, y) = pos g
  in
    if y > 0 then Just (seek (x, y-1) g) else Nothing 

left :: Grid a -> Maybe (Grid a)
left g =
  let
    (w, h) = ask g
    (x, y) = pos g
  in
    if x > 0 then Just (seek (x-1, y) g) else Nothing 

down :: Grid a -> Maybe (Grid a)
down g =
  let
    (w, h) = ask g
    (x, y) = pos g
  in
    if y < h-1 then Just (seek (x, y+1) g) else Nothing 

right :: Grid a -> Maybe (Grid a)
right g =
  let
    (w, h) = ask g
    (x, y) = pos g
  in
    if x < w-1 then Just (seek (x+1, y) g) else Nothing 
\end{code}

Next are some helper functions for calculating the product of a grid element
and its neighbours.

`iterateM` is the monadic equivalent of
[iterate](https://hackage.haskell.org/package/base/docs/Data-List.html#v:iterate).

`productN` calculates the product of the current grid element with its
adjacent neighbours in some direction. `example1` could be redefined as
`productN 3 right`.
\begin{code}
iterateM :: Monad m => (a -> m a) -> [a -> m a]
iterateM f = f : fmap (f <=<) (iterateM f)

productN :: Num a => Int -> (Grid a -> Maybe (Grid a)) -> Grid a -> Maybe a
productN n f g =
  foldr
    (\a b -> liftA2 (*) (extract <$> a g) b)
    (pure 1)
    (take n $ iterateM f)
\end{code}

Penultimately, we define a function for finding the greatest element in a
grid. It `peek`s at all the elements and finds the greatest one.
\begin{code}
maxInGrid :: Ord a => Grid a -> a
maxInGrid g =
  let
    (w, h) = ask g
  in
    maximum $ do
      x <- [0..w-1]
      y <- [0..h-1]
      pure $ peek (x, y) g
\end{code}

Last step. To find the largest product of `n` adjacent elements,
we find the largest product of `n` adjacent elements horizontally,
then vertically, then diagonally, and take the maximum of those.

We can write this logic as a series of `extend`s, because `productN n move`
and `maxInGrid` are both of the form `w a -> b`. (`(=>>)` is the flipped infix version of `extend`)

\begin{code}
largestProduct :: Int -> Grid Int -> Int
largestProduct n g =
  let
    Just g1 = extract $ g =>> productN n right            =>> maxInGrid
    Just g2 = extract $ g =>> productN n down             =>> maxInGrid
    Just g3 = extract $ g =>> productN n (down <=< left)  =>> maxInGrid
    Just g4 = extract $ g =>> productN n (down <=< right) =>> maxInGrid
  in
    maximum [g1, g2, g3, g4]
\end{code}

I'm still getting an intuition for comonads, but they seem to embody some kind
of "environment", and comonad transformers are like a
"composition of environments". In this example, there are two environments: the
grid's dimensions, and its content.

For more information about comonads, check out Bartosz Milewski's
[comonads post](https://bartoszmilewski.com/2017/01/02/comonads/)
and Dan Piponi's article about
[comonadic cellular automata](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html).

Footnote: I feel like `largestProduct` could be simplified if `Grid` were
`ComonadApply`, but I haven't tried to figure it out yet.
