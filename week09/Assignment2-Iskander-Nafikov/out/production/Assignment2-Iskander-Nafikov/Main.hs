module Main (main) where

-- ======================================================================
-- =========================== Assignment 2 =============================
-- =================== Iskander Nafikov B20-SD01 ========================
-- ============================ 31/10/22 ================================
-- ======================================================================

--import CodeWorld

main :: IO ()
main = putStrLn ""
--  animateConway 
--  ( Space (Line [Line [Dead, Dead, Dead] Alive [Alive, Alive, Dead], Line [Dead, Dead, Dead] Dead [Dead, Dead, Dead], Line [Dead, Dead, Dead] Dead [Dead, Dead, Dead]] (Line [Alive, Dead, Dead] Alive [Alive, Dead, Dead]) [Line [Dead, Dead, Dead] Dead [Dead, Dead, Dead], Line [Dead, Dead, Dead] Dead [Dead, Dead, Dead], Line [Dead, Dead, Dead] Dead [Dead, Dead, Dead]]))


-- ========================= Predefined in Document =====================

-- | A line with a focus.
-- Line xs y zs represents a discrete line:
-- * xs represents all elements to the left (below)
-- * y is the element in focus
-- * zs represents all elements after (above)
data Line a = Line [a] a [a]
  deriving (Show) -- required to enable printing (for finite lines)
  
data Cell = Alive | Dead
  deriving (Show)

-- | A discrete 2D space with a focus.
-- A 2D space is merely a (vertical) line
-- where each element is a (horizontal) line.
data Space a = Space (Line (Line a))

-- | A line of integers with focus at 0.
integers :: Line Integer
integers = Line [-1, -2..] 0 [1, 2..]

-- | Apply a function if a given value satisfies the predicate.
applyIf :: (a -> Bool) -> (a -> b) -> a -> Maybe b
applyIf p f x
  | p x       = Just (f x)
  | otherwise = Nothing

-- | Applies rule30 to each shifted version of the line to get the
-- new state for each cell
applyRule30 :: Line Cell -> Line Cell
applyRule30 line = mapLine rule30 (lineShifts line)

-- | Applies conwayRule to each shifted version of the space
-- to get new state for every cell
applyConwayRule :: Space Cell -> Space Cell
applyConwayRule space = mapSpace conwayRule (spaceShifts space)

-- ================================ Utils ===============================

-- | Performs OR boolean operations but on cells replacing
-- False with Dead
-- True with Alive
orCells :: Cell -> Cell -> Cell
orCells Alive _ = Alive
orCells Dead y = y

-- | Performs XOR boolean operations but on cells replacing
-- False with Dead
-- True with Alive
xorCells :: Cell -> Cell -> Cell
xorCells Dead Dead = Dead
xorCells Alive Alive = Dead
xorCells _ _ = Alive


-- | Returns the list of versions of the given line where every element
-- of the left part of this line is in focus.
-- Does not include given version of line
leftShifts :: Line a -> [Line a]
leftShifts line = helper (shiftLeft line)
  where
    helper Nothing = []
    helper (Just currentLine) = currentLine : helper (shiftLeft currentLine)

-- | Returns the list of versions of the given line where every element
-- of the right part of this line is in focus
-- Does not include given version of line
rightShifts :: Line a -> [Line a]
rightShifts line = helper (shiftRight line)
  where
    helper Nothing = []
    helper (Just currentLine) = currentLine : helper (shiftRight currentLine)
    
-- | Reverses list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (first:rest) = reverseList rest ++ [first]

-- | Converse line to list where left part is in correct order
lineToList :: Line a -> [a]
lineToList (Line left mid right) = reverseList left ++ [mid] ++ right

---- | Renders cell as a picture
--renderCell :: Cell -> Picture
--renderCell Dead = rectangle 1 1
--renderCell Alive = solidRectangle 1 1


-- | Get neighbours of the focus element
-- Does not include the focus element itself
lineNeighbours :: Line a -> [a]
lineNeighbours (Line (left:restLeft) mid (right:restRight)) = [left, right]
lineNeighbours (Line [] mid (right:restRight)) = [right]
lineNeighbours (Line (left:restLeft) mid []) = [left]
lineNeighbours (Line [] mid []) = []

-- | Get neighbours of the focus element including the focus element itself
lineNeighboursWithMid :: Line a -> [a]
lineNeighboursWithMid (Line (left:restLeft) mid (right:restRight)) = [left, mid, right]
lineNeighboursWithMid (Line [] mid (right:restRight)) = [mid, right]
lineNeighboursWithMid (Line (left:restLeft) mid []) = [left, mid]
lineNeighboursWithMid (Line [] mid []) = [mid]

-- | Get neighbours of the focus element
-- Does not include the focus element itself
spaceNeighbours :: Space Cell -> [Cell]
spaceNeighbours (Space (Line (upperNeighbourLine:restUpper)
                             midLine
                             (lowerNeighbourLine:restLower)
                        )
                 ) = lineNeighboursWithMid upperNeighbourLine ++
                     lineNeighbours midLine ++
                     lineNeighboursWithMid lowerNeighbourLine
spaceNeighbours (Space (Line [] midLine (lowerNeighbourLine:restLower))) = 
  lineNeighbours midLine ++
  lineNeighboursWithMid lowerNeighbourLine
spaceNeighbours (Space (Line (upperNeighbourLine:restUpper) midLine [])) = 
  lineNeighbours midLine ++
  lineNeighboursWithMid upperNeighbourLine
spaceNeighbours (Space (Line [] midLine [])) = 
  lineNeighbours midLine


-- | True for underpopulation or overpopulation (< 2 or > 3 live neighbours)
shouldDie :: [Cell] -> Bool
shouldDie = helper 0
  where
    helper 4 (neighbour:rest) = True
    helper liveCount []
      | liveCount < 2 = True
      | otherwise = False
    helper liveCount (Alive:rest) = helper (liveCount + 1) rest
    helper liveCount (Dead:rest) = helper liveCount rest

-- | Maps line of lines of lines to line of spaces
mapTripleLinesToLineOfSpace :: Line (Line (Line a)) -> Line (Space a)
mapTripleLinesToLineOfSpace (Line left mid right) =
  Line (map Space left) (Space mid) (map Space right)

-- ======================================================================
-- ========================= Exercise 1.1 Lines =========================
-- ======================================================================

---- Ex 1.1
-- | Keep up to a given number of elements in each direction in a line.
-- cutLine 3 integers = Line [-1,-2,-3] 0 [1,2,3]
cutLine :: Int -> Line a -> Line a
cutLine cnt (Line left mid right) = Line (take cnt left) mid (take cnt right)

---- Ex 1.2
-- | Generate a line by using generating functions.
-- (genLine f x g) generates a line with x in its focus,
-- then it applies f to x until reaching Nothing to produce
-- a list of elements to the left of x,
-- and, similarly, applies g to x until reaching Nothing to
-- produce a list of elements to the right of x.
genLine :: (a -> Maybe a) -> a -> (a -> Maybe a) -> Line a
genLine f x g = Line (helper f (f x)) x (helper g (g x))
  where
    helper _func Nothing = []
    helper func (Just xVal) = xVal : helper func (func xVal)

---- Ex 1.3
-- | Apply a function to all elements on a line.
-- mapLine (^2) integers = Line [1, 4, 9, ..] 0 [1, 4, 9, ..]
mapLine :: (a -> b) -> Line a -> Line b
mapLine func (Line left mid right) = Line (map func left) (func mid) (map func right)

---- Ex 1.4
-- | Zip together two lines.
-- zipLines integers integers
-- = Line [(-1,-1),(-2,-2),..] (0,0) [(1,1),(2,2),..]
zipLines :: Line a -> Line b -> Line (a, b)
zipLines (Line left1 mid1 right1) (Line left2 mid2 right2) = Line (zip left1 left2) (mid1, mid2) (zip right1 right2)

-- | Zip together two lines with a given combining function.
-- zipLinesWith (*) integers integers
-- = Line [1,4,9,..] 0 [1,4,9,..]
zipLinesWith :: (a -> b -> c) -> Line a -> Line b -> Line c
zipLinesWith func (Line left1 mid1 right1) (Line left2 mid2 right2) =
  Line (zipWith func left1 left2) (func mid1 mid2) (zipWith func right1 right2)
       

-- ======================================================================
-- ======================= Exercise 1.2 Rule 30 =========================
-- ======================================================================

---- Ex 1.5
-- | https://en.wikipedia.org/wiki/Rule_30
-- Considers empty left or right part as Dead cells
rule30 :: Line Cell -> Cell
rule30 (Line (left:_) mid (right:_)) = left `xorCells` (mid `orCells` right)
rule30 (Line (left:_) mid []) = left `xorCells` mid 
rule30 (Line [] mid (right:_)) = mid `orCells` right
rule30 (Line [] mid []) = mid

---- Ex 1.6
-- | Shifts the focus on the line by one position to the left.
-- Returns Nothing when the left part is empty
shiftLeft :: Line a -> Maybe (Line a)
shiftLeft (Line (left:restLeft) mid right) = Just (Line restLeft left (mid : right))
shiftLeft (Line [] _ _) = Nothing

-- | Shifts the focus on the line by one position to the right.
-- Returns Nothing when the right part is empty
shiftRight :: Line a -> Maybe (Line a)
shiftRight (Line left mid (right:restRight)) = Just (Line (mid : left) right restRight)
shiftRight (Line _ _ []) = Nothing

---- Ex 1.7
-- | Maps every element in a line into a version of the original line where that element is in focus.
-- The new line of lines should have the original line in focus
lineShifts :: Line a -> Line (Line a)
lineShifts line = Line (leftShifts line) line (rightShifts line)

------ Ex 1.8
---- | Render a line of 1x1 pictures.
--renderLine :: Line Picture -> Picture
--renderLine line = helper (lineToList line)
--  where
--    helper [] = blank
--    helper (picture:rest) = picture <> translated 1 0 (helper rest)
--
---- | Render the fist N steps of Rule 30, applied to a given starting line.
--renderRule30 :: Int -> Line Cell -> Picture
--renderRule30 0 _ = blank
--renderRule30 n line = 
--  renderLine (mapLine renderCell line) 
--  <> 
--  translated 0 (-1) (renderRule30 (n - 1) (applyRule30 line))


-- ======================================================================
-- =================== Exercise 1.3 Discrete spaces =====================
-- ======================================================================

---- Ex 1.9
-- Not implemented

---- Ex 1.10
-- | Apply a function to all elements on all lines in space
mapSpace :: (a -> b) -> Space a -> Space b
mapSpace func (Space line) = Space (mapLine (mapLine func) line)

-- | Zip together two spaces
zipSpaces :: Space a -> Space b -> Space (a, b)
zipSpaces (Space line1) (Space line2) =
  Space (zipLinesWith zipLines line1 line2)

-- | Zip together two lines with a given combining function
zipSpacesWith :: (a -> b -> c) -> Space a -> Space b -> Space c
zipSpacesWith func (Space line1) (Space line2) =
  Space (zipLinesWith (zipLinesWith func) line1 line2)

---- Ex 1.11
-- Check (Ex 1.10)


-- ======================================================================
-- ================ Exercise 1.4 Conway’s Game of Life ==================
-- ======================================================================

---- Ex 1.12
conwayRule :: Space Cell -> Cell
conwayRule space = helper (shouldDie (spaceNeighbours space))
  where
    helper True = Dead
    helper False = Alive


---- Ex 1.13
spaceShifts :: Space a -> Space (Space a)
spaceShifts (Space line) = quadrupleLineToDoubleSpace (lineShifts (mapLine lineShifts line))
  where
    quadrupleLineToDoubleSpace :: Line (Line (Line (Line a))) -> Space (Space a)
    quadrupleLineToDoubleSpace (Line left mid right) =
      Space (Line (map mapTripleLinesToLineOfSpace left)
                  (mapTripleLinesToLineOfSpace mid)
                  (map mapTripleLinesToLineOfSpace right))

--
------ Ex 1.4
---- | Render a space of 1x1 pictures.
--renderSpace :: Space Picture -> Picture
--renderSpace (Space line) = helper (map renderLine (lineToList line))
--  where
--    helper [] = blank
--    helper (picture:rest) = picture <> translated 0 (-1) (helper rest)
--
---- | Animate Conway's Game of Life,
---- starting with a given space
---- and updating it every second.
--animateConway :: Space Cell -> IO ()
--animateConway space = activityOf (0, space) helper (renderSpaceOfCells . snd)
--  where
--    helper (TimePassing dt) (prevTime, prevSpace)
--      | (prevTime + dt) >= 1 = (0, applyConwayRule prevSpace)
--      | otherwise = (prevTime + dt, prevSpace)
--    helper _ state = state
--    renderSpaceOfCells space = renderSpace (mapSpace renderCell space)
