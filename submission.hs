-- All ADTs were followed by "deriving (Eq, Show)" to print and compare:

-- ADT: There were four small squares in a big square (quadtree)
-- "Four a" could be used to preserve four ordered elements, used in two kinds of "trees" below
data Four a = Four a a a a 
    deriving (Eq, Show)

-- ADT: Store the internal structure of quadtrees without size information
-- For example, for the small square in a big square (the child node of a root), it could be pure Black, pure White or still four small squares in a child node
data Coretree = B | W | C (Four Coretree) 
    deriving (Eq, Show)

-- Clockwise (Four a b c d):
-- a b
-- d c
-- ADT: External interfaces of quadtrees with both color and size information
-- Clockwise was used to represent the situation four small suquares in a child node from the root 
-- For example, Black 3 means 2^3 * 2^3 black squares one by one
data Quadtree = Black Int | White Int | Clockwise (Four Quadtree) 
    deriving (Eq, Show)

-- ADT: Pathstep was used to record movement trend history on the "tail/path" from the root to the square (subtree), represented by [list]
-- For example, [LU, RD]: the upper-left subtree was selected firstly, then the lower-right subtree of this upper-left subtree was selected
data Pathstep = LU | RU | LD | RD 
    deriving (Eq, Show)
    

-- ex1 definition -- 

-- Several functions could be used to construct the Quadtree
-- Black and White were declared specifically and it was allowed to input the size information though not significant
allBlack :: Int -> Quadtree
allBlack = Black

allWhite :: Int -> Quadtree
allWhite = White

-- input: Four ordered quadtrees and return: One quadtree
-- clockwise --
-- a b
-- d c
clockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
clockwise a b c d = Clockwise (Four a b c d)

-- input: Four ordered quadtrees and return: One quadtree
-- anticlockwise -- 
-- a d
-- b c
anticlockwise :: Quadtree -> Quadtree -> Quadtree -> Quadtree -> Quadtree
anticlockwise a b c d = Clockwise (Four a d c b)


-- ex2 blur --

-- The Quadtree ADT was changed as the Coretree ADT
quadtocore :: Quadtree -> Coretree
-- If the subtree was pure Black or pure White, just change it without size consideration
quadtocore (Black _) = B
quadtocore (White _) = W
-- If the subtree was still quadtree, the recursion was required
-- Every subtree was viewed as a new root node and if it still had the quadtree, the recursion was kept until the Black/White information exposed
quadtocore (Clockwise (Four a b c d)) = C (Four (quadtocore a) (quadtocore b) (quadtocore c) (quadtocore d))

-- The 'depth' (size was not accurate here) of coretree was derived; /geometrically, depth=height(tree)-1/
-- There was no record for the pure Black or pure White so the max depth of subtrees should be added by one filling the level when the pure color was ignored
treedepth :: Coretree -> Int
treedepth B = 0
treedepth W = 0
-- The max function was used to find the max element in foldl arguments 
-- When the quadtree(subtrees) was found, map function was used to reflect the depth of subtrees recursively through passing subtrees [a, b, c, d]
treedepth (C (Four a b c d)) = 1 + foldl max 0 (map treedepth [a, b, c, d])

-- The path/tail to reach the coordinate
-- The path list would be returned directly if the pure color was found, since there was no need to continuously subdivide that tree
getPathstepHelper :: Coretree -> (Int, Int) -> Int -> [Pathstep] -> Maybe [Pathstep]
-- The color of the square was checked: pure Black? or pure White?
-- The lower-left square was (0,0), and the upper-right square was (2^n -1, 2^n -1)
getPathstepHelper B (x, y) _ acc = Just acc  
getPathstepHelper W (x, y) _ acc = Just acc 
-- The recursion was kept if the quadtree (subtree) was found and record the current (depth-1), used to continue the recursion
getPathstepHelper t (x, y) ts acc =
    let maxxy = 2^ts - 1  -- calculate the upper-right coordinate index through the depth, passed by the designed treedepth function
    -- For example, the depth was 2 with 2^2 * 2^2 squares, its lower-left was (0, 0) and upper-right was (3, 3)
        in if x < 0 || x > maxxy || y < 0 || y > maxxy || ts < 0  -- BEYOND
               then Nothing 
           else
               let mid = 2 ^ (ts - 1) - 1  -- intermediate limit
                   in let C (Four a b c d) = t  -- Four a
                   -- discuss respectively for upper-right, lower-right, upper-left or lower-left
                   in if x > mid  
                      then  -- right main part
                          if y > mid  
                               -- The second quadrant would derive a upper-right movement trend and the depth must be subtracted to 
                               -- make sure the representaion for the upper-right index or intermediate limit CORRECT in the next recursion
                              then getPathstepHelper b (x - mid - 1, y - mid - 1) (ts - 1) (acc ++ [RU])  -- For example, depth=2, with 2^2*2^2 squares: (2,2) at the second quadrant, it became (0, 0) after one time of getPathstepHelper
                          else   
                              getPathstepHelper c (x - mid - 1, y) (ts - 1) (acc ++ [RD])  -- the third quadrant
                      else  -- left main part
                          if y > mid  -- the first quadrant
                              then getPathstepHelper a (x, y - mid - 1) (ts - 1) (acc ++ [LU])  -- the index of y-axis was considered
                          else  --the fourth quadrant
                              getPathstepHelper d (x, y) (ts - 1) (acc ++ [LD])  -- not considered and keep original 

-- Get the tree depth through the defined function and return the [Pathstep]
getPathstep :: Coretree -> (Int, Int) -> Maybe [Pathstep]
getPathstep t (x, y) = getPathstepHelper t (x, y) (treedepth t) []  

-- Derive the lower-left and upper-right coordinates along the [Pathstep]
-- record the current path, lower-left and the depth 
-- prepare for the blur
getMinMaxHelper :: Coretree -> [Pathstep] -> (Int, Int, Int) -> (Int, Int, Int, Int)
-- depth=3, 8*8 squares: (0, 4) without Pathstep represents the min coordinate of the upper-left subtree and this subtree's upper-right coordinate was computed as the (3, 7)
getMinMaxHelper t [] (accx, accy, sz) = (accx, accy, accx + 2 ^ sz - 1, accy + 2 ^ sz - 1)
-- If the recursion was required (Pathstep list is not null), deriving the lower-left coordinate of the new subtree with the updated depth and computing the upper-right coordinate
-- depth=3, 8*8: (0, 0) executed the LD, and it became the min coordinate of the lower-left subtree, its upper-right was computed as (3, 3) not (7, 7), its depth was 2 not 3
getMinMaxHelper t (x : xs) (accx, accy, sz) =
    case x of
    LU -> getMinMaxHelper t xs (accx, accy + 2 ^ (sz - 1), sz - 1)
    RU -> getMinMaxHelper t xs (accx + 2 ^ (sz - 1), accy + 2 ^ (sz - 1), sz - 1)
    RD -> getMinMaxHelper t xs (accx + 2 ^ (sz - 1), accy, sz - 1)
    LD -> getMinMaxHelper t xs (accx, accy, sz - 1)

-- Get the tree depth through the defined function and return the (min, min, max, max)
getMinMax :: Coretree -> [Pathstep] -> (Int, Int, Int, Int)
getMinMax t ts = getMinMaxHelper t ts (0, 0, treedepth t)

-- Derive the expected coretree subtree along any Pathstep list
getsubtree :: Coretree -> [Pathstep] -> Coretree
getsubtree B _ = B
getsubtree W _ = W
getsubtree t [] = t -- default itself
-- If the received argument was the quadtree: "x" was the first element of the path list, rest of part was "xs"
getsubtree (C (Four a b c d)) (x : xs) =
    case x of
    -- Continue recursion according to the subtree and rest of the Pathstep list until getting the Black or White tag 
    LU -> getsubtree a xs
    RU -> getsubtree b xs
    RD -> getsubtree c xs
    LD -> getsubtree d xs

-- Remove the duplicate elements (path) and would be used when considering situations around the tree
-- Sometimes, same [Pathstep] would be found when the two next to each other subtrees near to the target square (but these two "squares" belonged to a whole subtree, which was a pure White or pure Black)
-- Since we checked the coordinates in "getedgecount" function one by one ...... and computer cannot see whose color was the same (in that case, it was a pure coretree so we cannot count twice)
removeDup :: (Eq a) => [a] -> [a]  -- same attribute
removeDup [] = []  -- special case with nothing
removeDup [x] = [x]  -- special case with one item
removeDup (x : xs) = x : [k | k <- removeDup xs, k /= x]  -- only the unique element could be added as k

getedgecount :: Coretree -> [Pathstep] -> (Int, Int)
--                                          B    W
-- The number of black and white cells around the target along the [Pathstep] list                                 
getedgecount t ts =
    let (x1, y1, x2, y2) = getMinMax t ts in -- catch the lower-left and the upper-right index
            let pos = [(x1 - 1, y) | y <- [y1 .. y2]] 
                   ++ [(x2 + 1, y) | y <- [y1 .. y2]] 
                   ++ [(x, y1 - 1) | x <- [x1 .. x2]] 
                   ++ [(x, y2 + 1) | x <- [x1 .. x2]]
            in -- All the coordinates around it were computed one by one through the target's min and max coordinates and put them in the pos list through the list comprehension like [y1 .. y2] or [x1 .. x2]
                let res = removeDup [getPathstep t xy | xy <- pos] -- remove duplicate paths for the pure color case and ready to count the color cells
                    -- consider the [Pathstep] in the foldl function
                    -- if we get the LU or RU or RD or LD from the res, we need to justify the situation through passing these tails to the getsubtree function
                    -- and check the attribute of the subtree coretree B? W? and did count
                    in foldl ( \acc ans -> case ans of
                              Nothing -> acc  -- if Nothing was found, directly return the scores
                              Just tr -> let (bc, wc) = acc
                                         in case getsubtree t tr of  -- getsubtree :: Coretree -> [Pathstep] -> Coretree
                                         B -> (bc + 1, wc)  -- Black++
                                         W -> (bc, wc + 1)) -- White++
                    (0, 0) -- initial scores as the first argument (default)
                    res -- scores, which could be derived from the "res" path list as the second argument

-- When the blur was executing, the [Pathstep] should be passed to the getedgecount function
-- The color of subtrees could be changed here
blurHelper :: Coretree -> [Pathstep] -> Coretree
blurHelper t ts =
  -- Check the attribute of the subtree
  let curr = getsubtree t ts
      in case curr of
      -- get related scores along the pathstep list and check realted scores on the B and W
      B ->
          let (bc, wc) = getedgecount t ts  
              in if wc > bc then W else B  -- if (wc>bc) ? W : B
      W ->
          let (bc, wc) = getedgecount t ts
              in if wc < bc then B else W  -- if (wc<bc) ? B : W
      -- FREQUENT case: continue doing the recursion until the B/W exposed, but the related path/tail MUST be recorded and passing to the getedgecount as the argument to do count
      _ ->
          let processchild dir = blurHelper t (ts ++ [dir])  -- previous path+current path direction
             in C (Four (processchild LU) (processchild RU) (processchild RD) (processchild LD))  -- tail/paths could be ADDED through recursive processchild back (clockwise)

-- Turn the Coretree ADT into the Quadtree ADT with the right "SIZE" (SIZE != DEPTH here)
coretoquad :: Coretree -> Int -> Quadtree
coretoquad B sz = Black sz
coretoquad W sz = White sz
coretoquad (C (Four a b c d)) sz =
  -- 2 ^ (n-1) because of the quadtree structure until the pure Black and White with related size information
    Clockwise (Four (coretoquad a (sz `div` 2)) (coretoquad b (sz `div` 2)) (coretoquad c (sz `div` 2)) (coretoquad d (sz `div` 2)))

-- blur 
blur :: Quadtree -> Quadtree
blur t =
  let ct = quadtocore t
   in let nct = blurHelper ct []
       in coretoquad nct (2 ^ treedepth nct)  -- treedepth
-- Firstly, the Quadtree ADT was changed as the Coretree ADT
-- Blurhelper function would provide the Pathstep through the recursive processchild, passing to the getedgecount, 
-- which contained two small useful functions "getPathstep" and "getMinMax" to do count for B/W, finishing the BLUR
-- Finally, the Quadtree ADT was created back with the correct size from the designed treedepth function


-- ex3 open-ended

-- The size information shoudl be ignored, therefore, the quadtocore function could be used directly
-- There was another algorithm report must be finished... the part three had to be gien up though the idea had been finished......

-- coarsework :: x -> Coretree -> f(x) -> f(x)


-- f(x)


-- my_solution :: Coretree -> f(x) -> Bool
