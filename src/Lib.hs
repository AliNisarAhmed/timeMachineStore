{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import           Data.Graph
import           Data.List
import qualified Data.Map   as M
import qualified Data.Set   as S
import           Data.Tree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (d - b) * (d - b)
  centroid list =
    let
      (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) list
      n = fromIntegral $ length list
    in
      ( u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

type Threshold = Double

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v]) -- function generating initial values
       -> Int -- number of centroids
       -> [e] -- the information (points)
       -> Threshold -- threshold of when to stop iterating
       -> [v] -- centroids after convergence
kMeans f k points = kMeans' (f k points) points

kMeans' :: (Vector v, Vectorizable e v)
        => [v] -> [e] -> Threshold -> [v]
kMeans' centroids points threshold =
  let
    assignments = clusterAssignmentPhase centroids points
    oldNewCentroids = newCentroidPhase assignments
    newCentroids = map snd oldNewCentroids
  in
    if shouldStop oldNewCentroids threshold
      then newCentroids
      else kMeans' newCentroids points threshold

initializeSample :: Int -> [e] -> [(Double, Double)]
initializeSample 0 _ = []
initializeSample n v =
  (fromIntegral n, fromIntegral n) : initializeSample (n - 1) v

info = [(1,1),(1,2),(4,4),(4,5)]::[(Double,Double)]


clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in
    foldr step initialMap points
  where
    step p m =
      let chosenC = minimumBy (compareDistances p) centroids
      in M.adjust (p:) chosenC m
    compareDistances p x y = compare (distance x $ toVector p) (distance y $ toVector p)

-- minimumBy :: (a -> a -> Ordering) -> [a] -> a
-- compare :: Ord a => a -> a -> Ordering

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

-------------------------------------------------------------


sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
  where
    sumForce' [] s = s
    sumForce' (y:ys) acc =
        sumForce' ys $! (y + acc)
        -- evalute (y + acc) and then apply the function "sumForce' ys"

---------------------------------------------------------------

-- primes :: [Integer]
-- primes = [ x | x <- [2..], let y = head x, mod x y /= 0]


---------------------------------------------------------------

data TimeMachine
  = TM { manufacturer :: String, year :: Integer }
  deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y + 1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

----------------------------------------------------------------

newtype MyMaybe a = MyMaybe (Maybe a) deriving (Eq, Show, Ord)

instance Functor MyMaybe where
  fmap f (MyMaybe (Just x)) = MyMaybe $ Just $ f x
  fmap _ (MyMaybe Nothing)  = MyMaybe Nothing

instance Foldable MyMaybe where
  foldMap f (MyMaybe (Just x)) = f x
  foldMap f (MyMaybe Nothing)  = mempty

----------------------------------------------------------------

-- Binary Tree implementation with a cache

data BinaryTree3 v c
  = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
  | Leaf3
  deriving (Eq, Show, Ord)

treeInsert3 :: (Ord v, Ord c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 left right) =
  case compare v v2 of
    EQ -> Node3 v2 c2 left right
    LT -> Node3 v2 (min c c2) (treeInsert3 v c left) right
    GT -> Node3 v2 (min c c2) left (treeInsert3 v c right)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c)
            => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 left right) =
  case compare v v2 of
    EQ -> Node3 v2 c2 left right
    LT ->
      let
        newLeft = treeInsert4 v c left
        newCache = c <> cached newLeft <> c2
      in
        Node3 v2 newCache newLeft right
    GT ->
      let
        newRight = treeInsert4 v c right
        newCache = c <> c2 <> cached newRight
      in
        Node3 v2 newCache left newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty


----------------------------------------------------------------

data BinaryTree2 a
  = Node2 a (BinaryTree2 a) (BinaryTree2 a)
  | Leaf2
  deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind2 t l
    GT -> treeFind2 t r
treeFind2 _ Leaf2 = Nothing

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t n@(Node2 v left right) =
  case compare t v of
    EQ -> n
    LT -> Node2 v (treeInsert2 t left) right
    GT -> Node2 v left (treeInsert2 t right)
treeInsert2 t Leaf2 = Node2 t Leaf2 Leaf2

newtype TGByPrice = TGByPrice TravelGuide deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

instance Functor BinaryTree2 where
  fmap f Leaf2                = Leaf2
  fmap f (Node2 v left right) = Node2 (f v) (fmap f left) (fmap f right)

instance Foldable BinaryTree2 where
  foldMap f (Leaf2)              = mempty
  foldMap f (Node2 v left right) = f v <> foldMap f left <> foldMap f right

----------------------------------------------------------------

data BinaryTree1
  = Node1 TravelGuide BinaryTree1 BinaryTree1
  | Leaf1
  deriving (Eq, Show, Ord)

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v left right) =
  case compare t v of
    EQ -> Just v
    LT -> treeFind1 t left
    GT -> treeFind1 t right
treeFind1 _ Leaf1 = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v left right) =
  case compare t v of
    EQ -> n
    LT -> Node1 v (treeInsert1 t left) right
    GT -> Node1 v (treeInsert1 t right) left
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1


data TravelGuide
  = TravelGuide { title   :: String
                , authors :: [String]
                , price   :: Double }
                deriving (Show, Eq, Ord)

-----------------------------------------------------------------------

class Nameable n where
  name :: n -> String

instance Nameable (Client a) where
  name Individual { person = Person { firstName = f, lastName = n }}
    = f ++ " " ++ n
  name client = clientName client

-- class Priceable a where
--   price :: a -> Double

-- instance Priceable TravelGuide where
--   price = getPrice

-- instance Priceable Tools where
--   price = toolPrice

-- totalPrice :: Priceable p => [p] -> Double
-- totalPrice = sum . map (price)

instance Eq Person where
  (==) (Person { firstName = f1, lastName = n1 }) (Person { firstName = f2, lastName = n2 })
    = f1 == f2 && n1 == n2

instance Eq a => Eq (Client a) where
  (==) (GovOrg id1 n1) (GovOrg id2 n2) = id1 == id2 && n1 == n2
  (==) (Company id1 n1 p1 d1) (Company id2 n2 p2 d2) =
    id1 == id2 && n1 == n2 && p1 == p2 && d1 == d2
  (==) (Individual id1 p1) (Individual id2 p2) = id1 == id2 && p1 == p2
  (==) _ _ = False

instance Ord Person where
  compare (Person { firstName = f1, lastName = n1}) (Person { firstName = f2, lastName = n2})
    | f1 > f2 = GT
    | f1 < f2 = LT
    | n1 > n2 = GT
    | n1 < n2 = LT
    | otherwise = EQ

instance Ord a => Ord (Client a) where
  compare c1 c2
    | name c1 > name c2 = GT
    | name c1 < name c2 = LT
  compare (Individual { person = p1}) (Individual { person = p2})
    = compare p1 p2
  compare (Individual {}) _ = GT
  compare _ (Individual {}) = LT
  compare (Company { person = p1, duty = d1 }) (Company { person = p2, duty = d2})
    | compare p1 p2 == EQ = compare p1 p2
    | otherwise = compare d1 d2
  compare (Company {}) _ = GT
  compare _ (Company {}) = LT
  compare _ _ = LT

-----------------------------------------------------------------------

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
  [("wood","wood",["walls"]), ("plastic","plastic",["walls","wheels"])
  ,("aluminum","aluminum",["wheels","door"]),("walls","walls",["done"])
  ,("wheels","wheels",["done"]),("door","door",["done"]),("done","done",[])]

timeMachinePrecedence :: (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel = buildG (103, 2013)
  [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
  ,(2013,1408),(1408,1993),(1408,917),(1993,917),(917,103),(103,917)]

-------------------------------------------------------------------------

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees)
  = let subtreesTraversed = concat $ map (preOrder f) subtrees
  in f v : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                              , Node 6 [] ]


-----------------------------------------------------------------

data Client a
  = GovOrg { clientId :: a, clientName :: String }
  | Company { clientId :: a, clientName :: String, person :: Person, duty :: String}
  | Individual { clientId :: a, person :: Person }
  deriving (Show)

data Person
  = Person { firstName :: String, lastName :: String }
  deriving (Show)

data ClientKind
  = GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Eq, Ord, Show)

data Tools
  = Tools { toolPrice :: Double }
  deriving (Eq, Ord, Show)

emptyMap :: M.Map ClientKind (S.Set (Client Integer))
emptyMap =
  M.fromList [
    (GovOrgKind, S.empty)
  , (CompanyKind, S.empty)
  , (IndividualKind, S.empty)
  ]

-- Classify Clients

-- Method 1

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients = foldr step emptyMap
  where
    step client@(GovOrg {}) map =
      M.adjust (S.insert client) GovOrgKind map
    step client@(Company {}) map =
      M.adjust (S.insert client) CompanyKind map
    step client@(Individual {}) map =
      M.adjust (S.insert client) IndividualKind map

-- Method 2

setOfGov :: [Client Integer] -> S.Set (Client Integer)
setOfGov = S.fromList . filter func
  where
    func (GovOrg {}) = True
    func _           = False

setOfComp :: [Client Integer] -> S.Set (Client Integer)
setOfComp = S.fromList . filter func
  where
    func (Company {}) = True
    func _            = False

setOfIndiv :: [Client Integer] -> S.Set (Client Integer)
setOfIndiv = S.fromList . filter func
  where
    func (Individual {}) = True
    func _               = False

classifyClients2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients2 list =
  M.fromList [
    (GovOrgKind, setOfGov list)
  , (CompanyKind, setOfComp list)
  , (IndividualKind, setOfIndiv list)
  ]

listOfClients :: [Client Integer]
listOfClients = [
    GovOrg 1 "Canada Govt"
  , Company 2 "Punchcard" (Person "Sam" "Jenkins") "Digital Platforms"
  , Individual 3 (Person "Ali" "Ahmed")
  , Individual 4 (Person "Ali" "Ahmef")
  , Individual 6 (Person "Samrah" "Akber")
  , GovOrg 7 "Alberta Govt"
  , Company 8 "Ecofitt" (Person "Jay" "Sayno") "Data Collection"
  , Individual 9 (Person "Alex" "Li")
  ]



-- M.alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a

-- myInsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
-- myInsert k v map = M.alter (\_ -> Just v) k map

-- m1 = M.singleton "hello" 3
-- m2 = myInsert "bye" 2 m1

-- myDelete :: Ord k => k -> M.Map k a -> M.Map k a
-- myDelete = M.alter (\_ -> Nothing)

-- m25 = myDelete "bye" m2

-- myAdjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
-- myAdjust f = M.alter (fmap f)

-- m3 = myAdjust (+100) "bye" m2

