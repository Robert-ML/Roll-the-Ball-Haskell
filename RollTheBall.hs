{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
-- import qualified Data.Set as S
import Data.Array as A
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {  tip :: Char,
                    north :: Bool,
                    sounth :: Bool,
                    west :: Bool,
                    east :: Bool
                    } deriving (Ord)

instance Eq Cell where
    Cell tip1 _ _ _ _ == Cell tip2 _ _ _ _ = tip1 == tip2

instance Show Cell where
    show (Cell tip1 _ _ _ _) = show tip1

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level { mat :: (A.Array (Int, Int) Cell)
                    } deriving (Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Eq Level where
    (Level mat1) == (Level mat2) = mat1 == mat2

{-
    (x, y)
    j0   y ->  j1
      0  1  2  3
 i0 0
  x 1
    2
 i1 3
-}
instance Show Level where
    show (Level mat_) = [endl] ++ foldr
                            (\ x z1
                                -> (foldr
                                        (\ (Cell tip_ _ _ _ _) z2
                                            -> [tip_] ++ z2) [] x)
                                    ++ [endl] ++ z1)
                        []
                        [[(mat_ A.! (x, y)) | y <- [j0 .. j1]] | x <- [i0 .. i1]]
                where
                    i0 = fst $ fst $ A.bounds mat_
                    i1 = fst $ snd $ A.bounds mat_
                    j0 = snd $ fst $ A.bounds mat_
                    j1 = snd $ snd $ A.bounds mat_
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel p = Level (A.array ((0, 0), (i1, j1))
                        (map
                                (\ x
                                    -> (x, (Cell emptySpace False False False False))
                                )
                            [(i, j) | i <- [i0 .. i1], j <- [j0 .. j1]]
                        )
                    )
            where
                i0 = 0
                i1 = fst p
                j0 = 0
                j1 = snd p

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (ch, pos) lv@(Level mat_)
                                | x < i0 || x > i1 || y < j0 || y > j1 = lv
                                | mat_ A.! pos == (Cell emptySpace False False False False) = (Level $ mat_ A.// [(pos, (Cell ch d1 d2 d3 d4))])
                                | otherwise = lv
                                    where
                                        d1
                                            | ch == verPipe || ch == botLeft || ch == botRight || ch == startUp || ch == winUp = True
                                            | otherwise = False
                                        d2
                                            | ch == verPipe || ch == topLeft || ch == topRight || ch == startDown  || ch == winDown = True
                                            | otherwise = False
                                        d3
                                            | ch == horPipe || ch == botRight || ch == topRight || ch == startLeft || ch == winLeft = True
                                            | otherwise = False
                                        d4
                                            | ch == horPipe || ch == topLeft || ch == botLeft || ch == startRight || ch == winRight = True
                                            | otherwise = False
                                        i0 = fst $ fst $ A.bounds mat_
                                        i1 = fst $ snd $ A.bounds mat_
                                        j0 = snd $ fst $ A.bounds mat_
                                        j1 = snd $ snd $ A.bounds mat_
                                        x = fst pos
                                        y = snd pos

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel colt vector = foldr (\x acc -> addCell x acc) (emptyLevel colt) vector


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir lv@(Level mat_)
    | scell_ == (Cell startUp False False False False) || scell_ == (Cell startDown False False False False) || scell_ == (Cell startLeft False False False False) || scell_ == (Cell startRight False False False False) = lv
    | scell_ == (Cell winUp False False False False) || scell_ == (Cell winDown False False False False) || scell_ == (Cell winLeft False False False False) || scell_ == (Cell winRight False False False False) = lv
    | tcell_ == (Cell emptySpace False False False False) = (Level (mat_ A.// [(pos, tcell_), (target_, scell_)]))
    | otherwise = lv
    where
        scell_ = mat_ A.! pos
        target_
                | dir == North && (fst pos) > (fst . fst $ A.bounds mat_) = ((fst pos) - 1, snd pos)
                | dir == South && (fst pos) < (fst . snd $ A.bounds mat_) = ((fst pos) + 1, snd pos)
                | dir == West && (snd pos) > (snd . fst $ A.bounds mat_) = (fst pos, (snd pos) - 1)
                | dir == East && (snd pos) < (snd . snd $ A.bounds mat_) = (fst pos, (snd pos) + 1)
                | otherwise = (-1, -1)
        tcell_
                | target_ == (-1, -1) = (Cell endl False False False False) -- o celula invalida
                | otherwise = mat_ A.! target_

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}

-- dar bine ca spuneti acum ca sa merg sa schimb structura Cell ca sa stie in ce capete se poate conecta o celula si sus sa fac un
-- tractor mare ca sa nu fac aici jos un tractor si mai mare
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell _ n1 s1 w1 e1) (Cell _ n2 s2 w2 e2) dir
    | dir == North =
        case () of
            ()  | n1 == s2 && n1 == True -> True
                | otherwise -> False
    | dir == South =
        case () of
            ()  | s1 == n2 && s1 == True -> True
                | otherwise -> False
    | dir == West =
        case () of
            ()  | w1 == e2 && w1 == True -> True
                | otherwise -> False
    | dir == East =
        case () of
            ()  | e1 == w2 && e1 == True -> True
                | otherwise -> False
    | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel lv@(Level mat_) = getStartCell lv (fst $ A.bounds mat_)

getStartCell :: Level -> Position -> Bool
getStartCell lv@(Level mat_ ) pos_
                    | curr_cell_type == startUp || curr_cell_type == startDown || curr_cell_type == startLeft || curr_cell_type == startRight = wonLevel_helper lv pos_ (-2,-2)
                    | x /= j1 && y == i1 = getStartCell lv (x + 1, i0)
                    | x == j1 && y == i1 = undefined
                    | otherwise = getStartCell lv (x, y + 1)

                    where
                        curr_cell_type = (tip (mat_ A.! pos_))
                        i0 = fst $ fst $ A.bounds mat_
                        i1 = fst $ snd $ A.bounds mat_
                        -- j0 = snd $ fst $ A.bounds mat_
                        j1 = snd $ snd $ A.bounds mat_
                        x = fst pos_
                        y = snd pos_

-- pos_ -> position of the current cell
-- old_pos_ -> position of the source cell to not go back and enter a loop
wonLevel_helper :: Level -> Position -> Position -> Bool
wonLevel_helper lv@(Level mat_) pos_ old_pos_
                    | curr_cell_type == winUp || curr_cell_type == winDown || curr_cell_type == winLeft || curr_cell_type == winRight = True
                    | next_cell_pos == (-2, -2) = False
                    | otherwise = wonLevel_helper lv next_cell_pos pos_
                    where
                        curr_cell = mat_ A.! pos_
                        curr_cell_type = tip curr_cell
                        next_cell_pos = getNextCell lv curr_cell pos_ old_pos_
                        -- next_cell = mat_ A.! next_cell_pos

-- pos_ -> position of the current cell
-- old_pos_ -> position of the source cell to not go back and enter a loop
-- returns the position of the next cell
getNextCell :: Level -> Cell -> Position -> Position -> Position
getNextCell (Level mat_) celula@(Cell _ d1 d2 d3 d4) pos_ old_pos_
                                    | x > i0 && d1 == True && x - 1 /= o_x && (connection celula (mat_ A.! (x - 1, y)) North) == True = (x - 1, y)
                                    | x < i1 && d2 == True && x + 1 /= o_x && (connection celula (mat_ A.! (x + 1, y)) South) == True = (x + 1, y)
                                    | y > j0 && d3 == True && y - 1 /= o_y && (connection celula (mat_ A.! (x, y - 1)) West) == True = (x, y - 1)
                                    | y < j1 && d4 == True && y + 1 /= o_y && (connection celula (mat_ A.! (x, y + 1)) East) == True = (x, y + 1)
                                    | otherwise = (-2, -2)
                                    where
                                        x = fst pos_
                                        y = snd pos_
                                        o_x = fst old_pos_
                                        o_y = snd old_pos_
                                        i0 = fst $ fst $ A.bounds mat_
                                        i1 = fst $ snd $ A.bounds mat_
                                        j0 = snd $ fst $ A.bounds mat_
                                        j1 = snd $ snd $ A.bounds mat_




instance ProblemState Level (Position, Directions) where
    successors lv@(Level mat_) = foldl (\ acc pos ->
                                        if tip (mat_ A.! pos) == emptySpace then
                                            (getPossibleMoves lv pos) ++ acc
                                        else
                                            acc
                                    ) [] parcurgere
        where
            parcurgere = [(v, u) | v <- [j0 .. j1], u <- [i0 .. i1]]
            i0 = fst $ fst $ A.bounds mat_
            i1 = fst $ snd $ A.bounds mat_
            j0 = snd $ fst $ A.bounds mat_
            j1 = snd $ snd $ A.bounds mat_
    isGoal = wonLevel 
    reverseAction (((x, y), dir), lv)
        | dir == North = (((x - 1, y), South), (moveCell (x - 1, y) South lv))
        | dir == South = (((x + 1, y), North), (moveCell (x + 1, y) North lv))
        | dir == West  = (((x, y - 1), East),  (moveCell (x, y - 1) East  lv))
        | dir == East  = (((x, y + 1), West),  (moveCell (x, y + 1) West  lv))
        | otherwise = undefined

data Reply a = Comment { content :: a}  |
    Tweet {content :: a}

instance (Eq a) => Eq (Reply a) where
    reply1 == reply2 = (content reply1) == (content reply2)

getPossibleMoves :: Level -> Position -> [((Position, Directions), Level)]
getPossibleMoves lv@(Level mat_) (x, y) = foldr (\ (Possibles valid dir new_pos_) acc ->
                                if valid == False then
                                    acc
                                else
                                    [((new_pos_, dir), (moveCell new_pos_ dir lv))] ++ acc
                            )
                            []
                            list_to_traverse
        where
            i0 = fst $ fst $ A.bounds mat_
            i1 = fst $ snd $ A.bounds mat_
            j0 = snd $ fst $ A.bounds mat_
            j1 = snd $ snd $ A.bounds mat_
            north_cell_type
                | x - 1 >= i0 = (tip (mat_ A.! (x - 1, y)))
                | otherwise = endl
            south_cell_type
                | x + 1 <= i1 = (tip (mat_ A.! (x + 1, y)))
                | otherwise = endl
            west_cell_type
                | y - 1 >= j0 = (tip (mat_ A.! (x, y - 1)))
                | otherwise = endl
            east_cell_type
                | y + 1 <= j1 = (tip (mat_ A.! (x, y + 1)))
                | otherwise = endl
            legit_north_cell
                | north_cell_type /= endl = not $ elem north_cell_type imovable_cells
                | otherwise = False
            legit_south_cell
                | south_cell_type /= endl = not $ elem south_cell_type imovable_cells
                | otherwise = False
            legit_west_cell
                | west_cell_type /= endl = not $ elem west_cell_type imovable_cells
                | otherwise = False
            legit_east_cell
                | east_cell_type /= endl = not $ elem east_cell_type imovable_cells
                | otherwise = False
            list_to_traverse = [(Possibles legit_north_cell South (x - 1, y)), (Possibles legit_south_cell North (x + 1, y)), (Possibles legit_west_cell East (x, y - 1)), (Possibles legit_east_cell West (x, y + 1))]
    
imovable_cells :: [Char]
imovable_cells = emptySpace : startCells ++ winningCells

data GetPossiblesHelperArray = Possibles { possible :: Bool,
                                            directia :: Directions,
                                            new_pos :: Position
                                            }
