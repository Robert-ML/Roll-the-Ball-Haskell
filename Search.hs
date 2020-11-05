{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
-- import Control.Applicative
-- import Debug.Trace
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {
    stare :: s,
    actiune :: Maybe a,
    dady :: Maybe (Node s a),
    copii :: [Node s a],
    adancime :: Int
}

instance Eq s => Eq (Node s a) where
    (Node s1_ _ _ _ _) == (Node s2_ _ _ _ _) = s1_ == s2_

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState (Node s_ _ _ _ _) = s_

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ (Just d_) _ _) = Just d_
nodeParent (Node _ _ _ _ _) = Nothing

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ _ ad_) = ad_

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ (Just ac_) _ _ _) = Just ac_
nodeAction (Node _ _ _ _ _) = Nothing

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ c_ _) = c_

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = Node s Nothing Nothing (map (\ x -> createStateSpace_sub_root x 1) succesori) 0
    where
        succesori = successors s

createStateSpace_sub_root :: (ProblemState s a, Eq s) => (a, s) -> Int -> Node s a
createStateSpace_sub_root (a, s) d = Node s (Just a) (Just parent) (map (\ x -> createStateSpace_sub_root x (d + 1)) succesori) d
    where
        past_action_state = reverseAction (a, s)
        parent
            | d == 1 = createStateSpace (snd past_action_state)
            | otherwise = (createStateSpace_sub_root past_action_state (d - 1))
        succesori = successors s
        -- succesori = my_delete (stare parent) (successors s) -- scoatem calea care ne intoarce in parinte

my_delete :: (ProblemState s a, Eq s) => s -> [(a, s)] -> [(a, s)]
my_delete _ []      = []
my_delete s_ (x:xs)
    | (snd x) == s_ = my_delete s_ xs
    | otherwise     = x : my_delete s_ xs


{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}
bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs n@(Node _ _ _ c_ _) = pereche : (bfs_deeper (snd pereche) (n : (snd pereche)))
    where
        pereche = (c_, c_)

bfs_deeper :: Ord s => [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfs_deeper [] _     = []
bfs_deeper (x:xs) p = (copii_noi, xs ++ copii_noi) : (bfs_deeper (xs ++ copii_noi) (p ++ copii_noi))
    where
        copii_noi = delete_repetitii (copii x) p

delete_repetitii :: Eq s => [Node s a] -> [Node s a] -> [Node s a]
delete_repetitii c p = filter (\ x -> not (elem x p)) c

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

-- start_stream [(copii_noi, coada)]
-- finish_stream [(copii_noi, coada)]
-- finish daca un copil se gaseste intr-o coada pe diagonala in chestiile de mai sus
bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start finish = bidirBFS_deeper start_stream finish_stream
    where
        start_stream = bfs start
        finish_stream = bfs finish

bidirBFS_deeper :: Eq s => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
bidirBFS_deeper [] _ = error "Dude, ce ne faceeem?"
bidirBFS_deeper _ [] = error "Nu stiu duuude..."
bidirBFS_deeper ((start_noi, start_frontiera):xs) ((finish_noi, finish_frontiera):ys)
    | null start_in_finish == False = (head start_in_finish, extract_equal (head start_in_finish) finish_frontiera)
    | null finish_in_start == False = (head finish_in_start, extract_equal (head finish_in_start) start_frontiera)
    | otherwise = bidirBFS_deeper xs ys
    where
        start_in_finish = filter (\ x -> elem x finish_frontiera) start_noi 
        finish_in_start = filter (\ x -> elem x start_frontiera) finish_noi

extract_equal :: Eq s => Node s a -> [Node s a] -> Node s a
extract_equal el l = head $ filter (\ x -> x == el) l


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

-- debug = flip trace

extractPath :: Node s a -> [(Maybe a, s)]
extractPath n = reverse (extractPath_helper n)

extractPath_helper :: Node s a -> [(Maybe a, s)]
extractPath_helper (Node s_ a_ d_ _ depth_)
    | depth_ >= 1 = (a_, s_) : extractPath (fromJust d_)
    | otherwise = [(Nothing, s_)]

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start finish = (extractPath from_start) ++ solutie_finish
    where
        node_start = createStateSpace start
        node_finish = createStateSpace finish
        (from_start, from_finish) =  bidirBFS node_start node_finish
        finish_path_f_s = extractPath from_finish
        finish_path_s1_sn = reverse $ tail finish_path_f_s
        solutie_finish = map (\ x -> make_reversed_action x) finish_path_s1_sn

make_reversed_action :: (ProblemState s a) => (Maybe a, s) -> (Maybe a, s)
make_reversed_action (actiune_, stare_) = the_good_reversed_a_s
    where
        reversed_a_s = reverseAction ((fromJust actiune_), stare_)
        maybe_a = Just (fst reversed_a_s)
        reversed_s = snd reversed_a_s
        the_good_reversed_a_s = (maybe_a, reversed_s)