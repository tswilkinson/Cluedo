module Main (main) where

import Data.Char
import Data.List(findIndex,findIndices)
import Control.Monad(forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as Map

type GridPosition = (Int,Int,Int)
type GridRow = (Int,Int)
type GridColumn = Int
type GridBlock = Int
data Disjunction = Two GridRow GridRow
                 | Three GridRow GridRow GridRow

data StateObject = SO (Map.Map GridPosition Bool)
                      (Map.Map GridRow Int)
                      (Map.Map GridColumn (Int,Int))
                      (Map.Map GridBlock Bool)
                      (Map.Map GridColumn [Disjunction])
                      Int

main :: IO ()
main = do
    putStrLn "please enter your first card:"
    l1 <- getLine
    let gp1 = case l1 of
                  (x:y:_)   -> (digitToInt x,digitToInt y,1)
                  _         -> error "invalid first card"

    putStrLn "please enter your second card:"
    l2 <- getLine
    let gp2 = case l2 of
                  (x:y:_)   -> (digitToInt x,digitToInt y,1)
                  _         -> error "invalid second card"

    putStrLn "please enter your third card:"
    l3 <- getLine
    let gp3 = case l3 of
                  (x:y:_)   -> (digitToInt x,digitToInt y,1)
                  _         -> error "invalid third card"
    
    putStrLn "which player accuses first?"
    l4 <- getLine
    let firstplayer = case l4 of
                          (x:_) -> digitToInt x
                          _     -> error "invalid first player"
    if firstplayer >= 1 && firstplayer <= 6 then return () else error "bad first player"

    let emptyStateObject = SO Map.empty
                              (Map.fromList $ [((1,i),0) | i <- [1..6]]
                                       ++ [((2,i),0) | i <- [1..6]]
                                       ++ [((3,i),0) | i <- [1..9]])
                              (Map.fromList [(i,(0,0)) | i <- [1..6]])
                              (Map.fromList [(1,False),(2,False),(3,False)])
                              Map.empty
                              firstplayer
    so <- execStateT (addToGrid [(gp1,True),(gp2,True),(gp3,True)]) emptyStateObject
    evalStateT loop so

addToGrid :: [(GridPosition,Bool)] -> StateT StateObject IO ()
addToGrid [] = return ()
addToGrid (x:xs) = do
    ys <- addToGrid' x
    addToGrid (ys++xs)

addToGrid' :: (GridPosition,Bool) -> StateT StateObject IO [(GridPosition,Bool)]
addToGrid' ((m,n,l),False) = do
    SO grid rowcounts columncounts successes disjunctions turn <- get
    let (oldv,grid') = Map.insertLookupWithKey (\_ a _ -> a) (m,n,l) False grid
    case oldv of
        Nothing -> do
            let (newrc,rowcounts') = Map.updateLookupWithKey (\_ a -> Just (a+1)) (m,n) rowcounts
            (successes',ys1) <- case newrc of
                Just 6 -> do
                    lift $ putStrLn $ "Block " ++ show m ++ " success, row " ++ show n
                    return (Map.insert m True successes,[])
                Just 5 -> case Map.lookup m successes of
                    Just True -> case findIndex (\i -> Map.notMember (m,n,i) grid') [1..6] of
                         Just i -> return (successes,[((m,n,i+1),True)])
                         Nothing -> error "weird"
                    _ -> return (successes,[])
                _ -> return (successes,[])

            let (newcc,columncounts') = Map.updateLookupWithKey (\_ (p,q) -> Just (p,q+1)) l columncounts
            ys2 <- case newcc of
                Just (p,18) -> if p == 3 then return [] else do
                    let ys21 = map (\i -> ((1,i+1,l),True)) $ findIndices (\i -> Map.notMember (1,i,l) grid') [1..6]
                        ys22 = map (\i -> ((2,i+1,l),True)) $ findIndices (\i -> Map.notMember (2,i,l) grid') [1..6]
                        ys23 = map (\i -> ((3,i+1,l),True)) $ findIndices (\i -> Map.notMember (3,i,l) grid') [1..9]
                    return (ys21 ++ ys22 ++ ys23)
                _ -> return []

            let disjunctions_list = case Map.lookup l disjunctions of
                    Just ds -> ds
                    Nothing -> []

                handle_disjunctions :: [Disjunction] -> ([Disjunction],[(GridPosition,Bool)])
                handle_disjunctions [] = ([],[])
                handle_disjunctions (d:ds) = let (es1,ys31) = handle_disjunction d
                                                 (es2,ys32) = handle_disjunctions ds
                                              in (es1++es2,ys31++ys32)

                handle_disjunction :: Disjunction -> ([Disjunction],[(GridPosition,Bool)])
                handle_disjunction (Two gp1 gp2) = if gp1 == (m,n) then
                    ([],[(addl gp2,True)]) else if gp2 == (m,n) then
                        ([],[(addl gp1,True)]) else ([Two gp1 gp2],[])
                handle_disjunction (Three gp1 gp2 gp3) = if gp1 == (m,n) then
                    ([Two gp2 gp3],[]) else if gp2 == (m,n) then
                        ([Two gp1 gp3],[]) else if gp3 == (m,n) then
                            ([Two gp1 gp2],[]) else ([Three gp1 gp2 gp3],[])

                addl :: (Int,Int) -> (Int,Int,Int)
                addl (a,b) = (a,b,l)

                (disjunctions_list',ys3) = handle_disjunctions disjunctions_list

                disjunctions' = Map.insert l disjunctions_list' disjunctions
            put $ SO grid' rowcounts' columncounts' successes' disjunctions' turn
            return $ ys1 ++ ys2 ++ ys3
        _ -> return []
addToGrid' ((m,n,l),True) = do
    SO grid rowcounts columncounts successes disjunctions turn <- get
    let (oldv,grid') = Map.insertLookupWithKey (\_ a _ -> a) (m,n,l) True grid
    case oldv of
        Nothing -> do
            let (oldrc,rowcounts') = Map.insertLookupWithKey (\_ a _ -> a) (m,n) 10 rowcounts
            let ys1 = case oldrc of
                    Just 5 -> []
                    _ -> map (\i -> ((m,n,i+1),False)) $ findIndices (\i -> Map.notMember (m,n,i) grid') [1..6]

            let (newcc,columncounts') = Map.updateLookupWithKey (\_ (p,q) -> Just (p+1,q)) l columncounts
            ys2 <- case newcc of
                Just (3,q) -> if q == 18 then return [] else do
                    let ys21 = map (\i -> ((1,i+1,l),False)) $ findIndices (\i -> Map.notMember (1,i,l) grid') [1..6]
                        ys22 = map (\i -> ((2,i+1,l),False)) $ findIndices (\i -> Map.notMember (2,i,l) grid') [1..6]
                        ys23 = map (\i -> ((3,i+1,l),False)) $ findIndices (\i -> Map.notMember (3,i,l) grid') [1..9]
                    return (ys21 ++ ys22 ++ ys23)
                _ -> return []

            let disjunctions_list = case Map.lookup l disjunctions of
                    Just ds -> ds
                    Nothing -> []

                keep_disjunction :: Disjunction -> Bool
                keep_disjunction (Two gp1 gp2) = gp1 /= (m,n) && gp2 /= (m,n)
                keep_disjunction (Three gp1 gp2 gp3) = gp1 /= (m,n) && gp2 /= (m,n) && gp3 /= (m,n)

                disjunctions_list' = filter keep_disjunction disjunctions_list

                disjunctions' = Map.insert l disjunctions_list' disjunctions
            put $ SO grid' rowcounts' columncounts' successes disjunctions' turn
            return $ ys1 ++ ys2
        _ -> return []

loop :: StateT StateObject IO ()
loop = do
    SO g _ _ _ _ n <- get
    case n of
        1 -> do
            let f :: Maybe Bool -> Char
                f Nothing = '-'
                f (Just True) = 'Y'
                f (Just False) = 'N'

                block1 = [[f (Map.lookup (1,i,j) g) | j <- [1..6]] | i <- [1..6]]
                block2 = [[f (Map.lookup (2,i,j) g) | j <- [1..6]] | i <- [1..6]]
                block3 = [[f (Map.lookup (3,i,j) g) | j <- [1..6]] | i <- [1..9]]

            lift $ do forM_ block1 putStrLn
                      putStrLn ""
                      forM_ block2 putStrLn
                      putStrLn ""
                      forM_ block3 putStrLn
                      putStrLn ""

            l1 <- lift $ do putStrLn "Your turn. What is your accusation?"
                            getLine
            let (a,b,c) = case l1 of
                    (x:y:z:_) -> (digitToInt x,digitToInt y,digitToInt z)
                    _ -> error "bad accusation"

            l2 <- lift $ do putStrLn "Which player responded?"
                            getLine
            let player = case l2 of
                    (x:_) -> digitToInt x
                    _ -> error "bad player"

            l3 <- lift $ do putStrLn "Which block did they show you?"
                            getLine
            let block = case l3 of
                    (x:_) -> digitToInt x
                    _ -> error "bad block"

            addToGrid [((1,a,i),False) | i <- [2..player-1]]
            addToGrid [((2,b,i),False) | i <- [2..player-1]]
            addToGrid [((3,c,i),False) | i <- [2..player-1]]

            case block of
                1 -> addToGrid [((1,a,player),True)]
                2 -> addToGrid [((2,b,player),True)]
                3 -> addToGrid [((3,c,player),True)]
                _ -> error "also bad block"

            modify (\(SO gr ro co su di _) -> SO gr ro co su di 2)
            
            lift $ putStrLn ""
            loop
        _ -> do
            l1 <- lift $ do putStrLn $ "What is player " ++ show n ++ "'s accusation?"
                            getLine
            let (a,b,c) = case l1 of
                    (x:y:z:_) -> (digitToInt x,digitToInt y,digitToInt z)
                    _ -> error "bad accusation"

            l2 <- lift $ do putStrLn "Which player responded?"
                            getLine
            let player = case l2 of
                    (x:_) -> digitToInt x
                    _ -> error "bad player"

                non_respondents = if player > n then [n+1..player-1]
                                                else [n+1..6]++[1..player-1]

            addToGrid [((1,a,i),False) | i <- non_respondents]
            addToGrid [((2,b,i),False) | i <- non_respondents]
            addToGrid [((3,c,i),False) | i <- non_respondents]

            SO g' _ _ _ _ _ <- get

            let addTwo :: GridRow -> GridRow -> StateObject -> StateObject
                addTwo gr1 gr2 (SO gr ro co su di tu) = SO gr ro co su (Map.alter go player di) tu
                    where go :: Maybe [Disjunction] -> Maybe [Disjunction]
                          go Nothing = Just [Two gr1 gr2]
                          go (Just ds) = Just (Two gr1 gr2:ds)

                addThree :: GridRow -> GridRow -> GridRow -> StateObject -> StateObject
                addThree gr1 gr2 gr3 (SO gr ro co su di tu) = SO gr ro co su (Map.alter go player di) tu
                    where go :: Maybe [Disjunction] -> Maybe [Disjunction]
                          go Nothing = Just [Three gr1 gr2 gr3]
                          go (Just ds) = Just (Three gr1 gr2 gr3:ds)

            case Map.lookup (1,a,player) g' of
                Nothing -> case Map.lookup (2,b,player) g' of
                    Nothing -> case Map.lookup (3,c,player) g' of
                        Nothing -> modify $ addThree (1,a) (2,b) (3,c)
                        Just True -> return ()
                        Just False -> modify $ addTwo (1,a) (2,b)
                    Just True -> return ()
                    Just False -> case Map.lookup (3,c,player) g' of
                        Nothing -> modify $ addTwo (1,a) (3,c)
                        Just True -> return ()
                        Just False -> addToGrid [((1,a,player),True)]
                Just True -> return ()
                Just False -> case Map.lookup (2,b,player) g' of
                    Nothing -> case Map.lookup (3,c,player) g' of
                        Nothing -> modify $ addTwo (2,b) (3,c)
                        Just True -> return ()
                        Just False -> addToGrid [((2,b,player),True)]
                    Just True -> return ()
                    Just False -> case Map.lookup (3,c,player) g' of
                        Nothing -> addToGrid [((3,c,player),True)]
                        Just True -> return ()
                        Just False -> error "paradox"

            case n of
                6 -> modify (\(SO gr ro co su di _) -> SO gr ro co su di 1)
                _ -> modify (\(SO gr ro co su di _) -> SO gr ro co su di (n+1))

            lift $ putStrLn ""
            loop
