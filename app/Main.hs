module Main (main) where

import Data.Char
import Data.Maybe
import Data.List(findIndex,findIndices)
import Control.Monad(forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as Map

type GridPosition = (Int,Int,Int)
type GridRow = (Int,Int)
type GridColumn = Int
type GridBlock = Int

data StateObject = SO (Map.Map GridPosition Bool)
                      (Map.Map GridRow Int)
                      (Map.Map GridColumn (Int,Int))
                      (Map.Map GridBlock Int)

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
            Map.empty
        jso = addToGrid [(gp1,True),(gp2,True),(gp3,True)] emptyStateObject
    case jso of
        Just so -> evalStateT loop ([so],firstplayer)
        Nothing -> error "confusing"

addToGridStateT :: [(GridPosition,Bool)] -> StateT ([StateObject],Int) IO ()
addToGridStateT xs = do
    (sos,n) <- get
    put (mapMaybe (addToGrid xs) sos,n)

addToGrid :: [(GridPosition,Bool)] -> StateObject -> Maybe StateObject
addToGrid [] so = Just so
addToGrid (x:xs) so = do
    (so',ys) <- addToGrid' x so
    addToGrid (ys++xs) so'

addToGrid' :: (GridPosition,Bool) -> StateObject -> Maybe (StateObject,[(GridPosition,Bool)])
addToGrid' ((m,n,l),False) (SO grid rowcounts columncounts successes) = 
    let (oldv,grid') = Map.insertLookupWithKey (\_ a _ -> a) (m,n,l) False grid in
    case oldv of
        Nothing ->
            let (newrc,rowcounts') = Map.updateLookupWithKey (\_ a -> Just (a+1)) (m,n) rowcounts
                (successes',ys1) = case newrc of
                    Just 6 -> (Map.insert m n successes,[])
                    Just 5 -> case Map.lookup m successes of
                        Just _ -> case findIndex (\i -> Map.notMember (m,n,i) grid') [1..6] of
                            Just i -> (successes,[((m,n,i+1),True)])
                            Nothing -> error "weird"
                        _ -> (successes,[])
                    _ -> (successes,[])

                (newcc,columncounts') = Map.updateLookupWithKey (\_ (p,q) -> Just (p,q+1)) l columncounts
                ys2 = case newcc of
                    Just (p,18) -> if p == 3 then [] else
                        let ys21 = map (\i -> ((1,i+1,l),True))
                                       (findIndices (\i -> Map.notMember (1,i,l) grid') [1..6])
                            ys22 = map (\i -> ((2,i+1,l),True))
                                       (findIndices (\i -> Map.notMember (2,i,l) grid') [1..6])
                            ys23 = map (\i -> ((3,i+1,l),True)) 
                                       (findIndices (\i -> Map.notMember (3,i,l) grid') [1..9])
                         in (ys21 ++ ys22 ++ ys23)
                    _ -> []
             in Just (SO grid' rowcounts' columncounts' successes',ys1++ys2)
        Just True -> Nothing
        Just False -> Just (SO grid rowcounts columncounts successes,[])
addToGrid' ((m,n,l),True) (SO grid rowcounts columncounts successes) =
    let (oldv,grid') = Map.insertLookupWithKey (\_ a _ -> a) (m,n,l) True grid in
    case oldv of
        Nothing ->
            let (oldrc,rowcounts') = Map.insertLookupWithKey (\_ a _ -> a) (m,n) 10 rowcounts
                ys1 = case oldrc of
                    Just 5 -> []
                    _ -> map (\i -> ((m,n,i+1),False)) $ findIndices (\i -> Map.notMember (m,n,i) grid') [1..6]

                (newcc,columncounts') = Map.updateLookupWithKey (\_ (p,q) -> Just (p+1,q)) l columncounts
                ys2 = case newcc of
                    Just (3,q) -> if q == 18 then [] else
                        let ys21 = map (\i -> ((1,i+1,l),False)) 
                                       (findIndices (\i -> Map.notMember (1,i,l) grid') [1..6])
                            ys22 = map (\i -> ((2,i+1,l),False)) 
                                       (findIndices (\i -> Map.notMember (2,i,l) grid') [1..6])
                            ys23 = map (\i -> ((3,i+1,l),False)) 
                                       (findIndices (\i -> Map.notMember (3,i,l) grid') [1..9])
                         in (ys21 ++ ys22 ++ ys23)
                    _ -> []
             in Just (SO grid' rowcounts' columncounts' successes,ys1++ys2)
        Just False -> Nothing
        Just True -> Just (SO grid rowcounts columncounts successes,[])

loop :: StateT ([StateObject],Int) IO ()
loop = do
    (sos,n) <- get
    case n of
        1 -> do
            let grs :: [Map.Map GridPosition Bool]
                grs = map (\(SO gr _ _ _) -> gr) sos
                
                g :: [Maybe Bool] -> Char
                g [] = error "odd"
                g (Nothing:_) = '-'
                g (Just True:xs)  = if all (== Just True) xs then 'Y' else '-'
                g (Just False:xs) = if all (== Just False) xs then 'N' else '-'

                block1 = [[g $ map (Map.lookup (1,i,j)) grs | j <- [1..6]] | i <- [1..6]]
                block2 = [[g $ map (Map.lookup (2,i,j)) grs | j <- [1..6]] | i <- [1..6]]
                block3 = [[g $ map (Map.lookup (3,i,j)) grs | j <- [1..6]] | i <- [1..9]]

            lift $ do forM_ block1 putStrLn
                      putStrLn ""
                      forM_ block2 putStrLn
                      putStrLn ""
                      forM_ block3 putStrLn
                      putStrLn ""

            l1 <- lift $ do putStrLn "Your turn. What is your accusation?"
                            getLine
            case l1 of
                ('n':_) -> return ()
                (x:y:z:_) -> do
                    let (a,b,c) = (digitToInt x,digitToInt y,digitToInt z)

                    player <- lift $ do putStrLn "Which player responded?"
                                        l2 <- getLine
                                        return $ case l2 of
                                            (k:_) -> digitToInt k
                                            _ -> error "bad player"

                    if player == 1 then do
                            addToGridStateT [((1,a,i),False) | i <- [2..6]]
                            addToGridStateT [((2,b,i),False) | i <- [2..6]]
                            addToGridStateT [((3,c,i),False) | i <- [2..6]]
                        else do
                            block <- lift $ do putStrLn "Which block did they show you?"
                                               l3 <- getLine
                                               return $ case l3 of
                                                   (k:_) -> digitToInt k
                                                   _ -> error "bad block" 

                            addToGridStateT [((1,a,i),False) | i <- [2..player-1]]
                            addToGridStateT [((2,b,i),False) | i <- [2..player-1]]
                            addToGridStateT [((3,c,i),False) | i <- [2..player-1]]

                            case block of
                                1 -> addToGridStateT [((1,a,player),True)]
                                2 -> addToGridStateT [((2,b,player),True)]
                                3 -> addToGridStateT [((3,c,player),True)]
                                _ -> error "also bad block"
                _ -> error "bad accusation"

            (sos',_) <- get

            let sucs :: [Map.Map GridBlock Int]
                sucs = map (\(SO _ _ _ su) -> su) sos'

                h :: [Maybe Int] -> Maybe Int
                h [] = error "bizarre"
                h (Nothing:_) = Nothing
                h (Just b:sus) = if all (== Just b) sus then Just b else Nothing

                block1suc = h $ map (Map.lookup 1) sucs
                block2suc = h $ map (Map.lookup 2) sucs
                block3suc = h $ map (Map.lookup 3) sucs

            case (block1suc,block2suc,block3suc) of
                (Just b1,Just b2,Just b3) -> lift $ do
                    putStrLn ""
                    putStrLn $ "Final accusation: " ++ show b1 ++ show b2 ++ show b3
                _ -> do
                    put (sos',2)

                    lift $ putStrLn ""
                    loop

        _ -> do
            l1 <- lift $ do putStrLn $ "What is player " ++ show n ++ "'s accusation?"
                            getLine

            let n' = if n == 6 then 1 else n+1

            case l1 of
                ('n':_) -> modify (\(so,_) -> (so,n'))
                (x:y:z:_) -> do
                    let (a,b,c) = (digitToInt x,digitToInt y,digitToInt z)

                    l2 <- lift $ do putStrLn "Which player responded?"
                                    getLine
                    let player = case l2 of
                            (k:_) -> digitToInt k
                            _ -> error "bad player"

                        non_respondents = if player > n then [n+1..player-1]
                                                        else [n+1..6]++[1..player-1]

                    addToGridStateT [((1,a,i),False) | i <- non_respondents]
                    addToGridStateT [((2,b,i),False) | i <- non_respondents]
                    addToGridStateT [((3,c,i),False) | i <- non_respondents]

                    let applyDisjunction :: StateObject -> [StateObject]
                        applyDisjunction (SO gr ro co su) = case Map.lookup (1,a,player) gr of
                            Nothing -> case Map.lookup (2,b,player) gr of
                                Nothing -> case Map.lookup (3,c,player) gr of
                                    Nothing -> catMaybes [addToGrid [((1,a,player),True)] (SO gr ro co su)
                                                         ,addToGrid [((2,b,player),True)] (SO gr ro co su)
                                                         ,addToGrid [((3,c,player),True)] (SO gr ro co su)
                                                         ]
                                    Just True -> [SO gr ro co su]
                                    Just False -> catMaybes [addToGrid [((1,a,player),True)] (SO gr ro co su)
                                                            ,addToGrid [((2,b,player),True)] (SO gr ro co su)
                                                            ]
                                Just True -> [SO gr ro co su]
                                Just False -> case Map.lookup (3,c,player) gr of
                                    Nothing -> catMaybes [addToGrid [((1,a,player),True)] (SO gr ro co su)
                                                         ,addToGrid [((3,c,player),True)] (SO gr ro co su)
                                                         ]
                                    Just True -> [SO gr ro co su]
                                    Just False -> catMaybes [addToGrid [((1,a,player),True)] (SO gr ro co su)]
                            Just True -> [SO gr ro co su]
                            Just False -> case Map.lookup (2,b,player) gr of
                                Nothing -> case Map.lookup (3,c,player) gr of
                                    Nothing -> catMaybes [addToGrid [((2,b,player),True)] (SO gr ro co su)
                                                         ,addToGrid [((3,c,player),True)] (SO gr ro co su)
                                                         ]
                                    Just True -> [SO gr ro co su]
                                    Just False -> catMaybes [addToGrid [((2,b,player),True)] (SO gr ro co su)]
                                Just True -> [SO gr ro co su]
                                Just False -> case Map.lookup (3,c,player) gr of
                                    Nothing -> catMaybes [addToGrid [((3,c,player),True)] (SO gr ro co su)]
                                    Just True -> [SO gr ro co su]
                                    Just False -> []

                    (sos',_) <- get
                    if player == n then put (sos',n')
                                   else put (concatMap applyDisjunction sos',n')
                _ -> error "bad accusation"

            lift $ putStrLn ""
            loop
