module Main (main) where

import Data.Char
import Data.Maybe
import Data.List(findIndex,findIndices,intercalate)
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
                      (Map.Map GridBlock (Either Int Int))

instance Show StateObject where
    show (SO gr _ _ _) =
        let g Nothing = '-'
            g (Just False) = 'N'
            g (Just True) = 'Y'
            
            block1 = [[g $ Map.lookup (1,i,j) gr | j <- [1..6]] | i <- [1..6]]
            block2 = [[g $ Map.lookup (2,i,j) gr | j <- [1..6]] | i <- [1..6]]
            block3 = [[g $ Map.lookup (3,i,j) gr | j <- [1..6]] | i <- [1..9]]

            line1 = intercalate "\n" block1
            line2 = intercalate "\n" block2
            line3 = intercalate "\n" block3

         in "\n" ++ line1 ++ "\n\n" ++ line2 ++ "\n\n" ++ line3 ++ "\n"

emptyStateObject :: StateObject
emptyStateObject = SO
    Map.empty
    (Map.fromList $ [((1,i),0) | i <- [1..6]]
        ++ [((2,i),0) | i <- [1..6]]
        ++ [((3,i),0) | i <- [1..9]])
    (Map.fromList [(i,(0,0)) | i <- [1..6]])
    Map.empty

getCard :: IO (Int,Int,Int)
getCard = do
    l1 <- getLine
    case l1 of
        ('q':_) -> error "quit"
        (x:y:_) -> return (digitToInt x,digitToInt y,1)
        _       -> getCard

getInt :: IO Int
getInt = do
    l1 <- getLine
    case l1 of
        ('q':_) -> error "quit"
        (x:_) -> return (digitToInt x)
        _     -> getInt

main :: IO ()
main = do
    putStrLn "please enter your first card:"
    gp1 <- getCard

    putStrLn "please enter your second card:"
    gp2 <- getCard

    putStrLn "please enter your third card:"
    gp3 <- getCard
    
    putStrLn "which player accuses first?"
    firstplayer <- getInt
    if firstplayer >= 1 && firstplayer <= 6 then return () else error "bad first player"

    let jso = addToGrid [(gp1,True),(gp2,True),(gp3,True)] emptyStateObject
    case jso of
        Just so -> evalStateT loop ([so],firstplayer)
        Nothing -> error "confusing"

entailed_by :: StateObject -> StateObject -> Bool
entailed_by (SO gr1 _ _ _) (SO gr2 _ _ _) = all go ([(1,i,j) | i <- [1..6],j <- [1..6]]
                                                 ++ [(2,i,j) | i <- [1..6],j <- [1..6]]
                                                 ++ [(3,i,j) | i <- [1..9],j <- [1..6]])
    where go :: GridPosition -> Bool
          go p = case Map.lookup p gr1 of
              Nothing -> True
              Just b -> Map.lookup p gr2 == Just b

insert' :: StateObject -> [StateObject] -> [StateObject]
insert' so1 [] = [so1]
insert' so1 (so2:sos) = if so2 `entailed_by` so1 then so2:sos else
                            if so1 `entailed_by` so2 then so1:filter (not.(so1 `entailed_by`)) sos else
                                so2:insert' so1 sos

reduce_list :: [StateObject] -> [StateObject]
reduce_list x = go x []
    where go :: [StateObject] -> [StateObject] -> [StateObject]
          go [] sos2 = sos2
          go (so1:sos1) sos2 = go sos1 (insert' so1 sos2)

addToGridStateT :: [(GridPosition,Bool)] -> StateT ([StateObject],Int) IO ()
addToGridStateT xs = do
    (sos,n) <- get
    put (reduce_list $ mapMaybe (addToGrid xs) sos,n)

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
                    Just 6 -> let
                        rows = findIndices (\i -> Map.lookup (m,i) rowcounts' == Just 5) [1..6]
                        f :: Int -> (GridPosition,Bool)
                        f t = case findIndex (\i -> Map.notMember (m,t+1,i) grid') [1..6] of
                            Just i -> ((m,t+1,i+1),True)
                            Nothing -> error "quirky"
                     in (Map.insert m (Right n) successes,map f rows)
                    Just 5 -> case Map.lookup m successes of
                        Just (Right _) -> case findIndex (\i -> Map.notMember (m,n,i) grid') [1..6] of
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

                g :: Maybe Int -> Bool
                g (Just c) = c < 10
                g Nothing = error "troubling"

                num_rows = if m == 3 then 9 else 6

                (successes',ys3) = case Map.lookup m successes of
                    Nothing -> (Map.insert m (Left 1) successes,[])
                    Just (Left k) -> if k == num_rows-2
                        then case findIndex (\r -> g $ Map.lookup (m,r) rowcounts') [1..num_rows] of
                                 Just r -> (successes,map (\i -> ((m,r+1,i+1),False))
                                     (findIndices (\i -> Map.notMember (m,r+1,i) grid') [1..6]))
                                 Nothing -> error "puzzling"
                        else (Map.insert m (Left (k+1)) successes,[])
                    Just (Right _) -> (successes,[])
             in Just (SO grid' rowcounts' columncounts' successes',ys1++ys2++ys3)
        Just False -> Nothing
        Just True -> Just (SO grid rowcounts columncounts successes,[])

printSummary :: [StateObject] -> IO ()
printSummary sos = do
    let grs :: [Map.Map GridPosition Bool]
        grs = map (\(SO gr _ _ _) -> gr) sos
                
        g :: [Maybe Bool] -> Char
        g [] = error "odd"
        g (Nothing:xs) = go xs
            where go :: [Maybe Bool] -> Char
                  go [] = '-'
                  go (Nothing:ys) = go ys
                  go (Just False:ys) = if any (== Just True) ys then '*' else '-'
                  go (Just True:ys) = if any (== Just False) ys then '*' else '-'
        g (Just True:xs)  = go xs
            where go :: [Maybe Bool] -> Char
                  go [] = 'Y'
                  go (Nothing:ys) = if any (== Just False) ys then '*' else '-'
                  go (Just False:_) = '*'
                  go (Just True:ys) = go ys
        g (Just False:xs) = go xs
            where go :: [Maybe Bool] -> Char
                  go [] = 'N'
                  go (Nothing:ys) = if any (== Just True) ys then '*' else '-'
                  go (Just True:_) = '*'
                  go (Just False:ys) = go ys

        block1 = [[g $ map (Map.lookup (1,i,j)) grs | j <- [1..6]] | i <- [1..6]]
        block2 = [[g $ map (Map.lookup (2,i,j)) grs | j <- [1..6]] | i <- [1..6]]
        block3 = [[g $ map (Map.lookup (3,i,j)) grs | j <- [1..6]] | i <- [1..9]]

    forM_ block1 putStrLn
    putStrLn ""
    forM_ block2 putStrLn
    putStrLn ""
    forM_ block3 putStrLn
    putStrLn ""

getAccusation :: IO (Maybe (Int,Int,Int))
getAccusation = do
    l1 <- getLine
    case l1 of
        ('q':_)   -> error "quit"
        ('n':_)   -> return Nothing
        (x:y:z:_) -> return (Just (digitToInt x,digitToInt y,digitToInt z))
        _         -> getAccusation

loop :: StateT ([StateObject],Int) IO ()
loop = do
    (sos,n) <- get

    case n of
        1 -> do
            lift $ printSummary sos

            macc <- lift $ do putStrLn "Your turn. What is your accusation?"
                              getAccusation
            case macc of
                Nothing -> return ()
                Just (a,b,c) -> do
                    player <- lift $ do putStrLn "Which player responded?"
                                        getInt

                    if player == 1 then do
                        addToGridStateT ([((1,a,i),False) | i <- [2..6]]
                                      ++ [((2,b,i),False) | i <- [2..6]]
                                      ++ [((3,c,i),False) | i <- [2..6]])
                    else do
                        block <- lift $ do putStrLn "Which block did they show you?"
                                           getInt

                        lift $ putStrLn ""

                        addToGridStateT ([((1,a,i),False) | i <- [2..player-1]]
                                      ++ [((2,b,i),False) | i <- [2..player-1]]
                                      ++ [((3,c,i),False) | i <- [2..player-1]])

                        case block of
                            1 -> addToGridStateT [((1,a,player),True)]
                            2 -> addToGridStateT [((2,b,player),True)]
                            3 -> addToGridStateT [((3,c,player),True)]
                            _ -> error "also bad block"

            (sos',_) <- get

            let sucs :: [Map.Map GridBlock (Either Int Int)]
                sucs = map (\(SO _ _ _ su) -> su) sos'

                h :: [Maybe (Either Int Int)] -> Maybe Int
                h [] = error "bizarre"
                h (Nothing:_) = Nothing
                h (Just (Left _):_) = Nothing
                h (Just (Right b):sus) = if all (== Just (Right b)) sus then Just b else Nothing

                block1suc = h $ map (Map.lookup 1) sucs
                block2suc = h $ map (Map.lookup 2) sucs
                block3suc = h $ map (Map.lookup 3) sucs

            case (block1suc,block2suc,block3suc) of
                (Just b1,Just b2,Just b3) -> lift $ do
                    putStrLn $ "Final accusation: " ++ show b1 ++ show b2 ++ show b3
                _ -> do
                    put (sos',2)

                    lift $ printSummary sos'
                    loop

        _ -> do
            macc <- lift $ do putStrLn $ "What is player " ++ show n ++ "'s accusation?"
                              getAccusation

            let n' = if n == 6 then 1 else n+1

            case macc of
                Nothing -> modify (\(so,_) -> (so,n'))
                Just (a,b,c) -> do
                    player <- lift $ do putStrLn "Which player responded?"
                                        getInt

                    let non_respondents = if player > n then [n+1..player-1]
                                                        else [n+1..6]++[1..player-1]

                    addToGridStateT ([((1,a,i),False) | i <- non_respondents]
                                  ++ [((2,b,i),False) | i <- non_respondents]
                                  ++ [((3,c,i),False) | i <- non_respondents])

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
                                   else put (reduce_list $ concatMap applyDisjunction sos',n')

            lift $ putStrLn ""
            loop
