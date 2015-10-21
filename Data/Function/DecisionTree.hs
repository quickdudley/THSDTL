{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Function.DecisionTree (learnDT) where
import Language.Haskell.TH
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.List
import Data.Function (on)

select [] = []
select (a:r) = (a,r) : map (\(b,l) -> (b,a:l)) (select r)

entropy l = let
  d = genericLength l :: Double
  e = foldl' (\r a -> M.insertWith (+) a 1 r) M.empty l
  in -(sum $ map (\t -> let c = t / d in c * logBase 2 c) $ M.elems e)

allSame [] = True
allSame [_] = True
allSame (a:r@(b:_))
  | a == b = allSame r
  | otherwise = False

learnDT :: forall a b t. (Ord b,Eq b,Ord t,Eq t) =>
 (t -> ExpQ) -> [(M.Map b Name -> ExpQ, a -> b)] -> [(a,t)] -> ExpQ
learnDT lq cr dp = do
  (rn,(_,lm)) <- runStateT (dtn (zip [0..] cr) dp) (M.empty, M.empty)
  lb <- forM (M.toList lm) $ \(n,e) -> 
    valD (varP n) (normalB e) []
  letE (map return lb) (varE rn)
-- _ $ dtn (zip [0..] cr) dp
 where
  dtn :: (Ord b,Eq b,Ord t,Eq t) =>
    [(Int,(M.Map b Name -> ExpQ, a -> b))] -> [(a,t)] ->
    StateT
      (
        M.Map (Either t (Int,M.Map b Name)) Name,
        M.Map Name ExpQ
      )
      Q
      Name
  dtn _ [] = fail "learnDT called with no training examples"
  dtn [] l = let
    t = fst $ maximumBy (compare `on` snd) $
      M.toList $
      M.fromListWith (+) $ map (\(_,t) -> (t,1)) l
    in do
      (ncache,ecache) <- get
      case M.lookup (Left t) ncache of
        Just n -> return n
        Nothing -> do
          n <- lift (newName "dtl")
          put (M.insert (Left t) n ncache, M.insert n (lq t) ecache)
          return n
  dtn c d = let
    l = genericLength d
    entr m = sum $
      map (\e -> genericLength e / l * entropy (map snd e)) $ M.elems m
    ((nx,c',cr'),_) = maximumBy (compare `on` snd) $
      map (\(c1@(_,(_,cdf)),cr1) -> let
        bm = M.map ($ []) $ M.fromListWith (.) $
          map (\d'@(a,_) -> (cdf a,(d' :))) d
        in ((bm,c1,cr1),entr bm)
       ) $
      select fc
    fc = filter (\(_,(_,lf)) -> not $ allSame $ map (lf . fst) d) c
    in case () of
      () | null fc -> dtn [] d
      () | allSame (map snd d) -> do
        let t = snd $ head d
        (ncache,ecache) <- get
        case M.lookup (Left t) ncache of
          Just n -> return n
          Nothing -> do
            n <- lift (newName "dtl")
            put (M.insert (Left t) n ncache, M.insert n (lq t) ecache)
            return n
      () | otherwise -> do
        m <- fmap M.fromList $ forM (M.toList nx) $ \(k,d') -> do
          bn <- dtn cr' d'
          return (k,bn)
        let m' = Right (fst c',m)
        (ncache,ecache) <- get
        case M.lookup m' ncache of
          Just n -> return n
          Nothing -> do
            n <- lift (newName "dtn")
            put (M.insert m' n ncache, M.insert n (fst (snd c') m) ecache)
            return n

