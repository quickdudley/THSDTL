{-# LANGUAGE TemplateHaskell #-}
module Data.Function.DecisionTree (learnDT) where
import Language.Haskell.TH
import qualified Data.Map as M

learnDT :: (Ord b,Ord t) =>
 (a -> ExpQ) -> [(M.Map b Name -> ExpQ, a -> b)] -> [a] -> ExpQ
learnDT = undefined -- TODO

