# THSDTL
Template Haskell library for decision tree learning

Currently there is just one exported function: `learnDT :: (Ord t, Ord b) => (t -> ExpQ) -> [(M.Map b Name -> ExpQ, a -> b)] -> [(a, t)] -> ExpQ`

This function uses the first 2 arguments to construct decision tree nodes, and the third argument to construct the decision tree. The result is an expression that can be spliced into a function definition.

The current version has no capacity for pruning the tree, it just creates a leaf node when all the remaining data points have the same label. But identical subtrees are detected and re-used.

This is a reasonably simple usage example:

    {-# LANGUAGE TemplateHaskell #-}
    import Language.Haskell.TH
    import System.IO
    import System.Process
    import Text.ParserCombinators.Parsec
    import qualified Data.Map as M
    import Data.List

    import Data.Function.DecisionTree

    $( do
      rawData <- runIO $ readProcess
        "/usr/bin/wget"
        ["-O", "-",
         "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data"]
        ""
      let
        cellParser = many $ satisfy (not . (`elem` ",\n"))
        lineParser :: Parser ((Double,Double,Double,Double),String)
        lineParser = do
          sepalLength <- fmap read cellParser
          char ','
          sepalWidth <- fmap read cellParser
          char ','
          petalLength <- fmap read cellParser
          char ','
          petalWidth <- fmap read cellParser
          char ','
          species <- cellParser
          try (char '\n') <|> return '\n'
          return ((sepalLength,sepalWidth,petalLength,petalWidth),species)
        datParser = many lineParser
      case parse datParser "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data" rawData of
        Left m -> fail $ show m
        Right rd -> do
          let
            [sepalLength,sepalWidth,petalLength,petalWidth] = map mkName ["sepalLength","sepalWidth","petalLength","petalWidth"]
            [slv,swv,plv,pwv] = map (\ef ->
              tail $ map head $ group $ sort $ map ef rd
             ) [
               \((l,_,_,_),_) -> l,
               \((_,w,_,_),_) -> w,
               \((_,_,l,_),_) -> l,
               \((_,_,_,w),_) -> w
             ]
            slc :: Double -> (M.Map Bool Name -> ExpQ, (Double,Double,Double,Double) -> Bool)
            slc n = (\b -> case (M.lookup True b, M.lookup False b) of
              (Just lt, Just geq) -> [| if $(varE sepalLength) < $(litE . rationalL . toRational $ n) then $(varE lt) else $(varE geq) |],
              \(a,_,_,_) -> a < n)
            swc n = (\b -> case (M.lookup True b, M.lookup False b) of
              (Just lt, Just geq) -> [| if $(varE sepalWidth) < $(litE . rationalL . toRational $ n) then $(varE lt) else $(varE geq) |],
              \(_,a,_,_) -> a < n)
            plc n = (\b -> case (M.lookup True b, M.lookup False b) of
              (Just lt, Just geq) -> [| if $(varE petalLength) < $(litE . rationalL . toRational $ n) then $(varE lt) else $(varE geq) |],
              \(_,_,a,_) -> a < n)
            pwc n = (\b -> case (M.lookup True b, M.lookup False b) of
              (Just lt, Just geq) -> [| if $(varE petalWidth) < $(litE . rationalL . toRational $ n) then $(varE lt) else $(varE geq) |],
              \(_,_,_,a) -> a < n)
          [d|
              classifyIris :: (Double,Double,Double,Double) -> String
              classifyIris ($(varP sepalLength),$(varP sepalWidth),$(varP petalLength),$(varP petalWidth)) =
                $(learnDT (litE . stringL) (concat $ zipWith map [slc,swc,plc,pwc] [slv,swv,plv,pwv]) rd)
            |]
     )
