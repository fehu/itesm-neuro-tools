-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.Tikz.ConfusionDiagram
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module ImgCharacteristics.Tikz.ConfusionDiagram (

  classesConfusion

, ClassesConfusion(..)
, ClassConfusion(..)
, classConfusionTikz

) where

import ImgCharacteristics

import Data.Map (Map)
import Data.List (intercalate, zip4)

import qualified Data.Map as Map

import GHC.Float

-----------------------------------------------------------------------------

type ClassName = String

classesConfusion :: (Show c, Integral n, Show n) =>
                    [(c, String)] -- ^ Classes and corresponding colors.
                 -> [[n]]         -- ^ Confusion (count)
                 -> String

classesConfusion cs ns = classConfusionTikz (ClassesConfusion $ Map.fromList ccs)
    where totals  = map sum ns
          correct = do (ns', i) <- zip ns  [1..]
                       (n  , j) <- zip ns' [1..]
                       [n | i == j]
          percs = zipWith (\t -> if t == 0 then map (const 0)
                                           else map ((/ fromIntegral t) . fromIntegral)
                            )
                          totals ns
          ccs = do ((clazz,color), corr, tot, perc) <- zip4 cs correct totals percs
                   let conf = Map.fromList $ zip (map (show . fst) cs) perc
                       cc   = ClassConfusion (fromIntegral tot)
                                             (fromIntegral corr)
                                             conf
                                             color
                   return (show clazz, cc)

newtype ClassesConfusion = ClassesConfusion (Map ClassName ClassConfusion)

data ClassConfusion = ClassConfusion {
    cTotal      :: Int
  , cCorrect    :: Int
  , cConfusion  :: Map ClassName Float
  , cColor      :: String
}


classConfusionTikz (ClassesConfusion ccs) =
    let n         = show $ Map.size ccs
        ccsAssocs = zip (Map.assocs ccs) [1..]
    in intercalate "\n" $ [
        "\\begin{tikzpicture}[>=triangle 60]"
      , "\\tikzset{ class/.style={align=center, minimum height=4ex,text width=6em,"
      , "                        draw, circle} };"
      , "\\def\\psize{15cm}"
      , ""
      , "\\node[draw=none,regular polygon,minimum size=\\psize,regular polygon sides="
        ++ n ++ "] (poly) {};"
      , ""
    ] ++ concatMap ccTikzNode ccsAssocs
      ++ [""] ++ intercalate [""] (map ccTikzArrows ccsAssocs)
      ++ [
        ""
      , "\\end{tikzpicture}"
    ]

ccTikzNode ((name, cc), i) = [
    "\\node[class] (N" ++ show i ++ ") at (poly.corner " ++ show i ++ ") { " ++ text ++ " };"
 ]
 where text = "\\textbf{" ++ name ++ "} \\\\ " ++ corr ++ " of " ++ totl
           ++ "\\\\$\\approx \\mathbf{" ++ perc ++ "\\%}$"
       corr = show $ cCorrect cc
       totl = show $ cTotal cc
       perc = show . float2Int $ 100 * snd (Map.elemAt (i-1) (cConfusion cc))


ccTikzArrows ((name, cc), i) = begin : nodes ++ [end]
 where begin = "\\begin{scope}[draw=" ++ color ++ ", text=" ++ color ++ "]"
       end   = "\\end{scope}"
       color = cColor cc
       nodes = do ((dest, perc), j) <- zip (Map.assocs (cConfusion cc)) [1..]
                  let bendDir = if odd (i+j) then "left" else "right"
                  let perc'   = show . float2Int $ perc*100
                  [   "\\draw (N" ++ show i ++ ") edge [->, bend " ++ bendDir ++  "] " ++
                      "node[label=left:{$" ++ perc' ++ "\\%$}]{} (N" ++ show j ++ ");"
                   | i /= j
                   ]

-----------------------------------------------------------------------------

