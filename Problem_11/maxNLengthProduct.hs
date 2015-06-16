import Data.Char 
import Data.List
import qualified Data.Packed.Vector as Vector
import qualified Data.Packed.Matrix as Matrix

largestProductList :: Int -> [Double] -> [Double]
largestProductList _ [] = [0]
largestProductList n xsa@(x:xs) = [maximum ((product $ take n xsa) : largestProductList n xs)]

largestProduct num xs = maximum $ map (largestProductList num) xs

takeDiags m
    | Matrix.cols m == 0 = []
    | otherwise = Vector.toList (Matrix.takeDiag m) : takeDiags (Matrix.dropColumns 1 m)
                                      
maxNLengthProduct num m = head . maximum $ (map (largestProduct num) rowColLists) ++ (map (largestProduct num) diagonalLists)
    where   maxProductMatrix = Matrix.fromLists parseMaxList
            rotateMaxProductMatrix = Matrix.fromLists . map reverse $ parseMaxList
            transpMaxProductMatrix = Matrix.trans maxProductMatrix
            rotateTranspMaxProductMatrix = Matrix.trans rotateMaxProductMatrix
            diagonalLists = map takeDiags [maxProductMatrix, 
                                      rotateMaxProductMatrix,
                                      transpMaxProductMatrix,
                                      rotateTranspMaxProductMatrix]
            rowColLists   = [parseMaxList, transpose parseMaxList]
            parseMaxList = foldr (\xs acc -> (map (read :: String -> Double) . words $ xs):acc) [] $ lines m    
