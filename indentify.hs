import qualified Data.Text as T
import qualified Data.List as L

tab = '{'
untab = '}'
curlyBrace c = any id $ map (c ==) [tab, untab]

shiftIndices :: [Int] -> [Int]
shiftIndices [] = []
shiftIndices (x:xs) = x : shiftIndices (map (minus x) xs) where
  minus = \x y -> (-) y x

splitMap :: [Int] -> String -> [String]
splitMap [] r = [r]
splitMap (x:xs) s = let left = fst $ splitAt x s
                        right = snd $ splitAt x s
                    in left : splitMap xs right

splitBy :: (Char -> Bool) -> String -> [String]
splitBy f s = let is = L.findIndices f s
                  is' = shiftIndices is
              in splitMap is' s

countLevel :: [String] -> [(Int, String)]
countLevel [] = []
countLevel (x:xs) = (0, x) : countLevel' 0 xs

countLevel' :: Int -> [String] -> [(Int, String)]
countLevel' _ []      = [(0, "")]
countLevel' n (x:xs)
  | (head x) == tab   = (n, x) : countLevel' (n+1) xs
  | (head x) == untab = (n-1, x) : countLevel' (n-1) xs
  | otherwise         = error "logic error"

tabify :: [(Int, String)] -> [String]
tabify []     = []
tabify (x:xs) = ((indent n) ++ (tabify' s)) : tabify xs
  where
    n = fst x
    s = snd x
    indent n = replicate n '\t'
    tabify' s = (T.unpack
                  . T.replace (p "{") (p $ "{" ++ "\n" ++ (indent (n+1)))
                  . T.replace (p "}") (p $ "}" ++ "\n" ++ (indent n))
                  . T.pack) s
    p = T.pack

indentify :: String -> String
indentify = unlines . tabify . countLevel . splitBy curlyBrace where

main :: IO ()
main = do
  s <- getLine
  print $ indentify s
