import System.Environment
import Data.List
import Data.Char
import Data.Maybe

data Regex = Letter Char | Choice Regex Regex | Lambda
           | Concat Regex Regex | Word String | Star Regex
           | NewLine | BeginLine | Escape Char | Group Regex
           | Plus Regex | Empty
           deriving Show
           
allChars = ['A'..'Z'] ++ ['a'..'z']

           
onlyLetters :: String -> Bool
onlyLetters str = all isLetter str
          
          
getIndex :: Char -> String -> Maybe Int
getIndex c str = elemIndex c str

lastIndexOf :: Char -> String -> Maybe Int
lastIndexOf c str = case findIndices (== c) str of
                      [] -> Nothing
                      indices -> Just (last indices)


splitAtIndex :: Int -> [a] -> Maybe ([a], [a])
splitAtIndex i xs =
  if i < 0 || i >= length xs
    then Nothing
    else let (prefix, suffix) = splitAt i xs in
         Just (take i xs, drop (i+1) xs)

           
string2Regex :: String -> Regex
string2Regex xs | '(' `elem` xs = let
                index = fromJust (lastIndexOf ')' xs)
                reg = fromJust (splitAtIndex index xs)
                l = drop 1 (fst reg)
                r = snd reg in
                if (head r == '|') then Choice (Group (string2Regex l)) (string2Regex (drop 1 r))
                else if (r /= "") then Concat (Group (string2Regex l)) (string2Regex r)
                else Group (string2Regex l)
string2Regex xs | '|' `elem` xs = let
                index = fromJust (lastIndexOf '|' xs)
                pair = fromJust (splitAtIndex index xs) in
                Choice (string2Regex (fst pair)) (string2Regex (snd pair)) 
string2Regex (x:[]) | elem x allChars = Letter x
string2Regex xs | onlyLetters xs = Word xs
string2Regex xs | '*' `elem` xs = let
                index = fromJust (lastIndexOf '*' xs)
                reg = fromJust (splitAtIndex index xs)
                l = fst reg
                r = snd reg in 
                if (r /= "") then Concat (Star (string2Regex l)) (string2Regex r)
                else Star (string2Regex l)
          
getLastFour :: String -> String
getLastFour str = drop (length str - 4) str

                
main :: IO ()
main = do
    putStrLn "Enter a pattern to match: "
    pattern <- getLine
    putStrLn "Enter a replacement string: "
    replacement <- getLine
    putStrLn $ "p = " ++ show (string2Regex pattern) ++ ", m = " ++ show (string2Regex replacement)
    putStrLn "Enter input file or string: "
    contents <- getLine
    if (getLastFour contents == ".txt") then do
        contents <- readFile contents
        writeFile "output.txt" contents
    else putStrLn contents