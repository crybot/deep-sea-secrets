-- Very Basic C declarations <--> English translator --
import Data.Char

type Token = String

expand :: String -> String
expand [] = []
expand (x:xs) 
    | isPunctuation x = ' ' : x : ' ' : expand xs
    | otherwise = x : expand xs

tokenize :: String -> [Token]
tokenize = words . expand

isQualifier :: Token -> Bool
isQualifier = (`elem` ["const", "volatile"])

isType :: Token -> Bool
isType = (`elem` ["void", "char", "short", "int",
    "long", "signed", "unsigned", "float", "double"])

isPointer :: Token -> Bool
isPointer = ( == "*")

isParentheses :: Token -> Bool
isParentheses = (`elem` ["[", "]", "(", ")"])

isIdentifier :: Token -> Bool
isIdentifier x = not $ isParentheses x || isPointer x 
    || isQualifier x || isType x

findIdentifier :: String -> ([Token], [Token])
findIdentifier = break isIdentifier . tokenize

parse :: String -> String
parse x = parseRight right left'
    where (left, right) = findIdentifier x
          left' = reverse left

parseRight :: [Token] -> [Token] -> String
parseRight [] left = parseLeft left []
parseRight right@(x:xs) left
    | isIdentifier x = x ++ " is " ++ parseRight xs left
    | x == "(" = parseParentheses right left
    | x == "[" = parseBrackets right left
    | otherwise = parseLeft left xs

parseLeft :: [Token] -> [Token] -> String
parseLeft [] _ = []
parseLeft left@(x:xs) right
    | isPointer x = "pointer to " ++ parseLeft xs right
    | isQualifier x = x ++ " " ++ parseLeft xs right
    | isType x = x ++ " " ++ parseLeft xs right
    | x == "(" = parseRight right xs 
    | otherwise = parseLeft xs right

parseParentheses :: [Token] -> [Token] -> String
parseParentheses [] _ = []
parseParentheses (x:xs) left
    | x == ")" = "Function returning " ++ parseRight xs left
    | otherwise = parseParentheses xs left

parseBrackets :: [Token] -> [Token] -> String
parseBrackets [] _ = []
parseBrackets (x:xs) left
    | x == "]" = "Array of " ++ parseRight xs left
    | otherwise = parseBrackets xs left


main :: IO ()
main =
    putStrLn $ parse "int *(*func[10])()"

