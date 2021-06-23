module GameLexer where
import qualified Data.Char


data Token = Verb String [String ]| Noun String [String] | Preposition String [String] 
    deriving(Show, Eq) 

data TokenMatch = TokenMatch String [Token] deriving (Show, Eq)

join :: Maybe TokenMatch -> Maybe TokenMatch -> Maybe TokenMatch
join Nothing Nothing = Nothing
join (Just a) Nothing = Just a
join Nothing (Just b) = Just b
join (Just (TokenMatch word1 tokens1)) (Just (TokenMatch word2 tokens2))
    | word1 == word2 = Just (TokenMatch word1 (tokens1 ++ tokens2))
    | otherwise = Nothing

lexTokens :: [Token] -> [String] -> [(Maybe TokenMatch, [String])] -> [TokenMatch]
lexTokens potentialTokens words [] = lexInput potentialTokens words
lexTokens potentialTokens words ((Nothing, _) : tokens) = lexTokens potentialTokens words tokens
lexTokens potentialTokens words ((Just token, tokenWords) : tokens) = token : lexInput potentialTokens tokenWords

tokenize :: String -> Token -> Maybe TokenMatch
tokenize "" _  = Nothing --Empty string can't match tokens

tokenize word token@(Verb _ synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = Data.Char.toLower (head word) : tail word

tokenize word token@(Noun name _)
    | word == name = Just (TokenMatch word [token])
    | lowerCaseWord == name = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = Data.Char.toLower (head word) : tail word

tokenize word token@(Preposition _ synonyms)
    | lowerCaseWord `elem` synonyms = Just (TokenMatch word [token])
    | otherwise = Nothing
        where lowerCaseWord = Data.Char.toLower (head word) : tail word

lexInput :: [Token] -> [String] -> [TokenMatch]
lexInput potentialTokens [] = []

lexInput potentialTokens (word1 : word2 : words) =
        lexTokens potentialTokens (word2 : words) [(foldl (\acc token -> tokenize (word1 ++ ' ' : word2) token `join` acc) Nothing potentialTokens, words), --Prioritize look-ahead by putting the look-ahead option first
        (foldl (\acc token -> tokenize word1 token `join` acc) Nothing potentialTokens, word2 : words)]

lexInput potentialTokens (word : words) =
    lexTokens potentialTokens words [(foldl (\acc token -> tokenize word token `join` acc) Nothing potentialTokens, words)]

