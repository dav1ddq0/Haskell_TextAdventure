module GameParser where
import GameLexer
import Data.List

data Sentence = NullSentence|
                Phrase Token|
                SimpleSentence Token Token |
                SimplePrepositionSentence Token Token Token |
                ComplexSentence Token Token Token Token |
                ComplexPrepositionSentence Token Token Token Token Token deriving (Show, Eq)

-- Verbs
-- ----------------------------------------------------------
-- given a list of token keep only those that are verbs
getVerbs :: [Token] -> [Token]
getVerbs [] = []
getVerbs (verb@(Verb _ _) : ts) = verb : getVerbs ts
getVerbs (_ : ts) = getVerbs ts
-- ----------------------------------------------------------
-- Nouns
-- given a list of token keep only those that are verbs
getNouns :: [Token] -> [Token]
getNouns [] = []
getNouns (noun@(Noun _ _ ) : ts) = noun : getNouns ts
getNouns (_ : ts) = getNouns ts
-- ----------------------------------------------------------
--------------------------------------------------------------
-- Prepositions
getPrepositions :: [Token] -> [Token]
getPrepositions [] = []
getPrepositions (preposition@(Preposition _ _):ts) = preposition : ts
getPrepositions (_:ts) = getPrepositions ts
-- -----------------------------------------------------------

myApplicative :: Applicative f => f a -> f [a] -> f [a]
myApplicative f1 f2 = (:) <$> f1 <*> f2
-- Obtener una sentencia valida
-- --------------------------------------------------------------------------------------------------------------------------------------
getSentence :: [[Token]] -> [Sentence]
getSentence [verbs] = fmap (\verb -> Phrase verb)  verbs

getSentence [verbs, nouns] = fmap (\[verb,noun] -> SimpleSentence verb noun) (myApplicative verbs (myApplicative nouns [[]]))

getSentence [verbs, prepositions, nouns] = fmap(\[verb, preposition, noun]-> SimplePrepositionSentence verb preposition noun) 
            (myApplicative verbs (myApplicative prepositions(myApplicative nouns [[]])))

getSentence [verbs, nouns1, prepositions, nouns2] = fmap(\[verb, noun1, preposition, noun2] -> ComplexSentence verb noun1 preposition noun2)
            (myApplicative verbs (myApplicative nouns1 (myApplicative prepositions (myApplicative nouns2 [[]]))))

getSentence [verbs,prepositions1, nouns1, prepositions2, nouns2] 
            = fmap(\[verb, preposition1, noun1, preposition2, noun2]-> ComplexPrepositionSentence verb preposition1 noun1 preposition2 noun2)
            (myApplicative verbs (myApplicative prepositions1 (myApplicative nouns1 (myApplicative prepositions2 (myApplicative nouns2 [[]])))))

-- necesary case for exhaustic pattern matching for any other list of lists of tokens
getSentence _ = []

parseSentence :: [TokenMatch] -> [Sentence]
parseSentence [TokenMatch _ t1, TokenMatch _ t2, TokenMatch _ t3, TokenMatch _ t4, TokenMatch _ t5]
            = getSentence [getVerbs t1, getPrepositions t2, getNouns t3, getPrepositions t4, getNouns t5] 

parseSentence [TokenMatch _ t1, TokenMatch _ t2, TokenMatch _ t3, TokenMatch _ t4]
            = getSentence [getVerbs t1, getNouns t2, getPrepositions t3, getNouns t4]

parseSentence [TokenMatch _ t1, TokenMatch _ t2, TokenMatch _ t3]
            = getSentence [getVerbs t1, getPrepositions t2, getNouns t3]

parseSentence [TokenMatch _ t1, TokenMatch _ t2]
            = getSentence [getVerbs t1, getNouns t2]

parseSentence [TokenMatch  _ t1] = getSentence [getVerbs t1]

parseSentence _ = []


buildMeaningfulSentence :: [Token] -> Sentence
buildMeaningfulSentence [verb] = Phrase verb
buildMeaningfulSentence [verb, noun] = SimpleSentence verb noun
buildMeaningfulSentence [verb, preposition, noun] =SimplePrepositionSentence verb preposition noun
buildMeaningfulSentence [verb, noun1, preoposition, noun2] = ComplexSentence verb noun1 preoposition noun2
buildMeaningfulSentence [verb, preposition1, noun1, preoposition2, noun2] = ComplexPrepositionSentence verb preposition1 noun1 preoposition2 noun2
buildMeaningfulSentence _ = NullSentence


meaningfulSentence :: [Token] -> [Token] -> [Token] -> [String] -> Sentence
meaningfulSentence verbs nouns prepositions [] = NullSentence
meaningfulSentence verbs nouns prepositions [verb] =
    buildMeaningfulSentence (searchVerb verbs verb)
meaningfulSentence verbs nouns prepositions [verb, noun]
    = buildMeaningfulSentence (searchVerb  verbs verb ++ searchNoun nouns noun)
meaningfulSentence verbs nouns prepositions [verb, preposition, noun]
    = buildMeaningfulSentence (searchVerb verbs verb ++ searchPreposition prepositions preposition ++ searchNoun nouns noun)
meaningfulSentence verbs nouns prepositions [verb, noun1, preposition, noun2]
    = buildMeaningfulSentence (searchVerb verbs verb ++ searchNoun nouns noun1 ++ searchPreposition prepositions preposition ++ searchNoun nouns noun2)
meaningfulSentence verbs nouns prepositions [verb, preposition1, noun1, preposition2, noun2]
    = buildMeaningfulSentence (searchVerb verbs verb ++ searchPreposition prepositions preposition1 ++ searchNoun nouns noun1 ++ searchPreposition prepositions preposition2 ++ searchNoun nouns noun2)


meaningfulSentence _ _ _ _ = NullSentence


findVerb :: Foldable t => String -> t Token -> Maybe Token
findVerb verb  = Data.List.find (\(Verb x _) -> x== verb) 


searchVerb :: [Token] -> String -> [Token]
searchVerb verbs verb = let finded = findVerb verb verbs
                            in case finded of
                                Just verbFinded@(Verb _ _) -> [verbFinded]
                                Nothing -> []
                                _ -> []


findNoun :: Foldable t => String -> t Token -> Maybe Token
findNoun noun = Data.List.find(\(Noun x _) -> x == noun)

searchNoun :: [Token] -> String -> [Token]
searchNoun nouns noun = let finded = findNoun noun nouns
                            in case finded of
                                Just nounFinded@(Noun _ _) -> [nounFinded]
                                Nothing -> []
                                _ -> []

findPreposition :: Foldable t => String -> t Token -> Maybe Token
findPreposition preposition = Data.List.find(\(Preposition x _) -> x == preposition)

searchPreposition :: [Token] -> String -> [Token]
searchPreposition prepositions preposition = let finded = findPreposition preposition prepositions
                                                in case finded of
                                                    Just prepFinded@(Preposition _ _) -> [prepFinded]
                                                    Nothing -> []
                                                    _ -> []

