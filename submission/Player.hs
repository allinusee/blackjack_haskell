-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import           Parser.Parser      -- This is the source for the parser from the course notes
import           Cards              -- Finally, the generic card type(s)

import           TwentyOne.Types    -- Here you will find types used in the game of TwentyOne
import           TwentyOne.Rules    -- Rules of the game

-- You can add more imports if you need them
import           Parser.Instances
import           Data.Maybe
import           Data.Char

-- | This function is called once it's your turn, and keeps getting called until your turn ends.
playCard :: PlayFunc
playCard dcard ppoints pinfo pid memo hand = (action hand dcard memo pid ppoints, newMemory hand dcard pinfo memo pid ppoints)

 
----------------- Action -----------------
-- | Determine Action of the player (Bid/Hit/Stand/DoubleDown/Split/Insurance)
action :: Hand -> Maybe Card -> Maybe String -> PlayerId -> [PlayerPoints] -> Action
action hand dcard memo pid ppoints
  | length hand == 0                                                     = Bid minBid
  | (take 1 <$> memo) /= Just ","                                        = memoAction memo
  | length hand == 2                                                     = twoCards hand dcard memo pid ppoints
  | handCalc hand <= 16                                                  = Hit
  | handCalc hand < 18 && (pSmallCards memo) == "High"                   = Hit
  | otherwise                                                            = Stand

-- | Handles possible action outcomes when player has 2 cards
twoCards :: Hand -> Maybe Card -> Maybe String -> PlayerId -> [PlayerPoints] ->  Action
twoCards hand dcard memo pid ppoints
  | let Just x = dcard, (getRank x) == Ace && (pBigcards memo) == "High" && (getActMem memo 'b') /= "i" && getPPoints pid ppoints >= (minBid + (minBid `div` 2))     = Insurance (minBid `div` 2)
  | (handCalc hand == 11) || (handCalc hand == 10 && getDCard dcard /= "Strong")                                                                                     = doubleAction pid ppoints
  | isEqual (getRank <$> hand)                                                                                                                                       = splitAction hand dcard pid ppoints
  | handCalc hand <= 16                                                                                                                                              = Hit
  | handCalc hand < 18 && (pSmallCards memo) == "High"                                                                                                               = Hit
  | otherwise                                                                                                                                                        = Stand

-- | Analyse dealer's upcard
getDCard :: Maybe Card -> String
getDCard Nothing = ""
getDCard (Just card)
  | getRank card == Two || getRank card == Three                           = "Fair"
  | getRank card == Four || getRank card == Five || getRank card == Six    = "Poor"
  | getRank card == Seven || getRank card == Eight || getRank card == Nine = "Good"
  | otherwise                                                              = "Strong"

-- | Determine actions based of memory (Hit/Stand from DoubleDown)
memoAction :: Maybe String -> Action
memoAction m
  | let Just x = m, take 1 x == "H" = Hit
  | otherwise                       = Stand

-- | Checks if player has enough points to DoubleDown
doubleAction :: PlayerId -> [PlayerPoints] -> Action
doubleAction pid ppoints
  | getPPoints pid ppoints >= (minBid*2) = DoubleDown minBid
  | otherwise                            = Hit

-- | Checks if Split is the best action and if player has enough points to Split
splitAction :: Hand -> Maybe Card -> PlayerId -> [PlayerPoints] -> Action
splitAction hand dcard pid ppoints
  | handCalc hand >= 18                                                                                                           = Stand
  | handCalc hand == 8                                                                                                            = Hit 
  | (handCalc hand == 4 || handCalc hand == 6 || handCalc hand == 14) && (getDCard dcard == "Strong" || getDCard dcard == "Good") = Hit
  | (getRank <$> hand) == [Six, Six] && getDCard dcard /= "Poor"                                                                  = Hit 
  | getPPoints pid ppoints >= (minBid*2)                                                                                          = Split minBid
  | otherwise                                                                                                                     = Hit 

-- | Get points of an individual player from PlayerPoints list
getPPoints :: PlayerId -> [PlayerPoints] -> Points
getPPoints _ [] = 0
getPPoints pid (x:xs)
  | getId x == pid = getPoints x
  | otherwise      = getPPoints pid xs

-- | Calculate probability of a small card(Ace, Two, Three, Four)
pSmallCards :: Maybe String -> String
pSmallCards Nothing  = "Low"
pSmallCards (Just m) 
  | ((division (getInt(getNumber m 'E' 'D') + getInt(getNumber m 'F' 'E') + getInt(getNumber m 'G' 'F') + getInt(getNumber m 'I' 'G') + getInt(getNumber m 'N' 'I') + getInt(getNumber m 'T' 'N') + getInt(getNumber m 'J' 'T') + getInt(getNumber m 'Q' 'J') + getInt(getNumber m 'K' 'Q')) 108) - (division (getInt(getNumber m 'A' ',') + getInt(getNumber m 'B' 'A') + getInt(getNumber m 'C' 'B') + getInt(getNumber m 'D' 'C')) 48)) > 0.5 = "High"
  | otherwise = "Low"

-- | Calculate probability of a Ten-Card(Ten, Jack, Queen, King)
pBigcards :: Maybe String -> String
pBigcards Nothing = "Low"
pBigcards (Just m)
  | ((division (getInt(getNumber m 'A' ',') + getInt(getNumber m 'B' 'A') + getInt(getNumber m 'C' 'B') + getInt(getNumber m 'D' 'C') + getInt(getNumber m 'E' 'D') + getInt(getNumber m 'F' 'E') + getInt(getNumber m 'G' 'F') + getInt(getNumber m 'I' 'G') + getInt(getNumber m 'N' 'I')) 108) - (division (getInt(getNumber m 'T' 'N') + getInt(getNumber m 'J' 'T') + getInt(getNumber m 'Q' 'J') + getInt(getNumber m 'K' 'Q')) 48)) > 0.8 = "High"
  | otherwise = "Low"

division :: Int -> Int -> Float 
division a b = (fromIntegral a) / (fromIntegral b)

-- | Check if Ranks in the list are equal 
isEqual :: [Rank] -> Bool
isEqual xs = and $ map (== head xs) (tail xs)

-- | Reduce list of lists to a list
flatten :: [[a]] -> [a]
flatten = foldr (++) []


----------------- Memory ------------------
-- | Update memory of a player
newMemory :: Hand -> Maybe Card -> [PlayerInfo] -> Maybe String -> PlayerId -> [PlayerPoints] -> String
newMemory hand dcard pinfo memo pid ppoints 
  | isNothing memo                                         = show initialPlayed
  | isDoubleDown (action hand dcard memo pid ppoints)      = "HS" ++ (justMemo memo pinfo)
  | isInsurance (action hand dcard memo pid ppoints)       = (justMemo memo pinfo) ++ "i"
  | length hand == 0 && (getActMem memo 'b') == "i"        = justMemo (Just (getActMem memo 'z')) pinfo
  | sumPlayedCards memo == 156                             = justMemo (Just ((getActMem memo 'f') ++ (show initialPlayed))) pinfo
  | otherwise                                              = justMemo memo pinfo

-- | Extract different parts of the memory
getActMem :: Maybe String -> Char -> String
getActMem Nothing _ = ""
getActMem (Just x) c
 | c == 'f'  = getBefore (parse (parseUntil ',') x)
 | c == 'b'  = drop ((length x) - 1) x
 | otherwise = take ((length x) - 1) x

-- | Handles memory when memory is not Nothing
justMemo :: Maybe String -> [PlayerInfo] -> String
justMemo Nothing _             = ""
justMemo (Just memo) pinfo = actMemo memo pinfo

-- | Handles action part of memory
actMemo :: String -> [PlayerInfo] -> String
actMemo memo pinfo
  | let (_:sx) = memo, (getBefore (parse (parseUntil ',') memo)) /= "" = updatePlayedCards sx pinfo
  | otherwise                                                          = updatePlayedCards memo pinfo

-- | Determines memory of the round
updatePlayedCards :: String -> [PlayerInfo] -> String
updatePlayedCards memo pinfo = ((getBefore (parse (parseUntil ',') memo)) ++ new) where
  rankList = flatten ((\x -> getRank <$> playerInfoHand x) <$> pinfo) 
  cardPart = drop (length (getBefore (parse (parseUntil ',') memo) )) memo
  new = foldl updatePlayedCards' cardPart rankList 

-- | update card-count part of memory
updatePlayedCards' :: String -> Rank -> String
updatePlayedCards' memo Ace   = show Played {ace'=getInt(getNumber memo 'A' ',')+1, two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Two   = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A')+1, three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Three = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B')+1, four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Four  = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C')+1, five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Five  = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D')+1, six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Six   = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E')+1, seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Seven = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F')+1, eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Eight = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G')+1, nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Nine  = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I')+1, ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Ten   = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N')+1, jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Jack  = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T')+1, queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo Queen = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J')+1, king'=getInt(getNumber memo 'K' 'Q')}
updatePlayedCards' memo King  = show Played {ace'=getInt(getNumber memo 'A' ','), two'=getInt(getNumber memo 'B' 'A'), three'=getInt(getNumber memo 'C' 'B'), four'=getInt(getNumber memo 'D' 'C'), five'=getInt(getNumber memo 'E' 'D'), six'=getInt(getNumber memo 'F' 'E'), seven'=getInt(getNumber memo 'G' 'F'), eight'=getInt(getNumber memo 'I' 'G'), nine'=getInt(getNumber memo 'N' 'I'), ten'=getInt(getNumber memo 'T' 'N'), jack'=getInt(getNumber memo 'J' 'T'), queen'=getInt(getNumber memo 'Q' 'J'), king'=getInt(getNumber memo 'K' 'Q')+1}

-- | data type for cards played
data Played = Played {ace', two', three', four', five', six', seven', eight', nine', ten', jack', queen', king' :: Int}

-- | show Played as ",?A?B?C?D?E?F?G?I?N?T?J?Q?K"
instance Show Played where
  show Played {ace', two', three', four', five', six', seven', eight', nine', ten', jack', queen', king'} = 
    "," ++ show ace' ++ "A" ++ show two' ++ "B" ++ show three' ++ "C" ++ show four' ++ "D" ++ show five' ++ "E" ++ show six' ++ "F" ++ show seven' ++ "G" ++ show eight' ++ "I" ++ show nine' ++ "N" ++ show ten' ++ "T" ++ show jack' ++ "J" ++ show queen' ++ "Q" ++ show king' ++ "K"

-- | Cards played before before start ",0A0B0C0D0E0F0G0I0N0T0J0Q0K"
initialPlayed :: Played
initialPlayed =  Played {ace'=0, two'=0, three'=0, four'=0, five'=0, six'=0, seven'=0, eight'=0, nine'=0, ten'=0, jack'=0, queen'=0, king'=0}
      
-- | Calculate sum of cards played
sumPlayedCards :: Maybe String -> Int
sumPlayedCards Nothing  = 0
sumPlayedCards (Just m) = getInt(getNumber m 'A' ',') + getInt(getNumber m 'B' 'A') + getInt(getNumber m 'C' 'B') + getInt(getNumber m 'D' 'C') + getInt(getNumber m 'E' 'D') + getInt(getNumber m 'F' 'E') + getInt(getNumber m 'G' 'F') + getInt(getNumber m 'I' 'G') + getInt(getNumber m 'N' 'I') + getInt(getNumber m 'T' 'N') + getInt(getNumber m 'J' 'T') + getInt(getNumber m 'Q' 'J') + getInt(getNumber m 'K' 'Q')

-- | Convert String to Int
getInt :: [Char] -> Int
getInt x = if (length x == 1) then (digitToInt (x !! 0)) else (10 + digitToInt (x !! 1))

-- | Get total number of a specific Rank played
getNumber :: String -> Char -> Char -> String
getNumber m x y = getBefore (parse (parseUntil x) (getAfter (parse (parseUntil y) m )))


-- ||----- Parsing of Memory ------
-- | Get part of memory after a matched Char
getAfter :: ParseResult String -> String
getAfter (Result x _) = x 
getAfter (Error _)    = ""

-- | Get part of memory before a matched Char
getBefore :: ParseResult String -> String
getBefore (Result _ x) = x 
getBefore (Error _)    = ""

parseUntil :: Char -> Parser [Char]
parseUntil c = list (isNot c) <* is c

list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = do
  first <- p
  rest <- list p
  pure (first:rest)

isNot :: Char -> Parser Char
isNot c = satisfy (/=c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    c <- character
    let next = if f c then pure else unexpectedCharParser
    next c