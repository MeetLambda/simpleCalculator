module SimpleCalculator where

import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.List (List(..), foldl, snoc)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show, show)
import Data.String.CodeUnits (toCharArray)
import Global (readFloat)

data Key = K_0 | K_1 | K_2 | K_3 | K_4 | K_5 | K_6 | K_7 | K_8 | K_9 | K_Dot | K_Add | K_Subtract | K_Divide | K_Multiple | K_Equal | K_C | K_AC | NOOP

-- data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 
data Operator = Addition | Subtraction | Multiplication | Division

operation :: Operator -> (Number -> Number -> Number)
operation Addition       = (+)
operation Subtraction    = (-)
operation Division       = (/)
operation Multiplication = (*)

type Status = {
    input    :: List Key,
    memory   :: Maybe Number,
    operator :: Maybe Operator,
    display  :: String
}

initialState :: Status
initialState = {
    input    : Nil,
    memory   : Nothing,
    operator : Nothing,
    display  : "0"
}

toString :: (List Key) -> String
toString xs = foldl (<>) "" (map show xs)  

toNumber :: List Key -> Number
toNumber xs = readFloat (toString xs)

computeMemory :: (Maybe Number) -> (Maybe Operator) -> Number -> Number
computeMemory (Just m) (Just o) n  = (operation o) m n
computeMemory Nothing Nothing n = n
computeMemory _ _ _ = 0.0

handleKey :: Status -> Key -> Status
handleKey s K_Equal = s { memory = Just (computeMemory s.memory s.operator (toNumber s.input)), input = Nil, display = show (computeMemory s.memory s.operator (toNumber s.input))}
handleKey s K_Add = s { memory = Just (computeMemory s.memory s.operator (toNumber s.input)), input = Nil, operator = Just Addition }
handleKey s K_Dot = s { input = snoc s.input K_Dot, display = s.display <> show K_Dot }
handleKey s k = s { input = snoc s.input k, display = (if s.display == "0" then "" else s.display) <> show k }

charToKey :: Char -> Key
charToKey '0' = K_0
charToKey '1' = K_1
charToKey '2' = K_2
charToKey '3' = K_3
charToKey '4' = K_4
charToKey '5' = K_5
charToKey '6' = K_6
charToKey '7' = K_7
charToKey '8' = K_8
charToKey '9' = K_9
charToKey '.' = K_Dot
charToKey '+' = K_Add
charToKey '-' = K_Subtract
charToKey '/' = K_Divide
charToKey '*' = K_Multiple
charToKey '=' = K_Equal
charToKey 'C' = K_C
charToKey '#' = K_AC
charToKey  _  = NOOP

toList :: forall a. (Array a -> List a)
toList xs = foldl snoc Nil xs

keys :: String -> List Key
keys s = map charToKey (toList (toCharArray s))

-- =========================================================

instance showKey :: Show Key where
    show K_0 =          "0"
    show K_1 =          "1"
    show K_2 =          "2"
    show K_3 =          "3"
    show K_4 =          "4"
    show K_5 =          "5"
    show K_6 =          "6"
    show K_7 =          "7"
    show K_8 =          "8"
    show K_9 =          "9"
    show K_Dot =        "."
    show K_Add =        "+"
    show K_Subtract =   "-"
    show K_Divide =     "/"
    show K_Multiple =   "*"
    show K_Equal =      "="
    show K_C =          "C"
    show K_AC =         "#"
    show NOOP =         ""


-- instance showDigit :: Show Digit where
--     show D0 = "0"
--     show D1 = "1"
--     show D2 = "2"
--     show D3 = "3"
--     show D4 = "4"
--     show D5 = "5"
--     show D6 = "6"
--     show D7 = "7"
--     show D8 = "8"
--     show D9 = "9"

-- instance showOperator :: Show Operator where
--     show Addition       = "[+]"
--     show Subtraction    = "[-]"
--     show Multiplication = "[*]"
--     show Division       = "[/]"
