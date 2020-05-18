module SimpleCalculator where

import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Functor (map)
import Data.Int (round)
import Data.List (List(..), foldl, snoc)
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (class Show, show)
import Data.String.CodeUnits (toCharArray)
import Global (readFloat)
import Math (round) as Math

data Key = K_0 | K_1 | K_2 | K_3 | K_4 | K_5 | K_6 | K_7 | K_8 | K_9 | K_Dot | K_Add | K_Subtract | K_Divide | K_Multiple | K_Equal | K_C | K_AC | NOOP

-- data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 
data Operator = Addition | Subtraction | Multiplication | Division

operation :: Operator -> (Number -> Number -> Number)
operation Addition       = (+)
operation Subtraction    = (-)
operation Division       = (/)
operation Multiplication = (*)

type Status = {
    input           :: List Key,
    memory          :: Maybe Number,
    operator        :: Maybe Operator,
    cleanDisplay    :: Boolean,
    digitInput      :: Boolean,
    display         :: String
}

initialState :: Status
initialState = {
    input           : Nil,
    memory          : Nothing,
    operator        : Nothing,
    cleanDisplay    : true,
    digitInput      : false,
    display         : "0"
}

toString :: (List Key) -> String
toString xs = foldl (<>) "" (map show xs)  

toNumber :: List Key -> Number
toNumber xs = readFloat (if s == "" then "0" else s)
    where
        s = toString xs

computeMemory :: (Maybe Number) -> (Maybe Operator) -> Number -> Number
computeMemory (Just m) (Just o) n  = (operation o) m n
computeMemory Nothing (Just o) n = (operation o) 0.0 n
computeMemory (Just m) Nothing 0.0 = m
computeMemory _ Nothing n = n

computeStatusMemory :: Status -> Number
computeStatusMemory s = computeMemory s.memory s.operator (toNumber s.input)

computeStatusDisplay :: Status -> Key -> String
computeStatusDisplay s K_Dot = if s.digitInput then s.display else ((if s.cleanDisplay then "0" else s.display) <> show K_Dot)
computeStatusDisplay s K_Equal = if ((computeStatusMemory s) - Math.round (computeStatusMemory s) == 0.0 ) then show (round (computeStatusMemory s)) else show (computeStatusMemory s)
computeStatusDisplay s k = if s.cleanDisplay then show k else (if s.display == "0" then "" else s.display) <> show k

computeStatusInput :: Status -> Key -> List Key
computeStatusInput s K_Dot = if s.digitInput then s.input else (snoc s.input K_Dot)
computeStatusInput s k = snoc s.input k

handleKey :: Status -> Key -> Status
handleKey s K_AC =          initialState
handleKey s K_C =           initialState { memory = s.memory,                       operator = s.operator }
handleKey s K_Equal =       initialState { memory = Just (computeStatusMemory s),                                   display = (computeStatusDisplay s K_Equal) }
handleKey s K_Add =         initialState { memory = Just (computeStatusMemory s),   operator = Just Addition,       display = s.display }
handleKey s K_Subtract =    initialState { memory = Just (computeStatusMemory s),   operator = Just Subtraction,    display = s.display }
handleKey s K_Multiple =    initialState { memory = Just (computeStatusMemory s),   operator = Just Multiplication, display = s.display }
handleKey s K_Divide =      initialState { memory = Just (computeStatusMemory s),   operator = Just Division,       display = s.display }
handleKey s K_Dot =         s { input = (computeStatusInput s K_Dot),   display = (computeStatusDisplay s K_Dot),   cleanDisplay = false, digitInput = true }
handleKey s k =             s { input = (computeStatusInput s k),       display = (computeStatusDisplay s k),       cleanDisplay = false }

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
