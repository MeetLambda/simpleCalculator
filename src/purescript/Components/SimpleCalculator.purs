module Components.SimpleCalculator where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category (identity)
import Control.Semigroupoid ((<<<))
import Data.Eq (class Eq, (==))
import Data.EuclideanRing ((/))
import Data.Foldable (foldl)
import Data.Function (($), const, flip)
import Data.Functor (map, (<$>))
import Data.Int (round)
import Data.Lens.Fold (firstOf, lastOf)
import Data.Lens.Traversal (traversed)
import Data.List (snoc)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show, class Show)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties
import Global (readFloat)

type Surface    = HTML.HTML

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 
instance showDigit :: Show Digit where
    show D0 = "0"
    show D1 = "1"
    show D2 = "2"
    show D3 = "3"
    show D4 = "4"
    show D5 = "5"
    show D6 = "6"
    show D7 = "7"
    show D8 = "8"
    show D9 = "9"

data Action     = NoAction
                | ClickDigit Digit
                | ClickDot
                | ClickOperation Operator
                | ClickEqual
                | Cancel

data Query a    = GetState (State -> a)
type Input      = Unit
data Output     = NoOutput      -- aka Message

data Operator   = Addition | Subtraction | Multiplication | Division
instance showOperator :: Show Operator where
    show Addition       = "[+]"
    show Subtraction    = "[-]"
    show Multiplication = "[*]"
    show Division       = "[/]"

type Memory     = Maybe Number
type InputValue  = Tuple (List Digit) (List Digit)

type State      = {
    memory                   :: Memory,
    operator                 :: Maybe Operator,
    isInsertingDecimalValues :: Boolean,
    input                    :: InputValue
}
type Slots      = ()

initialState :: Input -> State
initialState _ = {
    memory:   Nothing,
    operator: Nothing,
    input:    Tuple Nil Nil,
    isInsertingDecimalValues: false
}
initialStateValue :: State
initialStateValue = initialState unit

component :: forall m. MonadAff m => Halogen.Component Surface Query Input Output m
component = Halogen.mkComponent {
    initialState,   -- :: Input -> State
    render,         -- :: State -> Surface (ComponentSlot Surface Slots m Action) Action
    eval: Halogen.mkEval $ Halogen.defaultEval {
        handleAction = handleAction :: forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit,
        handleQuery  = handleQuery  :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a),
        receive      = receive      :: Input -> Maybe Action,
        initialize   = initialize   :: Maybe Action,
        finalize     = finalize     :: Maybe Action
    }
                    -- :: HalogenQ Query Action Input ~> HalogenM State Action Slots Output m
}

showDigits :: (List Digit) -> String
showDigits Nil = ""
showDigits (x:xs) = show (valueOf x) <> showDigits xs

showInput :: InputValue -> Boolean -> String
showInput (Tuple Nil Nil) false = "0"
showInput (Tuple Nil Nil) true = "0."
showInput (Tuple xs Nil)  false = showDigits xs
showInput (Tuple xs Nil)  true  = showDigits xs <> "."
showInput (Tuple Nil ys)  _     = "0." <> showDigits ys
showInput (Tuple xs ys)   _     = showDigits xs <> "." <> showDigits ys

valueOf :: Digit -> Int     -- verify if 'coercible' is applicable
valueOf D0 = 0
valueOf D1 = 1
valueOf D2 = 2
valueOf D3 = 3
valueOf D4 = 4
valueOf D5 = 5
valueOf D6 = 6
valueOf D7 = 7
valueOf D8 = 8
valueOf D9 = 9

updateInput :: InputValue -> Boolean -> Digit -> InputValue
updateInput (Tuple xs ys) false d = Tuple (snoc xs d) ys
updateInput (Tuple xs ys) true  d = Tuple  xs        (snoc ys d)

operation :: Operator -> (Number -> Number -> Number)
operation Addition       = (+)
operation Subtraction    = (-)
operation Division       = (/)
operation Multiplication = (*)

computeValue :: InputValue -> Number
--computeValue v = readFloat (showInput v)
computeValue = readFloat <<< ((flip showInput) false)

computeMemory :: Number -> Maybe Operator -> Memory -> Memory
computeMemory n Nothing  _        = Just n
computeMemory n (Just o) Nothing  = Just ((operation o) 0.0 n)
computeMemory n (Just o) (Just m) = Just ((operation o) m   n)

digitWithValue :: Char -> Maybe Digit
digitWithValue '0' = Just D0
digitWithValue '1' = Just D1
digitWithValue '2' = Just D2
digitWithValue '3' = Just D3
digitWithValue '4' = Just D4
digitWithValue '5' = Just D5
digitWithValue '6' = Just D6
digitWithValue '7' = Just D7
digitWithValue '8' = Just D8
digitWithValue '9' = Just D9
digitWithValue  _  = Nothing

toList :: forall a. (Array a -> List a)
toList xs = foldl snoc Nil xs

digitsWithInteger :: Int -> (List Digit)
digitsWithInteger 0 = Nil
digitsWithInteger i = maybe Nil identity (sequence (map digitWithValue (toList (toCharArray (show i))))) 

inputWithMemory :: Memory -> InputValue
inputWithMemory Nothing  = initialStateValue.input
inputWithMemory (Just m) = Tuple (digitsWithInteger integerPart) (digitsWithInteger decimalPart)
    where
        splitValues = split (Pattern ".") (show m)
--      integerPart = round m
        integerPart = maybe 0 (round <<< readFloat) (firstOf traversed splitValues)
        decimalPart = maybe 0 (round <<< readFloat) (lastOf  traversed splitValues)

-- ===============================

handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    NoAction ->
        pure unit
    ClickDigit d -> do
        s <- Halogen.get
        let currentInput = maybe s.input  (const initialStateValue.input)  s.operator
        let referenceState = maybe initialStateValue (const s) s.operator
        Halogen.modify_ (\state -> referenceState {
            input  = updateInput currentInput state.isInsertingDecimalValues d
        })
    ClickDot -> do
        Halogen.modify_ (\state -> state { isInsertingDecimalValues = true })
    ClickOperation o -> do
        s <- Halogen.get
        let memory' = computeMemory (computeValue s.input) s.operator s.memory
        Halogen.modify_ (\state -> initialStateValue {
            memory = memory',
            operator = Just o,
            --isInsertingDecimalValues = false,
            input  = inputWithMemory  memory'
        })
    ClickEqual -> do
        s <- Halogen.get
        let memory' = computeMemory (computeValue s.input) s.operator s.memory
        Halogen.modify_ (\state -> initialStateValue {
            memory = memory',
            operator = Nothing,
            input  = inputWithMemory  memory'
        })
    Cancel -> do
        Halogen.modify_ (\state -> state {
            input = initialStateValue.input,
            isInsertingDecimalValues = false
        })


handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
handleQuery = const (pure Nothing)

receive :: Input -> Maybe Action
receive = const Nothing

initialize :: Maybe Action
initialize = Just NoAction

finalize :: Maybe Action
finalize = Nothing


render :: forall m. {-MonadAff m =>-} State -> Halogen.ComponentHTML Action Slots m
render (state) = HTML.div [] [
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "memory")]   [HTML.text ("Memory: " <> (show state.memory))],
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "operator")] [HTML.text ("Operator: " <> (show state.operator))],
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "state")]    [HTML.text ("State: " <> (show state))],
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "display")]  [HTML.text (showInput  state.input  state.isInsertingDecimalValues)],
    HTML.table [] [
        HTML.tbody [] [
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D7)] [HTML.text "7"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D8)] [HTML.text "8"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D9)] [HTML.text "9"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickOperation Division)] [HTML.text "/"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D4)] [HTML.text "4"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D5)] [HTML.text "5"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D6)] [HTML.text "6"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickOperation Multiplication)] [HTML.text "*"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D1)] [HTML.text "1"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D2)] [HTML.text "2"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D3)] [HTML.text "3"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickOperation Subtraction)] [HTML.text "-"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 2, HTML.Events.onClick \_ -> Just (ClickDigit D0)] [HTML.text "0"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "decimal"),  HTML.Events.onClick \_ -> Just ClickDot] [HTML.text "."],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickOperation Addition)] [HTML.text "+"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 3, HTML.Properties.class_ (Halogen.ClassName "result"), HTML.Events.onClick \_ -> Just ClickEqual] [HTML.text "="],
                HTML.td [HTML.Properties.colSpan 1, HTML.Properties.class_ (Halogen.ClassName "cancel"), HTML.Events.onClick \_ -> Just Cancel] [HTML.text "C"]
            ]
        ]
    ]
]
