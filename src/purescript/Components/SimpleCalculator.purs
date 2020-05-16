module Components.SimpleCalculator where

import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Semigroupoid ((<<<))
import Data.Eq (class Eq)
import Data.EuclideanRing ((/))
import Data.Function (($), const)
import Data.Functor (map, (<$>))
import Data.List (snoc)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

type Surface    = HTML.HTML

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 
data Action     = NoAction
                | ClickDigit Digit
                | ClickDot
                | Cancel
                -- | Add -- | SubComponentOutput SubComponent.S_Output
data Query a    = GetState (State -> a)
type Input      = Unit
data Output     = NoOutput      -- aka Message

data Operator   = Addition | Subtraction | Multiplication | Division
type InputValue  = Tuple (List Digit) (List Digit)
type InputValue' = { integer :: List Digit, decimal :: List Digit }

type State      = {
    memory                   :: Maybe Number,
    operator                 :: Maybe Operator,
    isInsertingDecimalValues :: Boolean,
    input                    :: InputValue,
    input'                   :: InputValue'
}
type Slots      = ()

initialState :: Input -> State
initialState _ = {
    memory:   Nothing,
    operator: Nothing,
    input:    Tuple Nil Nil,
    input':   { integer: Nil, decimal: Nil },
    isInsertingDecimalValues: false
}
initialState' :: State
initialState' = initialState unit

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

showInput' :: InputValue' -> Boolean -> String
showInput' {integer: Nil, decimal: Nil} false = "0"
showInput' {integer: Nil, decimal: Nil} true = "0."
showInput' {integer: xs,  decimal: Nil}  false = showDigits xs
showInput' {integer: xs,  decimal: Nil}  true  = showDigits xs <> "."
showInput' {integer: Nil, decimal: ys}   _     = "0." <> showDigits ys
showInput' {integer: xs,  decimal: ys}   _     = showDigits xs <> "." <> showDigits ys

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

updateInput' :: InputValue' -> Boolean -> Digit -> InputValue'
updateInput' {integer: xs, decimal: ys} false d = {integer: snoc xs d, decimal: ys}
updateInput' {integer: xs, decimal: ys} true  d = {integer: xs,        decimal: snoc ys d}

render :: forall m. {-MonadAff m =>-} State -> Halogen.ComponentHTML Action Slots m
render (state) = HTML.div [] [
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "display")] [HTML.text (showInput  state.input  state.isInsertingDecimalValues)],
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "display")] [HTML.text (showInput' state.input' state.isInsertingDecimalValues)],
    HTML.table [] [
        HTML.tbody [] [
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D7)] [HTML.text "7"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D8)] [HTML.text "8"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D9)] [HTML.text "9"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator")] [HTML.text "/"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D4)] [HTML.text "4"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D5)] [HTML.text "5"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D6)] [HTML.text "6"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator")] [HTML.text "*"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D1)] [HTML.text "1"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D2)] [HTML.text "2"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickDigit D3)] [HTML.text "3"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator")] [HTML.text "-"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 2, HTML.Events.onClick \_ -> Just (ClickDigit D0)] [HTML.text "0"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "decimal"), HTML.Events.onClick \_ -> Just ClickDot] [HTML.text "."],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator")] [HTML.text "+"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 3, HTML.Properties.class_ (Halogen.ClassName "result")] [HTML.text "="],
                HTML.td [HTML.Properties.colSpan 1, HTML.Properties.class_ (Halogen.ClassName "cancel"), HTML.Events.onClick \_ -> Just Cancel] [HTML.text "AC"]
            ]
        ]
    ]

]

handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    NoAction ->
        pure unit
    ClickDigit d -> do
        Halogen.modify_ (\state -> state {
            input  = updateInput  state.input  state.isInsertingDecimalValues d,
            input' = updateInput' state.input' state.isInsertingDecimalValues d
        })
    ClickDot -> do
        Halogen.modify_ (\state -> state { isInsertingDecimalValues = true })
    Cancel -> do
        Halogen.modify_ (\state -> state { input = initialState'.input, isInsertingDecimalValues = false })
    -- Subtract -> do
    --     Halogen.liftEffect $ log "subtract"
    --     Halogen.modify_ (\(State s) -> State (s - 1))


handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
-- handleQuery = const (pure Nothing)
handleQuery = case _ of
    GetState k -> do
        Just <<< k <$> Halogen.get

receive :: Input -> Maybe Action
receive = const Nothing

initialize :: Maybe Action
initialize = Just NoAction

finalize :: Maybe Action
finalize = Nothing