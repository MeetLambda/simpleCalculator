module Components.SimpleCalculator where

import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Semigroupoid ((<<<))
import Data.Eq (class Eq)
import Data.EuclideanRing ((/))
import Data.Function (($), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Data.Ring ((-))
import Data.Semiring ((+), (*))
import Data.Show (show)
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
                | Cancel
                -- | Add -- | SubComponentOutput SubComponent.S_Output
data Query a    = GetState (State -> a)
type Input      = Unit
data Output     = NoOutput      -- aka Message

data Operator   = Addition | Subtraction | Multiplication | Division
type State      = {
    memory                   :: Maybe Number,
    operator                 :: Maybe Operator,
    isInsertingDecimalValues :: Boolean,
    input                    :: Maybe Number
}
type Slots      = ()

{-
The type variables involved:
- `surface` is the type that will be rendered by the component, usually `HTML`
* `state` is the component's state
- `query` is the query algebra; the requests that can be made of the component
* `action` is the type of actions; messages internal to the component that can be evaluated
* `slots` is the set of slots for addressing child components
- `input` is the input value that will be received when the parent of this component renders
- `output` is the type of messages the component can raise
- `m` is the effect monad used during evaluation


The values in the record:
- `initialState` is a function that accepts an input value and produces the state the component will start with.
  If the input value is unused (`Unit`), or irrelevant to the state construction, this will often be `const ?someInitialStateValue`.
- `render` is a function that accepts the component's current state and produces a value to render (`HTML` usually).
  The rendered output can raise actions that will be handled in `eval`.
- `eval` is a function that handles the `HalogenQ` algebra that deals with component lifecycle, handling actions, and responding to requests.

-}

initialState :: Input -> State
initialState _ = {
    memory:   Nothing,
    operator: Nothing,
    input:    Nothing,
    isInsertingDecimalValues: false
}

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

showInput :: Maybe Number -> String
showInput Nothing = "0.0"
showInput (Just n) = show n

valueOf :: Digit -> Number     -- verify if 'coercible' is applicable
valueOf D0 = 0.0
valueOf D1 = 1.0
valueOf D2 = 2.0
valueOf D3 = 3.0
valueOf D4 = 4.0
valueOf D5 = 5.0
valueOf D6 = 6.0
valueOf D7 = 7.0
valueOf D8 = 8.0
valueOf D9 = 9.0

updateInput :: Maybe Number -> Boolean -> Digit -> Maybe Number
updateInput Nothing  false d = Just (valueOf d)
updateInput (Just i) false d = Just ((i * 10.0) + (valueOf d))
updateInput Nothing  true  d = Just ((valueOf d) / 10.0)
updateInput (Just i) true  d = Just (i + (valueOf d))

render :: forall m. {-MonadAff m =>-} State -> Halogen.ComponentHTML Action Slots m
render (state) = HTML.div [] [
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "display")] [HTML.text (showInput state.input)],
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
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "decimal")] [HTML.text "."],
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
        Halogen.modify_ (\state -> state { input = updateInput state.input state.isInsertingDecimalValues d })
    Cancel -> do
        Halogen.modify_ (\state -> state { input = Nothing, isInsertingDecimalValues = false })
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