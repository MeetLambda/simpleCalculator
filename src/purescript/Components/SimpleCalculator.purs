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
import Data.HeytingAlgebra ((&&), (||), not)
import Data.Int (round)
import Data.Lens.Fold (firstOf, lastOf)
import Data.Lens.Traversal (traversed)
import Data.List (snoc, null)
import Data.List.Types (List(..), (:))
import Data.Maybe (Maybe(..), maybe, isNothing)
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

import SimpleCalculator as SimpleCalculator

type Surface    = HTML.HTML


data Action     = ClickKey SimpleCalculator.Key
data Query a    = GetState (State -> a)
type Input      = SimpleCalculator.Status
data Output     = NoOutput      -- aka Message

type State      = SimpleCalculator.Status
type Slots      = ()

initialState :: Input -> State
initialState = identity

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

-- ===============================

handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    ClickKey k -> do
        Halogen.modify_ (\state -> SimpleCalculator.handleKey state k)

handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
handleQuery = const (pure Nothing)

receive :: Input -> Maybe Action
receive = const Nothing

initialize :: Maybe Action
initialize = Nothing

finalize :: Maybe Action
finalize = Nothing

showC :: State -> Boolean
showC s = true

render :: forall m. {-MonadAff m =>-} State -> Halogen.ComponentHTML Action Slots m
render (state) = HTML.div [] [
    HTML.div [HTML.Properties.class_ (Halogen.ClassName "display")]  [HTML.text state.display],
    HTML.table [] [
        HTML.tbody [] [
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_7)] [HTML.text "7"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_8)] [HTML.text "8"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_9)] [HTML.text "9"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Divide)] [HTML.text "/"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_4)] [HTML.text "4"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_5)] [HTML.text "5"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_6)] [HTML.text "6"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Multiple)] [HTML.text "*"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_1)] [HTML.text "1"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_2)] [HTML.text "2"],
                HTML.td [HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_3)] [HTML.text "3"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Subtract)] [HTML.text "-"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 2, HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_0)] [HTML.text "0"],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "decimal"),  HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Dot)] [HTML.text "."],
                HTML.td [HTML.Properties.class_ (Halogen.ClassName "operator"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Add)] [HTML.text "+"]
            ],
            HTML.tr [] [
                HTML.td [HTML.Properties.colSpan 3, HTML.Properties.class_ (Halogen.ClassName "result"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_Equal)] [HTML.text "="],
                if (showC state)
                    then HTML.td [HTML.Properties.colSpan 1, HTML.Properties.class_ (Halogen.ClassName "cancel"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_C)]  [HTML.text "C"]
                    else HTML.td [HTML.Properties.colSpan 1, HTML.Properties.class_ (Halogen.ClassName "cancel"), HTML.Events.onClick \_ -> Just (ClickKey SimpleCalculator.K_AC)] [HTML.text "AC"]
            ]
        ]
    ]
]
