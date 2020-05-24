module Main where

import Control.Bind (discard, bind)
import Control.Category (identity)
import Data.Maybe (Maybe, maybe)
import Data.Unit (Unit)
import Effect (Effect)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement)

import SimpleCalculator as SimpleCalculator

import Halogen.Aff.Util (runHalogenAff, awaitBody, selectElement)
import Halogen.VDom.Driver (runUI)
import Components.SimpleCalculator as Components.SimpleCalculator

import Concur.React.Run (runWidgetInDom)
import Widgets.SimpleCalculatorWidget as SimpleCalculatorWidget

main :: Effect Unit
main = do
    runHalogenAff do
        body                ::  HTMLElement         <- awaitBody
        halogenAppElement'  :: (Maybe HTMLElement)  <- selectElement (QuerySelector "#halogen")
        let halogenAppElement = maybe body identity halogenAppElement'
        runUI Components.SimpleCalculator.component SimpleCalculator.initialState halogenAppElement
    do
        runWidgetInDom "concur" SimpleCalculatorWidget.widget
