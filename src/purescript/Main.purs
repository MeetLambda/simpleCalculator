module Main where

import Control.Bind (discard, pure, bind, (=<<))
import Control.Category (identity)
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..), maybe)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)

import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (HTMLElement, title)
import Web.HTML.Window (document)

import SimpleCalculator as SimpleCalculator

import Halogen as Halogen
import Halogen.Aff.Util (runHalogenAff, awaitBody, awaitLoad, selectElement)
import Halogen.VDom.Driver (runUI)
import Components.SimpleCalculator as Components.SimpleCalculator

import Concur.React.Run (runWidgetInDom)
import Widgets.Calculator as Calculator

-- main :: Effect Unit
main = do
    runHalogenAff do
        -- body :: HTMLElement <- awaitBody
        -- runUI Components.SimpleCalculator.component SimpleCalculator.initialState body
        body <- awaitBody
        halogenAppElement' <- selectElement (QuerySelector "#halogen_app")
        let halogenAppElement = maybe body identity halogenAppElement'
        runUI Components.SimpleCalculator.component SimpleCalculator.initialState halogenAppElement
    do
        log "hello!"
        runWidgetInDom "concur_app" Calculator.widget
