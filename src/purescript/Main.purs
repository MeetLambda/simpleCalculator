module Main where

import Components.SimpleCalculator as Components.SimpleCalculator
import Control.Bind (bind)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

main :: Effect Unit
main = runHalogenAff do
    body::HTMLElement <- awaitBody
    runUI Components.SimpleCalculator.component unit body
