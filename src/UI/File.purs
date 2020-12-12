module UI.File
    ( readFile
    ) where

import Web.Event.Event (Event)
import Control.Promise (Promise)

foreign import readFile :: Event -> Promise String
