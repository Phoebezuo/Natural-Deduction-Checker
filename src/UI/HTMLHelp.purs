module UI.HTMLHelp
    ( select
    ) where

import Prelude
    (class Ord,
    ($), (<$>), (>=>), (<>),
    flip, identity, otherwise)
import Data.Map as M
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

{-
    Make a select dropdown given the action function, the hide function,
    the default option, and a list of options
-}
select :: forall a b r m. Ord a => String -> (a -> Maybe b) -> (a -> Boolean)
    -> a -> Array (Tuple String a) -> H.ComponentHTML b r m
select id act hide sel options = HH.select
    [ HE.onValueChange $ flip M.lookup forwardMap >=> act
    , HP.value $ maybe "-- Choose one --" identity $ M.lookup sel backwardMap
    , HP.id_ id
    ]
    $ default <> (option <$> options)
    where
        default = [ HH.option
                [ HP.value "-- Choose one --"
                , HP.disabled true
                , HP.attr (H.AttrName "hidden") ""
                ]
                [ HH.text "-- Choose one --" ]
            ]
        forwardMap = M.fromFoldable options
        backwardMap = M.fromFoldable $ T.swap <$> options
        option (Tuple name acts)
            | hide acts = HH.option
                [ HP.value name
                , HP.disabled true
                , HP.attr (H.AttrName "hidden") ""
                ]
                [ HH.text name ]
            | otherwise = HH.option
                [ HP.value name ]
                [ HH.text name ]
