-- Widget library for Canvas.Hs
-- Copyright (C) 2013, Lennart Buit, Joost van Doorn, Pim Jager, Martijn Roo,
-- Thijs Scheepers
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
-- USA

module Widgets where

import CanvasHs
import CanvasHs.Data

import Data.List.Split (splitOn)

type Callback a = (a -> Event -> (a, Output))

data WidgetState
    = WidgetState {
        widgets :: [Widget],
        hasFocus :: Widget
    }

emptyWidgetState = WidgetState [] NoWidget

data Widget 
    = RadioButtons {
        radioId :: String,
        options :: [String]
    }
    | DropDown {
        dropDownId :: String,
        options :: [String],
        selected :: Int
    }
    | NoWidget

data CombinedState a 
    = CombinedState {
        widgetState :: WidgetState,
        userState :: a,
        callback :: Callback a
    }

data WidgetEvent
    -- | Radiobutton state from the specified Widget is changed to Int
    = RadioChanged Widget Int

data EventOrWidgetEvent = Either Event WidgetEvent

newRadioButton :: WidgetState -> String -> [String] -> WidgetState
newRadioButton state id options = newState
    where
        wids = widgets state
        newRadio = RadioButtons id options
        newWidgets = newRadio:(wids)
        newState = state{widgets = newWidgets}
 
drawAll :: WidgetState -> Shape
drawAll ws = Container 0 0 $ map (\x -> drawOne x) (widgets $ ws)

drawOne :: Widget -> Shape
drawOne (RadioButtons{radioId = radioId, options = options}) = Container 256 (32 * (len + 1)) $
    map (\i ->
        Translate 0 (i * 32) $ 
            Event defaults{eventId="checkbox:" ++ radioId ++ ":" ++ show i} $ 
                Container 256 32 [
                    Fill (255, 255, 255, 1.0) $ 
                        Stroke (0, 0, 0, 1.0) 1 $ Circle (16, 16) 12,
                    Text (64, 8) (options !! i) defaults{
                        font="Cantarell", 
                        alignment=AlignCenter
                    }
                ]
            ) 
        [0..(len - 1)]
    where
        len = length options

-- | deze functie is overduidelijk niet pure
installWidgetHandler :: Callback a -> a -> IO ()
installWidgetHandler cb state = installEventHandler widgetHandler startState
    where
        startState = (CombinedState{widgetState=emptyWidgetState, userState=state, callback=cb})

widgetHandler :: CombinedState a -> Event -> (CombinedState a, Output)
widgetHandler st e = (st', output)
--widgetHandler st (MouseClick (x,y) id)
--    | startsWith "checkbox:" id = cb st StartEvent
--
--
    where
        (us, output) = (callback st) (userState st) e
        st' = st{userState=us}
--widgetHandler st e = (st, shape $ drawAll $ widgetState $ st)

-- | Utility
startsWith :: Ord a => [a] -> [a] -> Bool
startsWith _ []            = False
startsWith [] _            = True
startsWith needle haystack = (head needle == head haystack) && (startsWith (tail needle) (tail haystack))

-- | Get the eventId
getEventId :: String -> (String, Int)
getEventId eidstring = (eid, selected)
    where
        all = splitOn ":" eidstring
        eid = all !! 1
        selected = read (all !! 2) :: Int
