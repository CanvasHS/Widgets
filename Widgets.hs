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

data Widget =
    RadioButtons {
        radioId :: String,
        options :: [String]
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
installWidgetHandler callback state = installEventHandler (widgetHandler) combinedState
    where
        widgetState = emptyWidgetState
        combinedState = CombinedState widgetState state callback

--widgetHandler :: CombinedState a => a -> t -> (a, Output)
widgetHandler st e = (st, cb st e)
--widgetHandler st (MouseClick (x,y) id)
--    | startsWith "checkbox:" id = cb st StartEvent
--
--
    where
        cb = callback st

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
