import CanvasHs
import CanvasHs.Data

import Widgets



data State = State Int

initState = State 3

-- | call widgetHandler with your personal state
main = widgetHandler handler initState

-- | Be wary, you get CombinedState here containing your state
handler state StartEvent = (state, shape $ Container 900 600 [checkboxShape])
    where
        (widgetState, checkboxShape) = newRadioButton 3 "myRadioButton" ["optie 1", "optie 2", "optie 3", "optie 4"] 


{-

data WidgetState = WidgetState {
        widgets :: [Widget],
        hasFocus :: Widget
    }
data Widget = DropDown { dropDownId :: String, options :: [String], selected :: Int, collapsed :: Bool, offset :: Int } | NoWidget

initState = WidgetState [] NoWidget

main = installEventHandler handler initState

handler :: WidgetState -> Event -> (WidgetState, Output)
handler st StartEvent = (st, shape $
    drawDropDown ndd)

    where
        widgs = widgets st
        ndd = newDropDown "abcde" (map (\x -> "Optie: " ++ (show x)) [1..10])
        newWidgs = (ndd):widgs

drawDropDown :: Widget -> Shape
drawDropDown DropDown{dropDownId = dropDownId, options = options, selected = selected, collapsed = collapsed, offset = offset} =
    Container 256 totalSize ([
        -- | De omtrek
        Fill (255, 255, 255, 1.0) $ Stroke (64, 64, 64, 1.0) 1 $ Rect (0, 0) 256 32,
        -- | Het tekstvak
        Translate 8 8 $ Container 216 16 [Text (0,0) (options !! selected) defaults{size=16}],
        -- | het pijltje van de dropdown
        Translate 224 0 $ Event defaults{eventId=dropDownId, mouseClick = True} $ Container 32 32 [Stroke (128, 128, 128, 1.0) 1 $ Fill (64, 64, 64, 1.0) $ Polygon [(8, 10), (16, 16), (24, 10), (16, 22)]],

        -- | De container waar straks de andere opties instaan
        Translate 0 32 $ Fill (255, 255, 255, 1.0) $ Stroke (64, 64, 64, 1.0) 1 $ Rect (0, 0) 256 dropDownSize
    ] ++ (if (not collapsed) then
    [   -- | De andere opties, als de dropdown open is :P
        Translate 8 40 $ Container 240 192 
            (map (\x -> 
                Translate 0 (32 * x) $ Text (0,0) (options !! (x + offset)) defaults{size=16}
            ) [0..(maxLen - 1)])
    ] else []))

    where
        len = length options
        maxLen = min len 6
        totalSize = if collapsed then 32 else (maxLen + 1) * 32
        dropDownSize = maxLen * 32

newDropDown :: String -> [String] -> Widget
newDropDown newId options = DropDown ("dropdown:" ++ newId) options 0 True 0

-}
