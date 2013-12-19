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
