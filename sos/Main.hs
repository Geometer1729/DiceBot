import Components
import Graphics.Vty
import Lens.Micro (Lens',lens)
import Sos.Build (buildChar, defChar)
import Sos.Components
import Sos.Types

main :: IO ()
main = do
   cfg <- standardIOConfig
   vty <- mkVty cfg
   runComponent vty buildChar defChar mainComp
   shutdown vty
