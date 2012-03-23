import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (getApplication, withYesodHeroku)

main :: IO ()
main = defaultMain (fromArgs parseExtra) withYesodHeroku
