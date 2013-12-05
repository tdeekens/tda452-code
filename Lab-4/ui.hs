import Haste
import Battleship

main :: IO ()
main = undefined

getShipModel :: String -> Model
getShipModel "aircraftcarrier" = AircraftCarrier
getShipModel "battleship"      = Battleship
getShipModel "submarine"       = Submarine
getShipModel "destroyer"       = Submarine
getShipModel "patrolboat"      = PatrolBoat