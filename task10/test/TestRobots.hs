import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        mitty = robot "Mitty" 20 200
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for survivors" $
            map getName survivors @?= ["Bo", "Alex"]
        , testCase "Test for threeFight" $
            getName (threeRoundFight walter mitty) @?= "Mitty"
        ]
