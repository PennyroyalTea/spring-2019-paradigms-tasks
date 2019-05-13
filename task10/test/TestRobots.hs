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

        , testCase "Test for getAttack" $
            getAttack mitty @?= 20

        , testCase "Test for getHealth" $
            getHealth mitty @?= 200

        , testCase "Test for setName" $
            setName "yo" mitty @?= robot "yo" 20 200

        , testCase "Test for setAttack" $
            setAttack 1 mitty @?= robot "Mitty" 1 200

        , testCase "Test for setHealth" $
            setHealth 0 mitty @?= robot "Mitty" 20 0

        , testCase "Test for is Alive" $
            isAlive mitty @?= True

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage + isAlive" $
            isAlive (damage mitty 10000) @?= False

        , testCase "Test for survivors" $
            map getName survivors @?= ["Bo", "Alex"]

        , testCase "Test for threeFight" $
            getName (threeRoundFight walter mitty) @?= "Mitty"

        , testCase "Test for fight" $
            fight mitty walter @?= robot "Walter" 50 30

        , testCase "Test for neue robot attak" $
            neueRobotAttak mitty @?= robot "Mitty" 20 186
        ]
