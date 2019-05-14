import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1
    , testCase "head' works on an infinite list" $
        head' [1,2..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]
    , testCase "tail' works on infinite list too" $
        take' 100 (tail' [56..])  @?= take' 100 ([57..])

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]
    , testCase "take' takes 5 elems from infinite list" $
        take' 5 [1..] @?= [1, 2, 3, 4, 5]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]
    , testCase "drop' drops 100 elements from infinite list" $
        head' (drop' 100 [1..]) @?= 101

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]
    , testCase "filter' selects only even numbers from infinite list" $
        filter' even [1..] !! 4 @?= 10

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1, 2, 3] @?= 6
    , testCase "foldl'' assymetric" $
        foldl'' (^) 2 [1, 2, 3, 4] @?= ((2 ^ 2) ^ 3) ^ 4
    , testCase "foldl'' super assymetric " $
        foldl'' (++) "xxx" ["a", "b", "c", "d"] @?= "xxxabcd"

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]
    , testCase "concat' works on finite + infinite" $
        (concat' [-1, -2, -3] [1..]) !! 102 @?= 100
    , testCase "concat' works on infinite + infinite" $
        (concat' [1..] [1..]) !! 1000 @?= 1001

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
