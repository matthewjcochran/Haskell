module MontyHallProblem where


import System.Random
import Text.Printf 
import Data.List


main :: IO ()
main = do
  let doors = [1..3]  -- You can change the total number of doors or their labels
  let m = 5           -- Number of game results to display
  let n = 1000000     -- Number of games to play

  g <- newStdGen

  let (switchWins, switchStrs, g') = playN n True  doors g
  let (stayWins,   stayStrs,   g') = playN n False doors g

  putStrLn $ "ALWAYS SWITCH\n" ++ (getSummary switchWins switchStrs m n)
  putStrLn $ "ALWAYS STAY\n"   ++ (getSummary stayWins   stayStrs   m n)


choose :: (Eq a, RandomGen g) => [a] -> g -> (a, g)
choose xs g = (xs!!i, g') where (i, g') = randomR (0, length xs-1) g


excluding :: (Eq a) => [a] -> [a] -> [a]
excluding xs ys = filter (`notElem` ys) xs


play :: (Eq a, Show a, RandomGen g) => Bool-> [a] -> g -> (Bool, String, g)
play willSwitch doors g1
  | willSwitch == True && repick == car = (True,  strSwitch ++ "won!",  g5)
  | willSwitch == True                  = (False, strSwitch ++ "lost!", g5)
  | pick == car                         = (True,  strStay   ++ "won!",  g5)
  | otherwise                           = (False, strStay   ++ "lost!", g5)
  where
  (car,    g2) = choose doors g1
  (pick,   g3) = choose doors g2
  (goat,   g4) = choose (doors `excluding` [car, pick] ) g3
  (repick, g5) = choose (doors `excluding` [pick, goat]) g4
  strIntro = printf
    "Door %s had the car. You picked %s and were shown a goat behind %s.\n"
    (show car) (show pick) (show goat)
  strSwitch = strIntro ++ printf "You switched to door %s and " (show repick)
  strStay   = strIntro ++ printf "You stayed with door %s and " (show pick)


playN :: (Eq a, Show a, RandomGen g) => Int -> Bool -> [a] -> g -> ([Bool], [String], g) 
playN 0 willSwitch doors g1 = ([]      , [],       g1)
playN n willSwitch doors g1 = (win:wins, str:strs, g3)
  where
  (win,  str,  g2) = play        willSwitch doors g1
  (wins, strs, g3) = playN (n-1) willSwitch doors g2


getSummary :: [Bool] -> [String] -> Int -> Int -> String
getSummary wins strs m n =
  printf "%s\n...\nYou won %i out of %i times.\n"
    (intercalate "\n" $ take m $ strs)
    (length $ filter (== True) wins) n
