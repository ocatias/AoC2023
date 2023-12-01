import Data.List.Split
import Data.List

is_number x = elem x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

adv_is_number (x:ls)
  | elem x ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] = [x] ++ (adv_is_number ls)
  | isPrefixOf "zero" (x:ls) = "0 " ++  (adv_is_number ls)
  | isPrefixOf "one" (x:ls) = "1 " ++  (adv_is_number ls)
  | isPrefixOf "two" (x:ls) = "2 " ++  (adv_is_number ls)
  | isPrefixOf "three" (x:ls) = "3 " ++  (adv_is_number ls)
  | isPrefixOf "four" (x:ls) = "4 " ++  (adv_is_number ls)
  | isPrefixOf "five" (x:ls) = "5 " ++  (adv_is_number ls)
  | isPrefixOf "six" (x:ls) = "6 " ++  (adv_is_number ls)
  | isPrefixOf "seven" (x:ls) = "7 " ++  (adv_is_number ls)
  | isPrefixOf "eight" (x:ls) = "8 " ++  (adv_is_number ls)
  | isPrefixOf "nine" (x:ls) = "9 " ++  (adv_is_number ls)
  | otherwise =  (adv_is_number ls)

adv_is_number _ = []
main = do
  rawInput <- readFile("input")
  let lines = init (splitOn "\n" rawInput)
  let numbers_in_line = map (filter is_number) lines
  print $ sum $ map (\x-> read [(x!!0), last x] :: Integer) numbers_in_line
  let numbers_in_line2 = map (filter is_number) (map adv_is_number lines)
  print $ sum $ map (\x-> read [(x!!0), last x] :: Integer) numbers_in_line2
