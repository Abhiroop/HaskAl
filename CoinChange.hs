module CoinChange where

-- Solution to this https://en.wikipedia.org/wiki/Change-making_problem

import Control.Monad.State

type Coins = [Int]

type StMach a = StateT Coins IO a

next_coin :: Int -> StMach Int
next_coin amt =
  do c:cs <- get
     if amt >= c
       then
        dispense c >> (return (amt - c))
       else
        put cs >>
         (return amt)

dispense :: Int -> StMach ()
dispense i =
  (lift $
     putStrLn
       ("dispense " ++ (show i)++cents)
  )
  where cents = if i==1 then " cent" else " cents"

recurs :: Int -> StMach Int
recurs amt =
  if amt == 0
     then return amt
     else next_coin amt >>= recurs

make_change :: Coins -> Int -> IO ()
make_change css amt =
  runStateT
   (recurs amt) css >> return ()

main =
  do putStrLn "Enter an amount : "
     line <- getLine
     make_change [6,2,1] (read line)


