module Stack where

-- A stack implementation which can run inside the state monad

import Control.Monad.State

type Stack a = State [a]

processAsStack :: [a] -> Stack a () -> [a]
processAsStack lst process = snd (runState process lst)

pop :: Stack a a
pop = state $ \(x:xs) -> (x,xs)

push :: a -> Stack a ()
push x = state $ \lst -> ((),x:lst)

printList lst = putStrLn (show lst)

main =
  printList $
    processAsStack
    "ct"
    $ do
      x<- pop
      push 'a'
      push x
