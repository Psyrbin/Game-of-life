module Stack where


data Stack a = Empty | Element a (Stack a) deriving(Show)

push :: Stack a -> a -> Stack a
push stack a = Element a stack

pop :: Stack a -> (Stack a, Maybe a)
pop Empty = (Empty, Nothing)
pop (Element a stack) = (stack, Just a)

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False