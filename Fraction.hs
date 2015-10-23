module Fraction where

import Data.Ratio

infix 7 :/

-------------------------------------------------------------------
--              Category: Basics
-------------------------------------------------------------------

data Fraction = Integer :/ Integer
        deriving (Eq)

num, den :: Fraction -> Integer
num (x:/_) = x
den (_:/y) = y

reduce  :: Fraction -> Fraction
reduce (x:/0)
        | x < 0     = (-1):/0
        | otherwise = 1:/0
reduce (x:/y) =
        (u `quot` d):/(v `quot` d)
        where
            d = gcd u v
            (u, v)
                | y < 0     = (-x, -y)
                | otherwise = (x, y)

(//) :: Integer -> Integer -> Fraction
x // y = reduce (x:/y)

instance Read Fraction where
    readsPrec p =
        readParen (p > 7) (\r -> [(x//y, u) | (x, s)     <- reads r,
                                              ("//", t) <- lex s,
                                              (y, u)    <- reads t ])

instance Show Fraction where
    showsPrec p (x:/y)
        | y == 1 = showsPrec p x
        | otherwise = showParen (p > 7) (shows x . showString "/" . shows y)

instance Ord Fraction where
    compare (x:/y) (x':/y') = compare (x * y') (x' * y)

instance Num Fraction where
    (x:/y) + (x':/y')  = reduce ((x * y' + x' * y):/(y * y'))
    (x:/y) - (x':/y')  = reduce ((x * y' - x' * y):/(y * y'))
    (x:/y) * (x':/y')  = reduce ((x * x'):/(y * y'))
    negate (x:/y)      = negate x:/y
    abs (x:/y)         = abs x:/y
    signum (x:/_)      = signum x:/1
    fromInteger n      = fromInteger n:/1

instance Fractional Fraction where
    (x:/0) / (x':/0)  = ((signum x * signum x'):/0)
    (_:/_) / (_:/0)   = (0:/1)
    (x:/0) / (_:/_)   = (x:/0)
    (x:/y) / (x':/y') = reduce ((x * y') :/ (y * x'))
    recip (x:/y)      = if x < 0 then (-y):/(-x) else y:/x
    fromRational a    = x:/y
                        where
                            x = numerator a
                            y = denominator a