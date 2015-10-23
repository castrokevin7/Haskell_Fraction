module Fraction where

import Data.Ratio

infix 7 :/

data Fraction = Integer :/ Integer
    deriving (Eq)

reduce  :: Fraction -> Fraction
reduce (num:/0) =
    if num < 0 
    then (-1):/0
    else 1:/0

reduce (num:/den) =
    (u `quot` d):/(v `quot` d)
    where
        d      = gcd u v
        (u, v) =
            if den < 0 
            then (-num, -den)
            else (num, den)

(//) :: Integer -> Integer -> Fraction
num // den = reduce (num:/den)

instance Read Fraction where
    readsPrec p =
        readParen (p > 7) (\r -> [(num//den, u) | (num, s)  <- reads r,
                                                  ("//", t) <- lex s,
                                                  (den, u)  <- reads t])

instance Show Fraction where
    showsPrec p (num:/den) =
        if den == 1
        then showsPrec p num
        else showParen (p > 7) (shows num . showString "/" . shows den)

instance Ord Fraction where
    compare (num:/den) (num':/den') = compare (num * den') (num' * den)

instance Num Fraction where
    (num:/den) + (num':/den')  = reduce ((num * den' + num' * den):/(den * den'))
    (num:/den) - (num':/den')  = reduce ((num * den' - num' * den):/(den * den'))
    (num:/den) * (num':/den')  = reduce ((num * num'):/(den * den'))
    negate (num:/den) = negate num:/den
    abs (num:/den)    = abs num:/den
    signum (num:/_)   = signum num:/1
    fromInteger n     = fromInteger n:/1

instance Fractional Fraction where
    (num:/0) / (num':/0) = ((signum num * signum num'):/0)
    (_:/_) / (_:/0)      = (0:/1)
    (num:/0) / (_:/_)    = (num:/0)
    (num:/den) / (num':/den') = reduce ((num * den') :/ (den * num'))
    recip (num:/den)          = 
        if num < 0 
        then (-den):/(-num) 
        else den:/num    
    fromRational a = num:/den
        where
            num = numerator a
            den = denominator a