module Fraction where
import Data.Ratio

infix 7 :/

data Fraction = Integer :/ Integer  -- Constructor.
    deriving (Eq)

simplify  :: Fraction -> Fraction   -- Fraction simplifier.
simplify (num:/0) =                 -- Denominator equals zero: do anything.
    if num < 0 
    then (-1):/0
    else 1:/0

simplify (num:/den) =               -- Simplification.
    (quot x d):/(quot x' d)
    where
        d       = gcd x x'          -- Great common divisor.
        (x, x') =
            if den < 0 
            then (-num, -den)
            else (num, den)

(//) :: Integer -> Integer -> Fraction 	-- Fraction common reader (includes simplify)
num // den = simplify (num:/den)

instance Read Fraction where       		-- // Reader.
    readsPrec p =
        readParen (p > 7) (\r -> [(num//den, u) | (num, s)  <- reads r,
                                                  ("//", t) <- lex s,
                                                  (den, u)  <- reads t])

instance Show Fraction where            -- Fraction show.
    showsPrec p (num:/den) =
        if den == 1                     -- If denominator equals 1,
        then showsPrec p num            -- show only the numerator.
        else showParen (p > 7) (shows num . showString "/" . shows den)

instance Ord Fraction where             -- Fraction compare.
    compare (num:/den) (num':/den') = compare (num * den') (num' * den)

instance Num Fraction where             -- Fraction numerables: + - *.
    (num:/den) + (num':/den')  = simplify ((num * den' + num' * den):/(den * den'))
    (num:/den) - (num':/den')  = simplify ((num * den' - num' * den):/(den * den'))
    (num:/den) * (num':/den')  = simplify ((num * num'):/(den * den'))
    negate (num:/den) = negate num:/den
    abs (num:/den)    = abs num:/den
    signum (num:/_)   = signum num:/1
    fromInteger n     = fromInteger n:/1

instance Fractional Fraction where      -- Fraction fractional: /.
    (num:/0) / (num':/0) = (signum num * signum num'):/0
    (_:/_) / (_:/0)      = (0:/1)
    (num:/0) / (_:/_)    = (num:/0)
    (num:/den) / (num':/den') = simplify ((num * den'):/(den * num'))
    recip (num:/den)          = 
        if num < 0 
        then (-den):/(-num) 
        else den:/num    
    fromRational a = num:/den 
        where
            num = numerator a
            den = denominator a