-- Module:
--
--      Fraction.hs
--
-- Language:
--
--      Haskell
--
-- Description: Rational with transcendental functionalities
--
--
--      This is a generalized Rational in disguise. Rational, as a type
--      synonim, could not be directly made an instance of any new class
--      at all.
--      But we would like it to be an instance of Transcendental, where
--      trigonometry, hyperbolics, logarithms, etc. are defined.
--      So here we are tiptoe-ing around, re-defining everything from
--      scratch, before designing the transcendental functions -- which
--      is the main motivation for this module.
--
--      Aside from its ability to compute transcendentals, Fraction
--      allows for denominators zero. Unlike Rational, Fraction does
--      not produce run-time errors for zero denominators, but use such
--      entities as indicators of invalid results -- plus or minus
--      infinities. Operations on fractions never fail in principle.
--
--      However, some function may compute slowly when both numerators
--      and denominators of their arguments are chosen to be huge.
--      For example, periodicity relations are utilized with large
--      arguments in trigonometric functions to reduce the arguments
--      to smaller values and thus improve on the convergence
--      of continued fractions. Yet, if pi number is chosen to
--      be extremely accurate then the reduced argument would
--      become a fraction with huge numerator and denominator
--      -- thus slowing down the entire computation of a trigonometric
--      function.
--
-- Usage:
--
--      When computation speed is not an issue and accuracy is important
--      this module replaces some of the functionalities typically handled
--      by the floating point numbers: trigonometry, hyperbolics, roots
--      and some special functions. All computations, including definitions
--      of the basic constants pi and e, can be carried with any desired
--      accuracy. One suggested usage is for mathematical servers, where
--      safety might be more important than speed. See also the module
--      Numerus, which supports mixed arithmetic between Integer,
--      Fraction and Cofra (Complex fraction), and returns complex
--      legal answers in some cases where Fraction would produce
--      infinities: log (-5), sqrt (-1), etc.
--
--
-- Required:
--
--      Haskell Prelude
--
-- Author:
--
--      Jan Skibinski, Numeric Quest Inc.
--
-- Date:
--
--      1998.08.16, last modified 2000.05.31
--
-- See also bottom of the page for description of the format used
-- for continued fractions, references, etc.
-------------------------------------------------------------------

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
            (u,v)
                | y < 0     = (-x, -y)
                | otherwise = (x, y)

(//) :: Integer -> Integer -> Fraction
x // y = reduce (x:/y)

approx :: Fraction -> Fraction -> Fraction
approx _ (x:/0) = x//0
approx eps x    =
    simplest (x - eps) (x + eps)
    where
        simplest y z
            | z < y     = simplest z y
            | y == z    = y
            | y > 0     = simplest' (num y) (den y) (num z) (den z)
            | z < 0     = - simplest' (-(num z)) (den z) (-(num y)) (den y)
            | otherwise = 0:/1
        simplest' n d n' d'        -- assumes 0 < n//d < n'//d'
            | r == 0    = q:/1
            | q /= q'   = (q + 1):/1
            | otherwise = (q * n'' + d''):/n''
            where
                (q, r)     = quotRem n d
                (q', r')   = quotRem n' d'
                (n'':/d'') = simplest' d' r' d r


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