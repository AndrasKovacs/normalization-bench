{-# language BlockArguments, BangPatterns #-}

module HOASCBV where

import Data.Time.Clock
import Control.Monad
import System.IO

data Val = VLam !(Val -> Val) | VVar !Int | VApp !Val !Val
data Tm  = Lam !Tm | App !Tm !Tm | Var !Int deriving Show

infixl 8 $$
($$) :: Val -> Val -> Val
VLam f $$ a = seq a (f a)
v      $$ a = VApp v a
{-# inline ($$) #-}

n2  = VLam \s -> VLam \z -> s $$ (s $$ z)
n5  = VLam \s -> VLam \z -> s $$ (s $$ (s $$ (s $$ (s $$ z))))
mul = VLam \a -> VLam \b -> VLam \s -> VLam \z -> a $$ (b $$ s) $$ z
suc n = VLam \s -> VLam \z -> s $$ (n $$ s $$ z)

n10      = mul $$ n2    $$ n5
n10b     = mul $$ n5    $$ n2
n20      = mul $$ n2    $$ n10
n20b     = mul $$ n2    $$ n10b
n21      = suc n20
n21b     = suc n20b
n22      = suc n21
n22b     = suc n21b
n100     = mul $$ n10   $$ n10
n100b    = mul $$ n10b  $$ n10b
n10k     = mul $$ n100  $$ n100
n10kb    = mul $$ n100b $$ n100b
n100k    = mul $$ n10k  $$ n10
n100kb   = mul $$ n10kb $$ n10b
n1M      = mul $$ n10k  $$ n100
n1Mb     = mul $$ n10kb $$ n100b
n5M      = mul $$ n1M   $$ n5
n5Mb     = mul $$ n1Mb  $$ n5
n10M     = mul $$ n1M   $$ n10
n10Mb    = mul $$ n1Mb  $$ n10b
n20M     = mul $$ n10M  $$ n2
n20Mb    = mul $$ n10Mb $$ n2
n100M    = mul $$ n10M  $$ n10
n100Mb   = mul $$ n10Mb $$ n10b

leaf     = VLam \l -> VLam \n -> l
node     = VLam \t1 -> VLam \t2 -> VLam \l -> VLam \n -> n $$ t1 $$ t2
fullTree = VLam \n -> n $$ (VLam \t -> node $$ t $$ t) $$ leaf

quote :: Int -> Val -> Tm
quote !d (VVar x)   = Var (d - x - 1)
quote  d (VApp t u) = App (quote d t) (quote d u)
quote  d (VLam t)   = Lam (quote (d + 1) (t (VVar d)))

quote0 = quote 0

conv :: Int -> Val -> Val -> Bool
conv !d t u = case (t, u) of
  (VVar x,   VVar x'   ) -> x == x'
  (VApp t u, VApp t' u') -> conv d t t' && conv d u u'
  (VLam t,   VLam t'   ) -> let v = VVar d in conv (d + 1) (t v) (t' v)
  _                      -> False

conv0 = conv 0

force :: Tm -> Bool
force Lam{} = True
force _     = False

natToInt :: Tm -> Int
natToInt (Lam (Lam t)) = go 0 t where
  go acc (App _ t) = go (acc + 1) t
  go acc _         = acc
natToInt _ = 0

timed :: Show b => String -> Int -> (a -> IO b) -> a -> IO ()
timed msg times act a = do
  putStrLn msg
  putStrLn $ "Iterations: " ++ show times
  t0 <- getCurrentTime
  replicateM_ times $ do
    b <- act a
    putStr $ show b ++ " "
  t1 <- getCurrentTime
  putStrLn $ "\nAverage time: " ++ show (diffUTCTime t1 t0 / fromIntegral times)
{-# noinline timed #-}


bench :: IO ()
bench = do
  hSetBuffering stdout NoBuffering
  timed "Nat 5M conversion"     20 (pure . conv0 n5M) n5Mb
  timed "Nat 5M normalization"  20 (pure . force . quote0) n5M
  timed "Nat 10M conversion"    20 (pure . conv0 n10M) n10Mb
  timed "Nat 10M normalization" 20 (pure . force . quote0) n10M
  timed "Tree 2M conversion"    20 (pure . conv0 (fullTree $$ n20)) (fullTree $$ n20b)
  timed "Tree 2M normalization" 20 (pure . force . quote0) (fullTree $$ n20)
  timed "Tree 4M conversion"    20 (pure . conv0 (fullTree $$ n21)) (fullTree $$ n21b)
  timed "Tree 4M normalization" 20 (pure . force . quote0) (fullTree $$ n21)
  timed "Tree 8M conversion"    20 (pure . conv0 (fullTree $$ n22)) (fullTree $$ n22b)
  timed "Tree 8M normalization" 20 (pure . force . quote0) (fullTree $$ n22)
