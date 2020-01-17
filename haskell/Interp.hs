{-# language
  BlockArguments, Strict, LambdaCase, MagicHash,
  UnboxedTuples, OverloadedStrings, UnicodeSyntax
#-}

module Interp where

import Common
import qualified Data.Primitive.Array as A
import GHC.Exts

data RTm   = RVar String | RApp RTm RTm | RLam String RTm
data RCmd  = RNormalize RTm | RConv RTm RTm
type RProg = ([(String, RTm)], RCmd)

data Tm   = Local Int | App Tm Tm | Lam Tm | Top Int deriving Show
data Cmd  = Normalize Tm | Conv Tm Tm deriving Show
type Prog = (A.Array Tm, Cmd)

data Env  = Nil | Skip Env | Def Env Val
data Val  = VLocal Int | VApp Val Val | VLam Env Tm
data VCmd = VNormalize Tm | VConv Bool deriving Show


envLength :: Env -> Int
envLength = go 0 where
  go acc Nil         = acc
  go acc  (Def env _) = go (acc + 1) env
  go acc  (Skip env)  = go (acc + 1) env

evalLocal :: Env -> Int -> Val
evalLocal (Def e a) 0 = a
evalLocal (Skip e)  0 = VLocal (envLength e)
evalLocal (Def e _) x = evalLocal e (x - 1)
evalLocal (Skip e)  x = evalLocal e (x - 1)
evalLocal _         _ = undefined

eval :: A.Array Val -> Env -> Tm -> Val
eval top ~env = \case
  Local x -> evalLocal env x
  Top x   -> A.indexArray top x
  App t u -> case eval top env t of
               VLam env' t' -> let uv = eval top env u
                               in eval top (Def env' uv) t'
               t            -> VApp t (eval top env u)
  Lam t   -> VLam env t

quote :: A.Array Val -> Int -> Val -> Tm
quote top d = \case
  VLocal x    -> Local (d - x - 1)
  VApp t u    -> App (quote top d t) (quote top d u)
  VLam env' t -> Lam (quote top (d + 1) (eval top (Def env' (VLocal d)) t))

instance IsString RTm where
  fromString = RVar

renameTm :: [String] -> [String] -> RTm -> Tm
renameTm = go where
  var :: [String] -> [String] -> String -> Tm
  var top local x = l 0 local where
    l acc (y:ys) | x == y = Local acc
    l acc  (y:ys) = l (acc + 1) ys
    l acc  []     = g top
    g (y:ys) | x == y = Top (length ys)
    g (y:ys)          = g ys
    g []              = undefined

  go :: [String] -> [String] -> RTm -> Tm
  go top local = \case
    RVar x   -> var top local x
    RApp t u -> App (go top local t) (go top local u)
    RLam x t -> Lam (go top (x:local) t)

renameProg :: RProg -> Prog
renameProg (defs, cmd) = (fromList defs', cmd') where
  goDefs :: [String] -> [(String, RTm)] -> [Tm]
  goDefs top ((x, t):ts) = renameTm top [] t : goDefs (x:top) ts
  goDefs top []          = []

  defs' = goDefs [] defs
  top   = reverse $ map fst defs
  cmd'  = case cmd of
    RNormalize t -> Normalize (renameTm top [] t)
    RConv t u    -> Conv (renameTm top [] t) (renameTm top [] u)

conv :: A.Array Val -> Int -> Val -> Val -> Bool
conv top d l r = case (l, r) of
  (VLocal x, VLocal x') -> x == x'
  (VApp l l', VApp r r') -> conv top d l r && conv top d l' r'
  (VLam envl l, VLam envr r) ->
    let v     = VLocal d
        envl' = Def envl v
        envr' = Def envr v
    in conv top (d + 1) (eval top envl' l)
                        (eval top envr' r)
  _ -> False

evalProg :: Prog -> VCmd
evalProg (defs, cmd) =
  let vdefs = fmap (eval vdefs Nil) defs
  in case cmd of
       Normalize t -> VNormalize (quote vdefs 0 (eval vdefs Nil t))
       Conv t u    -> VConv (conv vdefs 0 (eval vdefs Nil t) (eval vdefs Nil u))

------------------------------------------------------------

run :: RProg -> VCmd
run = evalProg . renameProg

(∙) :: RTm -> RTm -> RTm
(∙) = RApp
infixl 8 ∙

λ :: String -> RTm -> RTm
λ = RLam

natToInt :: Tm -> Int
natToInt (Lam (Lam t)) = go 0 t where
  go acc (App _ t) = go (acc + 1) t
  go acc _         = acc
natToInt _ = 0

force :: VCmd -> Bool
force (VNormalize (Lam{})) = True
force (VNormalize _      ) = False
force (VConv b           ) = b

test :: RCmd -> RProg
test cmd = (
  [
    ("n2"     , λ "s" $ λ "z" $ "s" ∙ ("s" ∙ "z")),
    ("n5"     , λ "s" $ λ "z" $ "s" ∙ ("s" ∙ ("s" ∙ ("s" ∙ ("s" ∙ "z"))))),
    ("mul"    , λ "a" $ λ "b" $ λ "s" $ λ "z" $ "a" ∙ ("b" ∙ "s") ∙ "z"),
    ("suc"    , λ "a" $ λ "s" $ λ "z" $ "s" ∙ ("a" ∙ "s" ∙ "z")),
    ("n10"    , "mul" ∙  "n2" ∙ "n5"),
    ("n10b"   , "mul" ∙  "n5" ∙ "n2"),
    ("n20"    , "mul" ∙  "n2" ∙ "n10"),
    ("n20b"   , "mul" ∙  "n2" ∙ "n10b"),
    ("n21"    , "suc" ∙  "n20"),
    ("n21b"   , "suc" ∙  "n20b"),
    ("n22"    , "suc" ∙  "n21"),
    ("n22b"   , "suc" ∙  "n21b"),
    ("n100"   , "mul" ∙  "n10"   ∙ "n10"),
    ("n100b"  , "mul" ∙  "n10b"  ∙ "n10b"),
    ("n10k"   , "mul" ∙  "n100"  ∙ "n100"),
    ("n10kb"  , "mul" ∙  "n100b" ∙ "n100b"),
    ("n100k"  , "mul" ∙  "n10k"  ∙ "n10"),
    ("n100kb" , "mul" ∙  "n10kb" ∙ "n10b"),
    ("n1M"    , "mul" ∙  "n10k"  ∙ "n100"),
    ("n1Mb"   , "mul" ∙  "n10kb" ∙ "n100b"),
    ("n5M"    , "mul" ∙  "n1M"   ∙ "n5"),
    ("n5Mb"   , "mul" ∙  "n1Mb"  ∙ "n5"),
    ("n10M"   , "mul" ∙  "n1M"   ∙ "n10"),
    ("n10Mb"  , "mul" ∙  "n1Mb"  ∙ "n10b"),
    ("n20M"   , "mul" ∙  "n10M"  ∙ "n2"),
    ("n20Mb"  , "mul" ∙  "n10Mb" ∙ "n2"),
    ("leaf"   , λ "l" $ λ "n" $ "l"),
    ("node"   , λ "t1" $ λ "t2" $ λ "l" $ λ "n" $ "n" ∙ ("t1" ∙ "l" ∙ "n")
                                                      ∙ ("t2" ∙ "l" ∙ "n")),
    ("fullTree" , λ "n" $ "n" ∙ (λ "t" $ "node" ∙ "t" ∙ "t") ∙ "leaf"),
    ("t2M"      , "fullTree" ∙ "n20" ),
    ("t4M"      , "fullTree" ∙ "n21" ),
    ("t8M"      , "fullTree" ∙ "n22" ),
    ("t2Mb"     , "fullTree" ∙ "n20b"),
    ("t4Mb"     , "fullTree" ∙ "n21b"),
    ("t8Mb"     , "fullTree" ∙ "n22b")
  ],
  cmd)

bench :: IO ()
bench = do
  timed "Nat 5M conversion"     20 (pure . force . run . test) (RConv "n5M" "n5Mb")
  timed "Nat 5M normalization"  20 (pure . force . run . test) (RNormalize "n5M")
  timed "Nat 10M conversion"    20 (pure . force . run . test) (RConv "n10M" "n10Mb")
  timed "Nat 10M normalization" 20 (pure . force . run . test) (RNormalize "n10M")
  timed "Tree 2M conversion"    20 (pure . force . run . test) (RConv "t2M" "t2Mb")
  timed "Tree 2M normalization" 20 (pure . force . run . test) (RNormalize "t2M")
  timed "Tree 4M conversion"    20 (pure . force . run . test) (RConv "t4M" "t4Mb")
  timed "Tree 4M normalization" 20 (pure . force . run . test) (RNormalize "t4M")
  timed "Tree 8M conversion"    20 (pure . force . run . test) (RConv "t8M" "t8Mb")
  timed "Tree 8M normalization" 20 (pure . force . run . test) (RNormalize "t8M")
