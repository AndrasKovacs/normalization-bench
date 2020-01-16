
// su
// ulimit -s unlimited
// dotnet run -c Release

module MyProgram

open System.Diagnostics

module HOASNf =
  type Val = VVar of int | VApp of Val * Val | VLam of (Val -> Val)
  type Tm = Var of int | App of Tm * Tm | Lam of Tm

  let inline ap t u = match t with VLam f -> f u | t -> VApp(t, u)
  let inline ap2 t u v = ap (ap t u) v

  let n2  = VLam (fun s -> VLam (fun z -> ap s (ap s z)))
  let n5  = VLam (fun s -> VLam (fun z -> ap s (ap s (ap s (ap s (ap s z))))))
  let mul = VLam (fun a -> VLam (fun b -> VLam (fun s -> VLam (fun z -> ap (ap a (ap b s)) z))))
  let suc n  = VLam (fun s -> VLam (fun z -> ap s (ap2 n s z)))
  let n10    = ap2 mul n2 n5
  let n10b   = ap2 mul n5 n2
  let n20    = ap2 mul n2 n10
  let n20b   = ap2 mul n2 n10b
  let n21    = suc n20
  let n21b   = suc n20b
  let n22    = suc n21
  let n22b   = suc n21b
  let n100   = ap2 mul n10  n10
  let n100b  = ap2 mul n10b n10b
  let n10k   = ap2 mul n100 n100
  let n10kb  = ap2 mul n100b n100b
  let n100k  = ap2 mul n10k n10
  let n100kb = ap2 mul n10k n10
  let n1M    = ap2 mul n10k n100
  let n1Mb   = ap2 mul n10kb n100b
  let n2M    = ap2 mul n1M  n2
  let n2Mb   = ap2 mul n1Mb n2
  let n5M    = ap2 mul n1M  n5
  let n5Mb   = ap2 mul n1Mb  n5
  let n10M   = ap2 mul n1M  n10
  let n10Mb  = ap2 mul n1Mb n10b

  let leaf = VLam(fun l -> VLam(fun n -> l))
  let node = VLam(fun t1 -> VLam(fun t2 -> VLam(fun l -> VLam(fun n ->
                  ap2 n t1 t2))))
  let fullTree = VLam(fun n -> ap2 n (VLam(fun t -> ap2 node t t)) leaf)

  let rec quote l v =
    match v with
      | VVar x      -> Var (l - x - 1)
      | VLam t      -> Lam (quote (l + 1) (t (VVar(l))))
      | VApp (t, u) -> App (quote l t, quote l u)

  let quote0 = quote 0

  let rec conv (d:int) l r =
      match (l, r) with
      | (VVar(x), VVar(y)) -> x = y
      | (VApp(l1, l2), VApp(r1, r2)) -> conv d l1 r1 && conv d l2 r2
      | (VLam(l), VLam(r)) -> conv (d + 1) (l(VVar(d))) (r(VVar(d)))
      | _ -> false

  let conv0 = conv 0

  let natToInt (t : Tm) : int =
    match t with
      | Lam (Lam t) ->
          let mutable acc = 0
          let mutable u = t
          let upd() =  match u with App(_, u') -> u <- u'; true
                                  | _          -> false
          while upd() do
            acc <- acc + 1
          acc
      | _ -> 1

  let force (t : Tm) =
    match t with Lam(_) -> true
               | _      -> false

  let timed (msg:string) (times: int64) (act: Unit -> 'a) =
        printfn "%s" msg
        printfn "Iterations: %A" times
        let t = Stopwatch()
        t.Start()
        for i in 1L .. times do
          printf "%A " (act ())
        t.Stop()
        printfn "\nAverage time: %A ms" (t.ElapsedMilliseconds / times)

open HOASNf

[<EntryPoint>]
let main argv =
  // timed "Nat 5M conversion"      20L (fun _ -> conv0 n5M n5Mb)
  // timed "Nat 5M normalization"   5L (fun _ -> force (quote0 n5M))
  // timed "Nat 10M conversion"     5L (fun _ -> conv0 n10M n10Mb)
  // timed "Nat 10M normalization"  20L (fun _ -> force (quote0 n10M))
  // timed "Tree 2M conversion"     20L (fun _ -> conv0 (ap fullTree n20) (ap fullTree n20b))
  // timed "Tree 2M normalization"  5L (fun _ -> force (quote0 (ap fullTree n20)))
  // timed "Tree 4M conversion"     20L (fun _ -> conv0 (ap fullTree n21) (ap fullTree n21b))
  // timed "Tree 4M normalization"  5L (fun _ -> force (quote0 (ap fullTree n21)))
  // timed "Tree 8M conversion"     20L (fun _ -> conv0 (ap fullTree n22) (ap fullTree n22b))
  timed "Tree 8M normalization"  5L (fun _ -> force (quote0 (ap fullTree n22)))
  0
