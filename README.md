### Normalization-bench

This repo contains some benchmarks for normalizing untyped lambda calculus and
doing beta-conversion checking on it. WIP, I plan to add more benchmarks and languages
to this and also different implementations in particular languages. PRs are welcome.

My motivation is type checking, where the more sophisticated
type systems, such as dependent type theories, require evaluating open lambda
terms at type checking time (and comparing them for conversion).

I've been using relatively simple AST interpreters for this purpose, and I
wanted to see what kind of difference compiled HOAS (higher-order
abstract syntax) makes in performance. I thought that perhaps a modern JIT language would be
good, because it would allow us to compile syntax to machine code via HOAS on
the fly. In contrast, GHC would be clumsier for compiled HOAS because of the
dependency on `ghc` and slow compilation. Or so I thought.

#### Setup

__Test computer__:
- Ubuntu 16.04 LTS
- i7-3770 @ 3.9 GHz
- 8GB RAM

__Scala__:
- sbt 1.3.4
- openjdk 1.8.0_232
- Runtime options: `export SBT_OPTS="-Xss1000M -Xmx4G"`

__nodejs__:
- node v13.6.0
- Runtime options: `su` and `ulimit -s unlimited`, running with `node
  --max-old-space-size=4000 --stack-size=6000000`.

__F#__:
- dotnet 3.0.101 (dotnet core)
- F# 4.0
- Runtime options: `su`, `ulimit -s unlimited`, running with `dotnet run -c Release`.

__GHC__:
- ghc 8.8.1
- LLVM 7.1
- Compile options: `-O2 -fllvm -rtsopts`.
- Runtime options: `+RTS -A1G -s`.

__Coq__
- coqc 8.10.2
- `coqc -impredicative-set`.

The implementations are virtually the same except in Coq, where we are using
well-typed impredicative Church encodings. The deep HOAS implementations use
idiomatic ADTs or a straightforward emulation of them (in the case of js) to
represent lambda terms and HOAS values.

The benchmarks are normalization and beta-conversion checking on Church-encoded unary natural numbers and
binary trees.

I made a non-trivial amount of effort trying to tune runtime options, but I
don't have much experience in tuning things besides GHC, so feel free to correct
me.

#### Results

GHC CBV is call-by-value, GHC CBN is call-by-need, all other columns are
call-by-value. Times are in milliseconds and are averages of 20 runs, except in
Coq, where it's a single `coqc` run.

|   | GHC HOAS CBV | GHC HOAS CBN | nodejs HOAS | Scala HOAS | F# HOAS  | Coq Church coding |
|:--|:--------|:-------|:------|:----|:------|:------
| Nat 5M conversion | 90 | 112 | 700 | 376        | 1246 | stack overflow
| Nat 5M normalization | 101 | 108 | 976 | 320    | 69592 | stack overflow
| Nat 10M conversion | 208 | 224 | 1395 | 1122    | 4462 | stack overflow
| Nat 10M normalization | 227 | 269 | 3718 | 4422 | too long | stack overflow
| Tree 2M conversion | 136 | 114  | 396 | 146     | 305 | 785
| Tree 2M normalization | 86 | 76 | 323  | 88     | 1514 | N/A
| Tree 4M conversion | 294 | 229 | 827 | 288      | 630 | 1530
| Tree 4M normalization | 192 | 194 | 635 | 174   | 3119 | N/A
| Tree 8M conversion | 723 | 457 | 1726 | 743     | 1232 | 2950
| Tree 8M normalization | 436 | 525 | 1398 | 750  | 5930 | N/A

#### Commentary

__F#__. Performance here was the most disappointing. I had hopes for F# since it is the nicest language
of the JIT pack by a significant margin, and I thought it would be tolerable or even superior to Haskell to
implement everything in F#, because of the support for high-performance non-persistent data structures, monomorphization, unpacked structures, etc. Now, there could be some GC option which magically repairs this, but I've tried and have not found a way to make it go faster.

__Scala__. I had never used Scala before, and I found the language relatively
pleasant, and definitely vastly better than Java or even Clojure. That said,
performance seems good for trees but degrades sharply from 5M to 10M with natural
numbers; perhaps there is some issue with deep stacks.

__nodejs__. Also pretty disappointing all around, although better than F#.

General comments.
- Stack space was an issue. All non-GHC contestants have very inadequate stack space defaults. The UX of increasing stack space was best with Scala, where I applaud JVM for at least providing a platform-independent stack size option, although sbt specifically has a [bug](https://github.com/sbt/sbt/issues/5181) which required me to set options via SBT_OPTS environment variable, instead of program flags. In contrast, F# and nodejs don't seem to have such option, and I had to use `ulimit`.
- All solutions benefit dramatically from throwing more memory at GC. Even GHC had 3-6 times speedup from `-A1G`. Scala and nodejs performance becomes dismal without free RAM; I plan to put these numbers up here as well. I suspect that F# is crippled by the same issues, but I was not able to use options to throw more memory at it.
- Startup time in nodejs is excellent, but F# and Scala are rather sluggish. GC pause variance is way more in the non-GHC solutions, with occasional multi-second pauses in nodejs and Scala.
- There are some stability issues in nodejs and Scala. In the former, I sometimes got runtime exceptions for 10M Nat, presumably related to too deep stacks (it was *not* stack overflow exception though, but some internal error!). In the latter, with 5M and 10M Nats, sometimes I got a sudden memory hike which resulted in out-of-memory process termination.
- __Call-by-need evaluation__ has only been benchmarked with GHC! CBN is critically important for type checking, and there is a good chance that CBN would make performance significantly worse elsewhere, since only GHC has native RTS support for CBN.

#### TODO
- Add bench figures without free memory options.
- Add figures for AST interpreters.
- Add OCaml, Coq, Agda, smalltt, Lean 3/4, [mlang](https://github.com/molikto/mlang).

#### Preliminary analysis & conclusions

Modern JIT platforms don't seem to be good at lambda calculus. From the current
benchmarking, JVM seems to be the best choice. However, I haven't yet
benchmarked call-by-need, which could possibly be a significant performance hit
on JVM. At the moment it does not seem worth to write high-performance proof
assistant implementation in any of the JIT languages, because plain hand-written
interpreters in GHC are much more convenient to maintain and develop and have
comparable performance to JIT HOAS. Perhaps compiling directly to CLR/JVM
bytecode would be better, but I am skeptical, and I would be hard pressed to
implement that myself.

A very high-effort solution is the Lean way: implement runtime system from
scratch. I would also like to do this once for a minimal untyped lambda
calculus, and just try to make it as fast as possible. I am sure GHC HOAS can be
beaten this way, but I am not sure that the effort is worth it.

I am entertaining more seriously the solution to use GHC HOAS in real-world type
theory implementations, by primarily using interpretation but constantly doing
compilation via GHC in the background, and switching out definitions. Combined
with more sophisticated diffing and incrementalization, this could have good UX
even in a single module.
