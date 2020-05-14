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
- `su`, `ulimit -s unlimited`, timing with `/usr/bin/time -v`. 

__smalltt__
- [smalltt 0.2.0.0](https://github.com/AndrasKovacs/smalltt)
- Runtime options: `+RTS -A1G`.

#### Benchmarks & implementations

Benchmarks are normalization and beta-conversion checking on Church-coded unary
numbers and binary trees.

All deep HOAS implementations are virtually the same; the use idiomatic ADTs for
terms and HOAS values, or straightforward ADT emulation in the case of javascript.

In GHC, we have a call-by-need and a call-by-value HOAS version. The difference
in the source code is just a couple of bangs. We also have an AST interpreted
CBV normalizer. The interpreter is fairly efficient, but we don't have any
optimization on the object language.

In Coq, we use well-typed impredicative Church encodings, which are slightly
more expensive than the untyped ones, because it also abstracts over a type
parameter.  We use `eq_refl` for conversion checking. For normalization, we use
conversion functions which go from Church encodings to inductive first-order
types. We use this because I was not able to find a good direct way in Coq to
force normalization of large Church trees, because `Eval compute` will attempt
to print the results, which has very large irrelevant overheads. Hence, the Coq
results don't measure exactly the same things as HOAS results.

Smalltt is similar to the Coq implementation, except that we can directly
normalize Church numbers and trees.

I made a non-trivial amount of effort trying to tune runtime options, but I
don't have much experience in tuning things besides GHC, so feel free to correct
me.

#### Results

Times in milliseconds. For Coq & smalltt results are from a single run. For everything
else results are averages of 20 runs.


|   | GHC HOAS CBV | GHC HOAS CBN | GHC interp CBV | nodejs HOAS | Scala HOAS | F# HOAS | Coq compute | Coq lazy | smalltt
|:--|:--------|:-------|:------|:----|:------|:------|:----|:----|:----
| Nat 5M conversion     | 90  | 112 | 234 | 700  | 376  | 1246     | 43080  | too long | 500
| Nat 5M normalization  | 101 | 108 | 167 | 976  | 320  | 69592    | 14630  | 50240    | 411
| Nat 10M conversion    | 208 | 224 | 695 | 1395 | 1122 | 4462     | 159880 | too long | 1681
| Nat 10M normalization | 227 | 269 | 439 | 3718 | 4422 | too long | 53280  | 220150   | 1148
| Tree 2M conversion    | 136 | 114 | 274 | 396  | 146  | 305      | 780    | 790      | 425
| Tree 2M normalization | 86  | 76  | 163 | 323  | 88   | 1514     | 760    | 850      | 346
| Tree 4M conversion    | 294 | 229 | 588 | 827  | 288  | 630      | 1620   | 1490     | 1429
| Tree 4M normalization | 192 | 194 | 343 | 635  | 174  | 3119     | 1500   | 1680     | 745
| Tree 8M conversion    | 723 | 457 | 1268| 1726 | 743  | 1232     | 3290   | 2920     | 2371
| Tree 8M normalization | 436 | 525 | 716 | 1398 | 750  | 5930     | 2940   | 3310     | 1544

#### Commentary

__F#__. Performance here was the most disappointing. I had hopes for F# since it is the nicest language
of the JIT pack by a significant margin, and I thought it would be tolerable or even superior to Haskell to
implement everything in F#, because of the support for high-performance non-persistent data structures, monomorphization, unpacked structures, etc. Now, there could be some GC option which magically repairs this, but I've tried and have not found a way to make it go faster.

__Scala__. I had never used Scala before, and I found the language relatively
pleasant, and definitely vastly better than Java or even Clojure. That said,
performance seems good for trees but degrades sharply from 5M to 10M with natural
numbers; perhaps there is some issue with deep stacks.

__nodejs__. Also pretty disappointing all around, although better than F#.

__GHC CBV interpreter__ is doing pretty well. It's already at worst half as fast
as Scala, and there are a number of optimizations still on the table. I'd first try
to add known call optimization.

__smalltt__ is slower than the barebones interpreter, which is as expected,
because smalltt has an evaluator which does significantly more bookkeeping
(serving elaboration purposes).

General comments.
- Stack space was an issue with JITs. All of those have very inadequate stack
  space defaults. The UX of increasing stack space was best with Scala, where I
  applaud JVM for at least providing a platform-independent stack size option,
  although sbt specifically has a [bug](https://github.com/sbt/sbt/issues/5181)
  which required me to set options via SBT_OPTS environment variable, instead of
  program flags. In contrast, F# and nodejs don't seem to have such option, and
  I had to use `ulimit`.
- All solutions benefit dramatically from throwing more memory at GC. GHC
  solutions & smalltt had 3-6 times speedup from `-A1G`. Scala and nodejs
  performance becomes dismal without free RAM; I plan to put these numbers up
  here as well. I suspect that F# is crippled by the same issues, but I was not
  able to use options to throw more memory at it. I also don't know how to throw
  memory at Coq.
- Startup time in nodejs is excellent, but F# and Scala are rather sluggish. GC
  pause variance is way more in the non-GHC solutions, with occasional
  multi-second pauses in nodejs and Scala.
- There are some stability issues in nodejs and Scala. In the former, I
  sometimes got runtime exceptions for 10M Nat, presumably related to too deep
  stacks (it was *not* stack overflow exception though, but some internal
  error!). In the latter, with 5M and 10M Nats, sometimes I got a sudden memory
  hike which resulted in out-of-memory process termination.
- All JIT solutions are CBV, however, CBN is essential for type checking and
  unfortunately there is a good chance that CBN would be a significant
  performance hit there. This should be also benchmarked.


#### TODO
- Add bench figures without free memory options.
- Add figures for AST interpreters.
- More benchmarks
- Add OCaml, Agda, Lean 3/4, [mlang](https://github.com/molikto/mlang).

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
