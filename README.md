### Normalization-bench

This repo contains some benchmarks for normalizing untyped lambda calculus and
doing beta-conversion checking on it. WIP, I plan to add more benchmarks and languages
to this and also different implementations in particular languages.

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
- Runtime options: `su`, `ulimit -s unlimited`, running with `dotnet run -C Release`.

__GHC__:
- ghc 8.8.1
- LLVM 7.1
- Compile options: `-O2 -fllvm -rtsopts`.
- Runtime options: `+RTS -A1G -s`.

In all of the above cases, the implementations are virtually the same. All implementations
use idiomatic ADTs or a straightforward emulation of them (in the case of js) to represent lambda terms and HOAS values.

The benchmarks are normalization and beta-conversion checking on Church-encoded unary natural numbers and
binary trees.

I made a non-trivial amount of effort trying to tune runtime options, but I
don't have much experience in tuning things besides GHC, so feel free to correct
me.

#### Results

GHC CBV is call-by-value, GHC CBN is call-by-need, all other columns are call-by-value. Times are in milliseconds and are averages of 20 runs.

|   | GHC CBV | GHC CBN | nodejs | Scala | F#  |
|:--|:--------|:-------|:------|:----|:------|
| Nat 5M conversion | 90 | 112 | 700 | 376        | 1246
| Nat 5M normalization | 101 | 108 | 976 | 320    | 69592
| Nat 10M conversion | 208 | 224 | 1395 | 1122    | 4462
| Nat 10M normalization | 227 | 269 | 3718 | 4422 | too long
| Tree 2M conversion | 55 | 51  | 356 | 160       | 298
| Tree 2M normalization | 73 | 88 | 379  | 116    | 1817
| Tree 4M conversion | 110 | 103 | 536 | 217      | 591
| Tree 4M normalization | 220 | 222 | 802 | 273   | 3897
| Tree 8M conversion | 217 | 205 | 1187 | 436     | 1182
| Tree 8M normalization | 618 | 620 | 1890 | 1442 | 10297

#### Commentary

__F#__. Performance here was the most disappointing. I had hopes for F# since it is the nicest language
of the JIT pack by a significant margin, and I thought it would be tolerable or even superior to Haskell to
implement everything in F#, because of the support for high-performance non-persistent data structures, monomorphization, unpacked structures, etc. Now, there could be some GC option which magically repairs this, but I've tried and have not found a way to make it go faster.

__Scala__. I had never used Scala before, and I found the language relatively pleasant, and definitely vastly better than Java or even Clojure. That said, performance is acceptable, but not really good. Normalization performance degrades disproportionately (apparently way worse than linear) when moving from 5M to 10M and from 4M to 8M.

__nodejs__. Also pretty disappointing all around, although better than F#.

General comments.
- Stack space was an issue. All non-GHC contestants have very inadequate stack space defaults. The UX of increasing stack space was best with Scala, where I applaud JVM for at least providing a platform-independent stack size option, although sbt specifically has a bug which required me to set options via SBT_OPTS environment variable, instead of program flags. In contrast, F# and nodejs don't seem to have such option, and I had to use `ulimit`.
- All solutions benefit dramatically from throwing more memory at GC. Even GHC had 3-6 times speedup from `-A1G`. Scala and nodejs performance becomes dismal without free RAM; I plan to put these numbers up here as well. I suspect that F# is crippled by the same issues, but I was not able to use options in F# to throw more memory at it.
- Startup time in nodejs is excellent, but F# and Scala are rather sluggish. GC pause variance is way more in the non-GHC solutions, with occasional multi-second pauses in nodejs and Scala.
- There are some stability issues in nodejs and Scala. In the former, I sometimes got runtime exceptions for 10M Nat, presumably related to too deep stacks (it was *not* stack overflow exception though, but some internal error!). In the latter, with 5M and 10M Nats, sometimes I got sudded a memory hike which  resulted in out-of-memory process termination.
- __Call-by-need evaluation__ has only been benchmarked with GHC! CBN is critically important for type checking, and there is a good chance that CBN would make performance significantly worse elsewhere, since only GHC has native RTS support for CBN.
- GHC uses a single core, while the JITs are typically using at least 50% of all of my cores.

#### TODO
- Add bench figures without free memory options.
- Add figures for AST interpreters.
- Add OCaml, Coq, Agda, smalltt, Lean 3/4.
- Benchmark molikto's [mlang](https://github.com/molikto/mlang), which is written in Scala but uses direct compilation to JVM bytecode instead of going through HOAS.

#### Preliminary analysis & conclusions

Modern JIT platforms suck at lambda calculus. There seems to be little sense in writing high-performance proof assistants in any of the JIT languages; plain hand-written interpreters in GHC are competitive with compiled JIT HOAS. Perhaps compiling directly to CLR/JVM bytecode would be better (benchmark TODO via mlang), but I am skeptical, and I would be hard pressed to implement that myself.

A very high-effort solution is the Lean way: implement runtime system from scratch. I would also like to do this once for a minimal untyped lambda calculus, and just try to make it as fast as possible. I am sure GHC HOAS can be beaten this way, but I am not sure that the effort is worth it.

I am entertaining more seriously the solution to use GHC HOAS in real-world type theory implementations, by primarily using interpretation but constantly doing compilation via GHC in the background, and switching out definitions. Combined with more sophisticated diffing and incrementalization, this could have good UX even in a single module.
