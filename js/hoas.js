
// su
// ulimit -s 6000000; node --max-old-space-size=4000 --stack-size=6000000 hoas.js

const { PerformanceObserver, performance } = require('perf_hooks');

const VAR  = 0
const LAM  = 1
const APP  = 2

const VVAR = 0
const VLAM = 1
const VAPP = 2

const mkVar  = (x)    => ({tag:VAR, p1: x})
const mkLam  = (t)    => ({tag:LAM, p1: t})
const mkApp  = (t, u) => ({tag:APP, p1:t, p2:u})

const mkVVar = (x)    => ({tag:VVAR, p1: x})
const mkVLam = (t)    => ({tag:VLAM, p1: t})
const mkVApp = (t, u) => ({tag:VAPP, p1:t, p2:u})

const ap = (t, u) => {
    switch (t.tag) {
    case VLAM: return (t.p1)(u);
    default  : return mkVApp(t, u);
    }
}

const ap2 = (t, u, v) => ap(ap(t, u), v)

const quote = (l, v) => {
    switch (v.tag) {
    case VVAR: return mkVar(l - v.p1 - 1);
    case VLAM: return mkLam(quote(l+1, (v.p1)(mkVVar(l))));
    case VAPP: return mkApp(quote(l, v.p1), quote(l, v.p2));
    }
}

const quote0 = (v) => quote(0, v)
const force = (n) => (n.tag === LAM)

const conv = (d, l, r) => {
    switch (l.tag) {
    case VVAR: return (r.tag === VVAR) && (l.p1 === r.p1);
    case VLAM: return (r.tag === VLAM) && conv(d+1, l.p1(mkVVar(d)), r.p1(mkVVar(d)));
    case VAPP: return (r.tag === VAPP) && conv(d, l.p1, r.p1) && conv(d, l.p2, r.p2);
    }
  }

const conv0 = (l, r) => conv(0, l, r)

// natural numbers
// ------------------------------------------------------------

const n2  = mkVLam ((s) => mkVLam ((z) => ap(s, ap(s, z))))
const n5  = mkVLam ((s) => mkVLam ((z) => ap(s, ap(s, (ap(s, ap(s, ap(s, z))))))))
const mul = mkVLam ((a) => mkVLam ((b) => mkVLam((s) => mkVLam((z) => ap2(a, ap(b, s), z)))))
const suc = (n) => mkVLam ((s) => mkVLam((z) => ap(s, ap2(n, s, z))))

const n10    = ap2(mul, n2,   n5)
const n10b   = ap2(mul, n5,   n2)
const n20    = ap2(mul, n2,   n10)
const n20b   = ap2(mul, n2,   n10b)
const n21    = suc(n20)
const n21b   = suc(n20b)
const n22    = suc(n21)
const n22b   = suc(n21b)
const n100   = ap2(mul, n10,  n10)
const n100b  = ap2(mul, n10b, n10b)
const n10k   = ap2(mul, n100, n100)
const n10kb  = ap2(mul, n100b, n100b)
const n100k  = ap2(mul, n10k, n10)
const n100kb = ap2(mul, n10k, n10)
const n1M    = ap2(mul, n10k, n100)
const n1Mb   = ap2(mul, n10kb, n100b)
const n2M    = ap2(mul, n1M,  n2)
const n2Mb   = ap2(mul, n1Mb, n2)
const n5M    = ap2(mul, n1M,  n5)
const n5Mb   = ap2(mul, n1Mb,  n5)
const n10M   = ap2(mul, n1M,  n10)
const n10Mb  = ap2(mul, n1Mb, n10b)

const natToInt = (n) => {
    let acc = 0;
    let m   = n.p1.p1;
    while (m.tag === APP){
        acc += 1;
        m = m.p2;
    }
    return acc;
}


// binary trees
// ------------------------------------------------------------

const leaf = mkVLam((l) => mkVLam((n) => l))
const node = mkVLam((t1) => mkVLam((t2) => mkVLam((l) => mkVLam((n) =>
                   ap2(n, t1, t2)))))
const fullTree = mkVLam((n) => ap2(n, mkVLam((t) => ap2(node, t, t)), leaf))


function timed(msg, times, act){
    console.log(msg);
    console.log("Iterations: " + times.toString());
    const t0 = performance.now();
    for (let i = 1; i <= times; i++){
	process.stdout.write(act() + " ");
    }
    const t1 = performance.now();
    console.log("\nAverage time: " + (t1 - t0)/times);
}

timed("Nat 5M conversion"     , 20, () => conv0(n5M, n5Mb));
timed("Nat 5M normalization"  , 20, () => force(quote0(n5M)));
timed("Nat 10M conversion"    , 20, () => conv0(n10M, n10Mb));
timed("Nat 10M normalization" , 20, () => force(quote0(n10M)));
timed("Tree 2M conversion"    , 20, () => conv0(ap(fullTree, n20), ap(fullTree, n20b)));
timed("Tree 2M normalization" , 20, () => force(quote0(ap(fullTree, n20))));
timed("Tree 4M conversion"    , 20, () => conv0(ap(fullTree, n21), ap(fullTree, n21b)));
timed("Tree 4M normalization" , 20, () => force(quote0(ap(fullTree, n21))));
timed("Tree 8M conversion"    , 20, () => conv0(ap(fullTree, n22), ap(fullTree, n22b)));
timed("Tree 8M normalization" , 20, () => force(quote0(ap(fullTree, n22))));
