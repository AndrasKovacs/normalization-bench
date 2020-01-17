
// before running:
// export SBT_OPTS="-Xss1000M -Xmx4G"

package normalization_bench

import java.util.concurrent.TimeUnit

abstract class Tm
case class Var(x: Long) extends Tm
case class Ap(t: Tm, u: Tm) extends Tm
case class Lam(t: Tm) extends Tm

abstract class Val
case class VVar(x: Long) extends Val
case class VAp(t: Val, u: Val) extends Val
case class VLam(t: Val => Val) extends Val


object Main extends App {
  def quote(l: Long, v:Val) : Tm = {
    v match {
      case VVar(x)   => Var(l - x - 1)
      case VAp(t, u) => Ap(quote(l, t), quote(l, u))
      case VLam(f)   => Lam(quote(l+1, f(VVar(l))))
    }
  }

  def quote0(v:Val) = quote(0, v)

  def ap(t:Val, u:Val):Val = {
    t match {
      case VLam(f) => f(u)
      case t       => VAp(t, u)
    }
  }

  // natural numbers
  // ------------------------------------------------------------
  def ap2(f: Val, t: Val, u: Val) : Val = ap(ap(f, t), u)

  def n2    = VLam((s:Val) => VLam((z:Val) => ap(s, ap(s, z))))
  def n5    = VLam ((s:Val) => VLam ((z:Val) => ap(s, ap(s, (ap(s, ap(s, ap(s, z))))))))
  def mul   = VLam ((a:Val) => VLam ((b:Val) => VLam((s:Val) => VLam((z:Val) =>
                 ap(ap(a, ap(b, s)), z)))))
  def suc(n:Val) = VLam((s:Val) => VLam((z:Val) => ap(s, ap2(n, s, z))))
  def n10    = ap2(mul, n2,   n5)
  def n10b   = ap2(mul, n5,   n2)
  def n20    = ap2(mul, n2,   n10)
  def n20b   = ap2(mul, n2,   n10b)
  def n21    = suc(n20)
  def n21b   = suc(n20b)
  def n22    = suc(n21)
  def n22b   = suc(n21b)
  def n100   = ap2(mul, n10,  n10)
  def n100b  = ap2(mul, n10b, n10b)
  def n10k   = ap2(mul, n100, n100)
  def n10kb  = ap2(mul, n100b, n100b)
  def n100k  = ap2(mul, n10k, n10)
  def n100kb = ap2(mul, n10k, n10)
  def n1M    = ap2(mul, n10k, n100)
  def n1Mb   = ap2(mul, n10kb, n100b)
  def n2M    = ap2(mul, n1M,  n2)
  def n2Mb   = ap2(mul, n1Mb, n2)
  def n5M    = ap2(mul, n1M,  n5)
  def n5Mb   = ap2(mul, n1Mb,  n5)
  def n10M   = ap2(mul, n1M,  n10)
  def n10Mb  = ap2(mul, n1Mb, n10b)

  def natToLong(t: Tm):Long = {
    t match {
      case Lam(Lam(t)) => {
        var acc:Long = 0
        var u:Tm = t
        def upd() = u match {
          case Ap(_, u2) => {u = u2; true}
          case _ => false
        }
        while (upd()) {
          acc += 1
        }
        acc
      }
      case _ => 1
    }
  }

  // binary trees
  // ------------------------------------------------------------
  def leaf = VLam((l:Val) => VLam((n:Val) => l))
  def node = VLam((t1:Val) => VLam((t2:Val) => VLam((l:Val) => VLam((n:Val) =>
                  ap2(n, ap2(t1, l, n), ap2(t2, l, n))))))
  def fullTree = VLam((n:Val) => ap2(n, VLam((t:Val) => ap2(node, t, t)), leaf))


  // conversion checking
  // ------------------------------------------------------------
  def conv(d:Long, l:Val, r:Val): Boolean = {
    l match {
      case VVar(x) => r match {
        case VVar(y) => x == y
        case r       => false}
      case VAp(l1, l2) => r match {
        case VAp(r1, r2) => conv(d, l1, r1) && conv(d, l2, r2)
        case r           => false}
      case VLam(l) => r match {
        case VLam(r) => conv(d+1, l(VVar(d)), r(VVar(d)))
        case r       => false}
    }
  }

  def conv0(l:Val,r:Val) = conv(0, l, r)

  def force(t:Tm) : Boolean = t match {
    case Lam(_) => true
    case _      => false}

  def Timed[A](msg: String, times: Int, act: () => A) = {
    println(msg)
    println(s"Iterations: ${times.toString()}")
    val t0 = System.currentTimeMillis()
    for (i <- 1 to times) {
      print(act() + " ")
    }
    val t1 = System.currentTimeMillis()
    val avg = ((t1-t0).toDouble / times.toDouble).toString()
    println(s"\nAverage time: $avg ms")
  }

  // Timed("Nat 5M conversion"     , 20, () => conv0(n5M, n5Mb))
  // Timed("Nat 5M normalization"  , 20, () => force(quote0(n5M)))
  // Timed("Nat 10M conversion"    , 20, () => conv0(n10M, n10Mb))
  // Timed("Nat 10M normalization" , 20, () => force(quote0(n10M)))
  Timed("Tree 2M conversion"    , 20, () => conv0(ap(fullTree, n20), ap(fullTree, n20b)))
  Timed("Tree 2M normalization" , 20, () => force(quote0(ap(fullTree, n20))))
  Timed("Tree 4M conversion"    , 20, () => conv0(ap(fullTree, n21), ap(fullTree, n21b)))
  Timed("Tree 4M normalization" , 20, () => force(quote0(ap(fullTree, n21))))
  Timed("Tree 8M conversion"    , 20, () => conv0(ap(fullTree, n22), ap(fullTree, n22b)))
  Timed("Tree 8M normalization" , 20, () => force(quote0(ap(fullTree, n22))))

}
