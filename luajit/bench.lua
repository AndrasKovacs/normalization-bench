
VAR  = 0
LAM  = 1
APP  = 2

VVAR = 0
VLAM = 1
VAPP = 2

function mkVar(x)
   return {tag = VAR, p1 = x}
end

function mkLam(t)
   return {tag = LAM, p1 = t}
end

function mkApp(t, u)
   return {tag = APP, p1 = t, p2 = u}
end

function mkVVar(x)
   return {tag = VVAR, p1 = x}
end

function mkVLam(t)
   return {tag = VLAM, p1 = t}
end

function mkVApp(t, u)
   return {tag = VAPP, p1 = t, p2 = u}
end

function ap(t, u)
   local tag = t.tag
   if tag == VLAM then
      return (t.p1)(u)
   else
      return mkVApp(t, u)
   end
end

function ap2(t, u, v)
   return ap(ap(t, u), v)
end

function quote(l, v)
   local tag = v.tag
   if tag == VVAR then
      return mkVar(l - v.p1 - 1);
   elseif tag == VLAM then
      return mkLam(quote(l+1, v.p1(mkVVar(l))))
   else
      return mkApp(quote(l, v.p1), quote(l, v.p2))
   end
end

function quote0(v)
   return quote(0, v)
end

function force(n)
   return n.tag == LAM
end

function conv(d, l, r)
   local tag = l.tag
   if tag == VVAR then
      return (r.tag == VVAR) and (l.p1 == r.p1)
   elseif tag == VLAM then
      return (r.tag == VLAM) and conv(d+1, l.p1(mkVVar(d)), r.p1(mkVVar(d)))
   else
      return (r.tag == VAPP) and conv(d, l.p1, r.p1) and conv(d, l.p2, r.p2)
   end
end

function conv0(l, r)
   return conv(0, l, r)
end

-- natural numbers
-- ------------------------------------------------------------

n2  = mkVLam (function(s) return mkVLam(function(z)
	    return ap(s, ap(s, z))end)end)
n5  = mkVLam (function(s) return mkVLam(function(z)
	    return ap(s, ap(s, (ap(s, ap(s, ap(s, z))))))end)end)
mul = mkVLam (function(a) return mkVLam(function(b)
	        return mkVLam(function(s) return mkVLam(function(z)
         	    return ap2(a, ap(b, s), z) end)end)end)end)

suc = function(n) return mkVLam(function(s) return mkVLam(function(z)
	    return ap(s, ap2(n, s, z))end)end)end

function natToInt(n)
   local acc = 0
   local m = n.p1.p1
   while (m.tag == APP) do
      acc = acc + 1
      m = m.p2
   end
   return acc
end

n10    = ap2(mul, n2, n5)
n10b   = ap2(mul, n5,   n2)
n20    = ap2(mul, n2,   n10)
n20b   = ap2(mul, n2,   n10b)
n21    = suc(n20)
n21b   = suc(n20b)
n22    = suc(n21)
n22b   = suc(n21b)
n100   = ap2(mul, n10,  n10)
n100b  = ap2(mul, n10b, n10b)
n10k   = ap2(mul, n100, n100)
n10kb  = ap2(mul, n100b, n100b)
n100k  = ap2(mul, n10k, n10)
n100kb = ap2(mul, n10k, n10)
n1M    = ap2(mul, n10k, n100)
n1Mb   = ap2(mul, n10kb, n100b)
n2M    = ap2(mul, n1M,  n2)
n2Mb   = ap2(mul, n1Mb, n2)
n5M    = ap2(mul, n1M,  n5)
n5Mb   = ap2(mul, n1Mb,  n5)
n10M   = ap2(mul, n1M,  n10)
n10Mb  = ap2(mul, n1Mb, n10b)


-- binary trees
--------------------------------------------------------------------------------

leaf = mkVLam(function(l) return mkVLam(function(n) return l end)end)
node = mkVLam(function(t1) return mkVLam(function (t2)
	     return mkVLam(function (l) return mkVLam(function (n) return
			    ap2(n, ap2(t1, l, n), ap2(t2, l, n))end)end)end)end)
fullTree = mkVLam(function(n)
      return ap2(n, mkVLam(function(t) return ap2(node, t, t)end), leaf)end)

function timed(msg, times, act)
   print(msg)
   print("Iterations: " .. times)
   local t0 = os.clock()
   for i=1,times do
      print(tostring(act()) .. " ")
   end
   local t1 = os.clock()
   print("\nAverage time: " .. (t1 - t0) / times)
end

-- Note: the natural number benchmarks all overflow the stack, and I was not
-- able to fix that with ulimit.

--
--------------------------------------------------------------------------------

-- timed("Tree 2M conversion"    , 20, function() return conv0(ap(fullTree, n20), ap(fullTree, n20b))end)
timed("Tree 2M normalization" , 20, function() return force(quote0(ap(fullTree, n20)))end)
-- timed("Tree 4M conversion"    , 20, function() return conv0(ap(fullTree, n21), ap(fullTree, n21b))end)
-- timed("Tree 4M normalization" , 20, function() return force(quote0(ap(fullTree, n21)))end)
-- timed("Tree 8M conversion"    , 20, function() return conv0(ap(fullTree, n22), ap(fullTree, n22b))end)
-- timed("Tree 8M normalization" , 20, function() return force(quote0(ap(fullTree, n22)))end)
