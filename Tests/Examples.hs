module Tests.Examples where

import Expr

-- variable shorthands
va = v "a"
vb = v "b"
vc = v "c"
vd = v "d"
ve = v "e"
vf = v "f"
vg = v "g"
vh = v "h"
vi = v "i"
vj = v "j"
vk = v "k"
vl = v "l"
vm = v "m"
vn = v "n"
vo = v "o"
vp = v "p"
vq = v "q"
vr = v "r"
vs = v "s"
vt = v "t"
vu = v "u"
vv = v "v"
vw = v "w"
vx = v "x"
vy = v "y"
vz = v "z"

-- Random Expressions
e0 = f "x" $ a vx vy
e1 = f "y" $ a (f "x" vx) (a vx vy)
e2 = a (f "x" $ a vx (f "y" $ a (a vx vy) vz)) (a vx (f "y" vx))
e3 = a vz (f "y" $ a (f "x" vx) (a vx vy))

-- Combinators
m = f "x" $ a vx vx
i = f "x" vx
k = f "x" $ f "y" vx
ki = f "x" $ f "y" vy
c = f "f" $ f "x" $ f "y" $ (a (a vf vy) vx)
b = f "f" $ f "g" $ f "x" $ a vf $ a vg vx
b1 = f "f" $ f "g" $ f "x" $ f "y" $ a vf $ (a (a vg vx) vy)
y = f "f" $ a (f "x" $ a vf (a vx vx)) (f "x" $ a vf (a vx vx)) 

-- Boolean values
ltrue = k
lfalse = ki
land = f "x" $ f "y" $ a (a vx vy) vx
lor = f "x" $ f "y" $ a (a vx vx) vy
lnot = c -- not is just the C combinator !

-- Numbers
zero = f "f" $ f "x" vx
one = f "f" $ f "x" $ a vf vx
two = f "f" $ f "x" $ a vf (a vf vx)
three = f "f" $ f "x" $ a vf (a vf (a vf vx))

num x = f "f" $ f "x" $ aux x
  where aux 0 = vx
        aux y = a vf (aux $ y - 1)

nsucc = f "n" $ f "f" $ f "x" $ a vf (a (a vn vf) vx)
add = f "m" $ f "n" $ a (a vn nsucc) vm
mult = b -- mult is just the B combinator !
iszero = f "n" $ a (a vn (f "x" lfalse)) ltrue

-- Expressions
f0 = a (f "x" vy) (a m m)
f1 = a (a ltrue vx) (a lfalse vy)

norm1 = [ f1
        , a (f "y" vx) (a lfalse vy)
        , vx
        ]

app1 = [ f1
       , a (f "y" vx) (a lfalse vy)
       , a (f "y" vx) (f "z" vz)
       , vx
       ]

-- Code examples
p0 = [ (Assign "and" land)
     , (Assign "true" ltrue)
     , (Assign "false" lfalse)
     , (Evaluate (a (a (macro "and") (macro "true")) (macro "false")))
     ]

p1 = [ (Assign "and" land)
     , (Assign "true" ltrue)
     , (Assign "false" lfalse)
     , (Assign "not" lnot)
     , (Evaluate (a (a (macro "and") (macro "true")) (macro "false")))
     , (Evaluate (a (a (macro "and") (macro "true")) (a (macro "not") (macro "false"))))
     ]

p2 = [ (Assign "and" land)
     , (Assign "a" ltrue)
     , (Assign "b" lfalse)
     , (Evaluate (a (a (macro "and") (macro "a")) (macro "b")))
     , (Assign "b" ltrue)
     , (Evaluate (a (a (macro "and") (macro "a")) (macro "b")))
     ]

p3 = [ (Assign "two" (num 2))
     , (Assign "three" (num 3))
     , (Evaluate (a nsucc (macro "two")))
     , (Evaluate (a (a add (macro "two")) (macro "three")))
     , (Evaluate (a (a mult (macro "two")) (macro "three")))
     ]

-- expected answers
a0 = [lfalse]
a1 = [lfalse, ltrue]
a2 = [lfalse, ltrue]
a3 = [num 3, num 5, num 6]
