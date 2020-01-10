from sympy import *
from sympy.parsing.latex import parse_latex
from mpmath import *
import sys

omega = Symbol(r"\omega")
phi = Symbol(r"\phi")
x, y, z, t = symbols("x y z t")
k, m, n = symbols("k m n", integer=True)
f, g, h = symbols("f g h", cls=Function)
arg = sys.argv[1].replace(r"\left(", "(").replace(r"\right)", ")")
expr = simplify(parse_latex(arg))
init_printing()
res = eval("latex(" + str(expr) + ")")
# res.replace("omega", r"\omega")
print(
    res, end="",
)
