
e1 = Var "alpha"
e2 = Object "x" []

e3 = Object "y" []
e4 = Object "f" [Var "alpha", Var "alpha"]
e5 = Object "f" [Var "gamma", Object "x" []]
e6 = Object "h" [Var "beta", Object "g" [Var "gamma"]]
e7 = Object "h" [Object "y" [], Var "delta"]

p2a = Object "p" [e4,e6]
p2b = Object "p" [e5,e7]
