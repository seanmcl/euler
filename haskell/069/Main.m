
AbsoluteTiming[Max[Table[{n / EulerPhi[n], n}, {n, 1, 1000000}]]]

sort[{x1_, n1_}, {x2_, n2_}] := x1 > x2

AbsoluteTiming[First[Sort[Table[{n / EulerPhi[n], n}, {n, 1, 1000000}], sort]]]
