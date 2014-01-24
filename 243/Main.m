
(* Trial and error *)

target = 15499 / 94744
N[target, 20]
f[n_] := Product[Prime[i], {i, 1, n}]
Resilience[n_] := EulerPhi[n] / (n - 1)
Good[n_] := Resilience[n] < target

Good[f[9]]
Good[f[10]]
Resilience[f[10]]//N

(* Decreasing *)
Table[N[g[f[i]], 20], {i, 2, 100}]


2 3 5 7 11 13 17 19 23 

2 3 5 7 11 13 17 19 23 29

4 3 5 7 11 13 17 19 23 

N[Resilience[2 3 5 7 11 13 17 19 23 ], 20]
Good[2 3 5 7 11 13 17 19 23 ]

N[Resilience[4 3 5 7 11 13 17 19 23 ], 20]

N[Resilience[4 9 5 7 11 13 17 19 23 ], 20]
N[Resilience[4 9 25 7 11 13 17 19 23 ], 20]
Good[4 3 5 7 11 13 17 19 23 ]
Good[4 9 25 7 11 13 17 19 23 ]

Good[8 3 5 7 11 13 17 19 23 ]

Good[2 3 5 7 11 13 17 19 23 ]
N[target, 20]
f[9]

Select[Table[i, {i, f[9], f[9] + 1000000}], lt]

Min[Table[, {i, 2, 10000000}]]
Min[Table[EulerPhi[i] / (i - 1), {i, 2, 10000000}]]

??First


EulerPhi[223099599]

Good[2 3 5 7 11 13 17 19 23 29]
Good[8 3 5 7 11 13 17 19 23]
