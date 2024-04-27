(* ::Package:: *)

(* ::Text:: *)
(*Get["/Users/victor/Documents/Projects/Markelov Birthday Present 2024/Stripes.wl"]*)


ClearAll[StripePolygon];
StripePolygon[n_Integer, k_Integer] := Module[{
    moves = IntegerDigits[n, 2] /. {0 -> {1, 0}, 1 -> {0, 1}}, 
    compress
    },
   compress[moves_] := (moves //. {
       {a : ___, {0, b_}, {0, c_}, d : ___} :> {a, {0, b + c}, d},
       {a : ___, {b_, 0}, {c_, 0}, d : ___} :> {a, {b + c, 0}, d}
       });
   {{0, 0},
    Splice @ Accumulate @ compress@{
        {0, 1},
        Splice@Flatten[Table[{{-1, 0}, {0, 1}}, k - 1], 1],
        Splice @ moves,
        Splice@Flatten[Table[{{1, 0}, {0, -1}}, k], 1],
        Splice[-Reverse[moves]]
        }}
   ];
StripePolygon[n_Integer] := StripePolygon[n, 1]


congruentStripes[base_, n_] := Module[{b = IntegerDigits[n, 2]},
  Sort@Select[{n, FromDigits[Reverse @ b, 2], FromDigits[Reverse[b /. {0 -> 1, 1 -> 0}], 2]}, # >= base &]]


stripePairs[n_] := GatherBy[
   Select[Range[2^(n - 2), 2^(n - 1) - 1], With[{b = IntegerDigits[#, 2]}, b != Reverse @ b] &], (* select non palindromes *)
   congruentStripes[2^(n - 2), #] &];


uniqueStripes[n_] := First /@ stripePairs[n];


Get["/Users/victor/Documents/packages/Cache/Cache.wl"];
Needs["Cache`"];


Get["/Users/victor/Documents/Projects/Markelov Birthday Present 2024/SquareGridPolygons.wl"];
Needs["SquareGridPolygons`"];


FindCuts[p_] := Cached[FindCongruentBisections][p];


minimalCopiesWithCut[n_] := Module[{k = 1, cuts = FindCuts[StripePolygon[n]]},
    While[cuts == {},
      k += 2;
      cuts = FindCuts[StripePolygon[n, k]]
      ];
    k];


LaunchKernels[];
Print[StringForm["Kernel count = ``", $KernelCount]];


DistributeDefinitions[minimalCopiesWithCut, uniqueStripes, stripePairs, congruentStripes, StripePolygon];
DistributeDefinitions["Cache`"];
DistributeDefinitions["SquareGridPolygons`"];


ParallelEvaluate[SetOptions[Cached, "CacheDir" -> "/Users/victor/Documents/Projects/Markelov Birthday Present 2024/"]];


calculate[ n_ ] := Print[AbsoluteTiming[Counts[Quiet @ ParallelMap[minimalCopiesWithCut, uniqueStripes[ n ], ProgressReporting-> False]]]];


calculate[ 6 ];
calculate[ 8 ];
calculate[ 10 ];
calculate[ 12 ];
calculate[ 14 ];
