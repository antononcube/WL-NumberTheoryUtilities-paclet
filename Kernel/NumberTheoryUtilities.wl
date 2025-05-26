(* ::Package:: *)
(*
  Number Theory utilities functions.
  Following the Raku package "Math::NumberTheory".
*)
(*Package Header*)


BeginPackage["AntonAntonov`NumberTheoryUtilities`"];


(* ::Text:: *)

SunflowerEmbedding::usage = "Sunflower embedding of integers from 1 to given upper limit.";


Begin["`Private`"];

ClearAll[SunflowerEmbedding];

SunflowerEmbedding::intlist = "Input must be positive integers.";
SunflowerEmbedding::angle = "Angle must be numeric.";

SunflowerEmbedding[n_Integer, withFunc_ : None, angle : (_?NumericQ | Automatic) : Automatic] :=
    SunflowerEmbedding[Range[n], withFunc, angle] /; n > 0;

SunflowerEmbedding[ints_List, withFunc_ : None, angleArg : (_?NumericQ | Automatic) : Automatic] :=
    Module[{ angle = angleArg, points, keys},

      If[!AllTrue[ints, IntegerQ[#] && # > 0 &],
        Message[SunflowerEmbedding::intlist];
        Return[$Failed];
      ];

      If[TrueQ[angle === Automatic],
        angle = 2 * Pi / GoldenRatio^2,
        If[!NumericQ[angle],
          Message[SunflowerEmbedding::angle, "Angle must be numeric."];
          Return[$Failed];
        ]
      ];

      points = Table[
        With[{theta = i * angle, r = Sqrt[i]},
          Association[
            "x" -> r * Cos[theta],
            "y" -> r * Sin[theta],
            If[TrueQ[withFunc === None],
              Sequence @@ {},
              (*ELSE*)
              "group" -> withFunc[i]
            ]
          ]
        ],
        {i, ints}
      ];

      keys = If[TrueQ[withFunc === None],
        {"x", "y"},
        (*ELSE*)
        {"x", "y", "group"}
      ];
      points[[All, keys]]
    ];

End[];
EndPackage[];