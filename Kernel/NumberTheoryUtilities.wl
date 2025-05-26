(* ::Package:: *)
(*
  Number Theory utilities functions.
  Following the Raku package "Math::NumberTheory".
*)
(*Package Header*)


BeginPackage["AntonAntonov`NumberTheoryUtilities`"];

SpiralLattice::usage = "Spiral square lattice.";

TriangleMatrixEmbedding::usage = "Gives a triangle within a matrix.";

SunflowerEmbedding::usage = "Sunflower embedding of integers from 1 to given upper limit.";


Begin["`Private`"];

(*===========================================================*)
(* SpiralLattice                                             *)
(*===========================================================*)

ClearAll[SpiralLattice];
SpiralLattice[n_Integer, lastAt_String : "bottom-right"] :=
    Module[{corners, directions, row, col, num, matrix, dir, nextRow, nextCol},

      corners = {"top-left", "top-right", "bottom-right", "bottom-left"};

      If[!MemberQ[corners, lastAt],
        Return["The second argument is expected to be one of the strings: " <> StringJoin[Riffle[corners, " "]] <> "."]];

      {row, col, num, directions} =
          Switch[lastAt,
            "top-left", {0, 0, 1, {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}},
            "bottom-right", {n - 1, n - 1, 1, {{0, -1}, {-1, 0}, {0, 1}, {1, 0}}},
            "top-right", {0, n - 1, 1, {{0, -1}, {1, 0}, {0, 1}, {-1, 0}}},
            "bottom-left", {n - 1, 0, 1, {{0, 1}, {-1, 0}, {0, -1}, {1, 0}}}
          ];

      matrix = ConstantArray[0, {n, n}];
      dir = 1;

      While[num <= n^2,
        matrix[[row + 1, col + 1]] = num++;
        {nextRow, nextCol} = {row, col} + directions[[dir]];
        If[nextRow < 0 || nextRow >= n || nextCol < 0 || nextCol >= n || matrix[[nextRow + 1, nextCol + 1]] != 0,
          dir = Mod[dir, 4] + 1];
        {row, col} = {row, col} + directions[[dir]];
      ];

      n^2 + 1 - matrix
    ];

(*===========================================================*)
(* TriangleMatrixEmbedding                                   *)
(*===========================================================*)

TriangleMatrixEmbedding[k_Integer?Positive, missingValue_ : 0] :=
    Module[
      {ncols = 2 * k - 1, matrix, start = 1, mid, row, col},
      mid = Quotient[ncols, 2];
      matrix = ConstantArray[missingValue, {k, ncols}];
      Do[
        With[{offset = 2 * row, numElements = 2 * row + 1},
          Do[
            matrix[[row + 1, mid - row + col + 1]] = start++;
            , {col, 0, numElements - 1}]
        ],
        {row, 0, k - 1}
      ];

      matrix
    ];


(*===========================================================*)
(* SunflowerEmbedding                                        *)
(*===========================================================*)

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