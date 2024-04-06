(* ::Package:: *)

BeginPackage["NewPolygon`", {"GeneralUtilities`"}];


SetUsage[DrawGrid,
"DrawGrid[{x$1, x$2}, {y$1, y$2}] draws a square grid from {x$1, y$1} to {x$2, y$2]."];


SetUsage[DrawPolygon,
"DrawPolygon[points$] draws a polygon of points$ on a square grid."];


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Drawing primitives*)


DrawGrid[ {x1_, x2_ }, {y1_, y2_} ]:= With[ {d = 0.2}, 
	Graphics[ {
		Thin, EdgeForm[LightGray],
		Table[ Line[{{x1 - d, y}, {x2 + d, y}}], {y, y1, y2}], 
		Table[ Line[{{x, y1 - d}, {x, y2 + d}}], {x, x1, x2}]
	} ]
];


Options[DrawPolygon] = { "Numbered" -> False, "Pad" -> 0 };


DrawPolygon[ points_, OptionsPattern[] ] := Module[ { p = OptionValue["Pad"] },
	Show[ 
		Graphics[ {
			EdgeForm[Darker @ Gray], FaceForm[ LightBlue ], Polygon @ points,
			If[ OptionValue["Numbered"], 
				MapIndexed[Text[Style[#2[[1]], 12, Bold], #1] &, points],
				{}
			]
		} ],
		DrawGrid [ MinMax @ points[[All, 1]] + { -p, p }, MinMax @ points[[All, 2]] + { -p, p } ]
	]
];


(* ::Subsubsection:: *)
(*Utilities*)


cyclicalPairs[ lst_ ] := Partition[ Append[ lst, First @ lst ], 2, 1 ];


(* ::Text:: *)
(*polygonWithMidPoints adds middle points to those line segments whose two angles sum up to 180\[Degree].*)


polygonWithMidPoints[ points_ ] := Module[ {
	angles = cyclicalPairs @ PolygonAngle[ Polygon @ points ],
	segments = cyclicalPairs @ points,
	f
	},
	
	f[ { {a1_, a2_}, {p1_, p2_} }, {i_} ] := If[ a1 == a2 == \[Pi]/2, {i + 0.5, (p1 + p2) / 2}, Nothing ];
	
	Last /@ SortBy[ Join[
		MapIndexed[ {First @ #2, #1}&, points ],
		MapIndexed[ f, MapThread[List, {angles, segments } ] ]
	], First ]
];


(* ::Text:: *)
(*rotate90right and rotate90left rotate a unit vector clockwise and counter-clockwise.*)


rotate90left[ {x_, y_} ] := Which[
	x == 1, {0, 1},
	x == -1, {0, -1},
	y == 1, {-1, 0},
	True, {1, 0}
];


rotate90right[ {x_, y_} ] := Which[
	x == 1, {0, -1},
	x == -1, {0, 1},
	y == 1, {1, 0},
	True, {-1, 0}
];


(* ::Text:: *)
(*add  returns a+b modulo n but starts at 1.*)


add[ n_, a_, b_ ] := ( Mod[ a + b - 1, n ] + 1 );


(* ::Subsubsection:: *)
(*Follow along the side*)


(* ::Text:: *)
(*orientedSides returns a list of oriented sides following along the sides of the polygon in clockwise (if clockwise=True) or counter-clockwise order.*)


orientedSides[ points_, clockwise_?BooleanQ ] := Module[{
	n = Length @ points,
	bottomLeft = First @ Sort @ points,
	bottomLeftId, topInc, inc, rotate, res
	},
	
	bottomLeftId = Position[ points, bottomLeft ][[ 1, 1 ]];
	
	(* how to increment indexes to move to the top vertex from here *) 
	topInc = With[ {next = points[[ add[ n, bottomLeftId, 1] ]]},
		If [ next[[1]] == bottomLeft[[1]], 1, -1 ] ];
	
	If [ clockwise,
		(* we need to move to the top vertex from here *)
		inc = topInc;
		rotate = rotate90left,
		
		(* we need to mofe to the right vertex from here *)
		inc = -topInc;
		rotate = rotate90right
	];
	
	res = Table[ 
		With[ { 
			this = points[[ add[ n, bottomLeftId, i * inc ] ]],
			next = points[[ add[ n, bottomLeftId, (i + 1) * inc ] ]]
			},
		{ Normalize[ next - this ], rotate @ Normalize[ next - this] }],
		{i, 0, n-1}];
		
	If[ inc == 1, 
		RotateRight[ res, bottomLeftId - 1 ],
		Reverse @ RotateLeft[ res, bottomLeftId ] ]
];


End[];
EndPackage[];
