(* ::Package:: *)

Needs["NewPolygon`"]


(* ::Subsubsection:: *)
(*polygonWithMidPoints*)


TestCreate[
	NewPolygon`Private`cyclicalPairs @ Range[5],
	{{1,2},{2,3},{3,4},{4,5},{5,1}}
]


TestCreate[
	NewPolygon`Private`polygonWithMidPoints[ {{0,0},{1,0},{1,1},{0,1}} ],
	{{0,0},{1/2,0},{1,0},{1,1/2},{1,1},{1/2,1},{0,1},{0,1/2}}
];


TestCreate[
	NewPolygon`Private`polygonWithMidPoints[{{0,0},{6,0},{6,1},{7,1},{7,2},{8,2},{8,3},{7,3},{7,4},{1,4},{1,3},{2,3},{2,2},{1,2},{1,1},{0,1}}],
	{{0,0},{3,0},{6,0},{6,1},{7,1},{7,2},{8,2},{8,5/2},{8,3},{7,3},{7,4},{4,4},{1,4},{1,7/2},{1,3},{2,3},{2,2},{1,2},{1,1},{0,1},{0,1/2}}
];


(* ::Subsubsection:: *)
(*rotate90*)


TestCreate[
	NewPolygon`Private`rotate90left /@ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	RotateLeft[ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}, 1]
];


TestCreate[
	NewPolygon`Private`rotate90right /@ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	RotateRight[ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}, 1]
];


TestCreate[
	(NewPolygon`Private`rotate90left @ NewPolygon`Private`rotate90right @ #)& /@ {
		{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
]


TestCreate[
	(NewPolygon`Private`rotate90right @ NewPolygon`Private`rotate90left @ #)& /@ 
		{{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
]


(* ::Subsubsection:: *)
(*add*)


TestCreate[
	Table[
		NewPolygon`Private`add[ x[[1]], x[[2]], x[[3]]],
		{x, {{18, 5, 10}, {18, 5, 13}, {18, 5, 15}, 
			 {5, 5, -5}, {5, 3, -1}, {10,2,-9}}}
	], 
	{15, 18, 2, 
	 5, 2, 3}
]; 


(* ::Subsubsection:: *)
(*orientedSides*)


TestCreate[
	With[{poly={{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}},
		{NewPolygon`Private`orientedSides[poly,True],
		NewPolygon`Private`orientedSides[poly,False],
		NewPolygon`Private`orientedSides[Reverse@poly,True],
		NewPolygon`Private`orientedSides[Reverse@poly,True],
		NewPolygon`Private`orientedSides[RotateLeft[poly,3],True],
		NewPolygon`Private`orientedSides[RotateRight[poly,2],True]}],
	{
		{{{1,0},{0,1}},{{0,-1},{1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}}},
		{{{0,-1},{-1,0}},{{-1,0},{0,1}},{{0,1},{1,0}},{{-1,0},{0,1}},{{0,1},{1,0}},{{1,0},{0,-1}},{{0,-1},{-1,0}},{{1,0},{0,-1}}},
		{{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,-1},{1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{1,0},{0,1}}},
		{{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,-1},{1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{1,0},{0,1}}},
		{{{0,-1},{1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{1,0},{0,1}}},
		{{{-1,0},{0,-1}},{{0,1},{-1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{1,0},{0,1}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{0,1},{-1,0}}}}
];
