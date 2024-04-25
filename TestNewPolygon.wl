(* ::Package:: *)

Needs["NewPolygon`"]


Begin["`NewPolygonTests`"]


(* ::Subsubsection:: *)
(*cyclicalPairs*)


TestCreate[
	NewPolygon`Private`cyclicalPairs @ Range[5],
	{{1,2},{2,3},{3,4},{4,5},{5,1}}
]


(* ::Subsubsection:: *)
(*polygonWithAllPoints*)


TestCreate[
	NewPolygon`Private`orderedPolygonAngle[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}],
	{\[Pi]/2,\[Pi]/2,(3 \[Pi])/2,\[Pi]/2,\[Pi]/2,\[Pi]/2,(3 \[Pi])/2,\[Pi]/2}
]


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
(*orientedSides*)


Module[ {
	crossPolygon = {
	{1,1},{1,2},{1,3},{0,3},{-1,3},{-1,2},{-1,1},
	{-2,1},{-3,1},{-3,0},{-3,-1},{-2,-1},{-1,-1},
	{-1,-2},{-1,-3},{0,-3},{1,-3},{1,-2},{1,-1},
	{2,-1},{3,-1},{3,0},{3,1},{2,1}
	},
	
	fn, f
	},
	
TestCreate[
	NewPolygon`Private`orientedSides[crossPolygon],
	{{{0,1},{1,0}},{{0,1},{1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,1},{1,0}},{{0,1},{1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,1},{1,0}},{{0,1},{1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}}}
];

TestCreate[
	NewPolygon`Private`orientedSides[Reverse@RotateLeft[crossPolygon,7]],
	{{{0,1},{-1,0}},{{0,1},{-1,0}},{{1,0},{0,1}},{{1,0},{0,1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{1,0},{0,1}},{{1,0},{0,1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{0,1},{-1,0}},{{1,0},{0,1}},{{1,0},{0,1}}}
];
		
];


(* ::Subsubsection:: *)
(*getRotation*)


allDirections = {{0, 1}, {0, -1}, {-1, 0}, {1, 0}};
allDirectionPairs = Tuples[ allDirections, {2} ];
testRotation[ {dir1_, dir2_ } ] := Module[ {
		r1 = NewPolygon`Private`getRotation[ dir1, dir2 ],
		r2 = NewPolygon`Private`getRotation[ dir2, dir1 ],
		r3 = NewPolygon`Private`getMirrorRotation[ dir1, dir2 ],
		r4 = NewPolygon`Private`getMirrorRotation[ dir2, dir1 ]
	}
	,
	And @@ {
		r1 @ dir1 == dir2,
		r2 @ dir2 == dir1,
		r3 @ dir1 == dir2,
		r4 @ dir2 == dir1
	}
];
TestCreate[
	AllTrue[ allDirectionPairs, testRotation ],
	True
];


(* ::Subsubsection:: *)
(*move and SGPolygonPoint*)


poly1 = NewPolygon`Private`makeSGPolygon[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}];


moveTest[ poly_, a_, dir_, step_ ] := Module[ { 
		r = QuietEcho @ NewPolygon`Private`move[ poly[a], dir, step ] 
	},
	If[ r === Null, 
		r, 
		Append[
			(r[[1,1]] /@ {"vertex", "offset", "inside" }) /. Missing[___] -> Null , 
			r[[2]] 
		]
	] 
];


TestCreate[
	moveTest[ poly1, 3, {1, 0}, 1 ],
	{ 4, 0, Null, False }
]


TestCreate[
	moveTest[ poly1, 3, {1, 0}, 2 ],
	Null
]


TestCreate[
	moveTest[ poly1, 8, {1, 0}, 3],
	{ 7, 1, Null, False }
]


TestCreate[
	moveTest[ poly1, 8, {1, 0}, 5],
	{ Null, Null, { 5, -4 } , True }
]


TestCreate[
	moveTest[ poly1, 8, {1, 0}, 7],
	{ 4, 2, Null, True }
]


TestCreate[
	moveTest[ poly1, 2, {0, -1}, 5],
	{ Null, Null, { 5, -5 }, True }
]


TestCreate[
	moveTest[ poly1, 2, {0, -1}, 10],
	{ 5, 1, Null, True }
]


poly1mid = NewPolygon`Private`makeSGPolygon[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{5,-7},{4,-7},{4,-4},{0,-4},{0,-2}}];


TestCreate[
	moveTest[ poly1mid, 10, {-1, 0}, 1 ],
	Null
]


(* ::Subsubsection:: *)
(*FindCongruentBisections*)


TestCreate[
	FindCongruentBisections[ Polygons[ "MartinGardner" ] ],
	{{{2,2},{2,1},{3,1}}}	
]


TestCreate[
	FindCongruentBisections[ Polygons[ "Eriksson" ] ],
	{{{2,-4},{2,-2},{5,-2}}}
]


TestCreate[
	FindCongruentBisections[ Polygons[ "Stripe67x5" ] ],
	{{{-1,2},{1,2},{1,6},{2,6}},{{-1,6},{-1,5},{0,5},{0,4},{1,4},{1,3},{2,3},{2,2},{3,2},{3,1}}}
]


TestCreate[
	FindCongruentBisections[ Polygons[ "ThreeCuts"] ],
	{{{2,2},{7,2}},{{3,0},{3,1},{4,1},{4,2},{5,2},{5,3},{4,3},{4,4}},{{3,4},{3,3},{4,3},{4,2},{5,2},{5,1},{4,1},{4,0}}}
]


TestCreate[
	FindCongruentBisections[ Polygons[ "Hexamino1"] ],
	{{{1,2},{1,1},{2,1}}}
]


TestCreate[
	FindCongruentBisections[ Polygons[ "Hexamino2"] ],
	{{{1,2},{1,1},{2,1}}}
]


(* ::Subsubsection:: *)
(*Epilogue*)


End[]
