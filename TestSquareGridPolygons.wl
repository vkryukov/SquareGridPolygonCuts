(* ::Package:: *)

Needs["SquareGridPolygons`"]


Begin["TestSquareGridPolygons`"]


(* ::Subsubsection:: *)
(*cyclicalPairs*)


TestCreate[
	SquareGridPolygons`Private`cyclicalPairs @ Range[5],
	{{1,2},{2,3},{3,4},{4,5},{5,1}}
]


(* ::Subsubsection:: *)
(*polygonWithAllPoints*)


TestCreate[
	SquareGridPolygons`Private`orderedPolygonAngle[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}],
	{\[Pi]/2,\[Pi]/2,(3 \[Pi])/2,\[Pi]/2,\[Pi]/2,\[Pi]/2,(3 \[Pi])/2,\[Pi]/2}
]


(* ::Subsubsection:: *)
(*rotate90*)


TestCreate[
	SquareGridPolygons`Private`rotate90left /@ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	RotateLeft[ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}, 1]
];


TestCreate[
	SquareGridPolygons`Private`rotate90right /@ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	RotateRight[ {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}, 1]
];


TestCreate[
	(SquareGridPolygons`Private`rotate90left @ SquareGridPolygons`Private`rotate90right @ #)& /@ {
		{1, 0}, {0, 1}, {-1, 0}, {0, -1}},
	{{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
]


TestCreate[
	(SquareGridPolygons`Private`rotate90right @ SquareGridPolygons`Private`rotate90left @ #)& /@ 
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
	SquareGridPolygons`Private`orientedSides[crossPolygon],
	{{{0,1},{1,0}},{{0,1},{1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,-1},{-1,0}},{{0,-1},{-1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,1},{1,0}},{{0,1},{1,0}},{{1,0},{0,-1}},{{1,0},{0,-1}},{{0,1},{1,0}},{{0,1},{1,0}},{{-1,0},{0,1}},{{-1,0},{0,1}}}
];

TestCreate[
	SquareGridPolygons`Private`orientedSides[Reverse@RotateLeft[crossPolygon,7]],
	{{{0,1},{-1,0}},{{0,1},{-1,0}},{{1,0},{0,1}},{{1,0},{0,1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{1,0},{0,1}},{{1,0},{0,1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,-1},{1,0}},{{0,-1},{1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{0,1},{-1,0}},{{-1,0},{0,-1}},{{-1,0},{0,-1}},{{0,1},{-1,0}},{{0,1},{-1,0}},{{1,0},{0,1}},{{1,0},{0,1}}}
];
		
];


(* ::Subsubsection:: *)
(*getRotation*)


allDirections = {{0, 1}, {0, -1}, {-1, 0}, {1, 0}};
allDirectionPairs = Tuples[ allDirections, {2} ];
testRotation[ {dir1_, dir2_ } ] := Module[ {
		r1 = SquareGridPolygons`Private`getRotation[ dir1, dir2 ],
		r2 = SquareGridPolygons`Private`getRotation[ dir2, dir1 ],
		r3 = SquareGridPolygons`Private`getMirrorRotation[ dir1, dir2 ],
		r4 = SquareGridPolygons`Private`getMirrorRotation[ dir2, dir1 ]
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


poly1 = SquareGridPolygons`Private`makeSGPolygon[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}];


moveTest[ poly_, a_, dir_, step_ ] := Module[ { 
		r = QuietEcho @ SquareGridPolygons`Private`move[ poly[a], dir, step ] 
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


poly1mid = SquareGridPolygons`Private`makeSGPolygon[{{0,0},{5,0},{5,-2},{6,-2},{6,-7},{5,-7},{4,-7},{4,-4},{0,-4},{0,-2}}];


TestCreate[
	moveTest[ poly1mid, 10, {-1, 0}, 1 ],
	Null
]


(* ::Subsubsection:: *)
(*FindCongruentBisections*)


TestCreate[
	FindCongruentBisections[ SGPolygons[ "MartinGardner" ] ],
	{{{2,2},{2,1},{3,1}}}	
]


TestCreate[
	FindCongruentBisections[ SGPolygons[ "Eriksson" ] ],
	{{{2,-4},{2,-2},{5,-2}}}
]


TestCreate[
	FindCongruentBisections[ SGPolygons[ "Stripe67x5" ] ],
	{{{-1,2},{1,2},{1,6},{2,6}},{{-1,6},{-1,5},{0,5},{0,4},{1,4},{1,3},{2,3},{2,2},{3,2},{3,1}}}
]


TestCreate[
	FindCongruentBisections[ SGPolygons[ "ThreeCuts"] ],
	{{{2,2},{7,2}},{{3,0},{3,1},{4,1},{4,2},{5,2},{5,3},{4,3},{4,4}},{{3,4},{3,3},{4,3},{4,2},{5,2},{5,1},{4,1},{4,0}}}
]


TestCreate[
	FindCongruentBisections[ SGPolygons[ "Hexamino1"] ],
	{{{1,2},{1,1},{2,1}}}
]


TestCreate[
	FindCongruentBisections[ SGPolygons[ "Hexamino2"] ],
	{{{1,2},{1,1},{2,1}}}
]


(* ::Subsubsection:: *)
(*Epilogue*)


End[]
