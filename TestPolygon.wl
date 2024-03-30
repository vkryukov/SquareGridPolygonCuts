(* ::Package:: *)

Needs["Polygon`"]


TestCreate[
	CyclicRange[18, 7, 15]
	,
	{{7,8,9,10,11,12,13,14,15},{7,6,5,4,3,2,1,18,17,16,15}}
];


TestCreate[
	CyclicRangeExcluding[18,7, 15, {3, 16}]
	,
	{{7,8,9,10,11,12,13,14,15}}
	];


TestCreate[
	CyclicRangeExcluding[18, 7, 15, {12, 1}]
	,
	{}];


With[{poly = {{0,0},{1,0},{1,1},{2,1}, {2,2},{6,2},{6,3},{5,3},{5,4},{4,4},{4,5},{-1,5},{-1,4},{-2,4},{-2,2},{-1,2},{-1,1},{0,1}}},

TestCreate[
	PolygonSideDirections[poly, 10]
	,
	{{0,1},{1,0}}
];

TestCreate[
	PolygonSideDirections[poly, 3]
	,
	{{1,0},{0,-1}}
];

TestCreate[
	PolygonSideDirections[poly, 13]
	,
	{{-1,0},{0,1}}
];

];


(* ::Subsubsection:: *)
(*X contains Y*)


TestCreate[
	And @@
		Flatten @ Table[
			SegmentContainsSegment[
			{{0, 0}, {5, 0}},
			{{a, 0}, {b, 0}}
		], {a, 0, 4}, {b, a+1, 5} ]
	,
	True
]


TestCreate[
	Or @@ Flatten @ Table[
		SegmentContainsSegment[
			{{0, 0}, {5, 0}},
			{{a, 0}, {a + 6, 0}}
		], {
	a, -2, 2} ]
	,
	False
]


TestCreate[
	And @@
		Flatten @ Table[
			SegmentContainsSegment[
			{{-1, 0}, {-1, 5}},
			{{-1, a}, {-1, b}}
		], {a, 0, 4}, {b, a+1, 5} ]
	,
	True
]


TestCreate[
	Or @@ Flatten @ Table[
		SegmentContainsSegment[
			{{2, 0}, {2, 5}},
			{{2, a}, {2, a + 6}}
		], {
	a, -2, 2} ]
	,
	False
]


With[{poly = {{0,0},{1,0},{1,1},{2,1}, {2,2},{6,2},{6,3},{5,3},{5,4},{4,4},{4,5},{-1,5},{-1,4},{-2,4},{-2,2},{-1,2},{-1,1},{0,1}}},

TestCreate [
	PolygonSideContainsPath[
		poly
		,
		poly [[ First @ CyclicRangeExcluding[ 18, 2, 9, {8}] ]] 
	]
	,
	True
];

TestCreate [
	PolygonSideContainsPath[
		poly
		,
		CongruentPathFrom[poly[[{15, 14, 13, 12}]], poly[[18]], {1, 0}] 
	]
	,
	False
];

];


TestCreate[
	And @@ Flatten @ Table[
		{
			Polygon`Private`orthogonalSegmentIntersectsSegment[
				{{1, 5}, {1, 10}},
				{{a, b}, {a + 4, b}}
			]
			,
			Polygon`Private`orthogonalSegmentIntersectsSegment[
				{{5, 1}, {10, 1}},
				{{b, a}, {b, a + 4}}
			]
		}
		, {a, -3, 1}, {b, 5, 10}]
	, 
	True
]


TestCreate[
	Or @@ Flatten @ Table[
		{
			Polygon`Private`orthogonalSegmentIntersectsSegment[
				{{1, 5}, {1, 10}},
				{{a, b}, {a + 4, b}}
			]
			,
			Polygon`Private`orthogonalSegmentIntersectsSegment[
				{{5, 1}, {10, 1}},
				{{b, a}, {b, a + 4}}
			]
		}
		, {a, {-4, 2}}, {b, {4, 5, 6}}]
	, 
	False
]


TestCreate[
	Polygon`Private`segmentTouchesSegment[
		{{1,5},{1,10}},
		{{-3,5},{1,5}}
	]
	,
	True
]


(* ::Text:: *)
(*testSegmentFunction allows to test segment functions for been symmetrical regarding axises X and Y.*)


testSegmentFunction[fn_, s1_, s2_, result_] := Module[{},
	TestCreate[fn[s1, s2], result];
	TestCreate[fn[Reverse /@ s1, Reverse /@ s2], result]
]


testSegmentFunction[
	Polygon`Private`orthogonalSegmentIntersectsSegment,
	{{1, 3}, {10, 3}},
	{{5, 3}, {6, 3}},
	False
]


testSegmentFunction[
	Polygon`Private`segmentOverlapsSegment,
	{{1, 3}, {10, 3}},
	{{5, 3}, {6, 3}},
	True
]


testSegmentFunction[
	Polygon`Private`segmentOverlapsSegment,
	{{1, 3}, {5, 3}},
	{{4, 3}, {6, 3}},
	True
]


testSegmentFunction[
	Polygon`Private`segmentOverlapsSegment,
	{{3, 3}, {5, 3}},
	{{2, 3}, {4, 3}},
	True
]


testSegmentFunction[
	Polygon`Private`segmentOverlapsSegment,
	{{3, 3}, {5, 3}},
	{{2, 3}, {6, 3}},
	True
]


With[{poly = {{0,0},{1,0},{1,1},{2,1}, {2,2},{6,2},{6,3},{5,3},{5,4},{4,4},{4,5},{-1,5},{-1,4},{-2,4},{-2,2},{-1,2},{-1,1},{0,1}}},

TestCreate[
	Sort @ Flatten [ Table[{x,y,PointInPolygon[ poly,{x,y} ]} ,{x, -3,7}, {y, -1, 6} ], 1]
	,
	{{-3,-1,"External"},{-3,0,"External"},{-3,1,"External"},{-3,2,"External"},{-3,3,"External"},{-3,4,"External"},{-3,5,"External"},{-3,6,"External"},{-2,-1,"External"},{-2,0,"External"},{-2,1,"External"},{-2,2,"Side"},{-2,3,"Side"},{-2,4,"Side"},{-2,5,"External"},{-2,6,"External"},{-1,-1,"External"},{-1,0,"External"},{-1,1,"Side"},{-1,2,"Side"},{-1,3,"Internal"},{-1,4,"Side"},{-1,5,"Side"},{-1,6,"External"},{0,-1,"External"},{0,0,"Side"},{0,1,"Side"},{0,2,"Internal"},{0,3,"Internal"},{0,4,"Internal"},{0,5,"Side"},{0,6,"External"},{1,-1,"External"},{1,0,"Side"},{1,1,"Side"},{1,2,"Internal"},{1,3,"Internal"},{1,4,"Internal"},{1,5,"Side"},{1,6,"External"},{2,-1,"External"},{2,0,"External"},{2,1,"Side"},{2,2,"Side"},{2,3,"Internal"},{2,4,"Internal"},{2,5,"Side"},{2,6,"External"},{3,-1,"External"},{3,0,"External"},{3,1,"External"},{3,2,"Side"},{3,3,"Internal"},{3,4,"Internal"},{3,5,"Side"},{3,6,"External"},{4,-1,"External"},{4,0,"External"},{4,1,"External"},{4,2,"Side"},{4,3,"Internal"},{4,4,"Side"},{4,5,"Side"},{4,6,"External"},{5,-1,"External"},{5,0,"External"},{5,1,"External"},{5,2,"Side"},{5,3,"Side"},{5,4,"Side"},{5,5,"External"},{5,6,"External"},{6,-1,"External"},{6,0,"External"},{6,1,"External"},{6,2,"Side"},{6,3,"Side"},{6,4,"External"},{6,5,"External"},{6,6,"External"},{7,-1,"External"},{7,0,"External"},{7,1,"External"},{7,2,"External"},{7,3,"External"},{7,4,"External"},{7,5,"External"},{7,6,"External"}}
]

];


With[{poly = {{0,0},{1,0},{1,1},{2,1}, {2,2},{6,2},{6,3},{5,3},{5,4},{4,4},{4,5},{-1,5},{-1,4},{-2,4},{-2,2},{-1,2},{-1,1},{0,1}}},

TestCreate[
	SegmentWithinPolygon[ poly, {{0, 0}, {0, 5}}]
	,
	True
];

TestCreate[
	SegmentWithinPolygon[ poly, {{0, 3}, {0, 6}}]
	,
	False
];

TestCreate[
	SegmentWithinPolygon[ poly, {{0, -1}, {0, 2}}]
	,
	False
];

TestCreate[
	SegmentWithinPolygon[ poly, {{-2, 2}, {6, 2}}]
	,
	True
];

TestCreate[
	SegmentWithinPolygon[ poly, {{-3, 2}, {6, 2}}]
	,
	False
];

TestCreate[
	SegmentWithinPolygon[ poly, {{-2, 2}, {7, 2}}]
	,
	False
];

TestCreate[
	PathWithinPolygon[ poly, poly [[Range @ 18]] ]
	,
	True
];

];


(* ::Subsubsection:: *)
(*FindAllCuts and friends*)


TestCreate[
	Polygon`Private`findSurroundingVertices[{{0,0},{0,4},{1,4},{1,5},{4,5},{4,4},{2,4},{2,3},{1,3},{1,0}},{0,0}]
	,
	{2, 10}
]


With[{poly = {{0,0},{1,0},{1,1},{2,1}, {2,2},{6,2},{6,3},{5,3},{5,4},{4,4},{4,5},{-1,5},{-1,4},{-2,4},{-2,2},{-1,2},{-1,1},{0,1}}},

TestCreate[
	FindCut[poly, poly[[ Range[7, 1, -1]]], First @ PolygonSideDirections[poly, 1]]
	,
	{{1,0},{1,4},{2,4},{2,5}}
];

TestCreate[
	FindCut[poly, poly [[ CyclicRange[18, 5, 1][[2]]]], PolygonSideDirections[poly, 1][[2]]]
	,
	Missing["p2s doesn't have last point on the side",{{0,0},{0,4}}]
];

TestCreate[
	 FindCut[poly,poly[[ {18,1,2,3,4}]],{-1,0}]
	 ,
	 Missing["No cut orthogonal to p1 in direction d",{{{0,1},{0,0},{1,0},{1,1},{2,1}},{-1,0}}]
];

TestCreate[
	FindAllCuts[poly]
	,
	{{{1,0},{1,4},{2,4},{2,5}}}
];

];


(* ::Text:: *)
(*If there are two identical cuts with the only difference is their direction, we need to return only one.*)


TestCreate[
	FindAllCuts[{{0,0},{0,3},{2,3},{2,4},{5,4},{5,3},{3,3},{3,2},{1,2},{1,0}}]
	,
	{{{2,2},{2,4}}}
];


(* ::Text:: *)
(*The following two polygons, stripePolygon[41,7] and stripePolygon[53,7] should not have any valid cuts.*)


TestCreate[
	FindAllCuts[{{0,0},{0,2},{1,2},{1,3},{2,3},{2,5},{4,5},{4,4},{3,4},{3,2},{2,2},{2,1},{1,1},{1,0}}]
	,
	{}
];


TestCreate[
	FindAllCuts[{{0,0},{0,2},{2,2},{2,3},{3,3},{3,4},{5,4},{5,3},{4,3},{4,2},{3,2},{3,1},{1,1},{1,0}}]
	,
	{}
]
