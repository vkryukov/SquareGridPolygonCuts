(* ::Package:: *)

BeginPackage["Polygon`", {"GeneralUtilities`"}];


(* ::Subsubsection:: *)
(*Overview*)


(* ::Text:: *)
(*This package deals with polygons. It defines some generic functions, as well as functions to work with polygons that has all their vertices and sides lying on a square grid ("square grid polygons").*)


(* ::Text:: *)
(*We represent a polygon as an ordered list of vertices, without repetitions. It is assumed that the last vertex in the list is connected to the first one.*)


SetUsage[PolygonDraw,
"PolygonDraw[polygon$] draws a polygon$ with numbered vertices.
PolygonDraw[polygon$, path$] draw a polygon with numbered vertices and a path.
PolygonDraw[polygon$, path$1, path$2] draws a polygon with numbered vertices and two paths."];


SetUsage[CyclicRange,
"CyclicRange[n$, from$, to$] returns two ranges between from$ and to$ in a cyclical group {1, ..., n$}."];


SetUsage[CyclicRangeExcluding,
"CyclicRangeExcluding[n$, from$, to$, excluding$] returns ranges between from$ and to$ in a cyclical group {1, ..., n$} 
that do not include any elements from excluding$. The result can be an empty list."];


SetUsage[PolygonSideDirections,
"PolygonSideDirections[polygon$, vertex$] returns two unit vectors from a vertex$ that go along the sides of the polygon$."];


SetUsage[CongruentPathFrom,
"CongruentPathFrom[path$, point$, direction$] returns a new path congruent to path$ going from point$ in given direction$."];


SetUsage[SegmentContainsSegment,
"SegmentContainsSegment[segment$1, segment$2] returns True if segment$1 contains segment$2."];


SetUsage[PathContainsSegment,
"PathContainsSegment[path$, segment$] returns True if any of the segments in path$ contain segment$."];


SetUsage[PolygonSideContainsSegment,
"PolygonSideContainsSegment[polygon$, segment$] returns True if segments lies on polygon$ side."];


SetUsage[PolygonSideContainsPath,
"PolygonSideContainsPath[polygon$, path$] returns True if path$ lies on polygon$ side."];


SetUsage[PointInPolygon,
"PointInPolygon[polygon$, point$] returns 'Internal' if the point$ is inside the polygon$,
'Side' if it lies on its side, and 'External' otherwise."];


SetUsage[SegmentWithinPolygon,
"SegmentWithinPolygon[polygon$, segment$] returns True if segment$ is fully contained within the polygon$."];


SetUsage[PathWithinPolygon,
"PathWithinPolygon[polygon$, path$] returns True if path$ is fully contained within the polygon$."];


SetUsage[FindCut,
"FindCut[polygon$, path$, dir$] return a cut that's ortogonal to path$ and starts from  last point in path$
in the direction dir$, or return Missing[] if it is not found"];


SetUsage[FindAllCuts,
"FindAllCuts[polygon$] returns a list of all possible cuts of polygon@ into two congruent sub-polygons"];


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Drawing primitives*)


PolygonDraw[points_] := Module[{x1,y1,x2,y2,d=0.2},
	{x1, x2} = MinMax[ points[[ All, 1 ]] ];
	{y1, y2} = MinMax[ points[[ All, 2 ]] ];
	
	Graphics[{
		EdgeForm[Darker @ Gray]
		, FaceForm[LightBlue]
		, Polygon[points]
		, MapIndexed[Text[Style[#2[[1]], 12, Bold], #1] &, points]
		, Thin
		, EdgeForm[LightGray]
		, Table[ Line[{{x1 - d, y}, {x2 + d, y}}], {y, y1, y2}]
		, Table[ Line[{{x, y1 - d}, {x, y2 + d}}], {x, x1, x2}]		
	}]
]


PolygonDraw[poly_, path_] := Graphics[{
   Splice @ First @ PolygonDraw[poly]
   , Thick
   , Opacity[.7]
   , Red
   , Line@path
   , Opacity[.25]
   , PointSize[.05]
   , Point[ First @ path ]
   }];


PolygonDraw[poly_, p1_, p2_] := Graphics[{
   Splice@First@PolygonDraw[poly, p1]
   , Opacity[.7]
   , Blue
   , Line@p2
   , Opacity[.25]
   , Point [ First @ p2 ]
   }];


(* ::Subsubsection:: *)
(*Cyclic ranges*)


CyclicRange[n_Integer, from_Integer, to_Integer] :=
 If[from < to,
  {Range[from, to]
   , Join[Range[from, 1,-1], Range[n, to, -1]]},
  {Range[from, to, -1]
   , Join[Range[from, n], Range[1, to]]}
  ];


CyclicRangeExcluding[n_Integer, from_Integer, to_Integer, exclude : {__Integer}] :=
 Select[CyclicRange[n, from, to], Intersection[#, exclude] === {} &];


(* ::Subsubsection:: *)
(*Path construction*)


PolygonSideDirections[points_, i_] := Module[{
   n = If[i == Length[points], 1, i + 1]
   , p = If[i == 1, Length[points], i - 1]
   },
  {
   Normalize[points[[n]] - points[[i]]]
   , Normalize[points[[p]] - points[[i]]]
   }];


CongruentPathFrom[path_, p_, dir_] := Module[
  {
   pathDir = Normalize[path[[2]] - path[[1]]],
   diffs = Differences@path,
   shifts
   },
   
   shifts := Which[
    dir == pathDir, diffs,
    dir == -pathDir, -diffs,
    dir == Reverse@pathDir, Reverse /@ diffs,
    dir == -Reverse@pathDir, -Reverse /@ diffs];
  Accumulate[Join[{p}, shifts]]
  ]


(* ::Subsubsection:: *)
(*Testing intersections for paths and polygons on the square grid*)


(* ::Text:: *)
(*Note that all of the tests below assume that paths and polygons are on the square grid: all vertices have integer coordinates, and all path and polygon side segments are either horizontal or vertical. We also assume that all paths and all the sides of the polygon are proper, i.e., two consecutive segments are always orthogonal to each other.*)


assertSegment[ {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}} ] := Assert[x1 == x2 || y1 == y2];


SegmentContainsSegment[
	s1: {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}}
	, s2: {{a1_Integer, b1_Integer}, {a2_Integer, b2_Integer}} 
] := Module[ {min, max},
	
	assertSegment[ s1 ];
	assertSegment[ s2 ];
	
	Which[
		x1 == x2,
		{min, max} = MinMax[{y1, y2}];
		a1 == x1 && a2 == x2 && min <= b1 <= max && min <= b2 <= max,
		
		y1 == y2,
		{min, max} = MinMax[{x1, x2}];
		b1 == y1 && b2 == y2 && min <= a1 <= max && min <= a2 <= max
	]
];


pathSegments[path_] := Partition[ path, 2, 1];


PathContainsSegment[ path_, segment_ ] := AnyTrue[ pathSegments[path], SegmentContainsSegment[ #, segment ]& ];


polygonSegments[polygon_] := Partition[ Append[ polygon, First @ polygon ], 2, 1 ];


PolygonSideContainsSegment[ polygon_, segment_ ] := PathContainsSegment [ Append[ polygon, First@polygon ], segment ];


PolygonSideContainsPath[ polygon_, path_ ] := Module[ { poly = Append[polygon, First @ polygon ] },
	AllTrue [ 
		pathSegments @ path, 
		PathContainsSegment [ poly, # ]&
	]
];


(* ::Text:: *)
(*The below helper function only returns true if two segments are orthogonal and intersect at an "internal" point (i.e., do not "touch").*)


orthogonalSegmentStrictlyIntersectsSegment[
	s1: {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}}
	, s2: {{a1_Integer, b1_Integer}, {a2_Integer, b2_Integer}} 
] := Module[ {minXY, maxXY, minAB, maxAB},
	
	assertSegment[ s1 ];
	assertSegment[ s2 ];
	
	Which[
		x1 == x2,
		{minXY, maxXY} = MinMax[{y1, y2}];
		{minAB, maxAB} = MinMax[{a1, a2}];
		minAB < x1 < maxAB && b1 == b2 && minXY < b1 < maxXY,
		
		y1 == y2,
		{minXY, maxXY} = MinMax[{x1, x2}];
		{minAB, maxAB} = MinMax[{b1, b2}];
		minAB < y1 < maxAB && a1 == a2 && minXY < a1 < maxXY
	]
];


segmentContainsPoint[
	s1: {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}}
	, {a1_Integer, b1_Integer}
] := Module[ { min, max },

	assertSegment[ s1 ];
	
	Which[
		x1 == x2,
		{min, max} = MinMax[{y1, y2}];
		a1 == x1 && min <= b1 <= max,
		
		y1 == y2,
		{min, max} = MinMax[{x1, x2}];
		b1 == y1 && min <= a1 <= max
	]
]


segmentOverlapsSegment[
	s1: {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}}
	, s2: {{a1_Integer, b1_Integer}, {a2_Integer, b2_Integer}}
] := Module[ {minXY, maxXY, minAB, maxAB},

	assertSegment[ s1 ];
	assertSegment[ s2 ];

	Which[
		x1 == x2,
		{minXY, maxXY} = MinMax[{y1, y2}];
		{minAB, maxAB} = MinMax[{b1, b2}];
		a1 == a2 == x1 && (
			minXY < minAB < maxXY ||
			minXY < maxAB < maxXY ||
			minAB <= minXY <= maxXY <= maxAB
		),
		
		y1 == y2,
		{minXY, maxXY} = MinMax[{x1, x2}];
		{minAB, maxAB} = MinMax[{a1, a2}];
		b1 == b2 == y1 && (
			minXY < minAB < maxXY ||
			minXY < maxAB < maxXY ||
			minAB <= minXY <= maxXY <= maxAB
		)
	]
]


segmentTouchesSegment[
	s1: {{x1_Integer, y1_Integer}, {x2_Integer, y2_Integer}}
	, s2: {{a1_Integer, b1_Integer}, {a2_Integer, b2_Integer}} 
] := Module[ {},

	assertSegment[ s1 ];
	assertSegment[ s2 ];
	
	Or @@ {
		segmentContainsPoint[s1, {a1, b1}],
		segmentContainsPoint[s1, {a2, b2}],
		segmentContainsPoint[s2, {x1, y1}],
		segmentContainsPoint[s2, {x2, y2}]
	} && Not @ segmentOverlapsSegment[ s1, s2 ]
];


orthogonalSegmentIntersectsSegment[ s1_, s2_ ] := (
	orthogonalSegmentStrictlyIntersectsSegment[ s1, s2 ] ||
	segmentTouchesSegment[ s1, s2 ]
)


(* ::Text:: *)
(*Now we have all the ingredients to test whether a point is strictly within a polygon.*)


PointInPolygon[polygon_, point: {x_, y_}] := Module[{
		segments = polygonSegments @ polygon
		, min = Min[polygon[[All, 1]]]
		, max = Max[polygon[[All, 1]]]
		, segment
		, intersects
		, touchedSegments
		, overUnder
		, touchingIntersects
	},
	
	If[ AnyTrue[ segments, segmentContainsPoint[#, point]& ], Return[ "Side" ]];
	
	(* the point is either internal or external *)
	If [ x <= min || x >= max, Return ["External"] ];
	segment = { point, { x + (max - min) + 1, y } };
	
	intersects = Total [ Boole /@ (orthogonalSegmentStrictlyIntersectsSegment[ segment, # ]& /@ segments )];
	
	touchedSegments = Select[ segments, segmentTouchesSegment[ segment, # ]& ];
	overUnder = If[ Max[ #[[All, 2]] ] > y, 1, 0 ]& /@ touchedSegments;
	touchingIntersects = Total [ Partition[overUnder, 2] /. {{0, 0} -> 0, {1, 1} -> 0, {0, 1} -> 1, {1, 0} -> 1} ];
	

	If[ EvenQ[ intersects + touchingIntersects ], "External", "Internal" ]
]


SegmentWithinPolygon[polygon_, s: {{x1_, y1_}, {x2_, y2_}}] := Module[ {},
	assertSegment[s];
	
	Which[
		x1 == x2,
		AllTrue[ Table[ {x1, y}, {y, y1, y2, Sign[ y2 - y1 ] } ]
			, PointInPolygon[ polygon, # ] != "External" &
		],
		
		y1 == y2,
		AllTrue[ Table[ {x, y1}, {x, x1, x2, Sign[ x2 - x1 ] } ]
			, PointInPolygon[ polygon, # ] != "External" &
		]
	]
];


PathWithinPolygon[ polygon_, path_ ] := AllTrue [ pathSegments @ path, SegmentWithinPolygon [ polygon, # ]& ]


(* ::Subsubsection:: *)
(*Helper utilities for cutting algorithm*)


polygonSideContainsPoint[ poly_, point_ ] := AnyTrue[ 
	polygonSegments @ poly, 
	segmentContainsPoint [ #, point ]& 
];


findSurroundingVertices[ poly_, point_ ] := Module[{
	c1 = Position [ poly, point ], 
	c2, 
	polySegments = polygonSegments @ poly
	},
	
	If [ c1 === {},
		(* p2s lies on the side - let's find this side *)
		Do[
			If [ segmentContainsPoint[ polySegments[[ i ]], point ],
				c1 = i;
				c2 = Mod[ i, Length @ polySegments ] + 1;
				Return[]
			]
			,
			{ i, Length @ polySegments }]
		,
		
		(* p2s is in the vertex of poly *)
		c1 = c1[[ 1, 1 ]];
		Which [
			c1 == Length @ poly, 
			c1 = c1 - 1; c2 = 1
			,
			
			c1 == 1,
			c1 = Length@poly; c2 = 2
			,
			
			True,
			c2 = c1 + 1; c1 = c1 - 1 
		]
	];
	Sort[{c1, c2}]
]


(* ::Text:: *)
(*findFirstTouchPoint find a shortest sub-path of p1 that touches p2 but doesn't intersect p2, and returns it,  or returns {} if it doesn't exist. In a case of a symmetrical polygon, when p1 and p2 touch at the start and the end points, it returns p1.*)


findFirstTouchPoint[ p1_, p2_ ] := Module[ { s1 = pathSegments @ p1, s2 = pathSegments @ p2 },
	Catch @ Do[
		If[ segmentTouchesSegment[ s1[[i]], s2[[j]] ] && (
			 i != 1 || Not @ segmentContainsPoint[ s2[[j]], s1[[i, 1]] ]
			), Throw @ p1[[ ;; i+1 ]] ];
		If[ orthogonalSegmentStrictlyIntersectsSegment[ s1[[i]], s2[[j]] ], Throw @ {} ];
		If[ segmentOverlapsSegment[ s1[[i]], s2[[j]] ], Throw @ {} ];
	, {i, 1, Length @ s1}, {j, 1, Length @ s2}]
];


(* ::Subsubsection:: *)
(*Cutting algorithms*)


(* ::Text:: *)
(*Here is the algorithm that finds a possible cut. Start with two points A and B on the sides of the polygon, a path P1 between them, and a possible direction D along the sides of the polygon at point B.*)
(**)
(*1. Define P2 as a path that's congruent to P1 but starts at point B and goes in the direction D.*)
(*2. Find P1s, which is a sub path of P1 starting in A that "touches" path P2. If P1 and P2 only intersect in point B, this is P1; this path might not exist if some of the segments of P1 properly intersects P2 (i.e., not in one of the vertices).*)
(*3. If such path doesn't exist, stop: there is not cut that is congruent to (some part of) P1.*)
(*4. If it does exist, let P2s be the corresponding congruent sub-path of P2 starting in B.*)
(*5a. If P1s == P1 and therefore P2 == P2s (the first touch happens in point B), that means that the polygon is symmetrical. We find a cut or not depending on whether its center point lies on the square grid.*)
(*5b. Otherwise, if either (a) P2s's end point is not on the side, (b) P2s goes outside of the polygon, (c) P2s goes along the sides of the polygon (e.g., doesn't go "inside" the polygon), stop:  there is not cut that is congruent to (some part of) P1.*)
(*6. Otherwise, P2s is a valid cut. We now have to check whether it divided the polygon into two equal parts.*)
(*7. For this, we need to extend P2s by adding a path from the end of P2s that goes on the side of the polygon and goes back into B, but does NOT go into A. (There is only one such path). Let's call this path P2f.*)
(*8. Build P1f: a path that starts in A, goes in the same direction as P1, and is congruent to P2f. If this path completely lies within the sides of the polygon plus P2s, and P1f with P2f contain all the vertices of the polygon, stop: P2s is the answer. Otherwise, stop:  there is not cut that is congruent to (some part of) P1.*)


FindCut[ poly_, p1_, d_ ] := Module [{ 
	a = First @ p1, 
	b = Last @ p1, 
	d2 = Normalize[ p1[[2]] - p1[[1]] ],
	p2, p1s, p2s, lastPoint, polySegments, centroid,
	c1, c2, r1, r2, ai, bi,
	p1f, p2f
	},

	(* Steps 1-4 *)
	p2 = CongruentPathFrom[ p1, b, d];
	p1s = findFirstTouchPoint[ p1, p2 ];
	
	If [ p1s == {}, Return @ Missing[ "p1 doesn't touch p2", {p1, p2} ] ];
	p2s = CongruentPathFrom[ p1s, b, d];
	
	(* Step 5 *)
	lastPoint = Last @ p2s;
	
	(* Step 5a: check for a symmetrical polygon *)
	If [ p1s == p1 && lastPoint == a && PolygonSideContainsPath[ poly, p2s ],
		centroid = Mean[ poly ];
		Echo[ centroid, "centroid" ];
		Return @ Which[
			PointInPolygon[ poly, centroid ] != "Internal",
			Missing[ "centroid is not internal", centroid ],
			
			IntegerQ[ centroid[[1]] ],
			Module[ { y1 = Floor[ centroid[[2]] ], y2 = Ceiling [ centroid[[2]] ] },
				While [ PointInPolygon[ { centroid[[1]], y1 } ] == "Internal", y1-- ];
				While [ PointInPolygon[ { centroid[[1]], y2 } ] == "Internal", y2++ ];
				{ { centroid[[1]], y1 }, { centroid[[1]], y2 } }
			],
			
			IntegerQ[ centroid[[2]] ],
			Module[ { x1 = Floor[ centroid[[1]] ], x2 = Ceiling [ centroid[[1]] ] },
				While [ PointInPolygon[ { x1, centroid[[2]] } ] == "Internal", x1-- ];
				While [ PointInPolygon[ { x2, centroid[[2]] } ] == "Internal", x2++ ];
				{ { x1, centroid[[2]] }, { x2, centroid[[2]] } }
			],		
			
			True,
			Missing[ "centroid is not on a square grid", centroid ]
		]
	]; 
	
	(* Step 5b *)
	If [ Not @ polygonSideContainsPoint[ poly, lastPoint ], 
		Return @ Missing["p2s doesn't have last point on the side", p2s ] ];
	If [ PolygonSideContainsPath[ poly, p2s ], 
		Return @  Missing["p2s lies on the side", p2s ] ];
	If [ Not @ AllTrue [ pathSegments @ p2s, SegmentWithinPolygon [ poly, # ]& ],
		Return @ Missing["p2s goes outside of polygon", p2s ] ];
		
	(* Steps 6-7 *)
	{c1, c2} = findSurroundingVertices[ poly, lastPoint ];
	
	ai = Position[ poly, a ][[ 1, 1 ]];
	bi = Position[ poly, b ][[ 1, 1 ]];
	r1 = CyclicRangeExcluding[ Length @ poly, c1, bi, {c2, ai}];
	r2 = CyclicRangeExcluding[ Length @ poly, c2, bi, {c1, ai}];
	r1 = First @ Join[r1, r2]; (* one of these ranges should be empty *)
	  
	(* Step 8 *)
	p2f = Join[ p2s, poly[[ r1 ]] ];
	p1f = CongruentPathFrom[ p2f, a, d2 ];
	
	If[ Complement[ poly, p1f, p2f ] =!= {}, Return[ Missing["No cut orthogonal to p1 in direction d", {p1, d} ] ] ];
	
	If[ AllTrue[
		pathSegments @ p1f,
		( PolygonSideContainsSegment[ poly, # ] || PathContainsSegment [ p2s, # ]) &],
		
		(* normalize p2s by removing the head that lies on the side of polygon *)
		While[ PolygonSideContainsSegment[ poly, p2s[[ ;; 2]] ], p2s = Rest @ p2s ];
		With[{ f = First @ p2s, l = Last @ p2s },
			If [ f[[1]] > l[[1]] || (f[[1]] == l[[1]] && f[[2]] > l[[2]]),
				p2s = Reverse @ p2s]
		];
		Echo[{p1, d}, "Findcut: found solution"];
		p2s
		,
		Missing["No cut orthogonal to p1 in direction d", {p1, d}]
	]
];


FindAllCuts[ poly_ ] := Module[{ 
		n = Length @ poly, 
		halfN = Ceiling[ Length @ poly / 2 ] 
	},
	
	Union @ DeleteMissing @ Flatten[
		Table[ 
			If [ i == j || Abs[i - j] > halfN, 
				Missing[],
				FindCut[ poly, poly[[ p ]], d ]]
			,
			{i, 1, n},
			{j, 1, n},
			{p, CyclicRange[n, i, j]},
			{d, PolygonSideDirections[poly, j]}]
		,
		3
	]	
];


(* ::Subsubsection:: *)
(*Epilogue*)


End[];
EndPackage[];
