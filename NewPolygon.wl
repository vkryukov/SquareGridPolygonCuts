(* ::Package:: *)

BeginPackage["NewPolygon`", {"GeneralUtilities`"}];


SetUsage[Polygons,
"Polygons is an association with string keys defining some interesting polygons."]


SetUsage[DrawGrid,
"DrawGrid[{x$1, x$2}, {y$1, y$2}] draws a square grid from {x$1, y$1} to {x$2, y$2]."];


SetUsage[DrawPolygon,
"DrawPolygon[points$] draws a polygon of points$ on a square grid."];


SetUsage[DrawPolygonWithLines,
"DrawPolygon[points$, lines$] draws a polygon of points$ and lines$ on a square grid."];


SetUsage[FindCongruentBisections,
"FindCongruentBisections[points$] returns a list of all bisections of polygon points$ in two congruent parts."]


Begin["`Private`"];


(* ::Subsubsection:: *)
(*Some interesting polygons*)


Polygons = <|
	"MartinGardner" -> {{0,0},{0,1},{1,1},{1,2},{2,2},{2,3},{3,3},{3,4},{4,4},{4,3},{5,3},{5,2},{4,2},{4,1},{3,1},{3,-1},{1,-1},{1,0}},
	"Eriksson" -> {{0,0},{5,0},{5,-2},{6,-2},{6,-7},{5,-7},{4,-7},{4,-4},{0,-4},{0,-2}},
	"Stripe67x5" -> {{0,0},{0,1},{-1,1},{-1,2},{-2,2},{-2,3},{-3,3},{-3,4},{-4,4},{-4,6},{0,6},{0,8},{1,8},{1,7},{2,7},{2,6},{3,6},{3,5},{4,5},{4,4},{5,4},{5,1},{1,1},{1,0}},
	"ThreeCuts" -> {{0,0},{6,0},{6,1},{7,1},{7,2},{8,2},{8,3},{7,3},{7,4},{1,4},{1,3},{2,3},{2,2},{1,2},{1,1},{0,1}},
	"Hexamino1" -> {{0,0},{0,2},{1,2},{1,3},{2,3},{2,2},{3,2},{3,1},{2,1},{2,0}},
	"Hexamino2" -> {{0,0},{0,2},{2,2},{2,3},{3,3},{3,1},{2,1},{2,0}}
|>


(* ::Subsubsection::Closed:: *)
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
				MapIndexed[Text[Style[#2[[1]], 12, Bold], #1, {1,-1} ] &, points],
				{}
			]
		} ],
		DrawGrid [ MinMax @ points[[All, 1]] + { -p, p }, MinMax @ points[[All, 2]] + { -p, p } ]
	]
];


Options[DrawPolygonWithLines] = Options[DrawPolygon];


DrawPolygonWithLines[ points_, lines_, opts: OptionsPattern[]] :=
	Show[
		DrawPolygon[ points, opts ],
		Graphics[{
			Thickness[0.01], 
			MapThread[{#1, Line[#2]}&,
				{Table[ColorData[3][n], {n, Length @ lines}],
				lines}]
		}]
	];


(* ::Subsubsection:: *)
(*Utilities*)


(* ::Text:: *)
(*mod is like Mod, but maps into 1..n instead of 0..n-1.*)


mod[ v_, n_ ] := (Mod[ v - 1, n ] + 1);


cyclicalPairs[ lst_ ] := Partition[ Append[ lst, First @ lst ], 2, 1 ];


(* ::Text:: *)
(*orderedPolygonAngles is similar to PolygonAngles, but guarantees to list them in the order of vertexes.*)
(**)
(*PolygonAngle does NOT give the list of angles in the same order as poly points.*)
(*Example: poly = {{0,0},{5,0},{5,-2},{6,-2},{6,-7},{4,-7},{4,-4},{0,-4}}*)


orderedPolygonAngle[ points_ ] := Module[ { poly = Polygon @ points },
	PolygonAngle[ poly, # ]& /@ points
];


(* ::Text:: *)
(*polygonWithAllPoints adds all grid points to sides whose two angles sum up to 180\[Degree].*)


polygonWithAllPoints[ points_ ] :=  Module[ {
		angles = cyclicalPairs @ orderedPolygonAngle[ points ],
		segments = cyclicalPairs @ points,
		f
	},
	
	f[ { {a1_, a2_}, {p1_, p2_} }, {i_} ] := Module[ { n = Total @ Abs[ p2 - p1 ], dir = Normalize[ p2 - p1 ] },
		If[ a1 == a2 == \[Pi]/2 && n > 1, 
			Table[ { i + j / n, p1 + j * dir}, {j, 1, n - 1} ], 
			Nothing ] 
	];
	
	Last /@ SortBy[ Join[
		MapIndexed[ {First @ #2, #1}&, points ],
		Flatten[MapIndexed[ f, MapThread[List, {angles, segments } ] ], 1]
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


(* ::Subsubsection:: *)
(*SGPolygon*)


(* ::Text:: *)
(*orientedSides takes a polygon (as a list of vertices) and returns a list of pairs of unit vectors: the first element of each pair goes from vertex[[i]] to vertex[[i+1]], and the second element is orthogonal to the first and looks "outside".*)


orientedSides[ points_ ] := Module[{
		n = Length @ points,
		bottomLeft = First @ Sort @ points,
		bottomLeftId, rotate
	},
	
	bottomLeftId = Position[ points, bottomLeft ][[ 1, 1 ]];
	
	(* how rotate the first element to get the second *) 
	rotate = If [ 
		points[[ mod[ bottomLeftId + 1, n ], 1 ]] == bottomLeft[[1]], 
		
		(* bottomLeft to next point is vertical *)
		rotate90left,
		
		(* bottomLeft to the next point is horizontal *)
		rotate90right];
		
	Table[
		With[ {
			next = points[[ If[i == n, 1, i+1] ]] 
			}, 
		{ Normalize[ next - points[[ i ]] ], 
		  rotate @ Normalize[ next - points[[ i ]] ] } ],
		{i, n}]
];


(* ::Text:: *)
(*makeSGPolygon gets a list of points and return a SGPolygon (Square Grid Polygon) data structure.*)


makeSGPolygon[ points_ ] := Module[ { sides = orientedSides[ points ] },
	SGPolygon[<|
		"points" -> points, (* set of points representing the polygon *)
		"n" -> Length @ points, (* number of vertexes *)
		"side" -> sides[[All, 1]], (* normalized vectors going from vertex i to i+1 *)
		"out" -> sides[[All, 2]] (* orthogonal vectors to side at vertex i that goes 'outside' *)
	|>]
];


SGPolygon[p_][s_String] := p[s]


Format[SGPolygon[p_], StandardForm] := 
	Show[ 
		DrawPolygon[ p["points"], "Numbered" -> True ], 
		ImageSize -> 150 
	];


(* ::Subsubsection:: *)
(*SGPolygonPoint*)


(* ::Text:: *)
(*SGPolygonPoint is a data structure that represents a point within a polygon: either a vertex i, a point between vertices i and i+1, or an internal point.*)


SGPolygon[p_][v_Integer] := 
	SGPolygonPoint[<| 
		"polygon" -> SGPolygon[p], 
		"vertex" -> mod[ v, p["n"] ],
		"offset" -> 0 
	|>];


vertexQ[ SGPolygonPoint[ p_ ] ] := ( KeyExistsQ[ p, "offset" ] && p["offset"] == 0 );


sideQ[ SGPolygonPoint[ p_ ] ] := ( KeyExistsQ[ p, "offset" ] && p["offset"] != 0 );


insideQ[ SGPolygonPoint[ p_ ] ] := KeyExistsQ[ p, "inside" ];


SGPolygonPoint[ a_ ][ "coord" ] := 
	If[ insideQ[ SGPolygonPoint[ a ] ],
		a["inside"]
		,
		a["polygon"]["points"][[ a["vertex"] ]] + a["polygon"]["side"][[ a["vertex"] ]] * a["offset"]
	];


SGPolygonPoint[ a_ ][ i_Integer ] := Module[ {a1 = a},
	a1["vertex"] = mod[ a["vertex"] + i, a["polygon"]["n"] ];
	a1["offset"] = 0;
	SGPolygonPoint[a1]
];


SGPolygonPoint[ a_ ]["sideLength", increase_: 1 ] := 
	distance[
		SGPolygonPoint[a]["coord"],
		SGPolygonPoint[a][increase]["coord"]
	];


SGPolygonPoint[ a_ ]["side", increase_: 1 ] := 
	If[ increase == 1, 
		a["polygon"]["side"][[ a["vertex"] ]],
		- a["polygon"]["side"][[ mod[ a["vertex"] - 1, a["polygon"]["n"] ]]]
	];


SGPolygonPoint[ a_ ][ key_String ] := a[key];


Format[ p: SGPolygonPoint[a_], StandardForm ] := Module[ { points = a["polygon"]["points"] },
	Show[
		DrawPolygon[ points, "Numbered" -> True ],
		Graphics[{ Red, PointSize[Large], Point[ p["coord"] ] }],
		ImageSize -> 150 ]
];


distance[ a_, b_ ] := Total @ Abs[ b - a ];


DrawPolygonWithLines[ p: SGPolygon[_], lines: { __:{_SGPolygonPoint __}}, opts: OptionsPattern[]] := Module[{},
	Show[
		DrawPolygon[ p["points"], opts ],
		Graphics[{
			Thickness[0.01], 
			MapThread[{#1, Line[#2]}&,
				{Table[ColorData[3][n], {n, Length @ lines}],
				(#["coord"]&/@#&)/@ lines}]
		}]
	]
];


(* ::Text:: *)
(*testDirection takes a point on the side of a polygon and a direction, and returns 1 if moving in that direction will take us outside of the polygon, 0 if we'll move along the side, and -1 if we'll move inside.*)


testDirection[ p: SGPolygonPoint[ a_ ], dir_ ] := Module[ {
		side = a["polygon"]["side"],
		out = a["polygon"]["out"],
		v = a["vertex"],
		prev = mod[ a["vertex"] - 1, a["polygon"]["n"] ]
	},
	Assert[ Not[ insideQ[ p ] ] ];
	If[ vertexQ[p],
		Which[
			dir == side[[ v ]] || dir == -side[[ prev ]], 0,
			(* it's a 90\[Degree] angle *) out[[ prev ]] == - side[[ v ]] && 
				(dir == out[[ v ]] || dir == out[[ prev ]]) , 1,
			(* it's a 180\[Degree] angle *) out[[ prev ]] == out [[ v ]] && dir == out[[ v ]], 1,
			True, -1 ],
		
		(* sideQ[p] *)
		Switch[ out[[v]],
			dir, 1,
			-dir, -1,
			_, 0 ]
	]
];


(* ::Text:: *)
(*pointOnSide returns True if the point lies on a side.*)


pointOnSide[ { {x1_,y1_}, {x2_,y2_} }, {x_,y_} ] := (
	( x1 == x2 == x && (y1 <= y <= y2 || y2 <= y <= y1) ) ||
	( y1 == y2 == y && (x1 <= x <= x2 || x2 <= x <= x1) )
);


pointOnSide[ { a: SGPolygonPoint[_], b: SGPolygonPoint[_] }, p: SGPolygonPoint[_] ] :=
	pointOnSide[ { a["coord"], b["coord"] }, p["coord"] ];


sideOverlapsSide[ {a_, b_}, {c_, d_} ] :=
	Or[
		pointOnSide[ {a, b}, c ],
		pointOnSide[ {a, b}, d ],
		pointOnSide[ {c, d}, a ],
		pointOnSide[ {c, d}, b ]
	];


(* ::Text:: *)
(*move starts at point p and attempts to move to p + dir * step. For points on the side, it aborts and returns Null if at any point it will have to move outside before going inside. Otherwise, it will stop at either p + dir * step, or at the first intersection with a side when it moves from the inside. It returns a pair (point, boolean: does the move ever moved inside).*)


move[ p: SGPolygonPoint[a_], dir_, step_ ] := Module[ { 
		test = -1,
		side = a["polygon"]["side"],
		points = a["polygon"]["points"],
		a1 = a,
		v = a["vertex"],
		o = a["offset"],
		n = a["polygon"]["n"],
		dist, i, j, p0, p1
	},
	If[ step == 0, Return @ {p, False } ];
	If[ Not[ insideQ[ p ] ], test = testDirection[ p, dir ]];
	If[ test == 1, Return @ Null ]; (* would move outside *)
	If[ test == 0,
		(* move along a side - calculate the distance to the next vertex *)
		Which[ 
			(* moving towards the next vertex *)
			dir == side[[ v ]],
			dist = distance [ points[[ v ]], points[[ mod[ v + 1, n ] ]] ];
			If[ step < dist - o,
				a1["offset"] = o + step; Return @ { SGPolygonPoint[a1], False }
				,
				a1["offset"] = 0; a1["vertex"] = mod[ v + 1, n ];
				Return @ move[ SGPolygonPoint[a1], dir, step - ( dist - o ) ]
			]
			,
			(* moving back towards the current vertex - reducing offset *)
			o > 0,
			If[ step <= o,
				a1["offset"] = o - step; Return @ { SGPolygonPoint[a1], False }
				,
				a1["offset"] = 0; Return @ move[ SGPolygonPoint[a1], dir, step - o ]
			]
			,
			(* moving towards the previous vertex *)
			True,
			dist = distance [ points[[ v ]], points[[ mod[ v - 1, n ] ]] ];
			a1["vertex"] = mod[ v - 1, n ];
			If[ step <= dist,
				a1["offset"] = dist - step;
				Return @ { SGPolygonPoint[a1], False }
				,
				a1["offset"] = 0;
				Return @ move[ SGPolygonPoint[a1], dir, step - dist ]
			]
		]
	];
	
	(* move inside *)
	p0 = p["coord"]; 
	For[ i = 1, i <= step, i++, 
		p1 = p0 + i * dir;
		For[ j = 1, j <= n, j++, 
			If[ pointOnSide[ { points[[ j ]], points[[ If[ j == n, 1, j + 1] ]] }, p1 ],
				a1 = KeyDrop[a, "inside"];
				If[ points[[ If[ j == n, 1, j + 1] ]] == p1, 
					a1["vertex"] = j + 1;
					a1["offset"] = 0
					,
					a1["vertex"] = j;
					a1["offset"] = distance[ points[[j]], p1 ]
				];
				Return @ { SGPolygonPoint[a1], True }
			]
		]
	];
	(* we're still inside *)
	a1 = KeyDrop[a, {"vertex", "offset"} ];
	a1["inside"] = p1;
	{ SGPolygonPoint[a1], True }
];


(* ::Text:: *)
(*getRotation returns a rotation that translates dir1 into dir2. Both are expected to be the vectors of the same length (e.g., unit vectors).*)


getRotation[ dir1_, dir2_ ] :=
	Which [
		dir1 == dir2, Identity,
		rotate90left @ dir1 == dir2, rotate90left, 
		rotate90right @ dir1 == dir2, rotate90right, 
		True, (* rotate 180\[Degree] *) (- # )& 
	];


getMirrorRotation[ dir1_, dir2_ ] := 
	With[ { r = getRotation[ Reverse @ dir1, dir2 ] },
		r[ {#[[2]], #[[1]]} ]&
	];


(* ::Text:: *)
(*followAlong rewritten with move*)


followAlong[ p: SGPolygon[_], a_, b_, increase_ ] := Module[{
		dirA, dirB, rotate,
		curA = p[a], curB = p[b],
		pa, pb, res, next
	}
	,
	dirA = curA["side", increase];
	dirB = curB["side", increase];
	Echo[{dirA,dirB},"dirA,dirB"];
	rotate = Which [
		dirA == dirB, Identity,
		rotate90left @ dirA == dirB, rotate90left, 
		rotate90right @ dirA == dirB, rotate90right, 
		True, (* rotate 180\[Degree] *) (- # )& 
	];
	
	{res, {pa, pb}} = Reap @ Catch [
		Sow[ curA, "a" ];
		Sow[ curB, "b" ];
		Echo[ { curA, curA["vertex"], curB, curB["vertex"] }, "curA (vertex), curB (vertex)"];
		While[ curA["vertex"] != b,
			next = move[ curB, rotate @ curA["side", increase], curA["sideLength", increase] ];
			curA = curA[ increase ];
			Echo[{curA,curB,next},"curA,curB,next"];
			Sow[ curA, "a" ];
			If[ next == Null, Throw[ "outside" ] ];
			curB = next[[1]];
			Sow[ curB, "b" ];
			If[ next[[2]] && Not[ insideQ @ curB ],
				Throw[ "candidate" ]
			]
		]
	];
	
	Switch[res,
		Null,        Missing["pa reached b", {pa, pb}],
		"outside",   Missing["pb went outside", {pa, pb}],
		"candidate", {pa, pb},
		_,           Missing["unknown res", {pa, pb, res}]
	]
];


dirAndStep[ a: SGPolygonPoint[_], b: SGPolygonPoint[_] ] := Module[
	{ a1 = a["coord"], b1 = b["coord"] },
	{ Normalize[ b1 - a1 ], distance[ a1, b1 ] }
];


mirrorFollow[ p: SGPolygon[_], a_, b_, increase_ ] := Module[{
		curA = p[a], curB = p[b], 
		nextA, step, dir, lastAId = 0,
		nextB, bWentInside = False, lastSideBId = 0, lastSideB,
		transform, pa, pb, result
	},
	transform = getMirrorRotation[ curA[ "side", increase ], curB[ "side", -increase ] ];
	pa = CreateDataStructure[ "DynamicArray", { curA } ];
	pb = CreateDataStructure[ "DynamicArray", { curB } ];
	nextA = curA[ increase ];
	result = Catch @ While [ True,
		Echo[{curA,curB},"curA,curB"];
		If[ lastAId == 0,
			nextA = curA[ increase ], (* pa still follows along the side of the polygon *)
			nextA = pb[ "Part", ++lastAId ] (* pa switched to following pb *)
		];
		{ dir, step } = dirAndStep[ curA, nextA ];
		Echo[{dir,step},"dir,step"];
		nextB = move[ curB, transform @ dir, step ];
		If[ nextB === Null, Throw[ "outside" ] ];
		{ nextB, bWentInside } = nextB;
		Echo[{nextB, bWentInside}, "nextB, bWentInside"];
		If[ Not @ bWentInside,
			If[ sideOverlapsSide[ { curA, nextA }, { curB, nextB } ], 
				Throw[ "pb met pa" ]
				,
				pa[ "Append", nextA ]; curA = nextA;
				pb[ "Append", nextB ]; curB = nextB;
				Continue[]
			];
		];
		(*  pb went inside, so we might need to adjust nextA (and therefore nextB): 
			when pa meets pb, from that point on pa should follow pb *)
		Assert[ bWentInside ];
		If[ lastSideBId == 0, 
			lastSideBId = pb["Length"];
			lastSideB = curB
		];
		(* see if we need to adjust nextA, and threfore step and nextB *)
		If[ lastAId == 0 && pointOnSide[ {curA, nextA}, lastSideB ],
			nextA = lastSideB;
			lastAId = lastSideBId;
			{ dir, step } = dirAndStep[ curA, nextA ];
			{ nextB, bWentInside } = move[ curB, transform @ dir, step ]
		];
		pa[ "Append", nextA ];
		pb[ "Append", nextB ];
		If[ Not @ insideQ[ nextB ], Throw[ "candidate" ] ];
		curA = nextA;
		curB = nextB;
		(* just in case - TODO need a better guarantee that the cycle finishes *)
		If[ pb[ "Length" ] > 1000, Throw[ "too many iterations "] ]; 
	];
	Echo[result, "result"];
	If[ result == "candidate", 
		{ Normal @ pa, Normal @ pb },
		Missing[ result, { Normal @ pa, Normal @ pb } ]
	]
];


followCandidates[ poly: SGPolygon[_], follow_ ] := Module[ { params, results },
	params = Select[ Tuples[{ Range[poly["n"]], Range[poly["n"]], {-1, 1} } ], #[[1]] != #[[2]] & ];
	results = {#[[1]], #[[2]], #[[3]], QuietEcho @ follow[poly, #[[1]], #[[2]], #[[3]]]}& /@ params;
	Select[ results, Not [ MissingQ [ #[[4]] ] ] &]
];


followCandidates[ points_ ] := Module[{ midPoly = makeSGPolygon[ polygonWithAllPoints[points] ] },
	Join[
		followCandidates[ midPoly, followAlong],
		followCandidates[ midPoly, mirrorFollow]
	]
];


(* ::Text:: *)
(*compressPath remove all internal points on the sides*)


compressPath[ s_ ] := ( s //. {
	{a___, {b_, c_}, {b_, d_}, {b_, e_}, f___} :> {a, {b,c}, {b,e}, f},
	{a___, {c_, b_},{ d_, b_}, {e_, b_}, f___} :> {a, {c,b}, {e,b}, f}
});


makeLoop[ p_ ] := If[ p[[-1]] == p[[1]], p, Append[p, p[[1]] ] ];


compressLoop[ p_ ] := compressPath @ makeLoop @ p;


(* ::Text:: *)
(*checkCandidate checks whether a candidate is a proper solution. To do that, we continue pb until it reaches a again, getting a new path pbFull, and draw a corresponding path paFull from a. pb is a proper cut iff (a) all of these paths lie on the polygon side + pb (which contains the cut). (b) they collectively contain all the vertices of the polygon. (c) the intersection between paFull and pbFull is only the minimal part of the cut.*)


checkCandidate[ pa_, pb_, mirror_ ] := Module[{
		n = pa[[1]]["polygon"]["n"],
		increase = If[ pa[[1]][1]["vertex"] == pa[[2]]["vertex"], 1, -1 ] * If[ mirror, -1, 1 ],
		bStart = pb[[1]]["vertex"], bLast = pb[[-1]]["vertex"], bLastOnSide = (pb[[-1]]["offset"] != 0),
		dirA = Normalize[ pa[[2]]["coord"] - pa[[1]]["coord"] ], 
		dirB = Normalize[ pb[[2]]["coord"] - pb[[1]]["coord"] ],
		poly = compressPath @ pa[[1]]["polygon"]["points"],
		rotate, transform,
		bNext, bRestIds, 
		fullPb, fullPa, cut, sidePa, sidePb
	},
	rotate = If [ mirror,
		getMirrorRotation[ dirB, dirA],
		getRotation[ dirB, dirA ]
	];
	transform[v_] := Total[Abs[v]] * rotate[Normalize[v]];
	
	(* complete pb till it reaches the starting point *)
	bNext = Which[
		bLastOnSide && increase == 1, mod[ bLast + 1, n ],
		bLastOnSide && increase == -1, bLast,
		True, mod[ bLast + increase, n ]
	];
	bRestIds = mod[ #, n ]& /@ Range[ 
		bNext,
		Which[
			bNext > bStart && increase == 1, bStart + n,
			bNext <= bStart && increase == 1, bStart,
			bNext >= bStart && increase == -1, bStart,
			bNext < bStart && increase == -1, bStart - n
			],
		increase ];
	fullPb = Join[
		#["coord"]& /@ pb,
		pb[[1]]["polygon"]["points"][[ bRestIds ]]
	];
	
	(* make fullPa a transformation of fullPB *)
	fullPa = Accumulate[ {pa[[1]]["coord"], Splice[transform /@ Differences[fullPb]]}];
	
	Echo[fullPa,"fullPa"];
	Echo[fullPb,"fullPb"];
	
	(*  TODO: this just tests for the set of vertices, but not for the edges; 
		so we can theoretically have a false positive but I'm too lazy 
		to write a proper test :) *)
	{poly, fullPa, fullPb} = compressLoop /@ {poly, fullPa, fullPb};
	cut = Complement[ fullPb, poly ];
	sidePa = Complement[ fullPa, cut ];
	sidePb = Complement[ fullPb, cut ];
	
	Echo[{poly, cut, sidePa, sidePb}, "poly, cut, sidePa, sidePb"];
	Union[ sidePa, sidePb ] == Union @ poly && Length[ Intersection[ sidePa, sidePb ] ] <= 2
];


(* ::Text:: *)
(*minimalCut removes the start of the cut that goes along the sides of the polygon. We use the fact that by construction, the end of the cut always lies on the side.*)


SGPolygonPoint[ a_ ][ "changeOffset", o_ ] := Module[ { a1 = a },
	a1["offset"] = o;
	SGPolygonPoint[a1]
];


minimalCut[ p_ ] := Module[ {i, dir, p1, r},
	i = 1;
	While[ i < Length[p] - 1 && Not @ insideQ @ p[[i+1]], i++ ];
	(* { p[[i]], p[[i+1]] } cuts the inside of the polygon *)
	dir = Normalize[ p[[i + 1]]["coord"] - p[[i]]["coord"] ];
	p1 = Which[
		testDirection[ p[[i]], dir ] == -1, p[[i]],
		vertexQ @ p[[i]] && p[[i]]["side"] == dir, p[[i]][1],
		vertexQ @ p[[i]] && p[[i]]["side"] != dir, p[[i]][-1],
		p[[i]]["side"] == dir, p[[i]][1],
		True, p[[i]][0] 
	];
	r = compressPath[ #["coord"]& /@ Prepend[ p[[ i+1;; ]], p1 ] ];
	If[ Order[ r[[1]], r[[-1]] ] == -1, Reverse @ r, r ]
]


FindCongruentBisections[ points_ ] := Module[ { midPoly = makeSGPolygon[ polygonWithAllPoints[points] ] },
	Union[ minimalCut[#[[-1,-1]]]& /@ Join[
		Select[ followCandidates[ midPoly, followAlong], checkCandidate[#[[-1,1]], #[[-1,2]], False]& ],
		Select[ followCandidates[ midPoly, mirrorFollow], checkCandidate[#[[-1,1]], #[[-1,2]], True]& ]
		]
	]
];


(* ::Subsubsection:: *)
(*Epilogue*)


End[];
EndPackage[];
