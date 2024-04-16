(* ::Package:: *)

BeginPackage["NewPolygon`", {"GeneralUtilities`"}];


SetUsage[DrawGrid,
"DrawGrid[{x$1, x$2}, {y$1, y$2}] draws a square grid from {x$1, y$1} to {x$2, y$2]."];


SetUsage[DrawPolygon,
"DrawPolygon[points$] draws a polygon of points$ on a square grid."];


SetUsage[DrawPolygonWithLines,
"DrawPolygon[points$, lines$] draws a polygon of points$ and lines$ on a square grid."];


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
(*polygonWithMidPoints adds middle points with integer coordinates to those line segments whose two angles sum up to 180\[Degree].*)


polygonWithMidPoints[ points_ ] := Module[ {
		angles = cyclicalPairs @ orderedPolygonAngle[ points ],
		segments = cyclicalPairs @ points,
		f
	},
	
	f[ { {a1_, a2_}, {p1_, p2_} }, {i_} ] := Module[ { mid = (p1 + p2) / 2 },
		If[ a1 == a2 == \[Pi]/2 && IntegerQ[ mid[[1]] ] && IntegerQ [ mid[[2]] ], 
			{i + 0.5, mid }, 
			Nothing ] 
	];
	
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
(*add returns a+b modulo n but starts at 1.*)


add[ n_, a_, b_ ] := ( Mod[ a + b - 1, n ] + 1 );


(* ::Subsubsection::Closed:: *)
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


(* ::Text:: *)
(*followAlongSameDirection takes indexes of two points, a and b, and a direction (clockwise or counter-clockwise), and starting following path Pa along the sides in a given direction from point a, and build a parallel congruent path Pb from point b, until one of the following happens:*)
(**)
(*1) Pb goes outside of the polygon: discard the attempt*)
(*2) Pb goes inside the polygon: wait for it to emerge outside, and receive a candidate.*)
(*3) Pa reached b: discard the attempt. NOTE: we can ignore the case with a rotational symmetry (or mirror symmetry), as we can detect this at the very start before searching for cuts.*)


followAlongSameDirection[ poly_, a_, b_, clockwise_?BooleanQ ] := Module[{
		sides = orientedSides[ poly, clockwise ],
		angles = orderedPolygonAngle[ poly ],
		n = Length @ poly,
		inc, rotate, stepLength,
		state, curA, curB, dir, pointB, step, lenB, nextB,
		res, pa, pb
	},
	
	(*  inc returns the index of the next point along the side in a choosen direction  *) 
	If[ sides[[1, 1]] == Normalize[ poly[[2]] - poly[[1]] ],
		inc[i_] := add[ n, i,  1 ];
		inc[i_, x_] := add [n, i, x ],
		
		inc[i_] := add[ n, i, -1 ];
		inc[i_, x_] := add[ n, i, -x ]
	];
			
	(*  rotate transforms a direction of Pa to a congruent direction for Pb  *)
	rotate = Which [
		sides[[ a, 1 ]] == sides[[ b, 1 ]],
		Identity,
		
		rotate90left @ sides[[ a, 1 ]] == sides[[ b, 1 ]],
		rotate90left, 
		
		rotate90right @ sides[[ a, 1 ]] == sides[[ b, 1 ]],
		rotate90right, 
		
		True, (* rotate 180\[Degree] *)
		(- # )& 
	];
	
	(*  length of the side at point x in move direction  *)
	stepLength[ i_ ] := Total [ Abs [ poly[[ inc @ i ]] - poly[[ i ]] ] ];
	
	
	state = "vertex"; (* state can be one of "vertex", "side", "inside" *)
	curA = a;
	curB = b; (* index of a vertex or side where the end of Pb is; irrelevant once Pb goes inside *)
	pointB = poly[[ b ]];
	{res, {pa, pb}} = Reap @ Catch [
		Sow[ poly[[ a ]], "a" ];
		Sow[ poly[[ b ]], "b" ];
		
		While[ curA != b,
			Sow [ poly[[ inc @ curA ]], "a" ];
			dir = rotate @ sides[[curA, 1]];
			step = stepLength @ curA;
			
			Switch[ state,
				"vertex",
				Which[
					(* we are going outside *)
					angles[[ curB ]] === \[Pi]/2 && (dir == - sides[[curB, 1]] || dir == sides[[curB, 2]]),
					Throw["outside"],
					
					(* we are moving along the side *)
					dir === sides[[curB, 1]],
					lenB = stepLength @ curB;
					Which[
						(* we are in the next vertex *)
						step == lenB,
						curB = inc @ curB;
						pointB = poly[[ curB ]];
						Sow[ pointB, "b" ],
						
						(* we are in the middle of the side *)
						step < lenB,
						state = "side";
						pointB = poly[[ curB ]] + step * dir;
						Sow[ pointB, "b" ],
						
						(* step > lenB; we are crossing another vertex *)
						True,
						curB = inc @ curB;
						If[ angles[[ curB ]] === \[Pi]/2 && (dir == - sides[[curB, 1]] || dir == sides[[curB, 2]]),
							Throw["outside"] ];
						pointB = poly[[ curB ]];
						step = step - lenB;
						state = "inside";
						Sow[ pointB, "b" ]
					],
					
					(* we are steping inside the polygon from the vertex *)
					True,
					state = "inside"
				],
				
				"side",
				Which[
					(* we are steping outside *)
					dir == sides[[curB, 2]],
					Throw["outside"],
					
					(* we are steping inside *)
					True,
					state = "inside"
				]
			];
			
			If[ state == "inside",
				nextB = findFirstIntersection[ poly, { pointB, pointB + dir * step } ];
				If[ nextB === Null,
					(* no intersection - we are still inside *)
					pointB = pointB + dir * step;
					Sow[ pointB, "b" ],
					
					(* found intersection - stop and return, we found a candidate *)
					Sow[ nextB, "b"];
					Throw["candidate"]
				]
			];
			
			curA = inc @ curA
		];
	];
	
	Which[
		res === "candidate",
		{pa, Append[Most@pb,First@Last@pb]},
		
		res === "outside",
		Missing["pb stepped outside", {pa, pb}],
		
		True,
		Missing["pa reached b", {pa, pb}]
	]
];


(* ::Text:: *)
(*findFirstIntersection gets a polygon and two points a and b, such that either a is inside or a is on the side and a->b is going inside, and returns the pair (coordinate of the first intersection of a->b with sides of polygon, sides of the polygon been intersected) or Null if there is no intersection (= b is still inside).*)


findFirstIntersection[ poly_, { a_, b_ }] := Module[{ 
		d = Normalize [ b - a ],
		l = Total @ Abs [ b - a ],
		i = 1,
		sides = cyclicalPairs @ poly,
		pointOnSide 
	},
	
	pointOnSide[ {{a1_, b1_}, {a2_, b2_}}, {x_, y_} ] := (
		(a1 == x == a2 && (b1 <= y <= b2 || b2 <= y <= b1)) ||
		(b1 == y == b2 && (a1 <= x <= a2 || a2 <= x <= a1))
		);
		
	Catch @ Do[
		If [ pointOnSide[ sides[[j]], a + i * d ], Throw[ {a + i * d, j} ] ]
		, {i, l}
		, {j, Length @ sides}
	]
];


(* ::Text:: *)
(*findSameDirectionCandidates find all the cut candidates that go in the same direction.*)


findSameDirectionCandidates[ poly_ ] := Module[ { n = Length @ poly },
	Select [
		Flatten[ Table [
		If[ a == b, 
			Nothing,
			{
				{a, b, True, followAlongSameDirection[ poly, a, b, True ]},
				{a, b, False, followAlongSameDirection[ poly, a, b, False ]}
			}],
		{a, n}, {b, n}]
		, 2 ],
		Not @ MissingQ @ Last[ # ]&
	]
];


(* ::Subsection:: *)
(*Alternative approach*)


(* ::Subsubsection:: *)
(*Utilities*)


(* ::Text:: *)
(*directionTester takes a polygon poly and return a function fn:*)
(**)
(*fn[i, dir, True] take an index of a vertex and a direction and returns 1 if it goes outside of polygon, 0 if it goes along the side, and -1 if it goes inside the polygon.*)
(**)
(*fn[i, dir, False] is similar, but tests for all internal points between vertices i and i+1.*)


directionTester[ poly_ ] := Module[ { 
	sides = orientedSides [ poly ],
	n = Length @ poly,
	fn
	},
	
	fn [ i_, dir_, True ] := Module[ { 
		prev = add[n, i, -1]
		},
		Which[
			(* 90 degree angle at point i *)
			sides[[prev, 2]] == -sides[[i, 1]],
			If[dir == sides[[i, 1]] || dir == -sides[[prev, 1]], 
				0,
				1],
			
			(* 180 degree  angle at point i *)
			sides[[prev, 1]] == sides[[i, 1]],
			Which[
				dir == sides[[i, 2]], 1,	
				dir == -sides[[i, 2]], -1,
				True, 0
			],
			
			(* 270 degree angle at point i *)
			True,
			If[dir == sides[[i, 1]] || dir == -sides[[prev, 1]],
				0,
				-1
			]
		]];
	
	fn [ i_, dir_, False ] := Switch[dir,
		sides[[i, 2]], 1,
		-sides[[i,2]], -1,
		_, 0];
	
	fn
]


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
		points[[ add[ n, bottomLeftId, 1], 1 ]] == bottomLeft[[1]], 
		
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
		"sides" -> sides[[All, 1]], (* normalized vectors going from vertex i to i+1 *)
		"outside" -> sides[[All, 2]] (* orthogonal vectors to side at vertex i that goes 'outside' *)
	|>]
];


SGPolygon[p_][s_String] := p[s]


Format[SGPolygon[p_], StandardForm] := Show[ DrawPolygon[ p["points"], "Numbered" -> True ], ImageSize -> 150 ];


(* ::Subsubsection:: *)
(*followAlongSameDirection*)


(* ::Text:: *)
(*followAlongSameDirection takes indexes of two points, a and b, a dirTester for poly, and an increasing or decreasing order of vertices, and starting following path Pa along the sides in a given direction from point a, building a parallel congruent path Pb from point b, until one of the following happens:*)
(**)
(*1) Pb goes outside of the polygon: discard the attempt*)
(*2) Pb goes inside the polygon: wait for it to emerge on the side, thus getting a candidate.*)
(*3) Pa reached b: discard the attempt. NOTE: we can ignore the case with a rotational symmetry (or mirror symmetry), as we can detect this at the very start before searching for cuts.*)


followAlongSameDirectionNew[ poly_, a_, b_, dirTester_, sides_, increase_ ] := Module[{
		n = Length @ poly,
		(* helper functions *) 
		inc, direction, dirA, dirB, rotate, stepLength,
		
		curA, curB, offset, inside,
		dir, step, test, toNext, nextB, pointB,
		
		res, pa, pb, ri
	},
	
	(* get to the next vertex *) 
	inc[ i_ ] := add[ n, i, increase ];
	
	(* direction of the side from vertex *)
	direction[ i_ ] := If[ increase == 1, sides[[ i, 1 ]], -sides[[ inc @ i, 1 ]] ];

	(*  rotate transforms a direction of Pa to a congruent direction for Pb  *)
	dirA = direction[ a ];
	dirB = direction[ b ];
	Echo[{dirA,dirB},"dirA,dirB"];
	rotate = Which [
		dirA == dirB,
		Identity,
		
		rotate90left @ dirA == dirB,
		rotate90left, 
		
		rotate90right @ dirA == dirB,
		rotate90right, 
		
		True, (* rotate 180\[Degree] *)
		(- # )& 
	];
	
	(*  length of the side from vertex i  *)
	stepLength[ i_ ] := Total [ Abs [ poly[[ inc @ i ]] - poly[[ i ]] ] ];
	
	curA = a; (* current end of pa *)
	curB = b; (* closest vertex to the end of pb *)
	offset = 0; (* offset of the end of pb from curB *)
	pointB = poly[[ b ]];
	inside = False; (* is pb inside the polygon *)
	
	{res, {pa, pb, ri}} = Reap @ Catch [
		Sow[ poly[[ a ]], "a" ];
		Sow[ poly[[ b ]], "b" ];
		Sow[ Null, "i" ];
		
		While[ curA != b,
			Echo[{curA, curB, offset},"(curA, curB, offset)"];
			(* Record the attempt to move. If we discard the attempt, we know at what point  *)
			Sow [ poly[[ inc @ curA ]], "a" ]; 
			(* we try to move x steps in given direction *)
			dir = rotate @ direction [ curA ];
			step = stepLength @ curA;
			Echo[{dir,step},"dir,step"];
			While [ Not[inside] && step > 0,
				Echo[{step,curB,offset}, "step,curB,offset"];
				test = dirTester [ 
					If[ offset == 0 || increase == 1, curB, inc @ curB], 
					dir, 
					offset == 0
				];
				Echo[test, "test"];
				Switch[test,
					-1, (* we're going inside *)
					Break[], 
					
					1, (* abort the attempt *)
					Throw["outside"], 
					
					0, (* move along the side some more *)
					toNext = stepLength @ curB - offset; 
					Echo[toNext, "toNext"];
					Assert[toNext > 0];
					If[toNext > step,
						offset = offset + step; step = 0,
						curB = inc @ curB; offset = 0; step = step - toNext
					];
					Echo[{curB, offset, step},"(curB,offset,step)"];
					pointB = poly[[ curB ]] + dir * offset;
					Sow[ pointB, "b" ],
					
					_, (* that should never happen *)
					Throw[ { "impossible test value", test } ]
				]
			];
			Echo["end of while"];
						
			If[ step > 0, 
				(* we're going inside *)
				inside = True;
				nextB = findFirstIntersection[ poly, { pointB, pointB + dir * step } ];
				Echo[{step, dir, pointB,nextB},"step,dir,pointB,nextB"];
				If[ nextB === Null, 
					(* no intersection - we are still inside *)
					pointB = pointB + dir * step;
					Sow[ pointB, "b" ],
					
					(* found intersection - stop and return, we found a candidate *)
					Sow[ nextB[[1]], "b"];
					Sow[ nextB[[2]], "i"];
					Throw["candidate"]
				]];
				
			(* move to the next vertex in pa *)
			curA = inc @ curA
		];
	];
	
	Switch[res,
		"candidate",
		{pa, pb, If[ri === {Null}, Null, Last @ ri]},
		
		Null,
		Missing["pa reached b", {pa, pb}],
		
		_,
		Missing[res, {pa, pb}]
	]
];


(* ::Text:: *)
(*findSameDirectionCandidates find all the cut candidates that go in the same direction.*)


findSameDirectionCandidatesNew[ originalPoly_ ] := Module[ {
		n, dirTester, sides, poly
	},
	poly = polygonWithMidPoints[ originalPoly ];
	n = Length @ poly;
	dirTester = directionTester[ poly ];
	sides = orientedSides[ poly ];
	QuietEcho @ Select [
		Flatten[ Table [
		If[ a == b, 
			Nothing,
			{
				{a, b, 1, followAlongSameDirectionNew[ poly, a, b, dirTester, sides, 1 ]},
				{a, b, -1, followAlongSameDirectionNew[ poly, a, b, dirTester, sides, -1 ]}
			}],
		{a, n}, {b, n}]
		, 2 ],
		Not @ MissingQ @ Last[ # ]&
	]
];


(* ::Text:: *)
(*compressPath remove all internal points on the sides*)


compressPath[ s_ ] := ( s //. {
	{a___,{b_,c_},{b_,d_},{b_,e_},f___} :> {a, {b,c}, {b,e}, f},
	{a___,{c_,b_},{d_,b_},{e_,b_},f___} :> {a, {c,b}, {e,b}, f}
});


(* ::Text:: *)
(*verifyCandidate takes a polygon and a potential cut candidate and returns a standardize cut if it indeed cuts the polygon into two congruent paths, and Nothing otherwise.*)
(**)
(*A standardize cut is a compressed path (see above) that doesn't have any segments overlapping with the sides of the polygon.*)


(* ::Text:: *)
(**)


(* ::Subsubsection:: *)
(*Epilogue*)


End[];
EndPackage[];
