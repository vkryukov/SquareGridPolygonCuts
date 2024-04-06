(* ::Package:: *)

BeginPackage["NewPolygon`", {"GeneralUtilities`"}];


SetUsage[DrawGrid,
"DrawGrid[{x$1, x$2}, {y$1, y$2}] draws a square grid from {x$1, y$1} to {x$2, y$2]."];


SetUsage[DrawPolygon,
"DrawPolygon[points$] draws a polygon of points$ on a square grid."]


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Drawing primitives*)


DrawGrid[ {x1_, x2_ }, {y1_, y2_} ]:= With[ {d = 0.2}, 
	Graphics[ {
		Thin, EdgeForm[LightGray],
		Table[ Line[{{x1 - d, y}, {x2 + d, y}}], {y, y1, y2}], 
		Table[ Line[{{x, y1 - d}, {x, y2 + d}}], {x, x1, x2}]
	} ]
];


Options[DrawPolygon] = { "NumberedVertices" -> False }


DrawPolygon[ points_, OptionsPattern[] ] := Module[ {},
	Show[ 
		Graphics[ {
			EdgeForm[Darker @ Gray], FaceForm[ LightBlue ], Polygon @ points,
			If[ OptionValue["NumberedVertices"], 
				MapIndexed[Text[Style[#2[[1]], 12, Bold], #1] &, points],
				{}
			]
		} ],
		DrawGrid [ MinMax @ points[[All, 1]], MinMax @ points[[All, 2]] ]
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


End[];
EndPackage[];
