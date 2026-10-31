(* ::Package:: *)

ClearAll[
	sectorLookup, sectorRemoveLabels, sectorDropLabels,
	sectorEdgeDropLabels, sectorCoverEdges,
	sectorToIndexSector, sectorLabelText, sectorDropLabelText, sectorVertexLabel,
	sectorFilterEmptyNodes
];

sectorAssociation[keys_,values_]:=Association[MapThread[Rule,{keys,values}]];
sectorLookup[assoc_,key_,default_:{}]:=Lookup[assoc,Key[key],default];
sectorRemoveLabels[labels_,remove_]:=Select[labels,!MemberQ[remove,#]&];

sectorDropLabels[fullCountings_,refineIndeterminates_:False]:=Module[
	{
		sectors,singularityCount,dropData,labels,zeroIndeterminateLabels
	},
	
	sectors = fullCountings // Last // Last;
	singularityCount = Length[fullCountings]-1;
	dropData = findCutsIndex[fullCountings,#]["data"]& /@ Range[singularityCount];
	labels = sectorAssociation[
		sectors,
		Table[
			Select[
				Range[singularityCount],
				MemberQ[dropData[[#,1]],sector] || MemberQ[dropData[[#,2]],sector]&
			]
			,
			{sector,sectors}
		]
	];
	If[!TrueQ[refineIndeterminates],Return[labels]];
	
	zeroIndeterminateLabels = sectorZeroIndeterminateLabels[fullCountings];
	Return[sectorAssociation[sectors,sectorRemoveLabels[sectorLookup[labels,#],sectorLookup[zeroIndeterminateLabels,#]]& /@ sectors]];
];


sectorZeroIndeterminateLabels[fullCountings_]:=Module[
	{
		genericCounts,sectors,singularityCount
	},
	
	genericCounts = fullCountings[[-1,2]];
	sectors = fullCountings[[-1,3]];
	singularityCount = Length[fullCountings]-1;
	
	Return[
		sectorAssociation[
			sectors,
			Table[
				Select[
					Range[singularityCount],
					genericCounts[[sectorIndex]]===0 && fullCountings[[#,2,sectorIndex]]===Indeterminate&
				]
			,
				{sectorIndex,Length[sectors]}
			]
		]
	];
];


sectorEdgeDropLabels[edges_,zeroIndeterminateLabels_]:=sectorAssociation[
	edges,
	sectorLookup[zeroIndeterminateLabels,First[List@@#]]& /@ edges
];


sectorCoverEdges[sectors_]:=DirectedEdge@@@Select[
	Tuples[sectors,2],
	Length[#[[2]]]===Length[#[[1]]]+1 && SubsetQ[#[[2]],#[[1]]]&
];


sectorToIndexSector[variables_][sector_]:=Sort[sector /. Thread[variables->Range[Length[variables]]]];


sectorLabelText[sector_]:=ToString[sector,InputForm];
sectorDropLabelText[{}]:="";
sectorDropLabelText[labels_]:=ToString[labels,InputForm];


(* Labels are VertexLabels rather than part of a VertexShapeFunction: Mathematica 15 clips edges to the full extent of a custom vertex shape, so arrows would stop at the label text instead of the node. *)
sectorVertexLabel[sectorLabels_,dropLabels_][vertex_]:=Module[
	{
		sectorText,dropText,sectorLabel
	},
	
	sectorText = sectorLabelText[sectorLookup[sectorLabels,vertex,vertex]];
	dropText = sectorDropLabelText[sectorLookup[dropLabels,vertex]];
	sectorLabel = Pane[Style[sectorText,10,GrayLevel[0.15]],FrameMargins->{{0,0},{7,0}}];
	
	Return[
		If[dropText==="",
			Placed[sectorLabel,Above],
			Placed[
				{sectorLabel,Pane[Style[dropText,10,Bold,RGBColor[0.55,0.1,0.08]],FrameMargins->{{0,0},{0,7}}]},
				{Above,Below}
			]
		]
	];
];


sectorFilterEmptyNodes[vertices_,sectors_,labels_,edges_,edgeLabels_,keepLabels_]:=Module[
	{
		labelledVertices,keepVertices,keepEdges,vertexSectors
	},
	
	labelledVertices = Select[vertices,sectorLookup[keepLabels,#] =!= {}&];
	keepVertices = Select[vertices,Function[vertex,AnyTrue[labelledVertices,SubsetQ[vertex,#]&]]];
	keepEdges = Select[edges,SubsetQ[keepVertices,List@@#]&];
	vertexSectors = sectorAssociation[vertices,sectors];
	
	Return[
		<|
			"Vertices"->keepVertices,
			"Sectors"->(sectorLookup[vertexSectors,#]& /@ keepVertices),
			"Labels"->sectorAssociation[keepVertices,sectorLookup[labels,#]& /@ keepVertices],
			"Edges"->keepEdges,
			"EdgeLabels"->sectorAssociation[keepEdges,sectorLookup[edgeLabels,#]& /@ keepEdges]
		|>
	];
];


Options[SectorDropGraphData] = Join[{"RefineIndeterminates"->False,"DeleteEmptyNodes"->True},Options[buildEulerChiData]];
SectorDropGraphData[gpol_,variables_,singList_,opts:OptionsPattern[]]:=Module[
	{
		fullCountings,sectors,labels,keepLabels,zeroIndeterminateLabels,edgeLabels,edges,
		toIndexSector,vertices,indexLabels,indexKeepLabels,indexEdges,indexEdgeLabels,filteredData
	},
	
	If[!constraintOptionFreeQ[SectorDropGraphData,{opts}],Return[$Failed]];
	If[!MemberQ[{True,False},OptionValue["RefineIndeterminates"]],
		Print["Error: \"RefineIndeterminates\" must be True or False"];
		Return[$Failed];
	];
	If[!MemberQ[{True,False},OptionValue["DeleteEmptyNodes"]],
		Print["Error: \"DeleteEmptyNodes\" must be True or False"];
		Return[$Failed];
	];
	
	fullCountings = buildEulerChiData[gpol,variables,singList,Sequence@@FilterRules[{opts},Options[buildEulerChiData]]];
	If[fullCountings===$Failed,Return[$Failed]];
	If[!singularitiesComputedQ[fullCountings,Range[Length[singList]]],Return[$Failed]];
	
	sectors = fullCountings // Last // Last;
	keepLabels = sectorDropLabels[fullCountings,False];
	labels = sectorDropLabels[fullCountings,OptionValue["RefineIndeterminates"]];
	zeroIndeterminateLabels = If[TrueQ[OptionValue["RefineIndeterminates"]],
		sectorZeroIndeterminateLabels[fullCountings],
		sectorAssociation[sectors,ConstantArray[{},Length[sectors]]]
	];
	edges = sectorCoverEdges[sectors];
	edgeLabels = sectorEdgeDropLabels[edges,zeroIndeterminateLabels];
	toIndexSector = sectorToIndexSector[variables];
	vertices = toIndexSector /@ sectors;
	indexLabels = sectorAssociation[vertices,sectorLookup[labels,#]& /@ sectors];
	indexKeepLabels = sectorAssociation[vertices,sectorLookup[keepLabels,#]& /@ sectors];
	indexEdges = DirectedEdge@@@((toIndexSector /@ #)& /@ (List@@@edges));
	indexEdgeLabels = sectorAssociation[indexEdges,sectorLookup[edgeLabels,#]& /@ edges];
	
	If[TrueQ[OptionValue["DeleteEmptyNodes"]],
		filteredData = sectorFilterEmptyNodes[vertices,sectors,indexLabels,indexEdges,indexEdgeLabels,indexKeepLabels];
		vertices = filteredData["Vertices"];
		sectors = filteredData["Sectors"];
		indexLabels = filteredData["Labels"];
		indexEdges = filteredData["Edges"];
		indexEdgeLabels = filteredData["EdgeLabels"];
	];
	
	Return[
		<|
			"EulerChiData"->fullCountings,
			"Sectors"->sectors,
			"Vertices"->vertices,
			"Labels"->indexLabels,
			"Edges"->indexEdges,
			"EdgeLabels"->indexEdgeLabels
		|>
	];
];


Options[SectorDropGraph] = Options[SectorDropGraphData];
SectorDropGraph[gpol_,variables_,singList_,opts:OptionsPattern[]]:=Module[
	{
		data,vertices,labels,sectorLabels,edgeLabelRules,edgeStyleRules
	},
	
	If[!constraintOptionFreeQ[SectorDropGraph,{opts}],Return[$Failed]];
	data = SectorDropGraphData[gpol,variables,singList,opts];
	If[data===$Failed,Return[$Failed]];
	
	vertices = data["Vertices"];
	labels = data["Labels"];
	sectorLabels = sectorAssociation[vertices,data["Sectors"]];
	edgeLabelRules = Cases[
		data["Edges"],
		edge_ /; sectorLookup[data["EdgeLabels"],edge] =!= {} :>
			(edge->Placed[Style[sectorDropLabelText[sectorLookup[data["EdgeLabels"],edge]],10,Bold,Orange],Center])
	];
	edgeStyleRules = Cases[
		data["Edges"],
		edge_ /; sectorLookup[data["EdgeLabels"],edge] =!= {} :>
			(edge->Orange)
	];
	
	Return[
		Graph[
			vertices,
			data["Edges"],
			VertexLabels->((#->sectorVertexLabel[sectorLabels,labels][#])& /@ vertices),
			EdgeLabels->edgeLabelRules,
			EdgeStyle->edgeStyleRules,
			VertexShapeFunction->"Circle",
			VertexStyle->Directive[White,EdgeForm[GrayLevel[0.35]]],
			GraphLayout->"LayeredDigraphEmbedding",
			ImagePadding->40,
			ImageSize->Large
		]
	];
];
