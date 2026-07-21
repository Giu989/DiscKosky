(* ::Package:: *)

deleteSupersets[list_]:=deleteSupersets[list]=Select[list,Function[x,!MemberQ[list,y_/;x=!=y&&SubsetQ[x,y]]]]


ClearAll[findCutsIndex];
findCutsIndex[fullCountings_,index_]:=findCutsIndex[fullCountings,index]=Module[{ans1,ans2,positions,positions2,cuts,cuts2},
	ans1=fullCountings // Last;
	ans2=fullCountings[[index]];


	positions = Position[ans2[[2]]-ans1[[2]],_?(#<0&)]//Flatten;
	positions2 = Position[ans2[[2]]-ans1[[2]],_?(#===Indeterminate&)]//Flatten;
	cuts = ans1[[3]][[positions]];
	cuts2 = ans1[[3]][[positions2]];

	(*If[(cuts2//Length)=!=0,Print["Indeterminate sector found"]];*)

	Return[<|"data"->{cuts,cuts2},"index"->index|>];
];


ClearAll[buildEulerChiData];
Options[buildEulerChiData] = Join[
	{"CheckCriticalPointValidity"->True},
	DeleteCases[Options[CountSectorsRegulated],"Constraint"->_]
];
countSectorsOptions[head_,opts_List]:=FilterRules[opts,DeleteCases[Options[head],"Constraint"->_]];
buildEulerChiData[gpol_,variables_,singList_,opts:OptionsPattern[]]:= buildEulerChiData[gpol,variables,singList,opts] = Module[
	{
		eulerChi,index,i,j,singularitieseulerChi,fulleulerChi,eulerChiReg
	},
	
	PrintTemporary["First run, computing singularity structure data"];
	PrintTemporary["Computing critical points sector by sector"];
	eulerChi = CountSectorsUnregulated[gpol,variables,{},Sequence@@countSectorsOptions[CountSectorsUnregulated,{opts}]];
	
	If[First[eulerChi]===Indeterminate,
		Print["Error: At least one (sub)sector has non isolated critical points. Cannot compute the Euler characteristic with this method. Run the command CountSectorsUnregulated[] for more information on the degenerate sectors."];
		Return[$Failed];
	];
	PrintTemporary["Sector by sector critical points characteristic = ", eulerChi // First];
	If[OptionValue["CheckCriticalPointValidity"],
		PrintTemporary["Computing the generic Euler characteristic regulated \[LongDash] this may take a while..."];
		eulerChiReg = CountSectorsRegulated[gpol,variables,{},Sequence@@countSectorsOptions[CountSectorsRegulated,{opts}]];
		PrintTemporary["Regulated Euler Characteristic = ", eulerChiReg];
		If[First[eulerChi]===eulerChiReg,
			PrintTemporary["Euler characteristics agree, proceeding"];
		,
			Print["Error: The sector by sector euler characteristic does not agree with the regulated euler characteristic"];
			Print["Sector by sector Euler characteristic = ", eulerChi // First];
			Print["Regulated Euler characteristic = ", eulerChiReg];
			Print["Incorrect predictions will be generated"];
			Return[$Failed];
		];
	,
		PrintTemporary["Warning: Skipping regulated Euler characteristic check"];
	];
	
	PrintTemporary["Computing critical points on the support of each singularity"];
	singularitieseulerChi = Monitor[
		Table[
			CountSectorsUnregulated[gpol,variables,{},Sequence@@countSectorsOptions[CountSectorsUnregulated,{opts}],"Constraint"->singList[[index]]]
		,
			{index,1,singList//Length}
		]
	,
		"Singularity " <> ToString[index] <> "/"<>ToString[Length[singList]]
	];
	
	fulleulerChi = Join[singularitieseulerChi,{eulerChi}];
	Return[fulleulerChi];
];


Options[findNewTrueCuts] = {"RefineIndeterminates"->False};
findNewTrueCuts[fullCountings_,data1_,index2_,opts:OptionsPattern[]]:=Module[
	{
		data2,max1,trueData,indData,zeroSectorIndeterminates
	},
	
	data2 = findCutsIndex[fullCountings,index2]["data"];
	max1 = deleteSupersets@DeleteDuplicates@Flatten[data1["data"],1];
	trueData = Select[data2[[1]],Function[c2,AnyTrue[max1,SubsetQ[c2,#]&]]];
	indData = Select[data2[[2]],Function[c2,AnyTrue[max1,SubsetQ[c2,#]&]]];
	
	If[OptionValue["RefineIndeterminates"],
		zeroSectorIndeterminates = sectorZeroIndeterminateLabels[fullCountings];
		indData = indData // DeleteCases[#,_?(SubsetQ[ReplaceAll[#,zeroSectorIndeterminates],{data1["index"]}] &)]&;
	];
	
	Return[<|"data"->{trueData,indData},"index"->index2|>];
	(*Return[{trueData,indData}];*)
];


Options[CheckSequentialDiscontinuity] = Join[Options[findNewTrueCuts],Options[buildEulerChiData]];
CheckSequentialDiscontinuity[gpol_,variables_,singList_,indexList_,opts:OptionsPattern[]]:=Module[
	{
		data1,outTrueCut,fullCountings
	},
	
	fullCountings = buildEulerChiData[gpol,variables,singList,Sequence@@FilterRules[{opts},Options[buildEulerChiData]]];
	If[fullCountings===$Failed,Return[$Failed]];
	
	data1 = findCutsIndex[fullCountings,indexList[[1]]];
	outTrueCut = Fold[findNewTrueCuts[fullCountings,#1,#2,Sequence@@FilterRules[{opts},Options[findNewTrueCuts]]]&,data1,indexList[[2;;]]]["data"];

	If[Length[outTrueCut[[1]]]>0,
		Return[True];
	];
	
	If[Length[outTrueCut[[2]]]>0,
		Return[Indeterminate];
	];
	
	Return[False];
];


Options[CheckDoubleDiscontinuities] = Options[CheckSequentialDiscontinuity];
CheckDoubleDiscontinuities[gpol_,variables_,singList_,opts:OptionsPattern[]]:=Module[
	{
		eulerChi,index,i,j,singularitieseulerChi,fulleulerChi,dropMatrix
	},
	
	If[buildEulerChiData[gpol,variables,singList,Sequence@@FilterRules[{opts},Options[buildEulerChiData]]]===$Failed,Return[$Failed]];
	
	PrintTemporary["Building the double discontinuity matrix"];
	dropMatrix = Monitor[Table[CheckSequentialDiscontinuity[gpol,variables,singList,{i,j},opts],{i,1,Length[singList]},{j,1,Length[singList]}],{ToString[i]<>"/"<>ToString[Length[singList]],ToString[j]<>"/"<>ToString[Length[singList]]}];
	
	Return[dropMatrix];
];
