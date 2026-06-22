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

	Return[{cuts,cuts2}];
];


checkDoubleDiscontinuity[fullCountings_,index1_,index2_]:=Module[
	{
		data1,data2,max1,superDrop,indDrop
	},
	
	data1=findCutsIndex[fullCountings,index1];
	data2=findCutsIndex[fullCountings,index2];
	max1=deleteSupersets@DeleteDuplicates@Flatten[data1,1];
	superDrop=AnyTrue[data2[[1]],Function[c2,AnyTrue[max1,SubsetQ[c2,#]&]]];
	indDrop=AnyTrue[data2[[2]],Function[c2,AnyTrue[max1,SubsetQ[c2,#]&]]];
	
	Return[superDrop||Replace[indDrop,True->Indeterminate]];
];


CheckDoubleDiscontinuities[gpol_,variables_,singList_]:=Module[
	{
		eulerChi,index,i,j,singularitieseulerChi,fulleulerChi,dropMatrix
	},
	
	PrintTemporary["Computing the Generic Euler Characteristic"];
	eulerChi = CountSectorsUnregulated[gpol,variables,{}];
	
	If[First[eulerChi]===Indeterminate,
		Print["Error: At least one (sub)sector has non isolated critical points. Cannot compute the Euler characteristic with this method. Run the command CountSectorsUnregulated[] for more information on the degenerate sectors."];
		Return[$Failed];
	];
	PrintTemporary["Generic Euler Characteristic = ", eulerChi // First];
	
	PrintTemporary["Computing the Euler characteristic for each singularity"];
	singularitieseulerChi = Monitor[
		Table[
			CountSectorsUnregulated[gpol,variables,{},"Constraint"->singList[[index]]]
		,
			{index,1,singList//Length}
		]
	,
		"Singularity " <> ToString[index] <> "/"<>ToString[Length[singList]]
	];
	
	fulleulerChi = Join[singularitieseulerChi,{eulerChi}];
	
	PrintTemporary["Building the double discontinuity matrix"];
	dropMatrix = Monitor[Table[checkDoubleDiscontinuity[fulleulerChi,i,j],{i,1,Length[singList]},{j,1,Length[singList]}],{ToString[i]<>"/"<>ToString[Length[singList]],ToString[j]<>"/"<>ToString[Length[singList]]}];
	
	Return[dropMatrix];
];
