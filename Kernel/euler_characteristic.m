(* ::Package:: *)

(*count number of master integrals in a single sector*)
Options[countInSector]={"Substitute"->True,"MonomialOrder" -> DegreeReverseLexicographic,"Sort"->True,"Constraint"->0,"Diophantine"->True};
countInSector[twistPoly1_,propagatorVariables_,opts : OptionsPattern[]]:=Module[
	{
	params,paramsNsub,numerators,denominator,system,systemVariables,monomials,
	masterCount,solutions,kinPoly,kinPolyVars,lowestPowerCoeff,kinPolyN,newvar,
	mandelstamVar,exponent,twistPoly,solvedConstraint,indexList,inst,primeIndexAndSub
	},
		
	(*checking kinematic poly*)
	kinPoly = OptionValue["Constraint"]//Together//Numerator;
	kinPolyVars = kinPoly // Variables;
	If[(kinPolyVars//Length)!=0,
		lowestPowerCoeff=CoefficientRules[kinPoly][[;;,1]] // Transpose // Map[Apply[Max]] // PositionSmallest // First;
		mandelstamVar = kinPolyVars[[lowestPowerCoeff]];
		exponent = Exponent[kinPoly,mandelstamVar];
	,
		mandelstamVar = {};
		exponent = 1;
	];
	
	(*If the polynomial is linear, it is faster to just solve the constraint and plug it in.*)
	If[(exponent == 1) && (mandelstamVar=!={}),
		solvedConstraint = Solve[kinPoly==0,mandelstamVar]//Flatten;
		twistPoly = twistPoly1 // ReplaceAll[solvedConstraint];
		params = Complement[twistPoly // Variables,propagatorVariables];
		paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];
		numerators = D[twistPoly // ReplaceAll[paramsNsub],{propagatorVariables}];
		If[(propagatorVariables//Length)===0, numerators = {}];
		denominator = twistPoly// ReplaceAll[paramsNsub];
		system = Join[numerators,{1-newvar*denominator}] // DeleteCases[0];
		systemVariables = Join[{newvar},propagatorVariables]//Flatten;
	,
		twistPoly = twistPoly1;
		params = Complement[Complement[(twistPoly//Variables)~Join~(kinPoly//Variables)//DeleteDuplicates,propagatorVariables],{mandelstamVar}];
		paramsNsub = Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];
		numerators = D[twistPoly // ReplaceAll[paramsNsub],{propagatorVariables}];
		If[(propagatorVariables//Length)===0, numerators = {}];
		denominator = twistPoly// ReplaceAll[paramsNsub];
		kinPolyN = kinPoly // ReplaceAll[paramsNsub];
		If[OptionValue["Diophantine"] && (mandelstamVar=!={}),
			(*solve diophantine equation*)
			indexList=Range[201]-1//RandomSample;
			primeIndexAndSub=Table[
				inst=FindInstance[kinPolyN==0,{mandelstamVar},Modulus->primeList[[pInd+1]]];
				If[Length[inst]>0,Return[{pInd,inst//Flatten},Table]];
				,
				{pInd,indexList}
			]//DeleteDuplicates;
			If[primeIndexAndSub==={Null},Print["Error: Diophantine solution not found. Try running again with \"Diophantine\"->False"]; Return[$Failed]];
			system = Join[numerators,{1-newvar*denominator}] // DeleteCases[0] // ReplaceAll[primeIndexAndSub[[2]]];
			systemVariables = Join[{newvar},propagatorVariables]//Flatten;
		,
			system = Join[{kinPolyN},numerators,{1-newvar*denominator}] // DeleteCases[0];
			systemVariables = Join[{mandelstamVar},{newvar},propagatorVariables]//Flatten;
		];
	];
	
	If[OptionValue["Diophantine"] && (mandelstamVar=!={}) && (exponent =!= 1),
		monomials = findIrreducibleMonomials[system,systemVariables,"PrimeIndex"->primeIndexAndSub[[1]],Sequence@@FilterRules[{opts},Options[findIrreducibleMonomials]]];
		masterCount = If[monomials===\[Infinity],Indeterminate,(monomials // Length)];
	,
		monomials = findIrreducibleMonomials[system,systemVariables,Sequence@@FilterRules[{opts},Options[findIrreducibleMonomials]]];
		masterCount = If[monomials===\[Infinity],Indeterminate,(monomials // Length)/exponent];
	];
	
	Return[masterCount];
];


(*count number of master integrals in all sectors*)
Options[CountSectorsUnregulated]={"Substitute"->True,"MonomialOrder" -> DegreeReverseLexicographic,"Sort"->True,"Constraint"->0,"Diophantine"->True};
CountSectorsUnregulated[lpPoly_,physicalPropagators_List,physicalPropagatorsCut_List,opts : OptionsPattern[]]:=Module[{sectors,sectorsLP,effectivePoly,effectiveVars,totalSum,sectorCounting,i},
	
	sectors = Complement[physicalPropagators,physicalPropagatorsCut] // Subsets;
	sectorsLP = sectors // Map[Join[#,physicalPropagatorsCut]&] // Map[Sort];
	Monitor[
		sectorCounting=Table[
			effectivePoly = lpPoly // ReplaceAll[Complement[physicalPropagators,sectorsLP[[i]]]->0//Thread] // Cancel; (*algebraic simplifications to see if zero*)
			effectiveVars = Intersection[physicalPropagators,sectorsLP[[i]]];
			If[effectivePoly===0,
				0
			,
				countInSector[effectivePoly,effectiveVars,Sequence@@FilterRules[{opts},Options[countInSector]]]
			]
		,
			{i,1,sectors//Length}
		]
	,
		"sector "<>ToString[i]<>"/"<>""<>ToString[sectors//Length]
	];

	(*postprocessing for output*)
	totalSum = sectorCounting // Apply[Plus];
	Return[{totalSum,sectorCounting,sectorsLP}];
];
