(* ::Package:: *)

(*count number of master integrals in a single sector*)
Options[countInSector]={"Substitute"->True,"MonomialOrder" -> DegreeReverseLexicographic,"Sort"->True,"Constraint"->0,"Diophantine"->True,"msolve"->Automatic,"PrimeIndex"->Random};
countInSector[twistPoly1_,propagatorVariables_,opts : OptionsPattern[]]:=Module[
	{
	params,paramsNsub,numerators,denominator,system,systemVariables,monomials,
	masterCount,solutions,kinPoly,kinPolyVars,lowestPowerCoeff,kinPolyN,(*DiscKoskyExtraVar,*)
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
		system = Join[numerators,{1-DiscKoskyExtraVar*denominator}] // DeleteCases[0];
		systemVariables = Join[{DiscKoskyExtraVar},propagatorVariables]//Flatten;
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
			indexList=Range[400]-1//RandomSample;
			primeIndexAndSub=Table[
				inst=FindInstance[kinPolyN==0,{mandelstamVar},Modulus->primeList[[pInd+1]]];
				If[Length[inst]>0,Return[{pInd,inst//Flatten},Table]];
				,
				{pInd,indexList}
			]//DeleteDuplicates;
			If[primeIndexAndSub==={Null},Print["Error: Diophantine solution not found. Try running again with \"Diophantine\"->False"]; Return[$Failed]];
			system = Join[numerators,{1-DiscKoskyExtraVar*denominator}] // DeleteCases[0] // ReplaceAll[primeIndexAndSub[[2]]];
			systemVariables = Join[{DiscKoskyExtraVar},propagatorVariables]//Flatten;
		,
			system = Join[{kinPolyN},numerators,{1-DiscKoskyExtraVar*denominator}] // DeleteCases[0];
			systemVariables = Join[{mandelstamVar},{DiscKoskyExtraVar},propagatorVariables]//Flatten;
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


resolvePrimeIndex[primeIndex_]:=Module[{index},
	index = If[primeIndex===Random,
		RandomInteger[{0,Length[primeList]-1}]
	,
		primeIndex
	];
	If[!IntegerQ[index] || index<0 || index>=Length[primeList],
		Print["Error: invalid prime index"];
		Return[$Failed]
	];
	index
];

randomKinematicSubstitution[{}]:={};
randomKinematicSubstitution[params_List]:=Thread[params->(RandomInteger[{1,10^8+(params//Length)},params//Length]//Map[Prime])];

prepareKinematicSpecializationMS[lpPoly_,physicalPropagators_,prime_,opts : OptionsPattern[countInSector]]:=Module[
	{
	params,paramsNsub,kinPoly,kinPolyVars,lowestPowerCoeff,kinPolyN,
	mandelstamVar,exponent,solvedConstraint,inst,attempt,found=False,
	polySpecialized,constraintSystem,variablePrefix,exponentFactor
	},

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

	If[(exponent == 1) && (mandelstamVar=!={}),
		solvedConstraint = Solve[kinPoly==0,mandelstamVar]//Flatten;
		polySpecialized = lpPoly // ReplaceAll[solvedConstraint];
		params = Complement[polySpecialized // Variables,physicalPropagators];
		paramsNsub = randomKinematicSubstitution[params];
		polySpecialized = polySpecialized // ReplaceAll[paramsNsub];
		constraintSystem = {};
		variablePrefix = {DiscKoskyExtraVar};
		exponentFactor = exponent;
	,
		params = Complement[Complement[(lpPoly//Variables)~Join~(kinPoly//Variables)//DeleteDuplicates,physicalPropagators],{mandelstamVar}];
		If[OptionValue["Diophantine"] && (mandelstamVar=!={}),
			Do[
				paramsNsub = randomKinematicSubstitution[params];
				kinPolyN = kinPoly // ReplaceAll[paramsNsub];
				inst=FindInstance[kinPolyN==0,{mandelstamVar},Modulus->prime];
				If[Length[inst]>0,found=True;Break[]];
				,
				{attempt,1,50}
			];
			If[!found,Print["Error: Diophantine solution not found. Try running again with \"Diophantine\"->False"]; Return[$Failed]];
			polySpecialized = lpPoly // ReplaceAll[paramsNsub] // ReplaceAll[inst//Flatten];
			constraintSystem = {};
			variablePrefix = {DiscKoskyExtraVar};
			exponentFactor = 1;
		,
			paramsNsub = randomKinematicSubstitution[params];
			polySpecialized = lpPoly // ReplaceAll[paramsNsub];
			kinPolyN = kinPoly // ReplaceAll[paramsNsub];
			constraintSystem = If[kinPolyN===0,{}, {kinPolyN}];
			variablePrefix = If[mandelstamVar==={},{DiscKoskyExtraVar},{mandelstamVar,DiscKoskyExtraVar}];
			exponentFactor = exponent;
		];
	];

	<|"Polynomial"->polySpecialized,"ConstraintSystem"->constraintSystem,"VariablePrefix"->variablePrefix,"ExponentFactor"->exponentFactor|>
];

prepareCountInSectorMS[twistPoly_,propagatorVariables_,specialization_Association,prime_,opts : OptionsPattern[countInSector]]:=Module[
	{numerators,denominator,system,systemVariables,job},

	numerators = D[twistPoly,{propagatorVariables}];
	If[(propagatorVariables//Length)===0, numerators = {}];
	denominator = twistPoly;
	system = Join[specialization["ConstraintSystem"],numerators,{1-DiscKoskyExtraVar*denominator}] // DeleteCases[0];
	systemVariables = Join[specialization["VariablePrefix"],propagatorVariables]//Flatten;
	job = prepareIrreducibleMonomialJob[system,systemVariables,prime,Sequence@@FilterRules[{opts},Options[findIrreducibleMonomials]]];
	<|"Job"->job,"Variables"->job["Variables"],"MonomialOrder"->job["MonomialOrder"],"ExponentFactor"->specialization["ExponentFactor"]|>
];

prepareCountRegulatedMS[twistPoly_,propagatorVariables_,propagatorVariablesCut_,useSameRho_,specialization_Association,prime_,opts : OptionsPattern[countInSector]]:=Module[
	{
	cutVariables,activeProduct,dParam,rhoParam,nuParams,numerators,denominator,
	system,systemVariables,job
	},

	cutVariables = DeleteDuplicates[propagatorVariablesCut];
	activeProduct = Times@@Complement[propagatorVariables,cutVariables];
	dParam = Unique["DiscKoskyD$"];
	rhoParam = Unique["DiscKoskyRho$"];
	nuParams = If[TrueQ[useSameRho],
		ConstantArray[rhoParam,Length[propagatorVariables]]
	,
		Table[Unique["DiscKoskyNu$"],{Length[propagatorVariables]}]
	];
	numerators = MapThread[
		If[MemberQ[cutVariables,#1],
			D[twistPoly,#1],
			#2*twistPoly - dParam*#1*D[twistPoly,#1]/2
		]&,
		{propagatorVariables,nuParams}
	] // DeleteCases[0];
	denominator = activeProduct*twistPoly;
	system = Join[specialization["ConstraintSystem"],numerators,{1-DiscKoskyExtraVar*denominator}] // DeleteCases[0];
	systemVariables = Join[specialization["VariablePrefix"],propagatorVariables]//Flatten;
	job = prepareIrreducibleMonomialJob[system,systemVariables,prime,Sequence@@FilterRules[{opts},Options[findIrreducibleMonomials]]];
	<|"Job"->job,"Variables"->job["Variables"],"MonomialOrder"->job["MonomialOrder"],"ExponentFactor"->specialization["ExponentFactor"]|>
];


	(*count number of master integrals in all sectors*)
Options[CountSectorsUnregulated]={
	"Substitute"->True,
	"MonomialOrder" -> DegreeReverseLexicographic,
	"Sort"->True,
	"Constraint"->0,
	"Diophantine"->True,
	"msolve"->Automatic,
	"PrimeIndex"->Random,
	"MSolveJobs"->Automatic,
	"MSolveThreads"->1,
	"MSolveBatchDirectory"->Automatic,
	"MSolveKeepFiles"->False,
	"MSolveProgress"->Automatic,
	"MSolveProgressInterval"->0.008,
	"debug"->False
};
CountSectorsUnregulated[lpPoly_,physicalPropagators_List,physicalPropagatorsCut_List,opts : OptionsPattern[]]:=Module[
	{
	sectors,sectorsLP,effectivePoly,effectiveVars,totalSum,sectorCounting,i,
	useMsolve,primeIndex,prime,specialization,specializedPoly,prepared,jobs,gbs,gbCounter,prep,
	monomialCount,showNotebookProgress,prepareSectorCounts
	},

	sectors = Complement[physicalPropagators,physicalPropagatorsCut] // Subsets;
	sectorsLP = sectors // Map[Join[#,physicalPropagatorsCut]&] // Map[Sort];
	showNotebookProgress = TrueQ[$Notebooks];
	useMsolve = OptionValue["msolve"];
	If[And[useMsolve,msolveExec===$Failed],Print["Error: msolve requested but executable not found"];Return[$Failed];];
	If[OptionValue["msolve"]===Automatic,
		If[msolveExec===$Failed,
			useMsolve=False
		,
			useMsolve=True
		]
	];
	If[And[useMsolve,OptionValue["MonomialOrder"]=!=DegreeReverseLexicographic],Print["Error: msolve only supports DegreeReverseLexicographic order"];Return[$Failed];];

	If[useMsolve,
		primeIndex = resolvePrimeIndex[OptionValue["PrimeIndex"]];
		If[primeIndex===$Failed,Return[$Failed]];
		prime = primeList[[1+primeIndex]];
		specialization = prepareKinematicSpecializationMS[lpPoly,physicalPropagators,prime,Sequence@@FilterRules[{opts},Options[countInSector]]];
		If[specialization===$Failed,Return[$Failed]];
		specializedPoly = specialization["Polynomial"];
		prepareSectorCounts := (
			prepared = Table[
				effectivePoly = specializedPoly // ReplaceAll[Complement[physicalPropagators,sectorsLP[[i]]]->0//Thread] // Cancel;
				effectiveVars = Intersection[physicalPropagators,sectorsLP[[i]]];
				If[effectivePoly===0,
					<|"Count"->0|>
				,
					prepareCountInSectorMS[effectivePoly,effectiveVars,specialization,prime,Sequence@@FilterRules[{opts},Options[countInSector]]]
				]
			,
				{i,1,sectors//Length}
			]
		);
		If[showNotebookProgress,
			Monitor[
				prepareSectorCounts,
				"Preparing sector "<>ToString[i]<>"/"<>""<>ToString[sectors//Length]
			]
		,
			prepareSectorCounts
		];
		If[MemberQ[prepared,$Failed],Return[$Failed]];
		jobs = Cases[prepared,assoc_Association /; KeyExistsQ[assoc,"Job"] :> assoc["Job"]];
		gbs = GroebnerBasisMS[jobs,
			"Modulus"->prime,
			"LeadingMonomialsOnly"->True,
			Sequence@@FilterRules[{opts},Options[GroebnerBasisMS]]
		];
		If[gbs===$Failed,Return[$Failed]];
		gbCounter = 0;
		sectorCounting = Table[
			prep = prepared[[i]];
			If[KeyExistsQ[prep,"Count"],
				prep["Count"]
			,
				gbCounter++;
				monomialCount = irreducibleMonomialCountFromLeadingMonomials[gbs[[gbCounter]],prep["Variables"]];
				If[monomialCount===\[Infinity],
					Indeterminate
				,
					monomialCount/prep["ExponentFactor"]
				]
			]
			,
			{i,1,Length[prepared]}
		];
	,
		prepareSectorCounts := (
			sectorCounting = Table[
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
		);
		If[showNotebookProgress,
			Monitor[
				prepareSectorCounts,
				"Sector "<>ToString[i]<>"/"<>""<>ToString[sectors//Length]
			]
		,
			prepareSectorCounts
		];
	];

	(*postprocessing for output*)
	totalSum = sectorCounting // Apply[Plus];
	Return[{totalSum,sectorCounting,sectorsLP}];
];

Options[CountSectorsRegulated]=Join[Options[CountSectorsUnregulated],{"UseSameRho"->False}];
CountSectorsRegulated[lpPoly_,physicalPropagators_List]:=
	CountSectorsRegulated[lpPoly,physicalPropagators,{}];
CountSectorsRegulated[lpPoly_,physicalPropagators_List,physicalPropagatorsCut_List,opts : OptionsPattern[]]:=Module[
	{
	useMsolve,useSameRho,primeIndex,prime,specialization,specializedPoly,
	prepared,gb,monomialCount,masterCount
	},

	If[!SubsetQ[physicalPropagators,physicalPropagatorsCut],
		Print["Error: cut variables must be a subset of variables"];
		Return[$Failed]
	];
	If[!MemberQ[{True,False},OptionValue["UseSameRho"]],
		Print["Error: \"UseSameRho\" must be True or False"];
		Return[$Failed]
	];
	useSameRho = TrueQ[OptionValue["UseSameRho"]];
	useMsolve = OptionValue["msolve"];
	If[And[useMsolve,msolveExec===$Failed],Print["Error: msolve requested but executable not found"];Return[$Failed];];
	If[OptionValue["msolve"]===Automatic,
		If[msolveExec===$Failed,
			useMsolve=False
		,
			useMsolve=True
		]
	];
	If[And[useMsolve,OptionValue["MonomialOrder"]=!=DegreeReverseLexicographic],Print["Error: msolve only supports DegreeReverseLexicographic order"];Return[$Failed];];

	primeIndex = resolvePrimeIndex[OptionValue["PrimeIndex"]];
	If[primeIndex===$Failed,Return[$Failed]];
	prime = primeList[[1+primeIndex]];
	specialization = prepareKinematicSpecializationMS[lpPoly,physicalPropagators,prime,Sequence@@FilterRules[{opts},Options[countInSector]]];
	If[specialization===$Failed,Return[$Failed]];
	specializedPoly = specialization["Polynomial"];
	prepared = prepareCountRegulatedMS[specializedPoly,physicalPropagators,physicalPropagatorsCut,useSameRho,specialization,prime,Sequence@@FilterRules[{opts},Options[countInSector]]];
	If[prepared===$Failed,Return[$Failed]];

	If[useMsolve,
		gb = GroebnerBasisMS[prepared["Job"]["Ideal"],prepared["Variables"],
			"Modulus"->prime,
			"LeadingMonomialsOnly"->True,
			Sequence@@FilterRules[{opts},Options[GroebnerBasisMS]]
		];
		If[gb===$Failed,Return[$Failed]];
		monomialCount = irreducibleMonomialCountFromLeadingMonomials[gb,prepared["Variables"]];
	,
		gb = GroebnerBasis[prepared["Job"]["Ideal"],prepared["Variables"],
			MonomialOrder->prepared["MonomialOrder"],
			CoefficientDomain->RationalFunctions,
			Modulus->prime
		];
		If[gb===$Failed,Return[$Failed]];
		monomialCount = irreducibleMonomialCountFromGroebnerBasis[gb,prepared["Variables"],prepared["MonomialOrder"]];
	];
	masterCount = If[monomialCount===\[Infinity],Indeterminate,monomialCount/prepared["ExponentFactor"]];
	Return[masterCount]
];
