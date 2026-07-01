(* ::Package:: *)

BeginPackage["DiscKosky`"(*,{}*)];


CountSectorsUnregulated::usage = "to add";
CountSectorsRegulated::usage = "CountSectorsRegulated[gpol, vars, cut] computes the number of irreducible monomials for the regulated critical-point ideal. Use {} for no cut variables. The option \"UseSameRho\" -> True uses one shared rho parameter for all nu_i.";
CheckDoubleDiscontinuities::usage = "to add";
GroebnerBasisMS::usage = "GroebnerBasisMS[ideal, vars] computes a Groebner basis of ideal in the variables vars using msolve.\nGroebnerBasisMS[jobs] computes a batch of Groebner bases, where each job is <|\"Ideal\" -> ideal, \"Variables\" -> vars|> or {ideal, vars}.\nOptions include \"Modulus\" -> p for computations over the prime field GF(p), \"LeadingMonomialsOnly\" -> True to return the leading ideal instead of the full Groebner basis, and \"EliminateVariables\" -> {x1, ...} to return the requested Groebner-basis operation for the elimination ideal in the remaining variables. Variables listed in \"EliminateVariables\" must appear in every job and at least one variable must remain.\nmsolve batch options include \"MSolveJobs\", \"msolveParallelThreads\" -> 1 to set msolve's -t thread count, \"MSolveBatchDirectory\", \"MSolveKeepFiles\", \"MSolveProgress\", and \"MSolveProgressInterval\". The older \"MSolveThreads\" option is still accepted as an alias.";
DiscKoskyExtraVar::usage = "extra internal variable required for critical point ideals";


Begin["`Private`"]


With[{pac = PacletFind["DiscKosky"]},
  If[Length[pac] > 0,
    version = ("Version" //ReplaceAll[pac[[1]][[1]]]);
  ];
];
pacletInstallLocation = PacletFind["DiscKosky"][[1,1]]["Location"];


(*load in source code*)
Get[FileNameJoin[{DirectoryName[$InputFileName], "paths.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "irreducible_monomials.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "euler_characteristic.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "discontinuities.m"}]];


(*finding the msolve executable*)
msolveData = RunProcess[{"bash","-lc","export PATH=\"$1:$PATH\"; command -v msolve","bash",msolvePath}];
msolveExec = If[msolveData["ExitCode"]===0,msolveData["StandardOutput"]//StringDelete[#,"\n"]&,$Failed];


(*welcome message*)
Print["DiscKosky " <> version <> ": Giulio Crisanti, Luke Lippstreu, Andrew J. McLeod and Maria Polackova (2026)"]
If[msolveExec===$Failed,Print["Warning: could not find msolve executable \[LongDash]\[LongDash] automatic Groebner Basis runs will default to Mathematica built in functions"]]


End[]


SetAttributes[
	{
		CountSectorsUnregulated,
		CountSectorsRegulated,
		CheckDoubleDiscontinuities,
		GroebnerBasisMS,
		DiscKoskyExtraVar
		,
		Nothing
	}
	, {(*Protected,*)ReadProtected}
];


EndPackage[]
