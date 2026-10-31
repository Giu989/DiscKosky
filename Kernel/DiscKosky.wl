(* ::Package:: *)

BeginPackage["DiscKosky`"(*,{}*)];


CountSectorsUnregulated::usage = "CountSectorsUnregulated[gpol, vars, cut] counts irreducible monomials sector by sector and returns {total, sectorCounts, sectors}.";
CountSectorsRegulated::usage = "CountSectorsRegulated[gpol, vars, cut] computes the number of irreducible monomials for the regulated critical-point ideal.";
CheckDoubleDiscontinuities::usage = "CheckDoubleDiscontinuities[gpol, vars, singList] returns the pairwise double-discontinuity matrix for the singularities in singList.";
CheckSequentialDiscontinuity::usage = "CheckSequentialDiscontinuity[gpol, vars, singList, indexList] tests whether the ordered singularities in indexList support a sequential discontinuity.";
SectorDropGraph::usage = "SectorDropGraph[gpol, vars, singList] builds a directed sector graph whose vertices are sectors and whose vertex labels are singularity indices.";
SectorDropGraphData::usage = "SectorDropGraphData[gpol, vars, singList] builds the Euler characteristic data used by SectorDropGraph and returns an association containing sectors, graph vertices, singularity labels, edges, and edge labels.";
GroebnerBasisMS::usage = "GroebnerBasisMS[ideal, vars] computes a Groebner basis of ideal in the variables vars using msolve.\nGroebnerBasisMS[jobs] computes a batch of Groebner bases, where each job is <|\"Ideal\" -> ideal, \"Variables\" -> vars|> or {ideal, vars}.";
DiscKoskyExtraVar::usage = "extra internal variable required for critical point ideals";
FindSingularitySectors::usage = "FindSingularitySectors[gpol, vars, singList, index] returns the list of sectors for which the singularity singList[[index]] causes a drop in the Euler Characteristic."


Begin["`Private`"]


(*detecting install version*)
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
Get[FileNameJoin[{DirectoryName[$InputFileName], "graphics.m"}]];


(*finding the msolve executable*)
msolveData = RunProcess[{"bash","-lc","export PATH=\"$1:$PATH\"; command -v msolve","bash",msolvePath}];
msolveExec = If[msolveData["ExitCode"]===0,msolveData["StandardOutput"]//StringDelete[#,"\n"]&,$Failed];


(*welcome message*)
logo = Import[FileNameJoin[{DirectoryName[$InputFileName], "logo.png"}]] // Image[#, ImageSize -> Large]&;
(*If[$Notebooks, Show[logo, ImageSize->500]];*)
If[$Notebooks, Print[logo]];

Print["DiscKosky " <> version <> ": Giulio Crisanti, Luke Lippstreu, Andrew J. McLeod and Maria Polackova (2026)"]
If[msolveExec===$Failed,
	Print["Warning: could not find msolve executable \[LongDash]\[LongDash] automatic Groebner Basis runs will default to Mathematica built in functions. Using msolve is highly recommended especially if the (defualt) option \"CheckCriticalPointValidity\"->True is used"];
	Print["The executable path can be set manually by editing: \n\n", ToString[pacletInstallLocation], "/Kernel/paths.m"];
]


(*catch if Mathematica bugs and cannot find the documentation files*)
safeHelpLookup[p_]:=Module[{res},
	res=Quiet@Check[Documentation`ResolveLink[p],$Failed];
	If[res===$Failed||!StringQ[res],Print["Mathematica failed to link to the documentation. This is a Mathematica bug. Quitting the kernel and restarting the Mathematica application should fix this. Try also loading DiscKosky on the default kernel."];
	$Failed,Documentation`HelpLookup[p]]
];
(*documentation button*)
If[TrueQ@$Notebooks,
  Print @ Button["Open documentation",
    safeHelpLookup["paclet:DiscKosky/guide/DiscKosky"],
    Method -> "Queued"
  ];
];


End[]


SetAttributes[
	{
		CountSectorsUnregulated,
		CountSectorsRegulated,
		CheckDoubleDiscontinuities,
		CheckSequentialDiscontinuity,
		SectorDropGraph,
		SectorDropGraphData,
		GroebnerBasisMS,
		DiscKoskyExtraVar,
		FindSingularitySectors
		,
		Nothing
	}
	, {(*Protected,*)ReadProtected}
];


EndPackage[]
