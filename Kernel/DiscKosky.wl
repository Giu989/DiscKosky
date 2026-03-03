(* ::Package:: *)

BeginPackage["DiscKosky`"(*,{}*)];


CountSectorsUnregulated::usage = "to add";


Begin["`Private`"]


With[{pac = PacletFind["DiscKosky"]},
  If[Length[pac] > 0,
    version = ("Version" //ReplaceAll[pac[[1]][[1]]]);
  ];
];


(*load in source code*)
Get[FileNameJoin[{DirectoryName[$InputFileName], "irreducible_monomials.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "euler_characteristic.m"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "discontinuities.m"}]];


(*welcome message*)
Print["DiscKosky " <> version <> ": Giulio Crisanti, Luke Lippstreu, Andrew J. McLeod and Maria Polackova (2026)"]


End[]


SetAttributes[
	{
		CountSectorsUnregulated
		,
		Nothing
	}
	, {(*Protected,*)ReadProtected}
];


EndPackage[]
