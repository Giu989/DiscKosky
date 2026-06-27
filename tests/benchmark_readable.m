(* ::Package:: *)

$HistoryLength = 0;

repoRoot = ParentDirectory[DirectoryName[$InputFileName]];
benchmarkFile = FileNameJoin[{repoRoot, "tests", "benchmark.m"}];
benchmarkText = Import[benchmarkFile, "Text"];

extractAssignment[name_String] := Module[{matches},
	matches = StringCases[
		benchmarkText,
		StartOfLine ~~ name ~~ Whitespace ... ~~ "=" ~~ Whitespace ... ~~
			value : Shortest[__] ~~ ";" ~~ EndOfLine :> value
	];
	If[matches === {},
		Print["ERROR missing assignment: " <> name];
		Exit[1]
	];
	First[matches]
];

PacletDirectoryLoad[repoRoot];
Needs["DiscKosky`"];

ToExpression["gpol = " <> extractAssignment["gpol"] <> ";"];
ToExpression["gpolVars = " <> extractAssignment["gpolVars"] <> ";"];
ToExpression["singularity = " <> extractAssignment["singularity"] <> ";"];

SetAttributes[runCase, HoldRest];
runCase[label_String, expr_, expected_] := Module[{timing, value, passed},
	Print["BEGIN " <> label];
	{timing, value} = AbsoluteTiming[Quiet[expr, FrontEndObject::notavail]];
	passed = value === expected;
	Print[
		"RESULT " <> label <>
			" expected=" <> ToString[expected, InputForm] <>
			" actual=" <> ToString[value, InputForm] <>
			" passed=" <> ToString[passed, InputForm] <>
			" timing=" <> ToString[NumberForm[timing, {Infinity, 3}], OutputForm]
	];
	If[! passed, Exit[1]];
	timing
];

runCase[
	"plain_count",
	CountSectorsUnregulated[gpol, gpolVars, {}, "msolve" -> True][[1]],
	631
];

runCase[
	"overall_monomial_factor_count",
	CountSectorsUnregulated[(Times @@ gpolVars) gpol, gpolVars, {}, "msolve" -> True][[1]],
	631
];

runCase[
	"constraint_hash",
	Hash[CountSectorsUnregulated[gpol, gpolVars, {}, "Constraint" -> singularity, "msolve" -> True]],
	499145272450227864
];

Print["ALL PASSED"];
