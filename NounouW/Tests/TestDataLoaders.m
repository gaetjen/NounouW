(* ::Package:: *)

$nnwResourcesFolder =FileNameJoin[{Nest[ParentDirectory, NotebookDirectory[],3], "resources"}]


$nnwTestFileFolder = Null;
$nnwTestFiles = Null;
$nnwTestDataOri = Null;


NNWTestLoadSPP067a :=
Block[{},
	$nnwTestFileFolder = FileNameJoin[{$nnwResourcesFolder, "nounou","Neuralynx","SPP067","a"}];
	$nnwTestFiles = FileNames[FileNameJoin[{$nnwTestFileFolder,"CSC*.ncs"}]];
	$nnwTestDataOri = NNLoad[$nnwTestFiles];
	Print[ "$nnwTestFileFolder = " <> $nnwTestFileFolder];
	Print[ "$nnwTestFiles = " <> $nnwTestFiles];
];


NNWTestLoadBG006 :=
Block[{},
	$nnwTestFileFolder = FileNameJoin[{$nnwResourcesFolder, "nounou","Neuralynx","BG006","2016-06-29_15-33-12"}];
	$nnwTestFiles = FileNames[FileNameJoin[{$nnwTestFileFolder,"CSC*.ncs"}]];
	$nnwTestDataOri = NNLoad[$nnwTestFiles];
	Print[ "$nnwTestFileFolder = " <> $nnwTestFileFolder];
	Print[ "$nnwTestFiles = " <> $nnwTestFiles];
];
