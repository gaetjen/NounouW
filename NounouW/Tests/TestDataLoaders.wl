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


NNWTestLoadBG001 :=
Block[{},
	$nnwTestFileFolder = FileNameJoin[
				{$nnwResourcesFolder, "nounou","Neuralynx","BG001",
				"2016-05-11_16-34-20","split","t0508_001", "segment 1","1-1"}];
	$nnwTestFiles = FileNames[FileNameJoin[{$nnwTestFileFolder,"CSC*.ncs"}]];
	$nnwTestDataOri = NNLoad[$nnwTestFiles];
	Print[ "$nnwTestFileFolder = " <> $nnwTestFileFolder];
	Print[ "$nnwTestFiles = " <> $nnwTestFiles];
];


NNWTestLoadSPP010 :=
Block[{},
	$nnwTestFileFolder = FileNameJoin[
				{$nnwResourcesFolder, "nounou","Neuralynx",
				"SPP010","2013-12-02_17-07-31"}];
	$nnwTestFiles = FileNames[FileNameJoin[{$nnwTestFileFolder,"Tet*.ncs"}]];
	$nnwTestDataOri = NNLoad[$nnwTestFiles];
	Print[ "$nnwTestFileFolder = " <> $nnwTestFileFolder];
	Print[ "$nnwTestFiles = " <> $nnwTestFiles];
];
