(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`NNTracePlot`", 
	{"HokahokaW`","HokahokaW`Graphics`","JLink`",
	"NounouW`","NounouW`Data`"}
];


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*Related options*)


(*NNOptGridLines::usage="";*)


(* ::Subsection:: *)
(*NNTracePlot*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


NNTracePlot$UniqueOptions = {
	NNOptTimeUnit -> "ms", NNOptStack -> Automatic,
	NNOptAmplificationFactor -> 16,
	(*NNOptGridLines -> {},*)
	NNOptPlotLabel -> Automatic
	(*NNValueUnit -> Absolute, ScaleBars->{None, None}, *)  
	(*NNBaselineCorrection->Mean,*) (*Automatic*)
	(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio -> 1/3, PlotStyle->{Opacity[0.75]}, AxesLabel->Automatic,
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};
Options[NNTracePlot] = HHJoinOptionLists[ 
	NNTracePlot$UniqueOptions, 
	NNTracePlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*NNTracePlotManipulate*)


NNTracePlotManipulate::usage=
"NNTracePlotManipulate provides a simple interface to view trace data in a simple interactive interface.";


NNTracePlotManipulate$UniqueOptions = {};
NNTracePlotManipulate$OverrideOptions = {};
Options[NNTracePlotManipulate] = HHJoinOptionLists[ 
	NNTracePlotManipulate$UniqueOptions, 
	NNTracePlotManipulate$OverrideOptions,
	Options[NNTracePlot],
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNTracePlot*)


(*Open up one-element lists*)
NNTracePlot[{dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]}, rest___]:= 
	NNTracePlot[dataObj, rest];


(*This signature will reshape for all channels*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			All, 
			rest___]:= NNTracePlot[dataObj, Range[0, dataObj@getChannelCount[] - 1], rest];


(*This signature will reshape for a single channel*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel:Integer, 
			rest___]:= NNTracePlot[dataObj, {channel}, rest];


(*Main plotting class*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_:{_Integer ..}, 
			range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:= 

Block[{ optTimeUnit, 
		tempData, tempDataUnit, tempTimepoints,
		optGridLines, optStack, optPlotLabel,
		tempgr},

	(*==========Handle unit options==========*)
	optTimeUnit = OptionValue[ NNOptTimeUnit ];

	(*==========Data==========*)
	tempData = NNReadTrace[ dataObj, channels, range];
	tempTimepoints = NNReadTimepoints[ dataObj, range, optTimeUnit ];
	tempDataUnit = dataObj@getUnit[];

	(*==========Handle graphing options: GridLines(*NNOptGridLines*)==========*)
	optGridLines = OptionValue[GridLines];
	optGridLines = Switch[ Head[optGridLines],
		NNTimestamp,  {NNConvert[ dataObj, optGridLines, optTimeUnit ], None},
		NNMillisecond, {NNConvert[ dataObj, optGridLines, optTimeUnit], None},
		List, optGridLines,
		_, optGridLines(*Message[NNTracePlot::invalidOptionValue, "GridLines", optGridLines]; {}*)
	];

		(*==========Handle graphing options==========*)
	optStack = OptionValue[NNOptStack];
	If[ optStack === Automatic, 
		optStack = (dataObj@scaling[]@maxValue[]- dataObj@scaling[]@minValue[])/2
	];
	If[ optStack =!= None && optStack =!= 0, 
		tempData = HHStackLists[tempData, optStack/OptionValue[NNOptAmplificationFactor]];
	];
	optPlotLabel = OptionValue[NNOptPlotLabel];

	(*==========Plot==========*)
	tempgr = ListLinePlot[ 
		Transpose[{tempTimepoints,#}]& /@ tempData, 
		Sequence@@HHJoinOptionLists[ ListLinePlot,
			{ GridLines -> optGridLines  },
			{opts},
			{  AxesLabel-> {optTimeUnit, tempDataUnit} },
			Options[NNTracePlot]
		]
	];
	
	If[ optPlotLabel === None, tempgr,
		If[Head[optPlotLabel]===String, 
			HHLabelGraphics[ tempgr, optPlotLabel ],
			(*Automatic*) 
			HHLabelGraphics[ tempgr, dataObj@toStringShort[]<>" "<> range@toString[], {Right, Top} ]
	]]
  
];


(*This signature will realize the range if it is specified in Mathematica style*)
NNTracePlot[nnDataObj_/;NNJavaObjectQ[nnDataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			range_/;(Head[range]===List || Head[range]===Rule), 
			opts:OptionsPattern[]]:= 
NNTracePlot[nnDataObj, channels, $ToNNRangeSpecifier[range], opts];
(*Block[{rangeSpecifier},
	rangeSpecifier = $ToNNRangeSpecifier[range];
	If[ rangeSpecifier === Null,
		Message[NNReadTimepoints::invalidArgs, {nnDataObj, channels, range}]; Null,
		NNTracePlot[nnDataObj, channels, rangeSpecifier, opts]
	]
];*)


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];
NNTracePlot::timingsMismatch = "Length of generated timings `1` is not the same as generated datapoints `2`... some endpoint overhang bug?";
NNTracePlot::dataWrongFormat = "Data `1` has wrong format!";


(* ::Subsection:: *)
(*NNTracePlotManipulate*)


(*Open up one-element lists*)
NNTracePlotManipulate[{dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]}, rest___]:= 
	NNTracePlotManipulate[dataObj, rest];


(*This signature will reshape for all channels*)
NNTracePlotManipulate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			All, 
			rest___]:= NNTracePlotManipulate[dataObj, Range[0, dataObj@getChannelCount[] - 1], rest];


(*This signature will reshape for a single channel*)
NNTracePlotManipulate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel:Integer, 
			rest___]:= NNTracePlotManipulate[dataObj, {channel}, rest];


$NNTracePlotManipulate$Notebook = None;
$NNTracePlotManipulate$Graphic = None;
$NNTracePlotManipulate$tempOpts = None;
$NNTracePlotManipulate$tempPlotRangeXFrames = None;
(*$NNTracePlotManipulate$tempStackTable = None;
$NNTracePlotManipulate$optAspectRatio = None;
$NNTracePlotManipulate$optPlotRangeY = None;*)


NNTracePlotManipulate[
			dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_, 
			Automatic,
			opts:OptionsPattern[]
]:= NNTracePlotManipulate[
		dataObj, channels, Round[dataObj@timing[]@sampleRate[]/10], opts
	]; 


NNTracePlotManipulate[
		dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
		channels:{_Integer ..}, 
		length_/;((*length === Automatic || *)( NumericQ[length]&& length > 0)), 
		(*ToDo: add timestamp specification*)
		opts:OptionsPattern[]
]:= 
Block[{
	tempSegmentList, tempSegmentLengthsMax
	},
	
	tempSegmentList = Table[n, {n, 0, dataObj@timing[]@segmentCount[]-1}];
	tempSegmentLengthsMax = Max[ dataObj@timing[]@segmentLengths[] ];
	(*==========Handle time range options==========*)
	$NNTracePlotManipulate$tempPlotRangeXFrames = Round[
		length
		(*If[length === Automatic, 
			dataObj@timing[]@sampleRate[]/10,  length
		]*)
	];

	$NNTracePlotManipulate$Notebook = SelectedNotebook[];
	$NNTracePlotManipulate$tempOpts = FilterRules[{opts}, Options[NNTracePlot]];

	CreateDialog[{
		ExpressionCell[Manipulate[
			$NNTracePlotManipulate$Graphic = 
			NNTracePlot[dataObj, All, 
				{start ;; start + $NNTracePlotManipulate$tempPlotRangeXFrames, segment},
				Sequence@@$NNTracePlotManipulate$tempOpts
			],
			
			Row[{
				Control[{{segment, 0, "segment:"}, tempSegmentList, ControlType->PopupMenu}],
				Control[{{start, 0, "trace start:"}, 
					0, tempSegmentLengthsMax, $NNTracePlotManipulate$tempPlotRangeXFrames}]
			}],
			Row[{
				Button["Print graph to notebook",
					NotebookWrite[$NNTracePlotManipulate$Notebook,
						TextCell[ "{ " <> ToString[start] <> " ;; " 
									<> ToString[ start + $NNTracePlotManipulate$tempPlotRangeXFrames]
									<> ", " <> ToString[segment] <> " }"
						]
					];
					NotebookWrite[$NNTracePlotManipulate$Notebook,
						Cell[ BoxData[ToBoxes[$NNTracePlotManipulate$Graphic]], "Output", CellTags-> {"testCT"}]
					]
				], 
				Spacer[20],
				Button["Print raster to notebook",
					NotebookWrite[$NNTracePlotManipulate$Notebook,
						TextCell[ "{ " <> ToString[start] <> " ;; " 
									<> ToString[ start + $NNTracePlotManipulate$tempPlotRangeXFrames]
									<> ", " <> ToString[segment] <> " }"
						]
					];
					NotebookWrite[$NNTracePlotManipulate$Notebook,
						Cell[ BoxData[ToBoxes[Rasterize[$NNTracePlotManipulate$Graphic]]], "Output", CellTags-> {"testCT"}]
					]
				], 
				Spacer[20],
				Button["Delete last plot",
					NotebookDelete[NotebookFind[$NNTracePlotManipulate$Notebook,"testCT",Previous, CellTags][[1]]]
				], 
				Spacer[20],
				DefaultButton[DialogReturn["Exit"]]
			}],
			ContinuousAction -> False
		](*Manipulate*)]
		}, Modal->True, Background -> White,
		WindowTitle->"NNTracePlotManipulate: " <> dataObj@toString[]
	];

	Print["Closed " <> DateString[]];
	
];


NNTracePlotManipulate[args___]:=Message[NNTracePlotManipulate::invalidArgs,{args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]


(* ::Section::Closed:: *)
(*Backup*)


(*NNTracePlot[ channels:{_Integer ..}, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channels, x];
NNTracePlot[ channel_Integer, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channel, x];
NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], x___] := NNTracePlot[dataReader@data[], x]; 
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, 0, opts];
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, segment_/;NumberQ[segment], opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, segment, opts];
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channels:{_Integer ..}, span_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, channels, span, 0, opts];*)


(*NNTracePlot$UniqueOptions = {
	NNStackLists \[Rule] Automatic, NNValueUnit \[Rule] Absolute, (*ScaleBars->{None, None}, *)  
	NNBaselineCorrection->Mean, NNTimeUnit \[Rule] "ms"(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio \[Rule] Automatic, PlotStyle->{Opacity[0.75]}, 
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};*)


(*	(*==========Create mask graphics==========*)
	grMask = If[ Length[tempMask]==0,
		Graphics[
					Flatten[Join[{Opacity[0.2, Black]},
					If[opNNTimeUnitMS, 
			             {Rectangle[{(xData@tsToMs[#[[1]]]), 0},
						     		{(xData@tsToMs[#[[2]]]), tempStackAmplitude*Length[channels]}]}& /@ tempMask,
					     {Rectangle[{(xData@tsToFrameSegmentA[#[[1]]])[[1]], 0},
								    {(xData@tsToFrameSegmentA[#[[2]]])[[1]], tempStackAmplitude}]*Length[channels]}& /@ tempMask
					]
					]]
				],
		Graphics[]
	];*)


	(*(*==========Handle masking options==========*)
	opMasking = OptionValue[NNMasking];
	If[ NNXMaskJavaObjectQ[opMasking],
		tempMask = opMasking@getActiveMasksA[frameRange[[1]], frameRange[[2]], segment, xData];
		tempMask = 
			If[Length[Flatten[tempMask]]==0,  
				{},
				If[opNNTimeUnitMS, 
					{xData@tsToMs[#[[1]]], xData@tsToMs[#[[2]]]}& /@ tempMask,
					{(xData@tsToFrameSegmentA[#[[1]]])[[1]], (xData@tsToFrameSegmentA[#[[1]]])[[2]]}& /@ tempMask
				]
			],
		tempMask = {}
	];*)




(*NNTracePlot[nnData_/;NNJavaObjectQ[nnData, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			sampleRange_/;NNJavaObjectQ[sampleRange, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:= 

Block[{ optValueAbsolute, optTimeUnit, 
			optAspectRatio, optAxesLabels,
tempTracesWidth, 
		tempStackAmplitude, tempDataRange, tempMask,
		opNNTimeUnitMS, opNNAbsoluteValue,  opMasking },

	(*==========Handle unit options==========*)
	optValueAbsolute = Switch[ OptionValue[ NNValueUnit ],
		Absolute, True,
		x_String/;MemberQ[ {"absolute"}, ToLowerCase[x]], True,
		x_String/;MemberQ[ {"microv","\[Mu]v"}, ToLowerCase[x]], False,
		x_, Message[NNTracePlot::invalidOptionValue, "NNValueUnit", ToString[x]]; True
	];	
	optTimeUnit = Switch[ OptionValue[ NNTimeUnit ],
		Automatic, "ms",
		x_String/;MemberQ[ {"ms"}, ToLowerCase[x] ], "ms",
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, ToLowerCase[x] ], "timestamps",
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, ToLowerCase[x] ], "frames",
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; "ms"
	];	

	(*==========Data==========*)
	If[ optValueAbsolute,
		nnData@readTrace[channels, sampleRange],
		nnData@readTraceAbs[channels, sampleRange]
	];
    
(*	(*==========Data stacking==========*)
	tempStackAmplitude = 150;(*(Max[traces]-Min[traces];*)
*)
	(*==========Handle graphing options==========*)
	optAspectRatio = OptionValue[AspectRatio];
	If[ optAspectRatio === Automatic, optAspectRatio = (Length[channels]+1)*10];
	optAxesLabels = OptionValue[AxesLabel];
	If[ optAxesLabels === Automatic, 
		optAxesLabels = {optTimeUnit, If[optValueAbsolute, "bits", "\[Mu]v"]}];

	(*==========Plot==========*)
	ListLinePlot[ NNStackLists[traces, tempStackAmplitude, NNBaselineCorrection-> None],
			Sequence@@NNJoinOptionLists[ ListLinePlot,
				{  AxesLabel->{tempTimeUnit, tempDataUnit}, DataRange->tempDataRange, AspectRatio->opAspectRatio,
					PlotRange->{tempDataRange, {0, tempStackAmplitude*Length[channels]}}
				 },
				NNTracePlot$UniqueOptions
			]
	]
  
];*)


(* ::Subsection::Closed:: *)
(*NNTracePlotImpl*)


(*NNTracePlotImpl[
			nnData_/;NNJavaObjectQ[nnData, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			NNTimestamp[ startTs_Integer ], lengthFr_Integer, stepFr_Integer,
			hhListLinePlotStackOpts:OptionsPattern[]
]:= 
	HHListLinePlotStack[nnData@readTraces[channels, NN`NNRangeTsEvent[startTs, 0, lengthFr, stepFr] ], Sequence@@hhListLinePlotStackOpts];*)


(*NNTracePlotImpl[args___]:=Message[NNTracePlotImpl::invalidArgs,{args}];*)


(*$NNPrivateTPMTempOptStack::usage="Private variable";
$NNPrivateTPMTempGraphic::usage="Private variable";

NNTracePlotManipulate[
			dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			length_/;(length === Automatic || ( NumericQ[length]&& length > 0)), (*ToDo: add timestamp specification*)
			opts:OptionsPattern[]
]:= 
Block[{
	tempOptStack, tempGraphic, 
	tempInitialStart = 0, tempInitialSegment = 0, tempSegmentList
	},
	tempSegmentList = Table[n, {n, 0, dataObj@timing[]@segmentCount[]}];
	Print["!!!Always delete manipulate interface before proceeding!!!"];

Manipulate[
	With[{
	(*==========Handle time range options==========*)
	tempRangeFrames=
	Round[If[length === Automatic, 
		dataObj@timing[]@sampleRate[]/10,
		length
	]],
	
	(*==========Handle graphing options==========*)
	optStack = (
		tempOptStack=OptionValue[NNOptStack];
		If[ tempOptStack === Automatic, 
			tempOptStack = (dataObj@scaling[]@maxValue[]- dataObj@scaling[]@minValue[])/2;
		];
		tempOptStack=tempOptStack / OptionValue[NNOptAmplificationFactor]),
	
	tempStackTable = Table[-n*tempOptStack - tempOptStack/2, {n, 0, Length[channels]-1}],

	optAspectRatio = Length[channels]/20,
	optPlotRangeY = {-tempOptStack*Length[channels]-tempOptStack, 0}
	},
		tempGraphic=ListLinePlot[ 
				dataObj@readTrace[#, NN`NNRange[start, start + tempRangeFrames, 1 , 0]]& /@ channels + tempStackTable,
				AspectRatio -> optAspectRatio,
				DataRange -> {start, start+tempRangeFrames},
				PlotRange -> {Automatic, optPlotRangeY},
				ImageSize -> Full
		]
	],
	
	(*Controls*)
	{{segment, tempInitialSegment, "segment:"}, tempSegmentList},
	{{start, tempInitialStart, "frame:"}, 0, 32000*20, 32000/4}, 
	Row[{
		Button["Print vector", Print[tempGraphic]],
		Spacer[20],
		Button["Print bitmap", Print[Rasterize[tempGraphic]]](*,
		Button["Deactivate", FrontEndExecute[FrontEndToken[EvaluationNotebook[],"SelectionConvert", "Bitmap"]]]*)
	}],
	
	ContinuousAction -> False
]];*)


(*NNTracePlotManipulate[
			dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			length_/;(length === Automatic || ( NumericQ[length]&& length > 0)), (*ToDo: add timestamp specification*)
			opts:OptionsPattern[]
]:= 
Module[{tempRangeFrames,
		optStack, tempStackTable,
		optAspectRatio, optPlotRangeY},

	(*==========Handle time range options==========*)
	tempRangeFrames=
	Round[If[length === Automatic, 
		dataObj@timing[]@sampleRate[]/10,
		length
	]];

	(*==========Handle graphing options==========*)
	optStack = OptionValue[NNOptStack];
	If[ optStack === Automatic, 
		optStack = (dataObj@scaling[]@maxValue[]- dataObj@scaling[]@minValue[])/2;
	];
	optStack = optStack / OptionValue[NNOptAmplificationFactor];
(*	tempData = HHStackLists[tempData, optStack];*)
	tempStackTable = Table[-n*optStack - optStack/2, {n, 0, Length[channels]-1}];

	optAspectRatio = Length[channels]/20;
	optPlotRangeY = {-optStack*Length[channels]-optStack, 0};

	Manipulate[
		ListLinePlot[ 
				dataObj@readTrace[#, NN`NNRange[start, start + tempRangeFrames, 1 , 0]]& /@ channels + tempStackTable,
				AspectRatio -> optAspectRatio,
				DataRange \[Rule] {start, start+tempRangeFrames},
				PlotRange -> {Automatic, optPlotRangeY},
				ImageSize -> Full
			],
		{start, 0, 32000*20, 32000/4}, 
		ContinuousAction -> False
	]

(*	DynamicModule[{start = 0},
		Panel[Column[{
			Row[{Slider[Dynamic[start], {0, 32000*20, 32000/4}], Dynamic[start]}],
			ListLinePlot[ 
				dataObj@readTrace[#, NN`NNRange[start, start + tempRangeFrames, 1 , 0]]& /@ channels + tempStackTable,
				AspectRatio -> optAspectRatio,
				DataRange \[Rule] {start, start+tempRangeFrames},
				PlotRange -> {Automatic, optPlotRangeY},
				ImageSize -> Full
			]
		}]], 
	ContinuousAction -> False]*)
	
];*)


(*Attempt at Dynamic... coordination is too difficult?*)

(*NNTracePlotManipulate[
			dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			length_/;(length === Automatic || ( NumericQ[length]&& length > 0)), (*ToDo: add timestamp specification*)
			opts:OptionsPattern[]
]:= 
Block[{
	tempSegmentList, (*tempRangeFrames,*)
	tempOptStack, tempGraphic, (*tempStackTable,*)
	(*optAspectRatio, optPlotRangeY, *)
	tempInitialStart = 0, tempInitialSegment = 0
	},
	
	tempSegmentList = Table[n, {n, 0, dataObj@timing[]@segmentCount[]}];
	(*==========Handle time range options==========*)
	$NNTracePlotManipulate$tempRangeFrames =
	Round[If[length === Automatic, 
		dataObj@timing[]@sampleRate[]/10,
		length
	]];

	(*==========Handle graphing options==========*)
	tempOptStack=OptionValue[NNOptStack];
	If[ tempOptStack === Automatic, 
		tempOptStack = (dataObj@scaling[]@maxValue[]- dataObj@scaling[]@minValue[])/2;
	];
	tempOptStack = tempOptStack / OptionValue[NNOptAmplificationFactor];
	$NNTracePlotManipulate$tempStackTable = Table[-n*tempOptStack - tempOptStack/2, {n, 0, Length[channels]-1}];
	
	$NNTracePlotManipulate$optAspectRatio = Length[channels]/20;
	$NNTracePlotManipulate$optPlotRangeY = {-tempOptStack*Length[channels]-tempOptStack, 0};
		
	$NNTracePlotManipulate$Notebook = SelectedNotebook[];

	DynamicModule[{start, tempgr},
	CreateDialog[{
		TextCell["Enter a factor: "],
		Control[{start,0,dataObj@timing[]@segmentLength[0]}],
		(*InputField[Dynamic[start], Number],*)
		ExpressionCell[ (*Dynamic[tempGraphic=Plot[Sin[nm*x], {x,0, 4 Pi}, ImageSize-> 72*8]],*)
			tempgr = Dynamic[ListLinePlot[ 
				dataObj@readTrace[#, NN`NNRange[start, start + $NNTracePlotManipulate$tempRangeFrames, 1 , 0]]& 
						/@ channels + $NNTracePlotManipulate$tempStackTable,
				AspectRatio -> $NNTracePlotManipulate$optAspectRatio,
				DataRange -> {start, start+$NNTracePlotManipulate$tempRangeFrames},
				PlotRange -> {Automatic, $NNTracePlotManipulate$optPlotRangeY},
				ImageSize -> Full
				]]
		],
		Grid[{{
			Button["Print graph onto notebook",
			(*CellPrint[ExpressionCell[{nm}]]    : This does not output, probably tries to print in dialog*)
			(*NotebookWrite[$NNTracePlotManipulate$Notebook, Cell[ExpressionCell[{nm}],"Output"]];*)
			NotebookWrite[$NNTracePlotManipulate$Notebook,
				Cell[ BoxData[ToBoxes[tempgr]], "Output", CellTags-> {"testCT"}]
			]
		],
		Button["Print global variable to messages",Print[$NNTracePlotManipulate$Notebook]],
		Button["Delete test cell tag",NotebookDelete[NotebookFind[$NNTracePlotManipulate$Notebook,"testCT",Previous, CellTags][[1]]]],
		DefaultButton[DialogReturn["Hello"]]
		}}]
	}, Modal\[Rule]True, WindowTitle\[Rule]"NNTracePlotManipulate: " <> dataObj@toString[]
	](*CreateDialog*)];
	Print["done"]
];*)
