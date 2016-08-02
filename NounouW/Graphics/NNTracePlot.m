(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`NNTracePlot`", 
	{"HokahokaW`","HokahokaW`Graphics`","JLink`",
	"NounouW`","NounouW`Data`"}
];


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNTracePlotManipulate*)


NNTracePlotManipulate::usage=
"NNTracePlotManipulate provides a simple interface to view trace data in a simple interactive interface.";


NNTracePlotManipulate$UniqueOptions = {
	NNOptTimeUnit -> "ms", NNOptStack -> Automatic
};
NNTracePlotManipulate$OverrideOptions = {
	AspectRatio -> Automatic, PlotStyle -> {Opacity[0.75]}, AxesLabel -> Automatic,
	PlotRange -> Automatic, ImageSize -> Full
};
Options[NNTracePlotManipulate] = HHJoinOptionLists[ 
	NNTracePlotManipulate$UniqueOptions, 
	NNTracePlotManipulate$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*NNTracePlot*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


NNTracePlot$UniqueOptions = {
	(*NNValueUnit -> Absolute, ScaleBars->{None, None}, *)  
	(*NNBaselineCorrection->Mean,*) NNOptTimeUnit -> "ms", NNOptStack -> Automatic
	(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio -> Automatic, PlotStyle->{Opacity[0.75]}, AxesLabel->Automatic,
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};
Options[NNTracePlot] = HHJoinOptionLists[ 
	NNTracePlot$UniqueOptions, NNTracePlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNTracePlotManipulate*)


NNTracePlotManipulate[
			dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			opts:OptionsPattern[]
]:= 
Block[{tempStack = 200, tempStackTable,
		optAspectRatio, optPlotRangeY},

	optAspectRatio = Length[channels]/20;
	optPlotRangeY = {-tempStack*Length[channels]-tempStack, 0};
	tempStackTable = Table[-n*tempStack - tempStack/2, {n, 0, Length[channels]-1}];

	DynamicModule[{start},
		Panel[Column[{
			Row[{Slider[Dynamic[start], {0, 32000*20, 32000/4}], Dynamic[start]}],
			ListLinePlot[ 
				dataObj@readTrace[#, NN`NNRange[start, start+ 32000, 1 , 0]]& /@ channels + tempStackTable,
				AspectRatio -> optAspectRatio,
				PlotRange -> {Automatic, optPlotRangeY},
				ImageSize -> Full
			]
		}]], 
	ContinuousAction -> False]

];


(*This signature will reshape for a single channel*)
NNTracePlotManipulate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel:Integer, 
			rest___]:= NNTracePlotManipulate[dataObj, {channel}, rest];


(*This signature will reshape for all channels*)
NNTracePlotManipulate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			All, 
			rest___]:= NNTracePlotManipulate[dataObj, Range[0, dataObj@getChannelCount[] - 1], rest];


NNTracePlotManipulate[args___]:=Message[NNTracePlotManipulate::invalidArgs,{args}];


(* ::Subsection:: *)
(*NNTracePlot*)


(*Open up one-element lists*)
NNTracePlot[{dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]}, rest___]:= 
	NNTracePlot[dataObj, rest];


(*Main plotting class*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_:{_Integer ..}, 
			range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:= 

Block[{ optTimeUnit, optAspectRatio, optAxesLabels,
		tempData, tempDataUnit, tempTimepoints},

	(*==========Handle unit options==========*)
	optTimeUnit = OptionValue[ NNOptTimeUnit ];
(*	
	optTimeUnit = Switch[ optTimeUnit,
		Automatic, "ms",
		x_String/;MemberQ[ {"ms"}, ToLowerCase[x] ], "ms",
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, ToLowerCase[x] ], "Timestamps",
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, ToLowerCase[x] ], "Frames",
		_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[optTimeUnit]]; "ms"
	];*)	

	(*==========Data==========*)
	tempData = NNReadTrace[ dataObj, channels, range];
	tempTimepoints = NNReadTimepoints[ dataObj, range, optTimeUnit ];
	tempDataUnit = dataObj@getUnit[];

	(*==========Handle graphing options==========*)
	tempData = HHStackLists[tempData, HHOptStack ->  OptionValue[NNOptStack]];

	(*==========Plot==========*)
	ListLinePlot[ 
			Transpose[{tempTimepoints,#}]& /@ tempData, 
			Sequence@@HHJoinOptionLists[ ListLinePlot,
				{opts},
				{  AxesLabel-> {optTimeUnit, tempDataUnit} },
				NNTracePlot$UniqueOptions
			]
	]
  
];


(*This signature will reshape for a single channel*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel:Integer, 
			rest___]:= NNTracePlot[dataObj, {channel}, rest];


(*This signature will reshape for all channels*)
NNTracePlot[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			All, 
			rest___]:= NNTracePlot[dataObj, Range[0, dataObj@getChannelCount[] - 1], rest];


(*This signature will realize the range if it is specified in Mathematica style*)
NNTracePlot[nnDataObj_/;NNJavaObjectQ[nnDataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			range_, 
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


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]


(* ::Section:: *)
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


(* ::Subsection:: *)
(*NNTracePlotImpl*)


(*NNTracePlotImpl[
			nnData_/;NNJavaObjectQ[nnData, $NNJavaClass$NNData], 
			channels:{_Integer ..}, 
			NNTimestamp[ startTs_Integer ], lengthFr_Integer, stepFr_Integer,
			hhListLinePlotStackOpts:OptionsPattern[]
]:= 
	HHListLinePlotStack[nnData@readTraces[channels, NN`NNRangeTsEvent[startTs, 0, lengthFr, stepFr] ], Sequence@@hhListLinePlotStackOpts];*)


(*NNTracePlotImpl[args___]:=Message[NNTracePlotImpl::invalidArgs,{args}];*)
