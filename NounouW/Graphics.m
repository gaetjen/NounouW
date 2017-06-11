(* ::Package:: *)

(* Mathematica Package *)
BeginPackage[ "NounouW`Graphics`", 
	{"HokahokaW`", "HokahokaW`Graphics`", "JLink`", "NounouW`","NounouW`Data`"}
];


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNDetectorPlot/NNDetectorInsetPlot*)


NNDetectorPlot::usage="Plots detector field.";
NNDetectorInsetPlot::usage="";
NNHexagon::usage="";


NNDetectorText::usage="Option for {NNDetectorPlot}: Whether to plot text";
NNDetectorTextFunction::usage="Option for {NNDetectorPlot}: function to use to plot text, #1 is text, #2 is coordinate.
 Requires NNDetectorText->True";


Options[NNDetectorPlot]=
	HHJoinOptionLists[
		{NNDetectorText->True, NNDetectorTextFunction -> (Text[Style[#1, Medium], #2, {0, 0}]&), ColorFunction -> ColorData["Rainbow"] },
		Options[Graphics]
	];


(* ::Subsection:: *)
(*NNDetectorComposePlot*)


NNDetectorComposePlot::usage=
"Takes a list of plots/images and a layout object and arranges the plots\
according to the layout. Uses ImageCompose and returns an Image object.";


(* ::Subsection::Closed:: *)
(*NNTraceOverviewPlot*)


NNTraceOverviewPlot::usage="";


NNTraceOverviewPlot$UniqueOptions = {
	NNTraceOverviewPlotCuttingInterval -> 10*60
};
NNTraceOverviewPlot$OverrideOptions = {
	AspectRatio -> 1/10, PlotStyle->{Opacity[0.5]}, AxesLabel->Automatic,
	PlotRange->Automatic, ImageSize->10*72
};

Options[NNTraceOverviewPlot] = HHJoinOptionLists[ 
	NNTraceOverviewPlot$UniqueOptions, NNTraceOverviewPlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*NNDetectorPlot*)


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], opts:OptionsPattern[]]:=
Module[{tempret, 
		channelCount, channelRadius,
		textFunc},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];
		
	tempret=Table[
		{If[ layoutObj@isMasked[n],
				{Gray, Disk[layoutObj@getChannelCoordinates[n], channelRadius ]}       ],
			{Circle[ layoutObj@getChannelCoordinates[n], channelRadius ]}
		},
		{n, 0, channelCount-1}
	];
	tempret = If[ OptionValue[NNDetectorText], 
		(*append text notations if NNDetectorText\[Rule]True*)
		textFunc = OptionValue[NNDetectorTextFunction];
		Append[tempret, 
			Table[ 
				{Black, textFunc[n, layoutObj@getChannelCoordinates[n]]},
				{n, 0, channelCount-1}
			]
		],
		(*return original if NNDetectorText\[Rule]False*)
		tempret 
	];

	Graphics[tempret, Sequence@@FilterRules[{opts}, Options[Graphics]]]
];


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], data_List, opts:OptionsPattern[]]:=
Module[{channelCount, channelRadius,
		tempGraphic},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];
	tempGraphic=If[channelCount < Length[data],
		Message[NNDetectorPlot::invalidChCount, channelCount, Length[data]];
		{},
		Table[ 
			{If[ layoutObj@isMasked[n],
				{Black, Circle[layoutObj@getChannelCoordinates[n], channelRadius ]},
				{(*EdgeForm[None],*)OptionValue[ColorFunction][data[[n+1]]],NNHexagon[ layoutObj@getChannelCoordinates[n], channelRadius*2/Sqrt[3] ]}
			]},
			{n, 0, channelCount-1}
		]
	];
		
	Graphics[tempGraphic, Sequence@@FilterRules[{opts}, Options[Graphics]]]
];

NNDetectorPlot::invalidChCount="Channel count for layout object (`1`) must be longer than data length (`2`)";


NNDetectorPlot[args___]:=Message[NNDetectorPlot::invalidArgs, {args}];


NNHexagon[{x_, y_}, r_]:= Polygon[ Table[{x,y}+r*{Sin[theta], Cos[theta]},{theta, Pi/3, 7 Pi/3, Pi/3}]];


NNHexagon[args___]:=Message[NNHexagon::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNDetectorInsetPlot*)


NNDetectorInsetPlot[insetList_List,
					layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], 
					opts:OptionsPattern[]]:=
Module[{tempReturn, 
		channelCount, channelRadius,
		textFunc},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];

	If[ channelCount < Length[insetList],
		Message[NNDetectorInsetPlot::notEnoughChannels, channelCount, Length[insetList]];
		Null,
		
		If[ channelCount > Length[insetList], NNDetectorInsetPlot::insetsNotForAllChannels, channelCount, Length[insetList]];
		tempReturn=
		MapThread[
			If[ layoutObj@isMasked[#1],
					{Gray, Disk[layoutObj@getChannelCoordinates[#1], channelRadius ]},
					{Inset[ #2, layoutObj@getChannelCoordinates[#1], Center, channelRadius*1.75 ]}
			]&,
			{Range[ Length[insetList] ]-1, insetList}
		];
		Graphics[tempReturn, Sequence@@FilterRules[{opts}, Options[Graphics]]]

	]
	
];

NNDetectorInsetPlot::notEnoughChannels="Not enough channels in layout object (`1`) to display all insets (`2`)";
NNDetectorInsetPlot::insetsNotForAllChannels="Not all channels in layout object (getChannelCount = `1`) will have insets (Length = `2`)";


NNDetectorInsetPlot[args___]:=Message[NNDetectorInsetPlot::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNDetectorComposePlot*)


NNDetectorComposePlot[pltLst_List,
					layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial]]:=
Module[
	{rtnImg, 
	channelCount, channelRadius,
	bg,
	coords,
	pltDim,
	maxc,
	xdist, ydist,
	layoutImg},
	channelCount = layoutObj@getChannelCount[];
	(*get the "default" channel coordinates*)
	coords =  layoutObj@getChannelCoordinates[#]& /@ (Range[channelCount] - 1);
	(*set minimum x and y to 0*)
	coords = # - Min /@ (coords\[Transpose])& /@ coords;
	(*calculate distances between channels*)
	channelRadius = layoutObj@getChannelRadius[];
	xdist = 2 * channelRadius;
	ydist = Sqrt[xdist^2-channelRadius^2];
	
	coords = # / {xdist, ydist}& /@ coords;
	(*determine plot image size*)
	pltDim = (ImageData[Image[#]]& /* Dimensions /* Most /* Reverse )[pltLst[[1]]];
	(*scale coordinates according to image*)
	coords = Map[#* pltDim&, coords];
	maxc = Max /@ (coords\[Transpose]) + pltDim ;
	(*create the "canvas" on which to add the plots*)
	bg =  Image[ConstantArray[0.95, Ceiling[maxc]]];

	If[ channelCount < Length[pltLst],
		Message[NNDetectorComposePlot::notEnoughChannels, channelCount, Length[pltLst]],
		If[ channelCount > Length[pltLst],
			Message[NNDetectorComposePlot::insetsNotForAllChannels, channelCount, Length[pltLst]],
			rtnImg= (*incrementally add the plot images at the correct coordinates to the background*)
				Fold[ImageCompose[
					#1,
					#2[[2]],
					#2[[1]] ,
					{0, 0}
					]&,
				bg,
				{coords,pltLst}\[Transpose]
				];
			(*adding the channels layout in the upper left corner*)
			layoutImg = Image[NNDetectorPlot[layoutObj]];
			ImageCompose[rtnImg, {layoutImg, 0.3}, Scaled[{0, 1}], Scaled[{0, 1}]]
		]
	]
];


(* ::Subsection::Closed:: *)
(*NNTraceOverviewPlot*)


NNTraceOverviewPlot[nnData_/;NNJavaObjectQ$NNData[nnData],
			channels:{_Integer ..}, 
			sampleRange_/;NNJavaObjectQ$NNSampleRangeSpecifier[sampleRange], 
			opts:OptionsPattern[]]:= 

Block[{ optCuttingInterval,

		tempStackAmplitude, tempDataRange, tempMask,
		opNNTimeUnitMS, opNNAbsoluteValue,  opMasking },

	(*==========Handle options==========*)
	optCuttingInterval = OptionValue[ NNTraceOverviewPlotCuttingInterval ];

	(*==========Read timestamps==========*)
	tempStart =
	tempEnd

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
  
];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
