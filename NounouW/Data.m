(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Data`", {"HokahokaW`", "JLink`", "NounouW`"}];


(* ::Section:: *)
(*Declarations*)


NNOptMask::usage="Option for functions such as NNTracePlot, to specify area of masking.";


(* ::Subsection::Closed:: *)
(*Data related markers*)


NNRange::usage="Marker for specifying data range and segment (e.g. NNRange[0 ;; 3200, 1])"; 


NNSegment::usage="Marker for a rule specifying the relevant data segment (e.g. NNSegment \[Rule] 0)"; 


(* ::Subsection::Closed:: *)
(*Data markers/specifier related converters*)


NNConvert::usage="Converts between (time) units.";
Options[NNConvert] = {NNSegment -> Automatic};


NNTimestamp::usage="Simple marker for specifying time entries as timestamps (not frames), use as \"NNTimestamp @ 1000000\" or \"NNTimestamp[1000000]\" .";
Ts::usage="Alias for NNTimestamp, especially useful in postfix form \"t // Ts\".";

NNMillisecond::usage="Simple marker for specifying time entries as milliseconds within a segment, use as \"NNMillisecond @ 1000000\" or \"NNMillisecond[1000000]\" .";
Ms::usage="Alias for NNMillisecond, especially useful in postfix form \"t // Ms\".";

NNFrame::usage="Simple marker for specifying time entries as frames, which is actually\
default for NounouuW and therefore not necessary. However, this can be sent as a token\
for options such as NNOptTimeUnit to specify plotting in frames.";


$ToNNRangeSpecifier::usage =
"Converts a Mathematica-style range specification to Nounou Java object. Returns $Failed if invalid.";


NNTimeMarkerToString::usage = "Converts time markers to string.";


(* ::Subsection::Closed:: *)
(*File Access (NNLoad, NNSave, NNFilenameSort)*)


NNLoad::usage="Load data object(s) from file(s).";
Options[NNLoad] = {NNOptFileNameSort -> True};


NNSave::usage="Save data object(s) to a file.";


NNFilenameSort::usage="Sorts data filenames based on trailing digits, \
which may not be straight forward due to lack of zero padding. \
For example, XXX\\CSC2.ncs => XXX\\CSC10.ncs => XXX\\CSC20.ncs";


(* ::Subsection:: *)
(*NNData Accessors*)


NNPrintInfo::usage =
"Prints out java object information for an NNElement child class. When called without argument,\
redirects to toStringFull[]. The following arguments can be given for what to print:\n          \
+ NNData: \"Timing\", \"Scaling\", \"Layout\"";


NNReadInfo::usage =
"Reads java object information for an NNElement child class. \
The following arguments can be given for what to read:\n          \
+ NNData: \"ChannelCount\", \"SegmentCount\"\n          \
+ NNLayout: \"ChannelCount\"";


(* ::Subsubsection:: *)
(*Continuous*)


NNReadTimepoints::usage =
"Reads a list of timepoints for a given data/timing object. Use for {x,y} plotting.";


(*NNOptTimepointUnit::usage="Specifies which units to use for timepoints. \"Frames\", \"ms\", or \"Timestamps\".";*)


NNReadTrace::usage="Reads a trace of data from a channel(s)";

Options[NNReadTrace] = {};


NNReadPage::usage="";

Options[NNReadPage] = {(*NNOptReturnTimepoints -> True*)};


(* ::Subsubsection:: *)
(*Discrete*)


NNReadEvents::usage="";
NNOptDurationEvents::usage="";

Options[NNReadEvents] = {
	NNOptDurationEvents -> True
};

NNReadTimestamps::usage="";
Options[NNReadTimestamps] = {
	NNOptDurationCheck -> False
};


(* ::Subsection::Closed:: *)
(*NNData and NNDataChannels*)


NNData::usage =
"Wrap an array of NNDataChannel object(s) to use (together) as a regular NNData object.";


NNDataChannels::usage =
"Decompose a NNData object to a List (Array) of NNDataChannel objects for individual use.";


NNDataChannel::usage =
"Extract a single specific NNDataChannel object from an NNData object.";


(* ::Subsection:: *)
(*NNFilter methods*)


NNFilterDownsample::usage="";
NNFilterDecimate::usage="";
NNFilterMedianSubtract::usage="";
NNFilterFIR::usage="";
NNFilterBuffer::usage="";
NNFilterTrodeRereference::usage="";
NNFilterMasked::usage="";
NNFilterMean::usage="";
NNFilterAppendCalculatedChannels::usage="";
	NNOptAppendCalculationType::usage="";(*
	Options[NNFilterAppendCalculatedChannels]={NNOptAppendCalculationType \[Rule] NNOpt`NNOptAppendAbsSum}*);


(* ::Subsection:: *)
(*NNToList*)


(*NNToList::usage="Import data objects into Mathematica List.";*)


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Data markers/specifier related converters*)


(* ::Subsubsection::Closed:: *)
(*NNConvert*)


$NNConvert$StringToUnitMarker[string_String]:=
Switch[ ToLowerCase[string],
		x_String/;MemberQ[ {"ms", "milliseconds", "millisecond"}, x ], 
			NNMillisecond,
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, x ], 
			NNTimestamp,
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, x ], 
			NNFrame,
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; NNFrame
	];
$NNConvert$StringToUnitMarker[type_]:= type;
$NNConvert$StringToUnitMarker[args___]:=Message[$NNConvert$StringToUnitMarker::invalidArgs, {args}];


NNConvert[dataObj_, times_, optUnit_String, opts:OptionsPattern[]]:=
NNConvert[dataObj, times, $NNConvert$StringToUnitMarker[optUnit], opts];


NNConvert[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
		timestamps_NNTimestamp/;(Head[timestamps[[1]]]===List),
		optUnit_String, opts:OptionsPattern[]
]:= Switch[ optUnit,
		NNMillisecond, Round[dataObj@timing[]@convertTsToMs[#]]& /@ timestamps[[1]],
		NNTimestamp, timestamps[[1]],
		NNFrame, (dataObj@timing[]@convertTsToFrsgArray[#][[1]])& /@ timestamps[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[optUnit]]; {}
	];


NNConvert[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
		milliseconds_NNMillisecond/;(Head[milliseconds[[1]]]===List),
		optUnit_, opts:OptionsPattern[]
]:= Switch[ optUnit,
		NNMillisecond, milliseconds[[1]],
		NNTimestamp,
			If[OptionValue[NNSegment]===Automatic,
				If[ dataObj@timing[]@segmentCount[] == 1, 
					(dataObj@timing[]@convertMssgToTs[#, 0])& /@ milliseconds[[1]],
					Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", 
						ToString[x]<>", timestamps cannot be generated from ms without segment specification, if there is more than 1 segment."
					];{}
				],
				(dataObj@timing[]@convertMssgToTs[#, OptionValue[NNSegment]])& /@ milliseconds[[1]]
			],
		NNFrame, (dataObj@timing[]@convertMsToFr[#])& /@ milliseconds[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[optUnit]]; {}
	];


(*NNConvert[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
		timestamps_NNTimestamp/;(Head[timestamps[[1]]]===List),
		optUnit_String, opts:OptionsPattern[]
]:= Switch[ ToLowerCase[optUnit],
		x_String/;MemberQ[ {"ms"}, x ], 
			Round[dataObj@timing[]@convertTsToMs[#]]& /@ timestamps[[1]],
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, x ], 
			timestamps[[1]],
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, x ], 
			(dataObj@timing[]@convertTsToFrsgArray[#][[1]])& /@ timestamps[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; {}
	];*)


(*NNConvert[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
			milliseconds_NNMillisecond/;(Head[milliseconds[[1]]]===List),
			optUnit_String, 
			opts:OptionsPattern[]
]:= Switch[ ToLowerCase[optUnit],
		x_String/;MemberQ[ {"ms"}, x ], 
			milliseconds[[1]],
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, x ],
			If[OptionValue[NNSegment]===Automatic,
				If[ dataObj@timing[]@segmentCount[] \[Equal] 1, 
					(dataObj@timing[]@convertMssgToTs[#, 0])& /@ milliseconds[[1]],
					Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", 
						ToString[x]<>", timestamps cannot be generated from ms without segment specification, if there is more than 1 segment."
					];{}
				],
				(dataObj@timing[]@convertMssgToTs[#, OptionValue[NNSegment]])& /@ milliseconds[[1]]
			],
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, x ], 
			(dataObj@timing[]@convertMsToFr[#])& /@ milliseconds[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; {}
	];*)


NNConvert[args___]:=Message[NNConvert::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNTimestamp, Ts (simple marker)*)


(*NNTimestamp[ timestamps:{_Real ..} ]:= NNTimestamp /@ timestamps;*)


(*NNTimestamp[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
			timestamps_NNTimestamp/;(Head[timestamps[[1]]]===List),
			(*NNTimestamp[timestamps:{__Real}],*)
			optUnit_String
]:= Switch[ ToLowerCase[optUnit],
		x_String/;MemberQ[ {"ms"}, x ], 
			Round[dataObj@timing[]@convertTsToMs[#]]& /@ timestamps[[1]],
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, x ], 
			timestamps[[1]],
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, x ], 
			(dataObj@timing[]@convertTsToFrsgArray[#][[1]])& /@ timestamps[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; {}
	];*)


(*NNTimestamp[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
			timestamp_Real]:= timestamp;*)


(*NNTimestamp[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
			timestamps_List]:= 
NNTimestamp[dataObj, #]& /@ timestamps;*)


NNTimestamp[]:=Message[NNTimestamp::invalidArgs, {}];
NNTimestamp[arg1_, arg2__]:=Message[NNTimestamp::invalidArgs, {arg1, arg2}];


Ts[ something_ ]:= NNTimestamp[ something ];
Ts[args___]:=Message[Ts::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNMillisecond, Ms (simple marker)*)


(*NNMillisecond[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
			milliseconds_NNMillisecond/;(Head[milliseconds[[1]]]===List),
			optUnit_String
]:= Switch[ ToLowerCase[optUnit],
		x_String/;MemberQ[ {"ms"}, x ], 
			milliseconds[[1]],
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, x ], 
			Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", 
				ToString[x]<>", timestamps cannot be generated from ms without segment specification."]; 
			{},
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, x ], 
			(dataObj@timing[]@convertMsToFr[#])& /@ milliseconds[[1]],
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; {}
	];*)


NNMillisecond[]:=Message[NNMillisecond::invalidArgs, {}];
NNMillisecond[arg1_, arg2__]:=Message[NNMillisecond::invalidArgs, {arg1, arg2}];


Ms[ something_ ]:= NNMillisecond[ something ];
Ms[args___]:=Message[Ms::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*$ToNNRangeSpecifier*)


(* NNRange[ start;;last , segment ]  *)
$ToNNRangeSpecifier[ NNRange[Span[start_/;NumberQ[start], last_/;NumberQ[last]], segment_Integer] ]:= 	
	NN`NNRange[Round[start], Round[last], 1, Round[segment]];

(* NNRange[ start;;last , NNSegment \[Rule] segment]  *)
$ToNNRangeSpecifier[ NNRange[Span[start_/;NumberQ[start], last_/;NumberQ[last]],  Rule[ NNSegment, segment_Integer]] ]:= 	
	NN`NNRange[Round[start], Round[last], 1, Round[segment]];

(* NNRange[ start;;last;;step , segment] *)
$ToNNRangeSpecifier[ NNRange[Span[start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step] ],  segment_/;NumberQ[segment]] ]:= 
	NN`NNRange[Round[start], Round[last], Round[step], Round[segment]];


(* { start;;last , segment}  *)
$ToNNRangeSpecifier[ {Span[start_/;NumberQ[start], last_/;NumberQ[last]], segment_Integer} ]:= 	
	NN`NNRange[Round[start], Round[last], 1, Round[segment]];

(* { start;;last , NNSegment \[Rule] segment}  *)
$ToNNRangeSpecifier[ {Span[start_/;NumberQ[start], last_/;NumberQ[last]],  Rule[ NNSegment, segment_Integer]} ]:= 	
	NN`NNRange[Round[start], Round[last], 1, Round[segment]];

(* { start;;last;;step , segment} *)
$ToNNRangeSpecifier[ {Span[start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step] ],  segment_/;NumberQ[segment]} ]:= 
	NN`NNRange[Round[start], Round[last], Round[step], Round[segment]];


(*  NNTimestamp[timestamp] -> start;;last  *)
$ToNNRangeSpecifier[ 
	Rule[ timestamp_NNTimestamp, 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last] ]
	]
]:= NN`NNRangeTsEvent[timestamp[[1]], Round[start], Round[last], 1];

(*  timestamp -> start;;last;;step  *)
$ToNNRangeSpecifier[ 
	Rule[ timestamp_NNTimestamp, 
		  Span[ start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step]]
	]
]:= NN`NNRangeTsEvent[timestamp[[1]], Round[start], Round[last], Round[step]];


(*  {timestamps} -> start;;last  *)
$ToNNRangeSpecifier[ 
	Rule[ timestamps_:{_NNTimestamps ..}, 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last]	]
	]
]:= NN`NNRangeTsEvent[#, Round[start], Round[last], 1]& /@ timestamps;

(*  {timestamps} -> start;;last;;step  *)
$ToNNRangeSpecifier[ 
	Rule[ timestamps_:{_NNTimestamps ..}, 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step] 	]
	]
]:= NN`NNRangeTsEvent[#, Round[start], Round[last], Round[step]]& /@ timestamps;


(* All *)
$ToNNRangeSpecifier[ {All, segment_Integer} ] := NN`NNRangeAll[1, segment];
$ToNNRangeSpecifier[ NNRange[All, segment_Integer] ] := NN`NNRangeAll[1, segment];
$ToNNRangeSpecifier[ {All, Rule[ NNSegment, segment_Integer]} ] := NN`NNRangeAll[1, segment];
$ToNNRangeSpecifier[ All ] := NN`NNRangeAll[];


$ToNNRangeSpecifier[args___] := (Message[$ToNNRangeSpecifier::invalidArgs2, {args}]; $Failed);
$ToNNRangeSpecifier::invalidArgs2 = "`1` is not a correctly formatted span specification!";


(* ::Subsubsection:: *)
(*NNTimeMarkerToString*)


NNTimeMarkerToString[NNMillisecond]:= "ms";
NNTimeMarkerToString[NNTimestamp]:= "ts";
NNTimeMarkerToString[NNFrame]:= "fr";
NNTimeMarkerToString[string_String]:= $NNConvert$StringToUnitMarker[string];


NNTimeMarkerToString[args___]:=Message[NNTimeMarkerToString::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*File Access (NNLoad, NNSave, NNFilenameSort)*)


(* ::Subsubsection::Closed:: *)
(*NNLoad*)


NNLoad[fileName_String, opts:OptionsPattern[]]:=NNLoad[{fileName}, opts];

NNLoad[fileNames:{__String}, opts:OptionsPattern[]]:=
Module[{tempret, optSort},
	optSort = OptionValue[NNOptFileNameSort];
		
	tempret = NN`load[
				If[ optSort, NNFilenameSort[ fileNames ], fileNames]
	];
	If[ Head[tempret]===List && Length[tempret]==1, tempret[[1]], tempret ]
];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNSave*)


NNSave[fileName_String, obj_/;NNJavaObjectQ[obj, $NNJavaClass$NNElement], opts:OptionsPattern[]]:=
NNSave[fileName, {obj}, opts];


NNSave[fileName_String, objList_List/;NNJavaObjectListQ[objList, $NNJavaClass$NNElement], opts:OptionsPattern[]]:=
Module[{tempret, tempFileName},
	(*If directory name is not given, go with current directory*)
	tempFileName = If[ DirectoryName[fileName] == "", 
		FileNameJoin[ {Directory[], fileName} ], 
		fileName
	];
	tempret = NN`save[tempFileName, objList]
];


NNSave[args___]:=Message[NNSave::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNFilenameSort*)


NNFilenameSort[fileNames:{__String}]:=
	SortBy[fileNames,
			StringCases[ #,
					Shortest[__] ~~ x:NumberString~~"."~~WordCharacter ..  :> ToExpression[x]
			]&
	];


NNFilenameSort[args___]:=Message[NNFilenameSort::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNData Accessors*)


(* ::Subsubsection::Closed:: *)
(*NNPrintInfo*)


NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNElement]]:= dataObj@toStringFull[];
NNPrintInfo[dataObj_/;JavaObjectQ[dataObj]]:= dataObj@toString[];

NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement], "Timing"]:= dataObj@getTiming[]@toStringFull[];
NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement], "Scaling"]:= dataObj@getScaling[]@toStringFull[];
NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement], "Layout"]:= dataObj@getLayout[]@toStringFull[];


NNPrintInfo[args___]:=Message[NNPrintInfo::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNReadInfo*)


NNReadInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], "ChannelCount"]:= dataObj@channelCount[];
NNReadInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNLayout], "ChannelCount"]:= dataObj@channelCount[];

NNReadInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], "SegmentCount"]:= dataObj@timing[]@segmentCount[];


NNReadInfo[args___]:=Message[NNReadInfo::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNReadTimepoints*)


(*type specified as string*)
NNReadTimepoints[ 
	timingObj_/;NNJavaObjectQ[timingObj, $NNJavaClass$NNTimingElement],
	range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier],
	type_String
]:= NNReadTimepoints[timingObj, range, $NNConvert$StringToUnitMarker[type]];


(*Main definition, range specified as java object*)
NNReadTimepoints[ 
	timingObj_/;NNJavaObjectQ[timingObj, $NNJavaClass$NNTimingElement],
	range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier],
	type_:NNTimestamp
]:=
	Switch[type,
		NNMillisecond, range@readTimepointsMs[ timingObj ],
		NNTimestamp, range@readTimepointsTs[ timingObj ],
		NNFrame, range@readTimepoints[ timingObj ],
		x_, Message[NNReadTimepoints::invalidArgs, "timeunit: " <> ToString[x]];
		    range@readTimepoints[ timingObj ]
	];


(*Deal with NounouW time range specifications*)
NNReadTimepoints[ 
	timingObj_/;NNJavaObjectQ[timingObj, $NNJavaClass$NNTimingElement], 
	range_, 
	rest___] := 
NNReadTimepoints[timingObj, $ToNNRangeSpecifier[range], rest];


NNReadTimepoints[args___]:=Message[NNReadTimepoints::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNReadTrace*)


NNReadTrace[
	dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
	channels:{_Integer ..}, 
	range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
	opts:OptionsPattern[]
]:=
	dataObj@readTrace[#, range]& /@ channels;


NNReadTrace[
	dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
	range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
	opts:OptionsPattern[]
]:=
	dataChannelObj@readTrace[range];
(*(*This signature immediately corrects positional arguments so that the signature is the same for dataChannel and dataChannelObject*)
NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			range_, 
			opts:OptionsPattern[]]:=
NNReadTrace[dataChannelObj, {0}, range, opts];*)


(*Beware of the order of the following section, it is critical to get right fallthrough!*)

(*Open up one-element lists*)
NNReadTrace[{data_}, rest___]:= NNReadTrace[data, rest];

(*Open up All for channels*)
NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], All, rest___]:= 
	NNReadTrace[dataObj, Range[0, dataObj@getChannelCount[] - 1], rest];

(*Open up single channel to list*)
NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], channel_Integer, rest___]:= 
	NNReadTrace[dataObj, {channel}, rest][[1]];

(*Open up Mathematica-style range specification*)
NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels:{_Integer ..}, range_, opts:OptionsPattern[]]:= 
	NNReadTrace[dataObj, channels, $ToNNRangeSpecifier[range], opts];
NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			range_, opts:OptionsPattern[]]:= 
	NNReadTrace[dataChannelObj, $ToNNRangeSpecifier[range], opts];


NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*NNReadPage*)


(*Expand "All" for channels*)
NNReadPage[ 
	dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData],
	All, 
	range_, opts:OptionsPattern[]]:= 
NNReadPage[dataObj, Range[dataObj@getChannelCount[]]-1, range, opts];


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_, 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $ToNNRangeSpecifier[range], opts];

NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_List/;(Head[range[[1]]===Span]), 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $ToNNRangeSpecifier[range], opts];


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadPage[dataObj, channels, #, opts]& /@ rangeList;


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_, 
			opts:OptionsPattern[]]:= 
NNReadPage[dataObj, channels, $ToNNRangeSpecifier[range], opts];


(*NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_List/;(Head[range[[1]]===Span]), 
			opts:OptionsPattern[]]:= 
NNReadPage[dataObj, channels, $ToNNRangeSpecifier[range], opts];*)


(*Main definition*)
NNReadPage[
	dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
	channels_List/;And@@(NumberQ/@channels), 
	range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
	opts:OptionsPattern[]]:=
Module[{(*optTimepoints, *)tempTimepoints, tempTrace},

(*	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints===True, optTimepoints="Frames" ];

	If[optTimepoints === Null || optTimepoints === None || optTimepoints === False,
		dataObj@readPage[Round[channels], range],
		Prepend[ dataObj@readPage[Round[channels], range], NNReadTimepoints[range, dataObj, optTimepoints] ]
	]*)
	dataObj@readPage[Round[channels], range]

];


(*NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadPage[dataObj, #, opts]& /@ rangeList;*)


(*NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints===True, optTimepoints="Frames" ];

	If[optTimepoints === Null || optTimepoints === None || optTimepoints === False,
		dataObj@readPage[range],
		Prepend[ dataObj@readPage[range], NNReadTimepoints[range, dataObj, optTimepoints] ]
	]
];*)


NNReadPage[args___]:=Message[NNReadPage::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNData and NNDataChannels*)


NNData[dataChannelObjs_/;NNJavaObjectListQ[dataChannelObjs, $NNJavaClass$NNDataChannel]]:=
	JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObjs];

NNData[args___]:=Message[NNData::invalidArgs, {args}];


NNDataChannel[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], number_Integer]:=
	dataObj@extractNNDataChannel[number];

NNDataChannel[args___]:=Message[NNDataChannel::invalidArgs, {args}];


NNDataChannels[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]]:=
	dataObj@extractNNDataChannels[];
NNDataChannels[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel]]:=
	{dataObj};

NNDataChannels[args___]:=Message[NNDataChannels::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNFilterXXX*)


NNFilterDownsample[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
				initialFactor_Integer:16, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterDownsample, dataObj, initialFactor];
	tempret
];

NNFilterDownsample[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:16, opts:OptionsPattern[]]:=
	(NNFilterDownsample[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, {dataChannelObj}], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterDownsample[args___]:=Message[NNFilterDownsample::invalidArgs, {args}];


NNFilterDecimate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], initialFactor_Integer:16, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterDecimate, dataObj, initialFactor];
	tempret
];

NNFilterDecimate[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:16, opts:OptionsPattern[]]:=
	(NNFilterDecimate[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterDecimate[args___]:=Message[NNFilterDecimate::invalidArgs, {args}];


NNFilterMedianSubtract[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], initialFactor_Integer:81, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterMedianSubtract, dataObj];
	tempret@setWindowLength[ initialFactor ];
	tempret
];

NNFilterMedianSubtract[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:81, opts:OptionsPattern[]]:=
	(NNFilterMedianSubtract[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterMedianSubtract[args___]:=Message[NNFilterMedianSubtract::invalidArgs, {args}];


NNFilterFIR[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], {highPass_/;NumberQ[highPass], lowPass_/;NumberQ[lowPass]}, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterFIR, dataObj];
	tempret@setTaps[ 32000 ];
	tempret@setFilterHz[ highPass, lowPass ];
	tempret
];

NNFilterFIR[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 {highPass_/;NumberQ[highPass], lowPass_/;NumberQ[lowPass]}, opts:OptionsPattern[]]:=
	(NNFilterFIR[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		{highPass, lowPass}, opts
	])@getNNDataChannel[ 0 ];

NNFilterFIR[args___]:=Message[NNFilterFIR::invalidArgs, {args}];


NNFilterBuffer[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], opts:OptionsPattern[]]:=
Module[{tempret},
	JavaNew[$NNJavaClass$NNFilterBuffer, dataObj]
];

NNFilterBuffer[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				opts:OptionsPattern[]]:=
	(NNFilterBuffer[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], opts
	])@getNNDataChannel[ 0 ];

NNFilterBuffer[args___]:=Message[NNFilterBuffer::invalidArgs, {args}];


NNFilterTrodeRereference[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], opts:OptionsPattern[]]:=
Module[{tempret},
	JavaNew[$NNJavaClass$NNFilterTrodeRereference, dataObj]
];

NNFilterTrodeRereference[
	dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData],
	weights_List
	]:=
Module[{tempret},
	JavaNew[$NNJavaClass$NNFilterTrodeRereference, dataObj, weights]
];

NNFilterTrodeRereference[args___]:=Message[NNFilterTrodeRereference::invalidArgs, {args}];


NNFilterMasked[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]]:=
	JavaNew[$NNJavaClass$NNFilterMasked, dataObj];
NNFilterMasked[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel]]:=
	NNDataChannel[
		NNFilterMasked[ NNData[ {dataChannelObj} ] ],
		0 
	];

NNFilterMasked[
	dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData],
	maskObj_/;NNJavaObjectQ[maskObj, $NNJavaClass$NNTimestampMask]
	]:=
JavaNew[$NNJavaClass$NNFilterMasked , dataObj, maskObj];

NNFilterMasked[
	dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
	maskObj_/;NNJavaObjectQ[maskObj, $NNJavaClass$NNTimestampMask]
	]:=
	NNDataChannel[
		NNFilterMasked[ NNData[ {dataChannelObj} ], maskObj ],
		0 
	];

NNFilterMasked[args___]:=Message[NNFilterMasked::invalidArgs, {args}];


NNFilterMean[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData]]:=
JavaNew[$NNJavaClass$NNFilterMean, dataObj];

NNFilterMean[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], channels_List/;(Depth[channels]==2)]:=
JavaNew[$NNJavaClass$NNFilterMean, dataObj, channels];

NNFilterAppendCalculatedChannels[args___]:=Message[NNFilterAppendCalculatedChannels::invalidArgs, {args}];


NNFilterAppendCalculatedChannels[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData](*, opts:OptionsPattern[]*)]:=
Module[{tempret},
	JavaNew[$NNJavaClass$NNFilterAppendCalculatedChannels, 
		dataObj(*,
		OptionValue[NNOptAppendCalculationType]*)
	]
];

NNFilterAppendCalculatedChannels[args___]:=Message[NNFilterAppendCalculatedChannels::invalidArgs, {args}];


NNFilterAppendCalculatedChannels


(* ::Subsection::Closed:: *)
(*NNReadEvents*)


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], "Ports", opts:OptionsPattern[]]:=
		dataObj@getPorts[];


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], port_Integer,
			opts:OptionsPattern[]]:=
Module[{tempReturn},
	
	(*If[!OptionValue[NNOptDurationEvents], dataObj@expandDurationEventsToStartAndReset[] ];*)

	tempReturn=Transpose[{Transpose[{
		dataObj@readEventTimestamps[port], 
		dataObj@readEventDurations[port]}],
		dataObj@readEventCodes[port],
		dataObj@readEventComments[port]
		(*dataObj@readPortEventArrayTimestamp[port], 
		dataObj@readPortEventArrayDuration[port]}],
		dataObj@readPortEventArrayCodes[port],
		dataObj@readPortEventArrayComments[port]*)}
	];

	If[!OptionValue[NNOptDurationEvents], 
		tempReturn=If[#[[1,2]]!=0,
			{{{#[[1,1]], 0}, #[[2]], #[[3]]}, {{#[[1,1]]+#[[1,2]], 0}, #[[2]], "END: "<>#[[3]]}},
			{#}]& /@ tempReturn;
		Flatten[tempReturn, 1],
		tempReturn
	]

];

NNReadEvents[args___]:=Message[NNReadEvents::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNReadTimestamps*)


NNReadTimestamps[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], {port_Integer, code_Integer},
	opts:OptionsPattern[]]:=
Module[{tempTimestamps, tempTimestampsChecked, optDurationCheck},

	tempTimestamps = dataObj@readPortCodeEventArray[ port, code ];

	optDurationCheck = OptionValue[NNOptDurationCheck];
	If[ optDurationCheck === False || optDurationCheck === None,
		tempTimestamps[[All, 1]],
		If[ HHFunctionQ[ optDurationCheck ],
			tempTimestampsChecked = Select[ tempTimestamps, optDurationCheck[ #[[2]] ]& ];
			If[ Length[tempTimestamps] != Length[tempTimestampsChecked],
				Message[ NNReadTimestamps::rejectDuration, Length[tempTimestamps]-Length[tempTimestampsChecked] ]
			];
			tempTimestampsChecked[[All, 1]],
			
			Message[NNReadTimestamps::invalidOptionValue, "NNOptDurationCheck", ToString[ optDurationCheck ] ];
			tempTimestamps[[All, 1]]
		]
	]

];

NNReadTimestamps::rejectDuration = "Some timestamps (n=`1`) rejected due to NNOptDurationCheck criteria";
NNReadTimestamps[args___]:=Message[NNReadTimestamps::invalidArgs, {args}];


(* ::Section:: *)
(*Ending*)


End[];


EndPackage[];


(* ::Section::Closed:: *)
(*Backup*)


(*NNEventSegmentTimestamps::usage="Extracts segment timestamps from event record.";
NNEventSelect::usage="";*)


(*NNERPExtractTS::usage="Extracts ERP traces from a data object.";
NNERPPlotTS::usage="Extracts segments from a data object and plots ERP.";*)


(*NNEventSegmentTimestamps[events_List]:=
NNEventSegmentTimestamps[events]=
Module[{tempEventStarts, tempEventEnds, tempSegs},
	tempEventStarts=Select[events, (#[[1]]==0 && #[[5]]=="Starting Recording")& ];
	tempEventEnds=Select[events, (#[[1]]==0 && #[[5]]=="Ending Recording")& ];
	tempSegs={#[[2]], NNEventSegmentEndTSSelect[#, tempEventEnds]}& /@ tempEventStarts;
	tempSegs=Table[ 
		If[ n >= Length[tempSegs], tempSegs[[n]], {tempSegs[[n, 1]], Min[tempSegs[[n + 1, 1]],tempSegs[[n, 2]]]} ],
		{n, 1, Length[tempSegs]}
	];
	tempSegs
];

NNEventSegmentEndTSSelect[startEvent_, eventEnds_]:=
	SelectFirst[eventEnds, (#[[2]] > startEvent[[2]])&, {Null,Infinity}][[2]];
NNEventSegmentTimestamps[args___]:=Message[NNEventSegmentTimestamps::invalidArgs, {args}];
NNEventSelect[events_List, eventNo_Integer]:=
Module[{tempEST},
	tempEST = NNEventSegmentTimestamps[events];
	If[eventNo<=0 || Length[tempEST]< eventNo,
		Message[NNEventSelect::invalidEventNo,Length[events], eventNo];,
		Select[events, (tempEST[[eventNo, 1]] <= #[[2]] && #[[2]] < tempEST[[eventNo, 2]])&]
	]
];
NNEventSelect::invalidEventNo="The event list only has `1` detected segments, and eventNo `2` is therefore invalid.";
NNEventSelect[args___]:=Message[NNEventSelect::invalidArgs, {args}];*)


(*NNERPExtractTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempEvents},
	tempEvents = JavaNew["nounou.data.ranges.RangeTSEvent", #, preFrames, postFrames]& /@ timeStamps;
	(dataObj@readTraceAbsA[channel, #]&) /@ tempEvents
];


NNERPExtractTS[args___]:=Message[NNERPExtractTS::invalidArgs, {args}];*)


(*NNERPPlotTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempERP},
	tempERP =  NNERPExtractTS[dataObj, channel, timeStamps, {preFrames, postFrames, step}];
	ListLinePlot[tempERP, PlotRange->All]
];


NNERPPlotTS[args___]:=Message[NNERPPlotTS::invalidArgs, {args}];*)


(* ::Subsection::Closed:: *)
(*FILTER RELATED: NNFilterData*)


(*NNFilterData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"]]:= 
	JavaNew["nounou.data.filters.XDataFilterFIR", dataObj];


NNFilterData[args___]:=Message[NNFilterData::invalidArgs, {args}];*)


(*NNDownsampleData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], factor_:10]:= 
	JavaNew["nounou.data.filters.XDataFilterDownsample", dataObj, factor];


NNDownsampleData[args___]:=Message[NNDownsampleData::invalidArgs, {args}];*)


(* ::Subsection::Closed:: *)
(*FILTER RELATED: NNFilterData, NNDownsampleData*)


(*NNFilterData::usage="Applies FIR filter to data object and gives resulting filter object.";
NNDownsampleData::usage="Applies Downsample filter to data object and gives resulting filter object.";*)


(*NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadTrace[dataObj, channel, #, opts]& /@ rangeList;*)


(*NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			range_Rule, 
			opts:OptionsPattern[]]:= NNReadTrace[dataChannelObj, $ToNNRangeSpecifier[range], opts];

NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			range_List/;(Head[range[[1]]]===Span), 
			opts:OptionsPattern[]]:= NNReadTrace[dataChannelObj, $ToNNRangeSpecifier[range], opts];*)


(*NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadTrace[dataChannelObj, #, opts]& /@ rangeList;*)


(*NNReadTrace[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel], 
			{0},
			range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, optTimepointUnit, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	(*optTimepointUnit = OptionValue[ NNOptTimepointUnit ];*)
	If[ optTimepoints===True, optTimepoints=False ];

	If[optTimepoints(* === Null || optTimepoints === None || optTimepoints === False*),
		Transpose[ {NNReadTimepoints[dataChannelObj, range, optTimepointUnit], 
					dataChannelObj@readTrace[range]
		} ],
		dataChannelObj@readTrace[range]
	]
];*)


(* ::Subsection::Closed:: *)
(*NNToList*)


(*NNToList[eventObj_/;HHJavaObjectQ[eventObj,$NNEventClass]]:=
Module[{tempret, tempPortEvt},
	tempret=Table[
		tempPortEvt=eventObj@filterByPortA[p];
		{p, #@timestamp[], #@duration[], #@code[], #@comment[]}& /@ tempPortEvt,
		{p,eventObj@ports[]}
	];
	tempret=Flatten[tempret,1];
	Sort[tempret, (#1[[2]] < #2[[2]])&]
];
NNToList[args___]:=Message[NNLoad::invalidArgs, {args}];*)
