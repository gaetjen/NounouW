(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`", {"HokahokaW`","JLink`"}];


(* ::Section:: *)
(*Declarations*)


(* Memo: Following from HokahokaW *) 
(*
General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `1` -> `2` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";
General::nullArgument="At least one of the required arguments is null!";
*)


HHPackageMessage["NounouW`"];


(* ::Subsection::Closed:: *)
(*Load Static NN Object*)


(*Convenience object for static methods*)
NN=LoadJavaClass["nounou.NN", StaticsVisible->False, AllowShortContext->True];
NNJ=LoadJavaClass["nounou.NNJ", StaticsVisible->False, AllowShortContext->True];
(*NNOpt=LoadJavaClass["nounou.options.NNOpt", StaticsVisible->False, AllowShortContext->True];*)
Print[NN`toString[]];


(* ::Subsection::Closed:: *)
(*Java class paths and object checking*)


NNJavaObjectQ::usage=
"Checks whether something is a Java object and an instance of the given class name (full).";
NNJavaObjectListQ::usage="";


$NNJavaClass$NNElement = "nounou.elements.NNElement";

$NNJavaClass$NNData          = "nounou.elements.data.NNData";
$NNJavaClass$NNTimingElement = "nounou.elements.traits.NNTimingElement";
$NNJavaClass$NNLayout    = "nounou.elements.layout.NNLayoutSpatial";
$NNJavaClass$NNLayoutSpatial = "nounou.elements.layout.NNLayoutSpatial";


$NNJavaClass$NNFilterDownsample      = "nounou.elements.data.filters.NNFilterDownsample";
$NNJavaClass$NNFilterDecimate        = "nounou.elements.data.filters.NNFilterDecimate";
$NNJavaClass$NNFilterMedianSubtract  = "nounou.elements.data.filters.NNFilterMedianSubtract";
$NNJavaClass$NNFilterFIR             = "nounou.elements.data.filters.NNFilterFIR";
$NNJavaClass$NNFilterBuffer             = "nounou.elements.data.filters.NNFilterBuffer";


$NNJavaClass$NNDataChannel          = "nounou.elements.data.NNDataChannel";
$NNJavaClass$NNDataChannelArray     = "nounou.elements.data.NNDataChannelArray";
$NNJavaClass$NNDataChannelExtracted = "nounou.elements.data.NNDataChannelExtracted";

$NNJavaClass$NNEvents = "nounou.elements.events.NNEvents";


(*NNJavaObjectQ$NNElement::usage=
"Checks whether something is a Java object and an instance of $NNJavaClass$NNElement ("<>$NNJavaClass$NNElement<>")";

NNJavaObjectQ$NNData::usage=
"Checks whether something is a Java object and an instance of $NNJavaClass$NNData ("<>$NNJavaClass$NNData<>")";
NNJavaObjectQ$NNTimingElement::usage=
"Checks whether something is a Java object and an instance of $NNJavaClass$NNTimingElement ("<>$NNJavaClass$NNTimingElement<>")";

NNJavaObjectQ$NNEvent::usage=
"Checks whether something is a Java object and an instance of $NNJavaClass$NNEvent ("<>$NNJavaClass$NNEvent<>")";*)


$NNJavaClass$NNRangeSpecifier = "nounou.ranges.NNRangeSpecifier";

$NNJavaClass$NNRange =          "nounou.ranges.NNRange";
$NNJavaClass$NNRangeEvent =     "nounou.ranges.NNRangeEvent";
$NNJavaClass$NNRangeAll =       "nounou.ranges.NNRangeAll";
$NNJavaClass$NNRangeTs =        "nounou.ranges.NNRangeTs";
$NNJavaClass$NNRangeTsEvent =   "nounou.ranges.NNRangeTsEvent";


(*NNJavaObjectQ$NNRangeSpecifier::usage=
"Checks whether something is a Java object and an instance of $NNJavaClass$NNRangeSpecifier ("<>$NNJavaClass$NNRangeSpecifier<>")";*)


(*NNFrameRangeJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.FrameRange";

NNXMaskJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XMask";*)


(* ::Subsection:: *)
(*Package-wide markers*)


NNTimestamp::usage="Marker for specifying times as timestamps, use as \"NNTimestamp @ 1000000\".";


(* ::Subsection:: *)
(*Package-wide option keys*)


NNTimeUnit::usage="Specifies what time units the data output should be in (\"ms\", \"samples\"). \
For trace reading, only relevant if time stamps are returned (i.e. NNReturnTimestamps -> True).";
NNOptStack::usage="Whether to stack multiple channels in NNTracePlot. Automatic will stack based on digitization range.";
NNOptStackGain::usage="Stack increment as a multiple of digitization range. Only valid when NNOptStack -> Automatic.";

NNValueUnit::usage="Specifies what units the data output should be in (Absolute, \"microV\")";
NNOptReturnTimepoints::usage=
"Whether to return a 2D array with time points ({{t1, x1}, {t2, x2}, ...}) when reading data (NNReadTrace). True or False.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Java object checking*)


NNJavaObjectQ[ obj_/;JavaObjectQ[obj], className_String ]:= InstanceOf[obj, className];
NNJavaObjectQ[ args___]:= False ;


NNJavaObjectListQ[ objList_List, className_String ]:= And@@( NNJavaObjectQ[#, className]& /@ objList );
NNJavaObjectListQ[ args___]:= False ;


(*NNJavaObjectQ$NNElement[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNElement ])]:= True ;
NNJavaObjectQ$NNElement[ args___]:= False ;*)


(*NNJavaObjectQ$NNData[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNData ])]:= True ;
NNJavaObjectQ$NNData[ args___]:= False ;

NNJavaObjectQ$NNTimingElement[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNTimingElement ])]:= True ;
NNJavaObjectQ$NNTimingElement[ args___]:= False ;*)


(*NNJavaObjectQ$NNEvents[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNEvents ])]:= True ;
NNJavaObjectQ$NNEvents[ args___]:= False ;*)


(*NNJavaObjectQ$NNRangeSpecifier[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNRangeSpecifier ])]:= True ;
NNJavaObjectQ$NNRangeSpecifier[ args___]:= False ;*)


(* ::Section:: *)
(*End Private*)


End[];


EndPackage[];


(* ::Section::Closed:: *)
(*Bak*)


(*
NNStackLists::usage=" ";
(*NNAxes::usage=" ";
NNBaselineCorrection::usage="";*)

NNTimeUnitMS::usage="True (plot in ms) or False.";
NNMasking::usage=" ";
NNSpikes::usage=" ";
NNAbsoluteValue::usage="Whether to plot traces in an absolute unit";



*)


(*NNReloadPackage[string_String]:=
	Module[{tempPackages},
		tempPackages=Select[ $Packages, StringMatchQ[#, (___ ~~ string ~~ ___)]& ];
		If[Length[tempPackages]>0,
			Unprotect[$Packages];
			($ContextPath=DeleteCases[$ContextPath, #])& /@ tempPackages;
			($Packages=DeleteCases[$Packages, # ])& /@ tempPackages;
			(Remove[Evaluate[#<>"*"]])& /@ tempPackages;
			Needs /@ tempPackages;
			Print["Reloaded: " <> # ]& /@ tempPackages;
		];
		(*Protect[$ContextPath];--$ContextPath is not Protected*)
		Protect[$Packages];
	];
NNReloadPackage[strings_List/;(And @@ StringQ /@ strings)]:= 
	Module[{},
		NNReloadPackage /@ strings;
	];
NNReloadPackage[args___]:=Message[NNReloadPackage::invalidArgs,{args}];*)


(*NNReloadPackage::usage=
"ReloadPackage[string_String].... unloads and reloads any package which \
contains the specified string within its name. \
This function is convenient during package developemnt--the package under development can \
be reloaded to test functionality, without losing any precalculated results in the kernel.";*)


(*NNNextPower[base_, n_]:= Ceiling[Log[base, n]];
NNNextPower[args___]:=Message[NNNextPower::invalidArgs,{args}];*)


(* ::Subsection:: *)
(*Old File Loading*)


(*NNData=LoadJavaClass["nounou.NNData", StaticsVisible->True, AllowShortContext\[Rule]True];*)
(*NNDataReader=LoadJavaClass["nounou.NNDataReader", StaticsVisible->False, AllowShortContext->True];*)

