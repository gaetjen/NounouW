(* ::Package:: *)

(* Mathematica Init File *)


(*Needed to ensure java classes available below during certain call chains*)
<<JLink`;
InstallJava[];
AddToClassPath[FileNameJoin[{ParentDirectory[DirectoryName[FindFile["NounouW`"]]], "Java"}]];


Needs["HokahokaW`"];


SetComplexClass["breeze.math.Complex"]; (*This allows Mathematica to interact transparently with Java/Scala/breeze complex numbers*)
NounouW`$JavaStackSize = 6144;


HHIncreaseJavaStack[NounouW`$JavaStackSize];


Get[ "NounouW`NounouW`"];


Needs["NounouW`Data`"];


Needs["NounouW`Graphics`"];


Needs["NounouW`Graphics`NNTracePlot`"];
