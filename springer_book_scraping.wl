(* ::Package:: *)

(* ::Title:: *)
(*Scraping Springer Books*)


(* ::Section:: *)
(*Data*)


(*Find and import the CSV all the book data*)
Clear@books
books=Import[SystemDialogInput["FileOpen",WindowTitle->"FIND THE BOOK DATA SHEET"]];


(*Pick out the subject column*)
Clear[subjectsColumn,subjects]
subjectsColumn=Position[books[[1]],"English Package Name"][[1,1]];
subjects=books[[2;;,subjectsColumn]];


(*Picks out and transforms the DOI urls of the book webpages into the urls of the downloadable files*)
Clear@urls
urls=StringReplace[#,"http://doi.org"->"https://link.springer.com/content/pdf"]&/@books[[2;;,Position[books[[1]],"DOI URL"][[1,1]]]];


(*Picks out the titles column of the book data.
NOTE:Some titles have a:in them,and Windows doesn't like those,so they have to first be edited into a friendly file name (e.g.":"\[Rule]"-").*)
Clear@titles
titles=Block[{n=1},StringReplace[#,{":"->",","/"->" and "}]&/@books[[2;;,1]]/.str:"Advanced Organic Chemistry":>str<>", Part "<>ToUpperCase@Alphabet[][[n++]]];


Clear@authors
authors=With[
	{
		authorColumn=Position[books[[1]],"Author"][[1,1]],
		getLastNamesList=(StringSplit[StringSplit[#,", "]][[;;,-1]]&),
		lastNamesListToString=(StringJoin@@If[Length@#>1,Riffle[#,"; "],#]&),
		fixNames=((*ImportString[#,"Text"]&*)FromCharacterCode[ToCharacterCode@#,"UTF8"]&)
	},
	fixNames@*lastNamesListToString@*getLastNamesList/@books[[2;;,authorColumn]]
];


Clear@years
years=ToString/@books[[2;;,5]];


Clear@bookDir
bookDir=FileNameJoin@{SystemDialogInput["Directory",WindowTitle->"WHERE TO SAVE ALL THE BOOKS"],"Springer_Books"};


Clear@uniqueName
uniqueName[title_String,year_String,author_String]:=title<>" ("<>year<>")"<>" - "<>author


Clear@fetchBook
fetchBook[{url_,subject_,name_}]:=(*URLSave*)List[url,FileNameJoin[{bookDir,subject,name<>".pdf"}]]


Clear@fetchBookList
fetchBookList[data:{urls_,subjects_,names_}]:=Monitor[
	Block[
		{n=0},
		(n++;fetchBook@#)&/@Transpose@data
	],
	ProgressIndicator[n/Length@data[[1]]]
]


(* ::Section:: *)
(*Downloads*)


(* ::Subsubsection:: *)
(*All the books*)


CreateDirectory@FileNameJoin@{bookDir,#}&/@DeleteDuplicates@subjects;


(* ::Subsubsection:: *)
(*Specific Subjects*)


Manipulate[
	If[
		subject===0,
		"",
		Column@{
			TableForm[
				{#[[1]],#[[2,;;,2]]}&/@data[[Sort@subject]],
				TableAlignments->{Left,Top},
				TableSpacing->{5,5,1.5}
			],
			Button[
				"Download",
				Thread/@data[[Sort@subject]]/.{subject_,{url_,name_}}:>{url,subject,name}//Catenate//Transpose//fetchBookList//Print
			]
		}
	],
	{
		{subject,0,"Subject"},
		Thread[Range@Length@#->#]&@Sort@DeleteDuplicates@subjects,
		TogglerBar,
		Appearance->"Horizontal"->{3,7}
	},
	Initialization:>(data={#1[[1,1]],#[[;;,2;;]]}&/@GatherBy[Transpose@{subjects,urls,MapThread[uniqueName,{titles,years,authors}]},First]//SortBy[#,First]&),
	Deinitialization:>Clear@data
]
