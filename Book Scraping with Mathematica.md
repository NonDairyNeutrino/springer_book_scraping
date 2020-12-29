Introduction
============

Many of you may know that the scientific book publisher Springer has [released](https://www.springernature.com/gp/librarians/news-events/all-news-articles/industry-news-initiatives/free-access-to-textbooks-for-institutions-affected-by-coronaviru/17855960) a large collection of their books to help teachers and learners.  In the announcement they even included a [complete Excel spreadsheet](https://resource-cms.springernature.com/springer-cms/rest/v1/content/17858272/data/v4) containing a bunch of the books' metadata, including the links to the respective pages for each freely downloadable books.  And it turns out, while the provided urls themselves don't link to the download pages/files, there's an easy rule to transform the DOI addresses into the file pages.  So after identifying this pattern/rule, I wrote some *Mathematica* to scrap the list and download each book.

I've divided the program into two different types, scraping all the books or choosing only the subjects you want (e.g. Physics and Astronomy, and Mathematics).  Though to start things, we first pick out the relevant data from the book data.

Book Data
=========

Once this code is run, you'll be prompted with two sequential file explorer dialogs.  The first is to identify the book data CSV, and the second is to specify which directory to place the folder that will contain all the books (e.g. [chosen directory]\Springer_Books).  The code automatically creates subdirectories for the different subjects and puts the books in the right one.

    books = Import[SystemDialogInput["FileOpen", WindowTitle -> "FIND THE BOOK DATA SHEET"]];;(*Opens a file explorer so you can more easily pick the book data CSV*)

    subjectsColumn = Position[books[[1]], "English Package Name"][[1, 1]];
    subjects = books[[2 ;;, subjectsColumn]]; (*Picks out the subjects column of the book data*)

    urls = StringReplace[#, "http://doi.org" -> "https://link.springer.com/content/pdf"] & /@ books[[2 ;;, Position[books[[1]], "DOI URL"][[1, 1]]]]; (*Picks out and transforms the DOI urls of the book webpages into the urls of the downloadable files*)

    titles = StringReplace[#, {":" -> ",", "/" -> " and "}] & /@ books[[2 ;;, 1]]; (*Picks out the titles column of the book data. NOTE: Some titles have a : in them, and Windows doesn't like those, so they have to first be edited into a friendly file name (e.g. ":" -> "-").*)

    authors = With[
      {
        authorColumn = Position[books[[1]], "Author"][[1, 1]],
        getLastNamesList = (StringSplit[StringSplit[#, ", "]][[;; , -1]] &),
        lastNamesListToString = (StringJoin @@ If[Length@# > 1, Riffle[#, "; "], #] &),
        fixNames = (FromCharacterCode[ToCharacterCode@#, "UTF8"] &) (*You could also use ImportString[#,"Text"]&, but the implemented method is much faster.*)
      },
      fixNames@*lastNamesListToString@*getLastNamesList /@ books[[2 ;;, authorColumn]]
    ];

    years = ToString /@ books[[2 ;;, 5]];

    (*Creating the subject directories*)
    bookDir = FileNameJoin@{SystemDialogInput["Directory", WindowTitle -> "WHERE TO SAVE ALL THE BOOKS"], "Springer_Books"}; (*This opens a file explorer to choose a directory in which to put the folder that will contain all the books*)

    CreateDirectory@FileNameJoin@{bookDir, #} & /@ DeleteDuplicates@subjects;

    uniqueName[title_String, year_String, author_String] := title <> " (" <> year <> ")" <> " - " <> author

    fetchBook[{url_, subject_, name_}] := URLSave[url, FileNameJoin[{bookDir, subject, name <> ".pdf"}]]

    fetchBookList[data : {urls_, subjects_, names_}] := Monitor[
      Block[
        {n = 0},
        (n++; fetchBook@#) & /@ Transpose@data
      ],
      ProgressIndicator[n/Length@data[[1]]]
    ]

Downloading
===========

All the books
-------------

With all our definitions above, scraping all the books from the list is relatively simple, or at least concise.

	fetchBookList@{urls, subjects, MapThread[uniqueName, {titles, years, authors}]}

Specific Subjects
-----------------

Instead of downloading each subject as a black box, I've made it so you can see what books comprise each subject you may want to scrape.  Basically, you click the subjects you're interested in and the program displays the books in that subject, and then you can simply click the "Download" button at the bottom of the prompt and *Mathematica* fetches the listed books.

    Manipulate[
      If[
        subject === 0,
        "",
        Column@{
          TableForm[
            {#[[1]], #[[2, ;; , 2]]} & /@ data[[Sort@subject]],
            TableAlignments -> {Left, Top},
            TableSpacing -> {5, 5, 1.5}
          ],
          Button[
            "Download",
            Thread /@ data[[Sort@subject]] /. {subject_, {url_, name_}} :> {url, subject, name} // Catenate // Transpose // fetchBookList
          ]
        }
      ],
      {
        {subject, 0, "Subject"},
        Thread[Range@Length@# -> #] &@Sort@DeleteDuplicates@subjects,
        TogglerBar,
        Appearance -> "Horizontal" -> {3, 8}
      },
      Initialization :> (data = {#1[[1, 1]], #[[;; , 2 ;;]]} & /@ GatherBy[Transpose@{subjects, urls, MapThread[uniqueName, {titles, years, authors}]}, First] // SortBy[#, First] &),
      Deinitialization :> Clear@data
    ]

Conclusion
==========

I elected to download all the books because I'm becoming a data hoarder.  It took a little over an hour to download everything over WIFI (This wasn't parallelized because I had some concerns when it came to writing the data to my hard disk.) and the finished folder of everything is about 8GB.
