# modernStats4biol_Holmes
Working through Modern Statistica for Biology book by Susan Homes and Wolfgang Huber

Link to the online book:
https://www.huber.embl.de/msmb/

# Organisation
Material is organised in chapter folders:

1) _**chapterXX/ChapterXX.pptx**_ is a powerpoint presentation which contains background information that can help understand the material in the book, either basic maths, or more detailed work of some of the equations, or summary slides of some of the key points. It is more a collection of relevant information than a presentation of the chapter.

2) _**chapterXX/ChapterXX.R**_ is R code with a working through of the material in the chapter. Mostly copying and pasting the code that is in the book. It also contains answers to the questions/tasks/excercises which are not always available in the book (?). This is meant to be used in parallel with reading the chapter to see the code working in practice.

3) _**chapterXX/ChapterXX_exercises.R**_ is R code with answers to the exercises that are at the end of the chapter. Try and work through the problems on your own before looking at this code.

4) _**chapterXX/equations.Rmd**_ is R markdown code with Latex to show a step-by-step manipulatios of some equations. The output can be seen in _equations.pdf_ and is also usually pasted into the powerpoint presentation.

5) _**chapterXX/lists.R, factors.R etc.**_ are R code demonstrating some basic R data structures or functions in a very general way so that the code in the chapter can be better understood.

## Installing packages in R
In general in r you can install packages with a single command. For instance to install the vnc package you type:
```
install.packages("vcd")
```

Some packages, specific for genomic data are part of the Bioconductor package. So you must first install bioconductor:
```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.10")
```

Then additional packages can be installed with BiocManager::install. For instance to install the "Biostrings" package you
must type:
```
BiocManager::install("Biostrings")
```

In any session where you want to use a package, you must load its namespace with library(). e.g.:
```
library(vcd)
library(Biostrings)
```
