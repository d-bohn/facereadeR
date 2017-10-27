# facereadeR
This is a simple R package for working with 
[FaceReader 6.xx](http://www.noldus.com/human-behavior-research/products/facereader)
data output because the default output is relatively difficult
to wrangle and utilize in any statistics program without major
manual overhaul. This package helps streamline the process
with a few additional functions I find useful.

***Note: This package is currently under development and not a complete package by any means.***

## Installation

You can install the development version of the facereadeR package using **devtools**:

    devtools::install_github("d-bohn/facereadeR")
    library(facereadeR)

## Using facereadeR

After installing the package, you can start to analyze FaceReader output with just a few simple function calls that read in 
and format the data, clean it, and generally tidy it up for further statistical analyses. To get started, try reading in the
included example data files using:

    data <- read.facereader(system.file("inst/", package="facereadeR"), skip = 6)

This data set includes raw FaceReader output (the text file that FaceReader 6.1 produces) from a selection of the first
Republican debate where reporter Megyn Kelly confronts Presidential candidate Donald Trump on recent sexist remarks. There are two files: one for Kelly where she asks the question, and one for Trump where he responds.

After the data are imported into a dataframe, we typically need to remove failures to find the face and replace them with
``NA`` values. Additionally, we will create appropriate factors for each of the variables to be analyzed.

    data <- clean.data(data, include = "Basic")

If we are interested in analyzing Action Units (AUs), and they are provided in the output, we could run the following to rename
them to a simpler format as well as score them so that they become numeric values instead of ``A, B, C, D``. We can also 
remove ``NA`` values if we feel inclined (for posterity), though this doesn't always have the intended effect.

    data <- rename.aus(data)
    data <- score.aus(data)
    #data <- remove.na(data)
    
Next, because FaceReader analyzes every frame of a video file, we create time epochs for which we will average the values over
to make it simpler once we get the the analyze step.




