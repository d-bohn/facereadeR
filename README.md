# facereadeR
A R package for working with [FaceReader 6.xx](http://www.noldus.com/human-behavior-research/products/facereader) data output.

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

After the data are imported into a dataframe, we typically need to remove ``NA`` values and create appropriate factors for
each of the variables to be analyzed.
