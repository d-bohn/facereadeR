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
included example data set:

   data <- read.facereader(system.file())
