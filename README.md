# facereadeR
This is a simple R package for working with 
[FaceReader 6.xx](http://www.noldus.com/human-behavior-research/products/facereader)
output data because the default output is relatively difficult
to wrangle and utilize in any statistics program without major
manual overhaul. This package helps streamline the process
with a few additional functions I find useful.

***Note: This package is currently under development and not a complete package by any means.***

## Installation

You can install the development version of the facereadeR package using **devtools**:

    install.packages('devtools')
    devtools::install_github('d-bohn/facereadeR')
    library(facereadeR)

## Using facereadeR

After installing the package, you can start to analyze FaceReader output with just a
few simple function calls that read in  and format the data, clean it,
and generally tidy it up for further statistical
analyses. To get started, try reading in the included example data files using:

    files <- list.files(system.file("extdata/", package="facereadeR"), full.names = TRUE)
    library(purrr);library(dplyr)
    
    data <- files %>% purrr::map(., read_facereader, as.is = TRUE) %>% 
      reduce(., rbind)
    

This data set includes raw FaceReader output (the text file that FaceReader 6.1 produces)
from a selection of the first 2016 Republican debate where reporter Megyn Kelly confronts
Presidential candidate Donald Trump on recent sexist 
remarks. There are two files: one for Kelly where she asks the question, and one for
Trump where he responds.

After the data are imported into a dataframe, we typically need to remove failures to find the face and replace them 
with ``NA`` values. Additionally, we will create appropriate factors for each of the variables to be analyzed.

    data <- clean_data(data, include = 'Basic')

If we are interested in analyzing Action Units (AUs), and they are provided in the output,
we could run the following 
to rename them to a simpler format as well as score them so that they become numeric values 
instead of ``A, B, C, D``.

    data <- rename_aus(data)
    data <- score_aus(data)

At this point, the data is in a tidy format having been read in and aggregated together.
Additionally, the AUs have been converted to numeric values for quantitative analysis.

    head(data[1:10])

