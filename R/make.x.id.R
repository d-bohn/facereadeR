#' Makes additional values for use in FaceReader output data frame
#'
#' @description
#' Functions to help parse out information from the naming structure of the files analyzed by FaceReader.
#'
#'
#' @details
#' In order to use these functions, the FaceReader output files should be named in the following manner:
#' Participant#_Analysis#_extra_details.txt, where 'Participant#' is a unique numeric identifier, the first number
#' of which denotes the condition that participant was in. FaceReader should automatically provide an analysis number if
#' participants have more than one video being analyzed. Note, \code{make.sub.id} should still work if this naming convention
#' is not followed as long as each file being analyzed has a numeric value.
#'
#' @param data the data frame produced by \code{read.facereader}.
#'
#' @return
#' A unique identifier for each participant, analysis, or condition based off of the naming structure of the files.
#'
#' @examples
#' data$id <-  make.sub.id(data)
#' data$analaysis <- make.analysis.id(data)
#' data$condition <- make.condition.id(data)
#'
make.sub.id <- function(data){
  subject_id <- sub(".*Participant *(.*?) *_Analysis.*", "\\1", data$Source)
  subject_id <- as.numeric(sub(".*_ *(.*?) *_Analysis.*", "\\1", data$Source))
  return(subject_id)
}


make.analysis.id <- function(data){
  analysis <- as.numeric(sub(".*_Analysis *(.*?) *_video.*", "\\1", data$Source))
  return(analysis)
}


make.condition.id <- function(data){
  condition <- as.numeric(substr(data$subject_id, 2,2))
}
