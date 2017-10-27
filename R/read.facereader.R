#' Reads FaceReader output data
#'
#' @description
#' \code{read.facereader} reads in FaceReader 6.x text files, strips excess front matter,
#' and aggragates them into a single data frame for further analysis.
#'
#' @details
#' The file path (\code{path}) should lead to a directory that contains all of the text
#' files that will be read into the R workspace.
#'
#' @param path The complete file path to location (folder) of your text files.
#' @param skip The number of lines in the text file to skip before the headers should be read. Defaults to 7 lines.
#' @param detailed Boolean noting whether files to be read are detailed or state files.
#'
#' @return
#' A data frame that includes output provided by FaceReader 6.xx.
#'
#' @examples
#'
#' data <- read.facereader(system.file("inst/", package="facereadeR"), skip = 6, detailed = TRUE)
#'
#' @export

read.facereader <- function(path, skip = 6, detailed = TRUE) {
  wd <- getwd()
  setwd(path)

  if(detailed==TRUE){
    get_files <- function(filename){
      if(file.info(filename)$size > 0){
        ret <- read.table(filename, skip = skip, header=TRUE, sep="\t")
        ret$Source <- filename
        ret
      }
    }

    # Get text files and merge them into a single data frame
    files_list <- list.files(path = path, pattern = "*.txt")

    # Merge all datasets into a single file
    data <- plyr::ldply(files_list, get_files)
    setwd(wd)
    return(data)
  }
  if(detailed==FALSE){
    get_files <- function(filename){
      if(file.info(filename)$size > 0){
        ret <- read.table(filename, skip = skip, header=TRUE, sep="\t")
        ret$Source <- filename
        ret
      }
    }

    # Get text files and merge them into a single data frame
    files_list <- list.files(path = path, pattern = "*.txt")

    # Merge all datasets into a single file
    data <- plyr::ldply(files_list, get_files)
    setwd(wd)
    return(data)
  }
  else{
    print('Please specify what type of file you want read')
  }
}


