
read.facereader <- function(path) {
  path <- path
  get_files <- function(filename){
    if(file.info(filename)$size > 0){
      ret <- read.table(filename,skip = 8, header=TRUE, sep="\t")
      ret$Source <- filename
      ret
    }
  }

  # Get text files and merge them into a single data frame
  files_list <- list.files(pattern = "*.txt")

  # Merge all datasets into a single file
  data <- ldply(files_list, get_files)
  return(data)
}

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
