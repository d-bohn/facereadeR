#' Cleans FaceReader data frame
#'
#' @description
#' \code{clean.data} removes all \emph{"FIT_FAILED"} and \emph{"FIND_FAILED"} values and replaces them with \emph{NA}.
#' This function also appropriately changes each column value to the approrpriate type (e.g., numeric or factor). This is a
#' a good second step in the FaceReader data analysis pipeline, as the file must be cleaned before other commands such as
#' \code{score.aus} are performed on the data frame.
#'
#' @param data The data frame created by \code{read.facereader}
#' @param include Value of either: \itemize{
#' \item \emph{Basic} for only the emotion values to be cleaned
#' \item \emph{All} for all values (columns) to be cleaned
#' }
#'
#' @return
#' A cleaned data frame ready for further processing.
#'
#' @examples
#' data <- read.facereader("path/to/folder/")
#' data <- clean.data(data, include = "All")
#'
#' @export
#'
clean.data <- function(data, include = c("Basic","All")){
  data <- data
  if(include=="Basic"){
  data$Neutral <- as.numeric(gsub("FIT_FAILED", NA, data$Neutral))
  data$Happy <- as.numeric(gsub("FIT_FAILED", NA, data$Happy))
  data$Sad <- as.numeric(gsub("FIT_FAILED", NA, data$Sad))
  data$Angry <- as.numeric(gsub("FIT_FAILED", NA, data$Angry))
  data$Surprised <- as.numeric(gsub("FIT_FAILED", NA, data$Surprised))
  data$Scared <- as.numeric(gsub("FIT_FAILED", NA, data$Scared))
  data$Disgusted <- as.numeric(gsub("FIT_FAILED", NA, data$Disgusted))
  data$Valence <- as.numeric(gsub("FIT_FAILED", NA, data$Valence))
  data$Arousal <- as.numeric(gsub("FIT_FAILED", NA, data$Arousal))
  }else{
  data$Neutral <- as.numeric(gsub("FIT_FAILED", NA, data$Neutral))
  data$Happy <- as.numeric(gsub("FIT_FAILED", NA, data$Happy))
  data$Sad <- as.numeric(gsub("FIT_FAILED", NA, data$Sad))
  data$Angry <- as.numeric(gsub("FIT_FAILED", NA, data$Angry))
  data$Surprised <- as.numeric(gsub("FIT_FAILED", NA, data$Surprised))
  data$Scared <- as.numeric(gsub("FIT_FAILED", NA, data$Scared))
  data$Disgusted <- as.numeric(gsub("FIT_FAILED", NA, data$Disgusted))
  data$Valence <- as.numeric(gsub("FIT_FAILED", NA, data$Valence))
  data$Arousal <- as.numeric(gsub("FIT_FAILED", NA, data$Arousal))
  data$Contempt <- as.numeric(gsub("FIT_FAILED", NA, data$Contempt))
  data$Gender <- as.factor(gsub("FIND_FAILED", NA, data$Gender))
  data$Gender <- as.factor(gsub("FIT_FAILED", NA, data$Gender))
  data$Age <- as.factor(gsub("FIT_FAILED", NA, data$Age))
  data$Age <- as.factor(gsub("FIND_FAILED", NA, data$Age))
  data$Beard <- as.factor(gsub("FIT_FAILED", NA, data$Beard))
  data$Beard <- as.factor(gsub("FIND_FAILED", NA, data$Beard))
  data$Moustache <- as.factor(gsub("FIT_FAILED", NA, data$Moustache))
  data$Moustache <- as.factor(gsub("FIND_FAILED", NA, data$Moustache))
  data$Ethnicity <- as.factor(gsub("FIT_FAILED", NA, data$Ethnicity))
  data$Ethnicity <- as.factor(gsub("FIND_FAILED", NA, data$Ethnicity))
  data$Y...Head.Orientation <- as.numeric(gsub("FIT_FAILED", NA,
                                               data$Y...Head.Orientation))
  data$X...Head.Orientation <- as.numeric(gsub("FIT_FAILED", NA,
                                               data$X...Head.Orientation))
  data$Z...Head.Orientation <- as.numeric(gsub("FIT_FAILED", NA,
                                               data$Z...Head.Orientation))
  data$Mouth <- as.factor(gsub("FIT_FAILED", NA, data$Mouth))
  data$Mouth <- as.factor(gsub("FIND_FAILED", NA, data$Mouth))
  data$Left.Eye <- as.factor(gsub("FIT_FAILED", NA, data$Left.Eye))
  data$Left.Eye <- as.factor(gsub("FIND_FAILED", NA, data$Left.Eye))
  data$Right.Eye <- as.factor(gsub("FIT_FAILED", NA, data$Right.Eye))
  data$Right.Eye <- as.factor(gsub("FIND_FAILED", NA, data$Right.Eye))
  data$Left.Eyebrow <- as.factor(gsub("FIT_FAILED", NA, data$Left.Eyebrow))
  data$Left.Eyebrow <- as.factor(gsub("FIND_FAILED", NA, data$Left.Eyebrow))
  data$Right.Eyebrow <- as.factor(gsub("FIT_FAILED", NA, data$Right.Eyebrow))
  data$Right.Eyebrow <- as.factor(gsub("FIND_FAILED", NA, data$Right.Eyebrow))
  }
  return(data)
}





