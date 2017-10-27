#' Read Facereader file
#'
#' @param file
#' @param subject
#' @param skip
#' @param ... Additional arguments to be passed to \code{\link[utils]{read.table}}
#'
#' @return
#' @export
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select mutate
#'
#' @examples
#'

read_facereader <- function(file, subject = NULL, skip = NULL, ...){

  src <- read.table(file, sep="\t", as.is = TRUE, strip.white = TRUE)[1:2]
  source <- as.character(src[src$V1=='Filename:',2])

  if(is.null(skip)){
    start <- grep("Video Time", src$V1)
    skip <- start - 1
  }
  if (is.null(subject)){
    subject <- file
  } else{
    subject <- stringr::str_extract(source, subject)
  }

  df <- read.table(file, skip = skip, header=TRUE, sep="\t", ...)

  data <- df %>%
    mutate(., subject_nr = subject,
           source = source) %>%
    select(subject_nr, source, everything())
  return(data)
}

#' Clean FaceReader data
#'
#' @param data
#' @param include
#'
#' @return
#' @export
#'
#' @examples
#'

clean_data <- function(data, include = "All"){
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
  } else if(include =='All') {
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
  }else{
    message('Please specify what to include in cleaning process.')
  }
  return(data)
}

#' Rename Action Units to their shortened names
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

rename_aus <- function(data){
  attach(data)
  names(data)[names(data)=="Action.Unit.01...Inner.Brow.Raiser"] <- "AU01"
  names(data)[names(data)=="Action.Unit.02...Outer.Brow.Raiser"] <- "AU02"
  names(data)[names(data)=="Action.Unit.04...Brow.Lowerer"] <- "AU04"
  names(data)[names(data)=="Action.Unit.05...Upper.Lid.Raiser"] <- "AU05"
  names(data)[names(data)=="Action.Unit.06...Cheek.Raiser"] <- "AU06"
  names(data)[names(data)=="Action.Unit.07...Lid.Tightener"] <- "AU07"
  names(data)[names(data)=="Action.Unit.09...Nose.Wrinkler"] <- "AU09"
  names(data)[names(data)=="Action.Unit.10...Upper.Lip.Raiser"] <- "AU10"
  names(data)[names(data)=="Action.Unit.12...Lip.Corner.Puller"] <- "AU12"
  names(data)[names(data)=="Action.Unit.14...Dimpler"] <- "AU14"
  names(data)[names(data)=="Action.Unit.15...Lip.Corner.Depressor"] <- "AU15"
  names(data)[names(data)=="Action.Unit.17...Chin.Raiser"] <- "AU17"
  names(data)[names(data)=="Action.Unit.18...Lip.Puckerer"] <- "AU18"
  names(data)[names(data)=="Action.Unit.20...Lip.Stretcher"] <- "AU20"
  names(data)[names(data)=="Action.Unit.23...Lip.Tightener"] <- "AU23"
  names(data)[names(data)=="Action.Unit.24...Lip.Pressor"] <- "AU24"
  names(data)[names(data)=="Action.Unit.25...Lips.Part"] <- "AU25"
  names(data)[names(data)=="Action.Unit.26...Jaw.Drop"] <- "AU26"
  names(data)[names(data)=="Action.Unit.27...Mouth.Stretch"] <- "AU27"
  names(data)[names(data)=="Action.Unit.43...Eyes.Closed"] <- "AU43"
  detach(data)
  return(data)
}

#' Score Action Units numerically
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom car recode
#'
#' @examples

score_aus <- function(data){
  AUs <- c(
    "AU01","AU02","AU04","AU05","AU06","AU07","AU09","AU10","AU12","AU14","AU15","AU17",
    "AU18","AU20","AU23","AU24","AU25","AU26","AU27","AU43"
  )

  data[AUs] <- lapply(data[AUs],
                      function(x) car::recode(x, recodes = "'A'=1; 'B'=2; 'C'=3; 'D'=4;
                                              'E'=5;'NotActive'=0; else=NA;", as.factor.result = FALSE))
  return(data)
}

