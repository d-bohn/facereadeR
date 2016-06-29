#' Title
#'
#' @param data
#' @param keep
#'
#' @return
#' @export
#'
#' @examples
reduce.data <- function(data, keep = c("Emotions","AUs","Both","All")){
  data <- data
  if(keep=="Emotions"){
    keep_cols <- c("Neutral","Happy","Sad","Angry","Surprised","Scared","Disgusted","Valence",
                   "Arousal","subject_id","analysis","condition","epoch")
    data <- data[,keep_cols]
    return(data)
  } else if (keep=="AUs") {
    keep_cols <- c("AU01","AU02","AU04","AU05","AU06","AU07","AU09","AU10","AU12","AU14","AU15","AU17",
                   "AU18","AU20","AU23","AU24","AU25","AU26","AU27","AU43","subject_id","analysis",
                   "condition","epoch")
    data <- data[,keep_cols]
    return(data)
  } else if (keep=="Both") {
    keep_cols <- c("Neutral","Happy","Sad","Angry","Surprised","Scared","Disgusted","Valence",
                   "Arousal","subject_id","analysis","condition","epoch","AU01","AU02","AU04",
                   "AU05","AU06","AU07","AU09","AU10","AU12","AU14","AU15","AU17",
                   "AU18","AU20","AU23","AU24","AU25","AU26","AU27","AU43")
    data <- data[,keep_cols]
    return(data)
  } else {
    return(data)
  }
}
