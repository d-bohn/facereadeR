#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
score.aus <- function(data){
  AUs <- c(
    "AU01","AU02","AU04","AU05","AU06","AU07","AU09","AU10","AU12","AU14","AU15","AU17",
    "AU18","AU20","AU23","AU24","AU25","AU26","AU27","AU43"
  )

  data[AUs] <- lapply(data[AUs],
                      function(x) car::recode(x, recodes = "'A'=1; 'B'=2; 'C'=3; 'D'=4;
                                   'E'=5;'NotActive'=0; else=NA;", as.factor.result = FALSE))
  return(data)
}
