#' Title
#'
#' @param data
#' @param type
#'
#' @return
#' @export
#'
#' @examples
summary.facereader <- function(data, type = c("Wide","Long")){
                    if(type=="Long"){
                      data <- Rmisc::summarySEwithin(data = data,
                                             measurevar = "value",
                                             withinvars = c("analysis","variable","epoch"),
                                             betweenvars = c("subject_id","condition"), idvar = "subject_id",
                                             na.rm = TRUE)
                      return(data)
                    }else{
                      data <- Rmisc::summarySEwithin(data = data,
                                             measurevar = "value",
                                             withinvars = c("analysis","variable","epoch"),
                                             betweenvars = c("subject_id","condition"), idvar = "subject_id",
                                             na.rm = TRUE)
                    }
}
