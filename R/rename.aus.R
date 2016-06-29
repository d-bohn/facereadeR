#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
rename.aus <- function(data){
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
