#' Title
#'
#' @param data
#' @param sample.rate
#' @param epoch
#'
#' @return
#' @export
#'
#' @examples
create.epochs <- function(data, sample.rate, epoch){
  # Make time numeric so that it is easier to work with
  data$Time2 <- as.numeric(data$Video.Time)

  # The sample rate was 30 fps, so let's divide the data by epochs of 15s
  sample.rate <- sample.rate
  # Epochs in seconds
  epoch <- epoch
  # Max values for each epoch
  epoch1 <- epoch*sample.rate
  epoch2 <- (epoch*2)*sample.rate
  epoch3 <- (epoch*3)*sample.rate
  epoch4 <- (epoch*4)*sample.rate

  data$epoch <- ifelse(data$Time2 < epoch1, "15s",
                       ifelse((data$Time2 > epoch1 & data$Time2 <= epoch2), "30s",
                              ifelse((data$Time2 > epoch2 & data$Time2 <= epoch3), "45s",
                                     ifelse((data$Time2 > epoch3 & data$Time2 <= epoch4), "60s",
                                            ifelse(data$Time2 > epoch4, "60s+",NA)))))

  data$epoch <- as.factor(data$epoch)
  return(data)
}
