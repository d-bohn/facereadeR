# Simple functiont to deal with NAs

remove_na <- function(data){
  # Deal with NAs
  ### remove NAs
  row.has.na <- apply(data, 1, function(x){any(is.na(x))})
  ### shows you how many missing values you have
  sum(row.has.na)
  ### then remove any row that will have an NA in any column
  data <- data[!row.has.na,]
}

# Function to attempt to read FR files
read_fr <- function(file, skip){
  out <- tryCatch({
    read.table(file, sep="\t", skip=skip, as.is = TRUE,
                     strip.white = TRUE, fill=TRUE,
                     header=TRUE)
  }, warning = function(w) {
    print(w)
    return(null)
  }, error = function(e) {
    return(e)
  })
  return(out)
}

# Plot landmarks
plot_fr <- function(points, image) {
  xy <- paste0(rep(c('x','y'), times = 2), '_', rep(1:49, each = 2))

  coords <- points %>%
    dplyr::select(., xy) %>%
    gather(., point, value) %>%
    separate(., point, into = c('xy', 'num'), sep = '_') %>%
    spread(., xy, value)

  library(raster)
  img <- EBImage::readImage(image)

  label = c(coords['num'])

  res = dim(img)[1:2]
  plot(1,1,xlim=c(0,res[1]),ylim=c(res[2],0),asp=1,type='n',xaxs='i',yaxs='i',
       xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(img,1,1,res[1],res[2])
  points(coords[c('x','y')], pch = 20, col='red')
  # points(0,0,col='red',lwd=.5)
  text(coords[c('x','y')], labels=label$num, pos=3, cex = 0.5)
}
