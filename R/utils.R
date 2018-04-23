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
