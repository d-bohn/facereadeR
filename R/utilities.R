remove_na <- function(data){
  # Deal with NAs
  ### remove NAs
  row.has.na <- apply(data, 1, function(x){any(is.na(x))})
  ### shows you how many missing values you have
  sum(row.has.na)
  ### then remove any row that will have an NA in any column
  data <- data[!row.has.na,]
}