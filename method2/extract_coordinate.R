extract_coordinate <- function(text, row, index) {
  return(as.integer(text$bbox[row][[1]][index]))
}