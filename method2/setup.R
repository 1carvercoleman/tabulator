setup <- function(text) {
  mydata <- data.frame(1, stringsAsFactors = FALSE)
  colnames(mydata) <- paste(text$split[[1]][c(1,3)], collapse = ",")
  rownames(mydata) <- paste(text$split[[1]][c(2,4)], collapse = ",")
  return(mydata)
}
