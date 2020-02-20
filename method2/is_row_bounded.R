is_row_bounded <- function(header, x) {
  result <- tryCatch({
    header_v <- as.integer(str_split(header, ",")[[1]])
    x_v <- as.integer(str_split(x, ",")[[1]])
    if ((x_v[1] >= header_v[1] & x_v[1] <= header_v[2]) | 
        (x_v[2] >= header_v[1] & x_v[2] <= header_v[1]) | 
        (header_v[1] >= x_v[1] & header_v[1] <= x_v[2])) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(err) {
    return(FALSE)
  })
  return(result)
}