tabulator <- function(text, img) {

  is_bounded <- function(header, x, column = TRUE) { # arguments are strings with comma separated bounding boxes
    
    result <- tryCatch({
      header_v <- as.integer(str_split(header, ",")[[1]])
      x_v <- as.integer(str_split(x, ",")[[1]])
      if (column) {
        if ((x_v[1] >= header_v[1] & x_v[1] <= header_v[3]) | 
            (x_v[3] >= header_v[1] & x_v[3] <= header_v[3]) | 
            (header_v[1] >= x_v[1] & header_v[1] <= x_v[3])) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        if ((x_v[2] >= header_v[2] & x_v[2] <= header_v[4]) | 
            (x_v[4] >= header_v[2] & x_v[4] <= header_v[2]) | 
            (header_v[2] >= x_v[2] & header_v[2] <= x_v[4])) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }, error = function(err) {
      return(FALSE)
    })
    return(result)
  }
  
  
  # METHOD 1
  NO_DECIMALS <- TRUE
  header_boxes <- c(text$bbox[1])
  header_names <- c(text$word[1])
  row_boxes <- c()
  inserted_header <- FALSE
  not_valid <- FALSE
  final_data <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), header_names[1])
  
  
  for (i in 2:nrow(text)) {
    if (text$confidence[i] < 5) {
      not_valid <- TRUE
    } else {
      not_valid <- FALSE
    }
    for (j in 1:length(header_boxes)) {
      tester <- is_bounded(header_boxes[j], text$bbox[i])
      if (tester & nrow(final_data) == 0) {
        if (!not_valid) {
          final_data[1,] <- ""
          final_data[1, j] <- paste(final_data[1,j], text$word[i], collapse = " ")
          row_boxes <- append(row_boxes, text$bbox[i])
        }
      } else if (tester & nrow(final_data) != 0 & j != 1) {
        for (k in 1:nrow(final_data)) {
          if(is_bounded(row_boxes[k], text$bbox[i], FALSE)) {
            if (!not_valid) {
              final_data[k, j] <- paste(final_data[k,j], text$word[i], collapse = " ")
            }
            break
          } else if (k == nrow(final_data)) {
            if (!not_valid) {
              final_data[nrow(final_data) + 1,] <- ""
              final_data[nrow(final_data), j] <- paste(final_data[nrow(final_data), j], text$word[i], collapse = " ")
              row_boxes <- append(row_boxes, text$bbox[i])
            }
            break
          }
        }
      } else if (tester & nrow(final_data) != 0) {
        if (!not_valid) {
          if (is_bounded(tail(row_boxes, n = 1), text$bbox[i], FALSE)) {
            final_data[nrow(final_data), j] <- paste(final_data[nrow(final_data), j], text$word[i], collapse = " ")
          } else {
            final_data[nrow(final_data) + 1,] <- ""
            final_data[nrow(final_data), j] <- paste(final_data[nrow(final_data), j], text$word[i], collapse = " ")
            row_boxes <- append(row_boxes, text$bbox[i])
          }
        }
      } else if (j == length(header_boxes)) {
        header_boxes <- append(header_boxes, text$bbox[i])
        header_names <- append(header_names, text$word[i])
        if (nrow(final_data) != 0) {
          final_data[,ncol(final_data) + 1] <- ""
          final_data <- setNames(final_data, header_names)
        } else {
          final_data <- setNames(data.frame(matrix(ncol = length(header_names), nrow = 0)), header_names)
        }
        inserted_header <- TRUE
      } else {
        next
      }
      # We need to check if the placed element is also bounded by the next header
      while(nrow(final_data) != 0) {
        if (inserted_header | not_valid) {
          inserted_header <- FALSE
          break
        }
        if (j == ncol(final_data)) {
          break
        } else if (is_bounded(header_boxes[j + 1], text$bbox[i])) {
          for (k in 1:nrow(final_data)) {
            final_data[k, j] <- paste(final_data[k, j], final_data[k, j + 1], collapse = " ")
          }
          #final_data[,j] <- paste(final_data[,j], final_data[,j + 1], collapse = " ")
          final_data <- final_data[,-(j+1)]
          header_names[j] <- paste(header_names[j], header_names[j + 1], collapse = " ")
          header_names <- header_names[-(j + 1)]
          final_data <- setNames(final_data, header_names)
          
          #FIX ME: COMBINE BOXES
          first <- as.integer(str_split(header_boxes[j], ",")[[1]])
          last <- as.integer(str_split(header_boxes[j + 1], ",")[[1]])
          first[3] <- last[3]
          header_boxes[j] <- paste(first, collapse = ",")
          header_boxes <- header_boxes[-(j + 1)]
        } else {
          break
        }
      }
      break # Breaks to next ocr output if it is found
    }
  }
  data.copy <- final_data
  
  options(warn=-1)
  
  result <- tryCatch({
    removals <- c()
    # Clean-up
    for (i in 1:ncol(final_data)) {
      if (sum(final_data[,i] == "") == nrow(final_data)) {
        removals <- append(removals, i)
      }
      non_na_sum <- sum(!is.na(as.integer(final_data[,i])))
      if (non_na_sum == nrow(final_data)) {
        next
      } else if (non_na_sum > 0) {
        if (NO_DECIMALS) {
          final_data[,i] <- gsub("\\.", "", final_data[,i])
        }
        final_data[,i] <- gsub("O|o|C", "0", final_data[,i])
        final_data[,i] <- gsub("l|L", "1", final_data[,i])
      }
    }
    
    final_data <- final_data[,-removals]
    return(final_data)
  }, error = function(err) {
    return(data.copy)
  })
  return(result)
}  
  
  
