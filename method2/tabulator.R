tabulator <- function(text) {
  text$bbox[1] <- "1424,0,1509,33"
  text$bbox[2] <- "1474,34,1489,45"
  
  text$split <- strsplit(text$bbox, ",")
  looper <- nrow(text)
  merging_strings_warning <- FALSE
  mydata <- setup(text)
  
  
  for (to_insert in 2:looper) {
    
    insert_column <- 0
    for (columns in 1:length(colnames(mydata))) {
      if (is_col_bounded(colnames(mydata)[columns], paste(text$split[[to_insert]][c(1,3)], collapse = ","))) {
        insert_column <- columns
      }
      
    }
    
    insert_row <- 0
    for (rows in length(rownames(mydata)):1) {
      if (is_row_bounded(rownames(mydata)[rows], paste(text$split[[to_insert]][c(2,4)], collapse = ","))) {
        insert_row <- rows
        break
      }
    }
    
    if (insert_column != 0 & insert_row != 0) { # In existing column and row
      if (mydata[insert_row, insert_column] == "") {
        mydata[insert_row, insert_column] <- to_insert
        merging_strings_warning <- FALSE
      } else {
        if (!is_col_bounded(paste(text$split[[as.integer(mydata[insert_row, insert_column])]][c(1,3)], collapse = ","), 
                            paste(text$split[[to_insert]][c(1,3)], collapse = ","))) {
          text[nrow(text) + 1,] <- ""
          text$word[nrow(text)] <- paste(text$word[as.integer(mydata[insert_row, insert_column])], 
                                         text$word[to_insert])
          text$confidence[nrow(text)] <- 99
          insert_split <- text$split[[as.integer(mydata[insert_row, insert_column])]]
          text$split[[nrow(text)]] <- c(insert_split[c(1,2)], text$split[[to_insert]][3], insert_split[4])
          text$bbox[nrow(text)] <- paste(text$split[[nrow(text)]], collapse = ",")
          interm <- colnames(mydata)[insert_column]
          interm <- strsplit(interm, ",")[[1]]
          if (as.integer(interm[2]) < as.integer(text$split[[to_insert]][3])) {
            interm[2] <- text$split[[to_insert]][3]
            colnames(mydata)[insert_column] <- paste(interm, collapse = ",")
          }
          mydata[insert_row, insert_column] <- nrow(text)
          merging_strings_warning <- TRUE
        } else {
          mydata[nrow(mydata) + 1, ] <- ""
          rownames(mydata)[nrow(mydata)] <- paste(text$split[[to_insert]][c(2,4)], collapse = ",")
          mydata[nrow(mydata), insert_column] <- to_insert
          merging_strings_warning <- FALSE
        }
      }
      
    } else if (insert_column != 0) { # Not in row
      mydata[nrow(mydata) + 1, ] <- ""
      rownames(mydata)[nrow(mydata)] <- paste(text$split[[to_insert]][c(2,4)], collapse = ",")
      mydata[nrow(mydata), insert_column] <- to_insert
      merging_strings_warning <- FALSE
    } else if (insert_row != 0) { # Not in column
      data_columns <- strsplit(colnames(mydata), ",")
      found <- FALSE
      for (columns in 1:length(data_columns)) {
        if (as.integer(text$split[[to_insert]][3]) < as.integer(data_columns[[columns]][1])) {
          found <- TRUE
          if (merging_strings_warning) {
            text$word[as.integer(mydata[insert_row, columns - 1])] <- 
              paste(text$word[as.integer(mydata[insert_row, columns - 1])], text$word[to_insert])
            text$split[[as.integer(mydata[insert_row, columns - 1])]][3] <- text$split[[to_insert]][3]
            text$bbox[nrow(text)] <- paste(text$split[[nrow(text)]], collapse = ",")
            interm <- colnames(mydata)[columns - 1]
            interm <- strsplit(interm, ",")[[1]]
            interm[2] <- text$split[[to_insert]][3]
            colnames(mydata)[columns - 1] <- paste(interm, collapse = ",")
          } else {
            mydata <- add_column(mydata, temp = "", .before = columns)
            colnames(mydata)[columns] <- paste(text$split[[to_insert]][c(1,3)], collapse = ",")
            mydata[insert_row, columns] <- to_insert
            merging_strings_warning <- FALSE
          }
          break
        }
      }
      if (!found) {
        mydata <- add_column(mydata, temp = "", .after = columns)
        colnames(mydata)[columns + 1] <- paste(text$split[[to_insert]][c(1,3)], collapse = ",")
        mydata[insert_row, columns + 1] <- to_insert
        merging_strings_warning <- FALSE
      }
    } else { # Not in column or row
      mydata[nrow(mydata) + 1, ] <- ""
      merging_strings_warning <- FALSE
      rownames(mydata)[nrow(mydata)] <- paste(text$split[[to_insert]][c(2,4)], collapse = ",")
      insert_row <- nrow(mydata)
      data_columns <- strsplit(colnames(mydata), ",")
      found <- FALSE
      for (columns in 1:length(data_columns)) {
        if (as.integer(text$split[[to_insert]][3]) < as.integer(data_columns[[columns]][1])) {
          found <- TRUE
          mydata <- add_column(mydata, temp = "", .before = columns)
          colnames(mydata)[columns] <- paste(text$split[[to_insert]][c(1,3)], collapse = ",")
          mydata[insert_row, columns] <- to_insert
          break
        }
      }
      if (!found) {
        mydata <- add_column(mydata, temp = "", .after = columns)
        colnames(mydata)[columns + 1] <- paste(text$split[[to_insert]][c(1,3)], collapse = ",")
        mydata[insert_row, columns + 1] <- to_insert
      }
    }
  }
  
  
  # Replace indices with actual values
  
  for (i in 1:ncol(mydata)) {
    for (j in 1:nrow(mydata)) {
      if (mydata[j,i] != "") {
        mydata[j,i] <- text$word[as.integer(mydata[j,i])]
      }
    }
  }
  
  return(mydata)
}


