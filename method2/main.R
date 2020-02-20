library(tesseract)
library(stringr)
library(tibble)
library(berryFunctions)
source('extract_coordinate.R')
source('is_col_bounded.R')
source('is_row_bounded.R')
source('setup.R')
source('tabulator.R')

image_file <- ('tester.png')

valid_chars <- tesseract(language = "eng", options = list(tessedit_char_whitelist = " 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ."))
text <- tesseract::ocr_data(image_file, engine = valid_chars)
mydata <- tabulator(text)

write.table(mydata, "test.csv", sep = ",", row.names = FALSE, col.names = FALSE)



