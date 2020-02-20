library(tesseract)
library(pdftools)
library(magick)
library(qdap)
library(stringr)
library(Rcpp)
source('tabulator.R')

cut_file <- ('example_page_county_codes_1.png')
valid_chars <- tesseract(language = "eng", options = list(tessedit_char_whitelist = " 0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ."))
text <- tesseract::ocr_data(cut_file, engine = valid_chars)
img <- magick::image_read(cut_file)

final_data <- tabulator(text, img)

write.csv(final_data, "example_codes.csv", row.names = FALSE)
