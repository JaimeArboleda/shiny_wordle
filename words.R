library(readr)
library(stringr)
library(stringi)
library(tidyverse)
library(rlang)


get_words_df <- function(filename = "words.txt"){
  words_unparsed <- c(readr::read_file("words.txt"))
  a <- str_split(words_unparsed, "solution:", simplify = TRUE)
  a <- a[2:length(a)]
  matches <- str_match(a, "\"([a-z0-9\\\\]*)\"")
  word_list <- stri_unescape_unicode(matches[,2])
  words <- lapply(word_list, 
                      function(a) 
                        str_replace_all(a, c("í" = "i", "ó" = "o", "é" = "e", "á" = "a", "ú" = "u" )))
  words <- unique(words)
  letters <- str_split("abcdefghijklmnñopqrstuvwxyz", "")[[1]]
  type_filter <- c("contains_exact", "contains_at_least", "position_is", "position_not_is")
  
  wordle_data <- data.frame(
    word = unlist(words)
  )
  wordle_data["pos_1"] <- apply(wordle_data, 1, function(x) substr(x, 1, 1))
  wordle_data["pos_2"] <- apply(wordle_data["word"], 1, function(x) substr(x, 2, 2))
  wordle_data["pos_3"] <- apply(wordle_data["word"], 1, function(x) substr(x, 3, 3))
  wordle_data["pos_4"] <- apply(wordle_data["word"], 1, function(x) substr(x, 4, 4))
  wordle_data["pos_5"] <- apply(wordle_data["word"], 1, function(x) substr(x, 5, 5))
  wordle_data["prob"] <- 1
  
  for (letter in letters){
    for (filter in type_filter){
      if (filter == "contains_exact"){
        ns <- c(0, 1, 2, 3)
      } else if (filter == "contains_at_least"){
        ns <- c(1, 2, 3)
      } else {
        ns <- c(1, 2, 3, 4, 5)
      }
      for (n in ns){
        if (filter == "contains_exact"){
          logical_col <- str_count(wordle_data[,"word"], letter) == n 
        } else if (filter == "contains_at_least"){
          logical_col <- str_count(wordle_data[,"word"], letter) >= n 
        } else if (filter == "position_is"){
          logical_col <- (wordle_data[paste("pos_", n, sep="")] == letter)[,]
        } else{
          logical_col <- (wordle_data[paste("pos_", n, sep="")] != letter)[,]
        }
        wordle_data[paste(letter, filter, n, sep=".")] <- logical_col
      }
    }
  }
  as_tibble(wordle_data)
}
