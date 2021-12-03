library(qdapDictionaries)

is_word  <- function(x) {
  
  x %in% GradyAugmented
}

# idea
# take the first word, and see if it starts with "A"
# replace the first "A" and see if the remainig is a valid word
# if it is, then split the first word into "A" "word"
# return same sentence otherwise
fix_first_word_if_possible <- function(x) {
  
  get_first_word <- function(y) { str_match(y, "^([A-Za-z]+).+")[1, 2] }
  
  fw <- get_first_word(x)
  
  if (strsplit(fw, "")[[1]][1] == "A" & is_word(str_replace(fw, "^A", ""))) {
    str_replace(x, fw, paste0("A ", str_replace(fw, "^A", "")))
  } else {
    x
  }
  
}

remove_last_incomplete_sent <- function(x) {
  res <- sent_detect_nlp(x)
  lastelem <- res[length(res)]
  if (!str_detect(lastelem, ".+[\\.?!]$")) {
    res <- res[1:(length(res) - 1)]
  }
  paste0(res, collapse = " ")
}

sent_count <- function(x) {
  length(sent_detect_nlp(x))
}

# find common words between sentences in
# vectors x and y
# metrics = count/percentage
common_words <- function(x, y, metric = "count") {
  
  require(purrr)
  
  stopifnot(length(x) == length(y))
  
  map2(x, y, function(sent1, sent2) {
    
    tryCatch({
      set1 <- qdap::all_words(sent1)$WORD
      set2 <- qdap::all_words(sent2)$WORD
      
      if (metric == "count") {
        return(length(intersect(set1, set2)))  
      } else if (metric == "percentage") {
        return(length(intersect(set1, set2)) / length(union(set1, set2)) * 100)  
      }
      
      
    }, error = function(e) {
      return(0)
    })
    
  }) %>%
    as.numeric()
}
