extract_excerpts <- function(grade) {
  
  # grade <- 5
  
  fnames <- list.files(file.path("engageny", paste0("grade", grade), "outp"), pattern = ".txt")
  
  txt <- lapply(fnames, function(x) {
    read_lines(file.path("engageny", paste0("grade", grade), "outp", x))
  })
  
  names(txt) <- fnames
  keep_pages_with_titles <- c("Homework", "Exit Ticket", "Problem Set", "Assessment")
  
  txt_filtered <- keep(txt, function(x) {
    str_detect(x[1], paste0(keep_pages_with_titles, collapse = "|")) & str_detect(x[1], "NYS")
  })
  
  # a document is a vector of strings, one element per line (as read by read_lines())
  process_document <- function(x) {
    
    if (length(x) <= 1) {
      warning("Cannot process a document with less than or equal to 1 sentence. Check your input.")
      return(c())
    }
    
    # create new element when you find new text
    # otherwise keep appending text in the last element
    all_sentences <- c()
    sentence_running <- FALSE
    
    for(i in seq_along(x)) {
      
      if (length(all_sentences) == 0) {
        
        if (trimws(x[i]) == "") { next }
        else {
          all_sentences <- c(all_sentences, x[i])
          sentence_running <- TRUE
        }
        
      } else {
        
        if (trimws(x[i]) == "") {
          sentence_running <- FALSE
          next
        } else {
          if (sentence_running) {
            # join x[i] in the last sentence if the sentence is running
            n <- length(all_sentences)
            all_sentences[n] <- paste(all_sentences[n], x[i])
          } else {
            # add new sentence if there is no sentence running
            all_sentences <- c(all_sentences, x[i])
            sentence_running <- TRUE
          }
        }
        
      }
      
    }
    
    return(all_sentences)
    
  }
  
  map2(txt_filtered, names(txt_filtered), function(x, y) {
    data.frame(excerpt = process_document(x),
               fname = y)
  }) %>%
    bind_rows() %>%
    as_tibble() -> excerpts
  
  excerpts
  
}