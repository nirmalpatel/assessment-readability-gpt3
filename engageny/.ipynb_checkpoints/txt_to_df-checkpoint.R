library(tidyverse)
library(purrr)

grade_excerpts <- function(grade) {
  
  # grade <- 5
  
  fnames <- list.files(paste0("grade", grade, "/outp/"), pattern = ".txt")
  
  txt <- lapply(fnames, function(x) {
    read_lines(file.path(paste0("grade", grade), "outp", x))
  })
  
  names(txt) <- fnames
  keep_pages_with_titles <- c("Homework", "Exit Ticket", "Problem Set", "Assessment")
  
  txt_filtered <- keep(txt, function(x) {
    str_detect(x[1], paste0(keep_pages_with_titles, collapse = "|")) & str_detect(x[1], "NYS")
  })
  
  # names(txt_filtered)
  # 
  # tibble(page_title = sapply(g1_txt, function(x) x[1]),
  #        fname = names(g1_txt)) %>%
  #   count(page_title) %>%
  #   View()
  # 
  # tibble(page_title = sapply(g1_txt_filtered, function(x) x[1]),
  #        fname = names(g1_txt_filtered)) %>%
  #   count(page_title) %>%
  #   View()
  
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
  
  # g1_txt_filtered
  
  map2(txt_filtered, names(txt_filtered), function(x, y) {
    data.frame(excerpt = process_document(x),
               fname = y)
  }) %>%
    bind_rows() %>%
    as_tibble() -> excerpts
  
  set.seed(100)
  
  excerpts %>%
    # mutate(excerpt = str_replace_all(excerpt, "[[:punct:][:blank:]]+", " "))
    mutate(len = str_length(excerpt)) %>%
    filter(between(len, 100, 300)) %>%
    group_by(excerpt) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    filter(
      
      # first select
      str_detect((excerpt), "^[A-Z0-9]{1}"), # excerpts need to start with something meaningful
      
      # then deselect
      
      !str_detect(str_to_lower(excerpt), "this work is licensed"),
      !str_detect(str_to_lower(excerpt), "licensed under"),
      !str_detect(str_to_lower(excerpt), "creative commons"),
      !str_detect(str_to_lower(excerpt), "create comm"),
      !str_detect(str_to_lower(excerpt), "©|@"),
      !str_detect(str_to_lower(excerpt), "great minds"),
      !str_detect(str_to_lower(excerpt), "noncomme"), # for non commercial
      !str_detect(str_to_lower(excerpt), "shareal"), # for ShareAlike
      !str_detect(str_to_lower(excerpt), "eureka lesson"),
      !str_detect((excerpt), "EUREKA"),
      !str_detect((excerpt), "\\{|\\}"),
      !str_detect((excerpt), "\\|"),
      !str_detect((excerpt), "[0-9]+\\s+[0-9]+\\s+[0-9]+"),
      !str_detect((excerpt), "Lesson [0-9]+"),
      !str_detect((excerpt), "[1-5]{1}\\.[A-Z]+\\.[0-9]+"),
      !str_detect((excerpt), "[[:punct:]] [[:punct:]]"),
      !str_detect((excerpt), "[[:punct:]]{2}"),
      !str_detect((excerpt), "[[:punct:]]{3}"),
      !str_detect((excerpt), "\\. \\.")) %>%
    sample_frac(1) %>%
    extract(fname, c("grade", "module", "pgnum"), 'math\\-g([1-5]{1})\\-m([0-9]+)\\-full\\-module\\-([0-9]+)',
            remove = F) -> excerpts_filtered
  
  excerpts_filtered
  
}

lapply(1:5, grade_excerpts) %>%
  bind_rows() -> all_excerpts

all_excerpts %>%
  mutate(rnum = row_number()) %>%
  select(rnum, fname, grade, module, pgnum, len, excerpt) %>%
  write_csv("engageny_excerpts_grades_1_to_5_export.csv")






