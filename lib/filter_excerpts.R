filter_len_keep_unq <- function(excerpts, n = 100) {
  
  excerpts %>%
    mutate(len = str_length(excerpt)) %>%
    dplyr::filter(len > n) %>%
    group_by(excerpt) %>%
    dplyr::filter(n() == 1) %>%
    ungroup() %>%
    extract(fname, c("grade", "module", "pgnum"), 'math\\-g([1-5]{1})\\-m([0-9]+)\\-full\\-module\\-([0-9]+)',
            remove = F)
}

filter_unwanted <- function(excerpts) {
  
  excerpts %>%
    dplyr::filter(
      
      # first select operations
      str_detect((excerpt), "^[A-Z0-9]{1}"), # excerpts need to start with something meaningful
      
      # then deselect operations
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
      !str_detect((excerpt), "Lesson [0-9]+"),
      !str_detect((excerpt), "[1-5]{1}\\.[A-Z0-9]+\\.[0-9]+") # for standard codes
    ) 
}
