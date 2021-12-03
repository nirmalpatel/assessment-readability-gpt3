# this function produces a pred_selected column
# this column will predict that the format of the selected passage is proper
mark_unformatted <- function(excerpts) {
  
  # first we do excerpt level filtering
  # producing the selected1 col
  excerpts %>%
    mutate(selected1 = case_when(
      
      str_detect(excerpt, "\\{|\\}") ~ F,
      str_detect(excerpt, "\\|") ~ F,
      str_detect(excerpt, "[0-9]+\\s+[0-9]+\\s+[0-9]+") ~ F,
      str_detect(excerpt, "[[:punct:]] [[:punct:]]") ~ F,
      str_detect(excerpt, "[[:punct:]]{2}") ~ F,
      str_detect(excerpt, "[[:punct:]]{3}") ~ F,
      str_detect(excerpt, "[a-z] ; [a-z]") ~ F,
      str_detect(excerpt, "\\. \\.") ~ F,
      TRUE ~ T
      
    )) -> excerpts_marked1
      
  
  # then we split excerpts into sentences
  # and check whether each sentence is valid or not
  # producing the selected2 col
  
  excerpts %>%
    # mutate(sentences = str_split(excerpt, "(?<!Mr|Mrs|Ms|Dr)[\\.?!] ")) %>%
    mutate(sentences = lapply(excerpt, sent_detect_nlp)) %>%
    unnest(sentences) %>%
    group_by(enum) %>%
    mutate(snum = row_number()) %>%
    ungroup() %>%
    mutate(slen = str_length(sentences)) -> all_sentences
  
  # finding % of valid words
  all_sentences %>%
    unnest_tokens("word", "sentences") %>%
    mutate(valid_word = is_word(word)) %>%
    group_by(enum, excerpt) %>%
    summarise(n_sentences = n_distinct(snum),
              word_correct_rate = mean(valid_word),
              n_words = n()) %>%
    ungroup() -> all_words
  
  all_sentences %>%
    group_by(enum, excerpt) %>%
    summarise(avg_slen = mean(slen),
              min_slen = min(slen)) %>%
    ungroup() %>%
    inner_join(all_words) %>%
    mutate(problem_found_in_excerpt = case_when(
      
      # can't have super short sentences
      # mininum one word sentence is "Explain."
      min_slen < 8 ~ T,
      
      # can't have these problematic sequences of chars
      str_detect(excerpt, "[a-zA-Z] = [a-zA-Z]") ~ T,
      str_detect(excerpt, " ; ") ~ T,
      
      # excerpt has to end with .!? otherwise we deselect
      !str_detect(excerpt, "[\\.?!]$") ~ T,
      
      # space/punct in the beginning
      str_detect(excerpt, "^\\s*=") ~ T,
      
      # there should be no standard code
      str_detect(excerpt, "[1-5]\\.[0-9A-Z]+\\.[0-9]+") ~ T,
      
      # numCharNum pattern not allowed e.g. 1a1
      str_detect(excerpt, "[0-9][a-zA-Z][0-9]") ~ T,
      
      # number and char or char and number without a s space between them
      str_detect(excerpt, "[0-9][A-Za-z]") ~ T,
      str_detect(excerpt, "[A-Za-z][0-9]") ~ T,
      
      # a non-alpha numeric with spaces around it
      str_detect(excerpt, " [^0-9A-Za-z] ") ~ T,
      
      # after = there should not be a char
      str_detect(excerpt, "=[0-9A-Za-z]") ~ T,
      
      TRUE ~ F
    ),
    # based on visual analysis
    selected2 = if_else(min_slen <= 100 & word_correct_rate >= 0.8 & !problem_found_in_excerpt, TRUE, FALSE)) -> excerpts_marked2
  
  excerpts_marked1 %>%
    inner_join(excerpts_marked2) %>%
    mutate(pred_selected = selected1 & selected2) %>%
    dplyr::select(colnames(excerpts), pred_selected)
    
}
