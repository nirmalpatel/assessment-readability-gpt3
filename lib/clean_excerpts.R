clean_excerpts <- function(excerpts) {
  # first we clean the input excerpt
  excerpts %>%
    mutate(excerpt = str_replace(excerpt, "^[0-9]+\\.|, ", ""),
           excerpt = str_replace(excerpt, "^ ", ""),
           excerpt = sapply(excerpt, fix_first_word_if_possible))
}