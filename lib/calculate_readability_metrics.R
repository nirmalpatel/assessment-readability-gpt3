calculate_readability_metrics <- function(x) {
  
  lapply(x, function(txt) {
    # cat(txt)
    # txt <- x[1]
    x_tok <- tokenize(txt, format = "obj", lang = "en")
    x_res <- koRpus::readability(x_tok)
    
    data.frame(
      ari = x_res@ARI$grade,
      flesch = x_res@Flesch$RE,
      fleschkincaid = x_res@Flesch.Kincaid$grade,
      fog = x_res@FOG$FOG,
      linsearwrite = x_res@Linsear.Write$raw,
      smog = x_res@SMOG$grade
    )
    
  }) %>%
    bind_rows()
  
}
