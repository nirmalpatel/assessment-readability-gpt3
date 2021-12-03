library(tidyverse)
library(httr)
library(parallel)

# load("02_exp_design_v1.Rdata")
load("02_exp_design_v2.Rdata")

# for new conditions 6 7 and 8
exp_design_c678 <- exp_design %>%
  filter(condcode %in% c(6, 7, 8))

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)

orgid <- "your_org_id"
apikey <- "your_api_key"

# source("creds/gpt3_creds.R")

clusterExport(cl, "apikey")
clusterExport(cl, "orgid")

exp_design_split <- split(exp_design_c678, exp_design_c678$enum)

clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(httr))

ts1 <- Sys.time()
parLapply(cl, exp_design_split, function(x) {

  # x <- exp_design_split[[1]]
  n_translations <- 3

  lapply(split(x, x$condcode), function(x_cond) {
    
    POST(
      url = 'https://api.openai.com/v1/engines/davinci-instruct-beta-v3/completions',
      config = add_headers(Authorization = paste0('Bearer ',  apikey),
                           `OpenAI-Organization` = orgid,
                           `Content-type` = "application/json"),
      body = list(
        "n" = n_translations,
        "temperature" = 0.8,
        "prompt" = x_cond$prompt,
        "max_tokens" = x_cond$max_tokens,
        "stop" = if_else(!is.na(x_cond$stopseq), x_cond$stopseq, NULL),
        "frequency_penalty" = 0.2
        
      ),
      encode = "json"
    ) -> res
    
    translation <- sapply(content(res, as = "parsed")$choices, function(arg) { arg$text })
    
    expand(x_cond, crossing(x_cond, translation)) %>%
      mutate(tnum = row_number())
    
  }) %>%
    bind_rows()

}) -> exp_translations
ts2 <- Sys.time()

ts2 - ts1
stopCluster(cl)

exp_translations %>%
  bind_rows() %>%
  dplyr::select(enum, grade, excerpt, condcode, condname, tnum, translation, prompt) -> exp_output

# save(exp_output, file = "03_gpt3_data/sample2_excerpts_output.Rdata")

# exp_output_c678 <- exp_output
# save(exp_output_c678, file = "03_gpt3_data/sample2_excerpts_output_c678.Rdata")

# load("03_gpt3_data/sample2_excerpts_output.Rdata")
# load("03_gpt3_data/sample2_excerpts_output_c678.Rdata")
# 
# exp_output %>%
#   bind_rows(exp_output_c678) %>%
#   arrange(enum, condcode, tnum) -> exp_output_combined
# 
# save(exp_output_combined, file = "03_gpt3_data/sample2_excerpts_output_combined.Rdata")
