library(tidyverse)
library(GGally)
library(factoextra)
library(magrittr)

load("04_exp_dataprep.Rdata")

# helpers -----------------------------------------------------------------

add_altcondname <- function(df) { df %>% mutate(altcondname = altcondnames[altcondcode]) %>% arrange(enum, altcondcode, tnum) }


# available data ----------------------------------------------------------

# machine translation
exp_rdf
exp_metrics
exp_csims
exp_bertfeats
altcondnames

# human translations
prompt_rdf
prompt_metrics
prompt_csims
prompt_bertfeats

exp_csims %>%
  group_by(altcondcode) %>%
  summarise(n = n()) %>%
  ungroup() -> altcondcode_counts

exp_rdf %>%
  inner_join(exp_metrics) %>%
  inner_join(exp_csims) %>%
  add_altcondname() %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch,
         n_common_words = common_words(excerpt, translation, metric = "count"),
         pct_common_words = common_words(excerpt, translation, metric = "percentage")) -> exp_rdf_extra

# num outputs by condition ------------------------------------------------

exp_rdf %>% count(altcondname)

# bert readability distribution -------------------------------------------

bert_input <- read_csv("imports/kaggle_bert_train.csv")

bert_input %>%
  ggplot(aes(target)) +
  geom_histogram(color = "black", fill = "white") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  labs(x = "BERT Readability", y = "# of passages in the training data",
       title = "Distribution of the BERT Readability metric in the training data")


# random examples ---------------------------------------------------------

exp_csims %>%
  add_altcondname() %>%
  inner_join(exp_metrics) %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch,
         prompt_perform = bert_imp * csim) %>%
  filter(bert_imp > 0.1, between(csim, 0.95, 0.975)) %>%
  inner_join(exp_rdf) -> h2_df

set.seed(10)
h2_df %>%
  sample_n(10) %>%
  View()

# how does human improvement look? ----------------------------------------

prompt_rdf %>%
  inner_join(prompt_metrics) %>%
  inner_join(prompt_csims) %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch) %>%
  select(enum, tnum, bert_imp, flesch_imp) %>%
  ggplot(aes(flesch_imp, bert_imp)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, alpha = .3) +
  geom_hline(yintercept = 0, alpha = .3) +
  scale_x_continuous(limits = c(-30, 30)) +
  scale_y_continuous(limits = c(-0.3, 0.3)) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Flesch Reading Ease Improvement (RE+)",
       y = "BERT Readability Improvement (BR+)",
       title = "Readability improvements for human simplifications (P = 11)",
       subtitle = "Each dot is one simplification")

# are we improving the readability overall? -------------------------------

mean(exp_rdf_extra$bert_imp)
mean(exp_rdf_extra$flesch_imp)

cor.test(exp_rdf_extra$bert_imp, exp_rdf_extra$flesch_imp)

exp_rdf_extra %>%
  select(ends_with("imp")) %>%
  rename(`BR+` = bert_imp,
         `RE+` = flesch_imp) %>%
  select(2, 1) %>%
  GGally::ggpairs(lower = list(continuous = wrap("points", alpha = 0.075))) +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(hjust = .25),
        axis.title.y = element_text(hjust = .25)) +
  labs(title = "Readability improvements for GPT-3 simplifications (M = 5653)",
       subtitle = "Each dot is one simplification",
       x = "RE+", y = "BR+")
  
cor.test(exp_rdf_extra$flesch, exp_rdf_extra$flesch_imp)

exp_rdf %>%
  add_altcondname() %>%
  inner_join(exp_metrics) %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch) %>%
  ggplot(aes(flesch, flesch_imp)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Input Flesch Reading Ease",
       y = "Flesch Reading Ease Improvement (RE+)",
       title = "Improvement v/s input readability (Flesch)",
       subtitle = "M = 5653 total simplifications, r = -0.445") -> p1


cor.test(exp_rdf_extra$bert, exp_rdf_extra$bert_imp)

exp_rdf %>%
  add_altcondname() %>%
  inner_join(exp_metrics) %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch) %>%
  ggplot(aes(bert, bert_imp)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Input BERT Readability",
       y = "BERT Readability Improvement (BR+)",
       title = "Improvement v/s input readability (BERT)",
       subtitle = "M = 5653 total simplifications, r = -0.744") -> p2

gridExtra::grid.arrange(p1, p2, ncol = 2)

exp_rdf %>%
  add_altcondname() %>%
  inner_join(exp_metrics) %>%
  mutate(bert_imp = bert_trn - bert,
         flesch_imp = flesch_trn - flesch) %>%
  select(altcondname, bert_imp, flesch_imp) %>%
  group_by(altcondname) %>%
  summarise(bertimpavg = mean(bert_imp),
            fleschimpavg = mean(flesch_imp),
            N = n()) %>%
  mutate_if(is.numeric, ~ round(., 2))
# write_csv("misc/table.csv")

exp_rdf_extra %>%
  select(altcondname, flesch_imp, bert_imp) %>%
  rename(`RE+` = flesch_imp,
         `BR+` = bert_imp) %>%
  gather(key = "metric", value = "measure", -altcondname) %>%
  mutate(metric = factor(metric, c("RE+", "BR+"))) %>%
  ggplot(aes(measure, str_wrap(fct_rev(altcondname), 20))) +
  geom_violin() +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Improvement in the readability measure",
       y = "Condition",
       title = "Readability improvement for different conditions")
  
# is the meaning being carried? -------------------------------------------

exp_rdf_extra %>%
  select(pct_common_words, csim) %>%
  rename(`% Common Words` = pct_common_words,
         `Cosine Similarity` = csim) %>%
  GGally::ggpairs(lower = list(continuous = wrap("points", alpha = 0.075))) +
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(hjust = .15),
        axis.title.y = element_text(hjust = .15)) +
  labs(title = "Text similarity metrics for GPT-3 simplifications (M = 5653)",
       subtitle = "Each dot is one simplification",
       x = "% common words", y = "Cosine similarity")

exp_rdf_extra %>%
  select(altcondname, pct_common_words, csim) %>%
  rename(`% Common Words` = pct_common_words,
         `Cosine Similarity` = csim) %>%
  gather(key = "metric", value = "measure", -altcondname) %>%
  mutate(metric = factor(metric, c("% Common Words", "Cosine Similarity"))) %>%
  ggplot(aes(measure, str_wrap(fct_rev(altcondname), 20))) +
  geom_violin() +
  facet_wrap(~ metric, scales = "free_x") +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(x = "Similarity measure value",
       y = "Condition",
       title = "Text similarity metrics for different conditions")

# attempt 1
# super small sample
exp_rdf_extra %>%
  filter(pct_common_words > 50, pct_common_words < 95, csim > 0.95, csim < 0.98, bert_imp > 0.03, flesch_imp > 3.0) %>%
  View()

# lets run Kmeans
exp_rdf_extra %>%
  select(pct_common_words, csim, bert_imp, flesch_imp) %>%
  as.matrix() -> km_input

fviz_nbclust(km_input, kmeans, method = "wss")

set.seed(100)
km_mod <- kmeans(km_input, centers = 15)

(
  km_mod$centers %>%
    as_tibble() %>%
    mutate(clustnum = row_number(),
           clustsize = km_mod$size) -> km_mod_stats
)

km_mod_stats %>%
  select(clustnum, clustsize, flesch_imp, bert_imp, pct_common_words, csim) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  write_csv("exports/km_res.csv")

exp_rdf_extra %>%
  mutate(clustnum = km_mod$cluster) %>%
  filter(clustnum == 15) %>%
  inner_join(sample2_excerpts) %>%
  select(grade, module, pgnum, enum, excerpt, translation, flesch, flesch_trn, flesch_imp, bert, bert_trn, bert_imp, pct_common_words, csim) %>%
  mutate(pgnum = as.integer(pgnum) + 1) %>% # offsetting the 0-based pagenum which was generated by python
  distinct() %>%
  
  # write_csv("exports/cluster_15_public.csv")
  View()



# discussion --------------------------------------------------------------

exp_rdf_extra %>%
  ggplot(aes(bert, bert_imp)) +
  geom_point() +
  facet_wrap(~ altcondname)

exp_rdf_extra %>%
  group_by(altcondname) %>%
  summarise(re_cor = cor(flesch, flesch_imp),
            bert_inp_imp_cor = cor(bert, bert_imp),
            n = n()) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>%
  write_csv("exports/discussion_inp_imp_cors.csv")
