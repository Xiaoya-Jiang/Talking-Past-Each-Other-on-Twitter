######## Part 1. Topic modeling - supplementary analysis for validation
pkgs = c("tm", "topicmodels", 'stm', "stringr", "textstem", "textclean", "data.table", "tidyr", "quanteda", "ldatuning", "lubridate",  "purrr", "dplyr", "tidyverse", "tidytext", "ggplot2", "qdapRegex", "readxl", "readr", "lmSupport", "car", "plot")
load_pkgs = lapply(pkgs, library, character.only = TRUE)

### validation for ideological stance of tweets based on manual coding
## process in generating the files
# sample tweets
df_imgt_unique = readRDS("df_imgt_unique.rds")
set.seed(202203)
sample_tweets = df_imgt_unique %>% sample_n(200)
ts_imgt = readRDS("imgt_classified.rds")
colnames(ts_imgt)
temp = ts_imgt %>% dplyr::select(tweet, ideology) %>% distinct()
sample_tweets_from_all = sample_tweets %>% left_join(temp, by = "tweet")
table(sample_tweets_from_all$ideology)

set.seed(202203)
sample_tweets_from_classified = temp %>% sample_n(400)
table(sample_tweets_from_classified$ideology)
sample_tweets_from_classified = sample_tweets_from_classified %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = ideology, values_from = value) %>% 
  replace(is.na(.), 0)
rio::export(sample_tweets_from_all, file = "sample_tweets_recall_03042022.xlsx")
rio::export(sample_tweets_from_classified, file = "sample_tweets_precision_03042022.xlsx")

# sample users
sample_lib = ts_imgt %>% filter(ideology == "liberal") %>% distinct(user.id_str) %>% sample_n(size = 500) %>% mutate(ideology = "liberal")
sample_con = ts_imgt %>% filter(ideology == "conservative") %>% distinct(user.id_str) %>% sample_n(size = 500) %>% mutate(ideology = "conservative")
users = ts_imgt %>% dplyr::select (user.id_str:user.verified) %>% group_by (user.id_str) %>% slice_max(order_by = user.followers_count, n = 1, with_ties = FALSE)
sampe_users = bind_rows(sample_lib, sample_con) %>% left_join(users, by = "user.id_str")
write.csv(sampe_users, file = "sample_users_10242021.csv")

### validation of ideological stance of tweets based on user ideology
d1 <- read.csv("sample_users_scale1031_to1500.csv", header=T)
d = d1 %>%
  filter(user_ideo != 0) %>%
  filter(user_ideo != "Inf")

varDescribe(d$user_ideo[d$tweet_ideo == "liberal"]) #mean user ideology score in the liberal tweet group: -0.65
varDescribe(d$user_ideo[d$tweet_ideo == "conservative"]) #mean user ideology score in the conservative tweet group: 0.21
pdf(file = "AppendixH.pdf") # Appendix H
boxplot(user_ideo ~ tweet_ideo, xlab = "Tweet Ideology", ylab="Scaled User Ideology", data = d)
dev.off()
