######## Part 1. Topic modeling - Main analysis
pkgs = c("tm", "topicmodels", 'stm', "stringr", "textstem", "textclean", "data.table", "tidyr", "quanteda", "ldatuning", "lubridate",  "purrr", "dplyr", "tidyverse", "tidytext", "ggplot2", "qdapRegex", "readxl", "furrr")
load_pkgs = lapply(pkgs, library, character.only = TRUE)

### Step 1. read and process data
# custom lemmatization functions
tokens_lemma <- function(x) {
  type <- attr(x, 'types')
  type_new <- lemmatize_words(type)
  attr(x, 'types') <- type_new
  quanteda:::tokens_recompile(x)
}

# read original data with batches of data downloads and remove duplicate tweets
df_out = readRDS("immigration_all_terms.Rds") %>% 
  dplyr::select(-string) %>% unique() %>%
  # mutate (date = substr(created_at, 1, 10)) %>% 
  mutate(created_at1 = as.POSIXct(strptime(created_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
         created_at1 = trimws(gsub(" EST", "", as.character(format(created_at1, tz="EST",usetz=TRUE)))),
         date = substr(created_at1, 1, 10)) %>%
  filter(date < "2018-01-01")

# detect and remove non-English tweets after creating a separate text variable
df_out$text1 = gsub("@\\w+", " ", df_out$tweet) 
df_out$text1 = gsub("#", "", df_out$text1)
df_out$text1 = rm_url(df_out$text1, pattern = pastex("@rm_twitter_url", "@rm_url"))
df_out$text1 = trimws(df_out$text1)

df_out = df_out %>% mutate(lang3 = cld3::detect_language(text1), lang2 = cld2::detect_language(text1) )
df_imgt = df_out %>% filter (lang3 == "en" | lang2 == "en" | (is.na(lang3) & is.na(lang2))) 

# remove special characters, take unique tweets 
df_imgt$tweet = rm_url(df_imgt$tweet, pattern = pastex("@rm_twitter_url", "@rm_url"))
df_imgt$tweet = gsub("[^\x01-\x7F]", "", df_imgt$tweet)
df_imgt$tweet = gsub("\r?\n|\r", " ", df_imgt$tweet) 
df_imgt$tweet = gsub("RT @\\w+", "", df_imgt$tweet)
df_imgt$tweet = gsub("@\\w+", "", df_imgt$tweet)
df_imgt$tweet = iconv(df_imgt$tweet, to = "ASCII", sub = " ") 
df_imgt$tweet = trimws(df_imgt$tweet)
df_imgt$tweet = tolower(df_imgt$tweet)
saveRDS(df_imgt, file = "df_imgt.rds")
# df_imgt = readRDS("df_imgt.rds") # note: could also directly read this file which is available in the data file

# take the unique tweets
df_imgt_unique = df_imgt %>% dplyr::select(tweet) %>% unique()
saveRDS(df_imgt_unique, file = "df_imgt_unique.rds")

corpus <- corpus(df_imgt_unique$tweet) #build "corpus"
toks <- quanteda::tokens(corpus,remove_numbers=T, remove_punct=T, remove_symbols=T)
toks <- tokens_lemma(toks)
toks <- tokens_remove(toks, pattern = stopwords('en'))
toks = tokens_select(toks, c("s", "t"), selection = "remove", case_insensitive = TRUE)

dfm <- dfm(toks)
dfm 
dfm_trim <- dfm_trim(dfm, min_docfreq = 0.00005, max_docfreq = 0.9, docfreq_type = "prop") #see (Grinberg et al., 2019) "Words occurring in fewer than 0.02% or more than 90% of tweets are ignored"
dfm_trim <- dfm_trim[ntoken(dfm_trim) > 0,]

### Step 2. find the optimal k
# note: code in this step may take a long time to run
many_models <- tibble(K = seq(10, 50, by = 2)) %>% 
   mutate(topic_model = future_map(K, ~stm(dfm_trim, K = ., verbose = FALSE)))
 
heldout <- make.heldout(dfm_trim)
 
k_result <- many_models %>%
   mutate(exclusivity = map(topic_model, exclusivity),
          semantic_coherence = map(topic_model, semanticCoherence, dfm_trim),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
          residual = map(topic_model, checkResiduals, dfm_trim),
          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
          lbound = bound + lfact,
          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
k_result
 
k_result %>%
   transmute(K,
             `Lower bound` = lbound,
             Residuals = map_dbl(residual, "dispersion"),
             `Semantic coherence` = map_dbl(semantic_coherence, mean),
             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
   ) %>%
   gather(Metric, Value, -K) %>%
   ggplot(aes(K, Value, color = Metric)) +
   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
   geom_point() +
   facet_wrap(~Metric, scales = "free_y") +
   labs(x = "K (number of topics)",
        y = NULL)
# ggsave(file = "data_20211021/imgt_bestk.jpeg", dpi = 600, height = 8, width = 10)

### Step 3. topic modeling
k = 24  # optimal k obtained by the gibbs method
control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE, #the starting value for alpha is 50/k suggested by Griffiths & Steyvers (2004)
                          verbose = 0, prefix = tempfile(),
                          save = 0, keep = 0,  #no information is printed during the algorithm; no immediate results are saved
                          seed = 999, #random seed for reproducibility
                          nstart = 1, #number of repeated runs with random initializations
                          best = TRUE, #returns only the best one model
                          delta = 0.1, #specifies the parameter of the prior distribution of the term distribution over topics. The default is 0.1
                          iter = 2000, #iterations 
                          burnin = 100, #the first 100 iterations are discarded 
                          thin = 2000) #then every 2000th iteration is returned
lda <- topicmodels::LDA(dfm_trim, k=k, method="Gibbs", control = control_LDA_Gibbs)
saveRDS(lda, file = "Gibbs_lda.rds")

### Step 4. explore the topics
lda = readRDS("Gibbs_lda.rds") # note: could directly read the Gibbs_lda file which is available in the data file
td_beta = tidy(lda, matrix="beta")
top_terms <- td_beta %>% group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
write.csv(top_terms, file = "top_terms.csv")

td_gamma = tidy(lda, matrix="gamma")
td_gamma$document <- gsub("text", "", td_gamma$document)
td_gamma$document <- as.numeric(td_gamma$document)
df_imgt_unique = df_imgt_unique %>% mutate(document = row_number())
top_docs = td_gamma %>%
  arrange(-gamma) %>%
  group_by(topic) %>%
  slice(1:1000) %>% 
  left_join(df_imgt_unique, by = "document")
write.csv(top_docs, file = "top_docs.csv")

### Step 5. topic interpretation
labels = read_excel("topic_label.xlsx", sheet = "Sheet2") %>% 
  mutate(topic_label = paste0(trimws(topic), "-", topic_label), topic = as.integer(trimws(topic))) %>% 
  arrange(topic)
top_terms <- td_beta %>% group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms = top_terms %>% left_join(labels, by = "topic")
top_terms$topic_label = factor(top_terms$topic_label, levels = labels$topic_label)

g1= top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, nrow = 6, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme_bw(base_size = 8) +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )
pdf("g1.pdf", height = 14, width = 12) # Figure1_top terms_08212023
print(g1)
dev.off() 

### Step 6. classification
df_imgt = readRDS("df_imgt.rds")
df_imgt_unique = readRDS("df_imgt_unique.rds") %>% mutate(document = row_number())
lda = readRDS("data_20211021/Gibbs_lda.rds")
td_gamma = tidy(lda, matrix="gamma")
td_gamma$document <- gsub("text", "", td_gamma$document)
td_gamma$document <- as.numeric(td_gamma$document)

topic_tweet = td_gamma %>% 
  filter(gamma>= 0.1) %>%
  group_by(document) %>%
  slice_max(order_by = gamma, n =1) %>%
  add_count(document) %>% 
  left_join(df_imgt_unique, by = "document") 

ts_imgt = df_imgt %>% left_join(topic_tweet, by = "tweet") %>% 
  filter(!is.na(topic)) %>% 
  left_join(labels, by = "topic")
saveRDS(ts_imgt, file = "imgt_classified.rds")

### visualize the outcome variables
ts_ideology = ts_imgt %>% filter(ideology != "irrelevant") %>% group_by(date, ideology) %>% summarise(num_tweets = n())
ts_ideology$ideology = factor(ts_ideology$ideology, c("liberal", "conservative", "indeterminate")) 
g2 =ts_ideology %>% ggplot(aes(x = ymd(date), y = num_tweets)) + geom_line() + facet_wrap(~ideology, nrow= 3, scales = "free_y")  + 
  labs(y = 'Number of Tweets', x = "Date") +  theme_bw(base_size = 8)
pdf("g2.pdf", height = 10, width = 12) # Figure2_time series of tweets by ideology08212023
print(g2)
dev.off() 