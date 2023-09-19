pkgs = c("tidyverse", "tidytext", "tidyr", "ggplot2", "tm", "topicmodels", 'stm', "stringr", "textstem", "textclean", "data.table", 
         "quanteda", "ldatuning", "lubridate",  "purrr", "dplyr", "qdapRegex", "readxl", "corrplot", "psych", "fracdiff", "forecast", 
         "tseries", "vars", "lmtest", "tsDyn", "xts", "readr", "readxl","corrplot", "lmSupport", "prais")
load_pkgs = lapply(pkgs, library, character.only = TRUE)

### Section 1. Assembling dataset
## event dataset
# event timeline
df_event = read_excel("Immigration Event Timeline_0306_YZ_XY.xlsx", sheet = "Sheet2")
df_event = df_event %>% mutate(date = ymd(`Event Date`)) %>% dplyr::select(-`Event Date`:-Reference, -`Restricting of immigratoin\r\n(Restricting: 1, allowing:2)`)

# trump tweet
df_trump_tweets = read_excel("trump_tweets_2017.xlsx")
df_trump_tweets_imgt = df_trump_tweets %>% filter(str_detect(text, "migra")) %>% mutate(date = substr(created_at, 1, 10))
temp = data.frame(date = ymd(unique(df_trump_tweets_imgt$date)), trump_tweet = 1)

df_event = df_event %>% left_join(temp, by = "date")
colnames(df_event) = c("borderwall", "travelban", "visas", "sanctuary","family_sep", "dacadapa", "ice", "refugee","restrict", "allow", "immi_pro_events", "immi_p_crime", "imm_v_crime", "immi_hardships", "trump_related", "date", "trump_tweet")
df_event[is.na(df_event)] = 0

df_event = df_event %>%
  mutate(
    borderwall_restrict = ifelse(borderwall ==1 & restrict == 1, 1, 0),
    borderwall_allow = ifelse(borderwall ==1 & allow == 1, 1, 0),
    # borderwall_neutral = ifelse(borderwall ==1 & restrict == 0 & allow == 0, 1, 0),
    
    travelban_restrict = ifelse(travelban ==1 & restrict == 1, 1, 0),
    travelban_allow = ifelse(travelban ==1 & allow == 1, 1, 0),
    # travelban_neutral = ifelse(travelban ==1 & restrict == 0 & allow == 0, 1, 0),
    
    visas_restrict = ifelse(visas ==1 & restrict == 1, 1, 0),
    visas_allow = ifelse(visas ==1 & allow == 1, 1, 0),
    # visas_neutral = ifelse(visas ==1 & restrict == 0 & allow == 0, 1, 0),
    
    sanctuary_restrict = ifelse(sanctuary ==1 & restrict == 1, 1, 0),
    sanctuary_allow = ifelse(sanctuary ==1 & allow == 1, 1, 0),
    # sanctuary_neutral = ifelse(sanctuary ==1 & restrict == 0 & allow == 0, 1, 0),
    
    family_sep_restrict = ifelse(family_sep ==1 & restrict == 1, 1, 0),
    # family_sep_allow = ifelse(family_sep ==1 & allow == 1, 1, 0),
    # family_sep_neutral = ifelse(family_sep ==1 & restrict == 0 & allow == 0, 1, 0),
    
    dacadapa_restrict = ifelse(dacadapa ==1 & restrict == 1, 1, 0),
    dacadapa_allow = ifelse(dacadapa ==1 & allow == 1, 1, 0),
    # dacadapa_neutral = ifelse(dacadapa ==1 & restrict == 0 & allow == 0, 1, 0),
    
    ice_restrict = ifelse(ice ==1 & restrict == 1, 1, 0),
    ice_allow = ifelse(ice ==1 & allow == 1, 1, 0),
    # ice_neutral = ifelse(ice ==1 & restrict == 0 & allow == 0, 1, 0),
    
    refugee_restrict = ifelse(refugee ==1 & restrict == 1, 1, 0),
    # refugee_allow = ifelse(refugee ==1 & allow == 1, 1, 0),
    # refugee_neutral = ifelse(refugee ==1 & restrict == 0 & allow == 0, 1, 0),
  ) %>% #lapply(df_event[, c(18:41)], table)
  dplyr::select(date, everything())

df_event = df_event %>% group_by(date) %>% summarise_all(sum)
df_event[df_event == 2] = 1

# plot correlation of event
df_event2 = df_event[,c(12:31)]
df_event2_matrix <- cor(df_event2)
pdf(file = "AppendixC.pdf")
corrplot(df_event2_matrix, order = 'FPC', type = 'lower',tl.col = "indianred4") # Appendix C
dev.off()

## further assemble dataset
ts_imgt = readRDS("imgt_classified.rds") # saved file from the topic modeling result
ts_date = data.frame(date = seq(ymd("2017-01-01"), ymd("2017-12-30"), by = "day"))
ts_ideo = ts_imgt %>% filter(ideology != "irrelevant") %>% 
  count(date, ideology) %>% ungroup() %>% 
  mutate(date = ymd(date)) %>% dplyr::select(date, ideology, n) %>% 
  spread(key = ideology, value = n)
ts_ideo = ts_date %>% left_join(ts_ideo, by = "date")

final_ts = ts_ideo %>% left_join(df_event, by = "date")
final_ts[, c(5:34)][is.na(final_ts[, c(5:34)])] = 0
# write.csv(final_ts, file = "expression_polarization_ts_04122022.csv", row.names = FALSE) # code for saving the file

final_ts$liberal = na.interp(final_ts$liberal) # fill in missing data
final_ts$conservative = na.interp(final_ts$conservative)
final_ts$indeterminate = na.interp(final_ts$indeterminate)

### Section 2. Time series analysis
## Step 1. plot acf and pacf for Appendix F
pdf(file = "Appendix F(1).pdf")
par(mfrow = c(1, 2))
acf(final_ts$liberal,main = "")
pacf(final_ts$liberal,main = "")
dev.off()

pdf(file = "Appendix F(2).pdf")
par(mfrow = c(1, 2))
acf(final_ts$conservative,main = "")
pacf(final_ts$conservative,main = "")
dev.off()

pdf(file = "Appendix F(3).pdf")
par(mfrow = c(1, 2))
acf(final_ts$indeterminate,main = "")
pacf(final_ts$indeterminate,main = "")
dev.off()

## step 2. check stationarity
# note: all series are stationary
adf.test(final_ts$liberal) # p<.05, stationary
kpss.test(final_ts$liberal)

adf.test(final_ts$conservative) # p<.05, stationary
kpss.test(final_ts$conservative)

adf.test(final_ts$indeterminate) # p<.05, starionary
kpss.test(final_ts$indeterminate)

## step 3. detect lag using VAR
var.endo <- data.frame(conservative = final_ts$conservative, 
                       liberal = final_ts$liberal)
tsDyn::lags.select(var.endo) # lag = 1 based on best BIC

## step 4. Granger causality
lmtest::grangertest(final_ts$conservative ~ final_ts$liberal, order = 1)
lmtest::grangertest(final_ts$liberal ~ final_ts$conservative, order = 1)

## step 5. Prais-Winsten estimation
pw <- prais_winsten(final_ts$liberal ~
                      #policy variables
                      final_ts$borderwall_restrict + final_ts$borderwall_allow +
                      final_ts$travelban_restrict + final_ts$travelban_allow +
                      final_ts$visas_restrict + final_ts$visas_allow +
                      final_ts$sanctuary_restrict + final_ts$sanctuary_allow +
                      final_ts$family_sep_restrict + final_ts$dacadapa_restrict +
                      final_ts$dacadapa_allow + final_ts$ice_restrict +
                      final_ts$ice_allow + final_ts$refugee_restrict +
                      #immigrants centered variables
                      final_ts$immi_pro_events + final_ts$immi_p_crime +
                      final_ts$imm_v_crime + final_ts$immi_hardships +
                      #trump variables
                      final_ts$trump_related + final_ts$trump_tweet,
                    data = final_ts)
summary(pw)

pw <- prais_winsten(final_ts$conservative ~
                      #policy variables
                      final_ts$borderwall_restrict + final_ts$borderwall_allow +
                      final_ts$travelban_restrict + final_ts$travelban_allow +
                      final_ts$visas_restrict + final_ts$visas_allow +
                      final_ts$sanctuary_restrict + final_ts$sanctuary_allow +
                      final_ts$family_sep_restrict + final_ts$dacadapa_restrict +
                      final_ts$dacadapa_allow + final_ts$ice_restrict +
                      final_ts$ice_allow + final_ts$refugee_restrict +
                      #immigrants centered variables
                      final_ts$immi_pro_events + final_ts$immi_p_crime +
                      final_ts$imm_v_crime + final_ts$immi_hardships +
                      #trump variables
                      final_ts$trump_related + final_ts$trump_tweet,
                    data = final_ts)
summary(pw)

pw <- prais_winsten(final_ts$indeterminate ~
                      #policy variables
                      final_ts$borderwall_restrict + final_ts$borderwall_allow +
                      final_ts$travelban_restrict + final_ts$travelban_allow +
                      final_ts$visas_restrict + final_ts$visas_allow +
                      final_ts$sanctuary_restrict + final_ts$sanctuary_allow +
                      final_ts$family_sep_restrict + final_ts$dacadapa_restrict +
                      final_ts$dacadapa_allow + final_ts$ice_restrict +
                      final_ts$ice_allow + final_ts$refugee_restrict +
                      #immigrants centered variables
                      final_ts$immi_pro_events + final_ts$immi_p_crime +
                      final_ts$imm_v_crime + final_ts$immi_hardships +
                      #trump variables
                      final_ts$trump_related + final_ts$trump_tweet,
                    data = final_ts)
summary(pw)
