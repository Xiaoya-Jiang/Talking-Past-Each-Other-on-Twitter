###### supplementary analysis: data wrangling and analysis at the theme level for appendix
pkgs = c("tidyverse", "tidytext", "tidyr", "ggplot2", "tm", "topicmodels", 'stm', "stringr", "textstem", "textclean", "data.table", 
         "quanteda", "ldatuning", "lubridate",  "purrr", "dplyr", "qdapRegex", "readxl", "corrplot", "psych", "fracdiff", "forecast", 
         "tseries", "vars", "lmtest", "tsDyn", "xts", "readr", "readxl","corrplot", "lmSupport", "prais")
load_pkgs = lapply(pkgs, library, character.only = TRUE)

### Section 1. Assembling dataset
ts_imgt = readRDS("imgt_classified.rds") # file from topic modeling, available in data file
ts_date = data.frame(date = seq(ymd("2017-01-01"), ymd("2017-12-30"), by = "day"))
ts_theme = ts_imgt %>% filter(theme != "irrelevant") %>% 
  count(date, theme) %>% ungroup() %>% 
  mutate(date = ymd(date)) %>% dplyr::select(date, theme, n) %>% 
  spread(key = theme, value = n)
ts_theme = ts_date %>% left_join(ts_theme, by = "date")

final_ts_theme = ts_theme %>% left_join(df_event, by = "date")
final_ts_theme[, c(9:38)][is.na(final_ts_theme[, c(9:38)])] = 0
View(final_ts_theme)

# fill in missing data
final_ts_theme$`call to support immigrants` = na.interp(final_ts_theme$`call to support immigrants`)
final_ts_theme$`immigrants' value` = na.interp(final_ts_theme$`immigrants' value`)
final_ts_theme$`call to restrict immigrants` = na.interp(final_ts_theme$`call to restrict immigrants`)
final_ts_theme$`immigrants' threat` = na.interp(final_ts_theme$`immigrants' threat`)
final_ts_theme$`news` = na.interp(final_ts_theme$`news`)
final_ts_theme$`emotional expression` = na.interp(final_ts_theme$`emotional expression`)
final_ts_theme$`mixed` = na.interp(final_ts_theme$`mixed`)
View(final_ts_theme)

### Section 2. Time series analysis
## check stationarity. note: all series are stationary
adf.test(final_ts_theme$`call to support immigrants`) # p<.05, stationary
adf.test(final_ts_theme$`immigrants' value`) # p<.05, stationary
adf.test(final_ts_theme$`call to restrict immigrants`) # p<.05, stationary 
adf.test(final_ts_theme$`immigrants' threat`) # p<.05, stationary
adf.test(final_ts_theme$`news`) # p<.05, stationary
adf.test(final_ts_theme$`emotional expression`) # p<.05, stationary  
adf.test(final_ts_theme$`mixed`) # p<.05, stationary

## Prais-Winsten estimation
#call to support immigrants
pw_call_to_support <- prais_winsten(final_ts_theme$`call to support immigrants` ~
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
                                    data = final_ts_theme)
summary(pw_call_to_support)

#immigrants' value
pw_immigrants_value <- prais_winsten(final_ts_theme$`immigrants' value` ~
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
                                     data = final_ts_theme)
summary(pw_immigrants_value)

#call to restrict immigrants
pw_call_to_restrict <- prais_winsten(final_ts_theme$`call to restrict immigrants` ~
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
                                     data = final_ts_theme)
summary(pw_call_to_restrict)

#immigrants' threat
pw_immigrants_threat <- prais_winsten(final_ts_theme$`immigrants' threat` ~
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
                                      data = final_ts_theme)
summary(pw_immigrants_threat)

#news
pw_news <- prais_winsten(final_ts_theme$`news` ~
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
                         data = final_ts_theme)
summary(pw_news)

#emotional expression
pw_emotional_expression <- prais_winsten(final_ts_theme$`emotional expression` ~
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
                                         data = final_ts_theme)
summary(pw_emotional_expression)

#mixed
pw_mixed <- prais_winsten(final_ts_theme$`mixed` ~
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
                          data = final_ts_theme)
summary(pw_mixed)
