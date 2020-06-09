library(blscrapeR)
library(dplyr)
library(tidyr)

df <- get_bls_county(c("February 2020", "March 2020", "April 2020"), seasonality = FALSE)
df$fips <- as.numeric(df$fips)
df$period <- as.character(df$period)

df$period[df$period == "2020-02-01"] <- "Feb2020"
df$period[df$period == "2020-03-01"] <- "Mar2020"
df$period[df$period == "2020-04-01"] <- "Apr2020"

network <- c(5069,
             8067,
             19157,
             20037,
             20111,
             21195,
             23011,
             26055,
             26103,
             27049,
             29031,
             35055,
             37195,
             39145,
             40123,
             41053,
             49021,
             50027,
             51015,
             55043, 39027)



network_df <- df %>% filter(fips %in% network)  

network_emp<- network_df %>% select(fips, area_title, period, employed) %>% 
  pivot_wider(names_from =  period, values_from = employed, names_prefix = "emp.") %>%
  mutate (emp_change = (emp.Apr2020 - emp.Feb2020)/emp.Feb2020) %>%
  select(fips, area_title, emp.Feb2020, emp.Apr2020, emp_change)

network_lf<- network_df %>% select(fips, area_title, period, labor_force) %>% 
  pivot_wider(names_from =  period, values_from = labor_force, names_prefix = "lf.") %>%
  mutate (lf_change = (lf.Apr2020 - lf.Feb2020)/lf.Feb2020) %>%
  select(fips, area_title, lf.Feb2020, lf.Apr2020, lf_change)

network_ue<- network_df %>% select(fips, area_title, period, unemployed_rate) %>% 
  pivot_wider(names_from =  period, values_from = unemployed_rate, names_prefix = "ue.") %>%
  select(fips, area_title, ue.Feb2020, ue.Apr2020)

network_final <- network_emp %>% left_join(network_lf, by = c('fips', 'area_title')) %>%
  left_join(network_ue, by = c('fips', 'area_title'))

write.csv(network_final, "network_emp_update_Apr2020.csv")
