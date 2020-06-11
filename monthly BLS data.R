library(blscrapeR)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(sf)


require(DBI)
require(dbplyr)
require(config)
source("../base/connections/coririsi.R")

prop <- tbl(coririsi, in_schema("sch_layer", "bea_caemp25n_county_01_18")) %>% 
  collect() %>% select(geoid,year, prop_emp, total_emp) %>% 
  filter(year == 2007 | year == 2018) %>%
  gather("var", "emp", prop_emp, total_emp) %>%
  unite(temp, var, year) %>%
  spread(temp, emp) %>%
  mutate(dprop07_18 = (prop_emp_2018 - prop_emp_2007)/prop_emp_2007) %>%
  mutate(fips = as.numeric(geoid)) %>%
  select(fips,dprop07_18, prop_emp_2007, prop_emp_2018) 

pop <- tbl(coririsi, in_schema("sch_layer", "bea_cainc4_county_69_18")) %>% 
  collect() %>% 
  select(geoid,year, pop) %>% 
  filter(year == 2007 | year == 2018) %>%
  gather("var", "pop", pop) %>%
  unite(temp, var, year) %>%
  spread(temp, pop) %>%
  mutate(dpop07_18 = (pop_2018 - pop_2007)/pop_2007) %>%
  mutate(fips = as.numeric(geoid)) %>%
  select(fips, dpop07_18, pop_2007, pop_2018) 
 

broadband <- tbl(coririsi, in_schema("sch_analysis", "la_counties_broadband")) %>% 
  collect() %>% mutate(broadband = f477_maxad_downup_2018dec_25_3_popsum/pop2018_sum) %>%
  mutate(fips = as.numeric(geoid_co)) %>%
  select(fips, broadband,f477_maxad_downup_2018dec_25_3_popsum,pop2018_sum)
  
young_firm <- tbl(coririsi, in_schema("sch_layer", "lodes_county_2017")) %>% 
  collect() %>%
  mutate(firm_10yr = lodes_firm_age_0_1_2017 + lodes_firm_age_2_3_2017 + lodes_firm_age_4_5_2017 + lodes_firm_age_6_10_2017) %>%
  mutate(fips = as.numeric(geoid)) %>%
  select(fips, firm_10yr, lodes_all_jobs_2017, share_young_firm_10yr_less) 

amenity <- tbl(coririsi, in_schema("sch_layer", "natural_amenity_score_county")) %>% 
  collect() %>%
  mutate(fips = as.numeric(geoid)) %>%
  rename(amenity = natural_amenity_scale) %>%
  select(fips, amenity) 

black2010 <- tbl(coririsi, in_schema("sch_source", "ers_people")) %>% 
  collect() %>%
  mutate(fips = as.numeric(FIPS)) %>%
  rename(black2010 = BlackNonHispanicPct2010) %>%
  select(fips, black2010) 

hispanic2010 <- tbl(coririsi, in_schema("sch_source", "ers_people")) %>% 
  collect() %>%
  mutate(fips = as.numeric(FIPS)) %>%
  rename(hispanic2010 = HispanicPct2010) %>%
  select(fips, hispanic2010) 



emp.2007 <- read.csv("2007 emp.csv")
metro <- read.csv("metro.csv")
broadband_sub <- read.csv("broadband_sub.csv")
bach2010 <- read.csv("bach2010.csv")
bach2018 <- read.csv("bach2018.csv")
emp2019 <- read.csv("emp2019.csv")


df <- get_bls_county(c("February 2020", "March 2020", "April 2020"), seasonality = FALSE)
df$fips <- as.numeric(df$fips)
df$period <- as.character(df$period)

df$period[df$period == "2020-02-01"] <- "Feb2020"
df$period[df$period == "2020-03-01"] <- "Mar2020"
df$period[df$period == "2020-04-01"] <- "Apr2020"

df_emp<- df %>% select(fips, area_title, period, employed) %>% 
  pivot_wider(names_from =  period, values_from = employed, names_prefix = "emp.") %>%
  mutate (demp.covid = (emp.Apr2020 - emp.Feb2020)/emp.Feb2020) %>%
  select(fips, area_title, emp.Feb2020, emp.Apr2020, demp.covid)

ue <- df %>% select(fips, period, unemployed_rate) %>%
  pivot_wider(names_from =  period, values_from = unemployed_rate, names_prefix = "ue.") %>%
  select(fips, ue.Apr2020)

df_emp <- df_emp %>% left_join(ue, "fips")

df_emp <- df_emp %>% left_join(metro, "fips")%>% 
  left_join(broadband_sub, "fips")%>% 
  left_join(bach2010, "fips") %>% 
  left_join (bach2018, "fips") %>% 
  mutate(dbach = (bach2018 - bach2010)/bach2010) %>% 
  left_join(emp2019,"fips")%>% 
  left_join(broadband, "fips") %>%
  left_join(prop, "fips") %>%
  left_join(young_firm, "fips") %>%
  left_join(amenity, "fips") %>%
  left_join(black2010, "fips") %>%
  left_join(hispanic2010, "fips") %>%
  left_join(pop, "fips")

df_emp <- df_emp %>% left_join(emp.2007, "fips") %>% 
  mutate(demp.2007.2019 = (emp.2019 - emp.2007)/emp.2007)
df_emp <- df_emp %>%
  mutate(demp.2007.Apr20 = (emp.Apr2020 - emp.2007)/emp.2007)


df_emp$recovered <- 0
df_emp$recovered[df_emp$demp.2007.2019>=0] <- 1 

df_emp$high_black <- 0
df_emp$high_black[df_emp$black2010>12.5] <-1


########################### Summary Stats ###############

df_analysis <- df_emp %>% filter(!is.na(amenity), !is.na(bach2018), !is.na(bach2010), !is.na(emp.2007), !is.na(emp.Apr2020), !is.na(broadband))

recovery_compare <- df_analysis %>%
  group_by(metro, recovered) %>%
  summarize(
    n = n(),
    recession = mean(demp.2007.2019),
    covid = mean(demp.covid),
    recession.covid = mean(demp.2007.Apr20),
    pop = mean(dpop07_18),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2010 = mean(bach2010_share),
    bach2018 = mean(bach2018_share),
    dbach = mean(dbach),
    broadband_sub = mean(broadband_sub),
    broadband = mean(broadband),
    young_firm = mean(share_young_firm_10yr_less),
    prop = mean(dprop07_18),
    amenity = mean(amenity)
  )

recovery_compare_nometro <- df_analysis %>%
  group_by(recovered) %>%
  summarize(
    n = n(),
    recession = mean(demp.2007.2019),
    covid = mean(demp.covid),
    recession.covid = mean(demp.2007.Apr20),
    pop = mean(dpop07_18),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2010 = mean(bach2010_share),
    bach2018 = mean(bach2018_share),
    dbach = mean(dbach),
    broadband_sub = mean(broadband_sub),
    broadband = mean(broadband),
    young_firm = mean(share_young_firm_10yr_less),
    prop = mean(dprop07_18),
    amenity = mean(amenity)
  )

recovery_compare_alt <- df_analysis %>%
  group_by(metro, recovered) %>%
  summarize(
    n = n(),
    recession = (sum(emp.2019) - sum(emp.2007))/sum(emp.2007),
    covid = (sum(emp.Apr2020) - sum(emp.Feb2020))/sum(emp.Feb2020),
    recession.covid = (sum(emp.Apr2020) - sum(emp.2007))/sum(emp.2007),
    bach2010 = mean(bach2010_share),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2018 = mean(bach2018_share),
    dbach = (sum(bach2018) - sum(bach2010))/sum(bach2010),
    broadband_sub = mean(broadband_sub),
    broadband = sum(f477_maxad_downup_2018dec_25_3_popsum)/sum(pop2018_sum),
    young_firm = sum(firm_10yr)/sum(lodes_all_jobs_2017),
    prop = (sum(prop_emp_2018)-sum(prop_emp_2007))/sum(prop_emp_2007),
    amenity = mean(amenity)
  )

recovery_compare_alt_nometro <- df_analysis %>%
  group_by(recovered) %>%
  summarize(
    n = n(),
    recession = (sum(emp.2019) - sum(emp.2007))/sum(emp.2007),
    covid = (sum(emp.Apr2020) - sum(emp.Feb2020))/sum(emp.Feb2020),
    recession.covid = (sum(emp.Apr2020) - sum(emp.2007))/sum(emp.2007),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2010 = mean(bach2010_share),
    bach2018 = mean(bach2018_share),
    dbach = (sum(bach2018) - sum(bach2010))/sum(bach2010),
    broadband_sub = mean(broadband_sub),
    broadband = sum(f477_maxad_downup_2018dec_25_3_popsum)/sum(pop2018_sum),
    young_firm = sum(firm_10yr)/sum(lodes_all_jobs_2017),
    prop = (sum(prop_emp_2018)-sum(prop_emp_2007))/sum(prop_emp_2007),
    amenity = mean(amenity)
  )

recovery_compare_black <- df_analysis %>%
  group_by(high_black) %>%
  summarize(
    n = n(),
    recession = (sum(emp.2019) - sum(emp.2007))/sum(emp.2007),
    covid = (sum(emp.Apr2020) - sum(emp.Feb2020))/sum(emp.Feb2020),
    recession.covid = (sum(emp.Apr2020) - sum(emp.2007))/sum(emp.2007),
    pop = mean(dpop07_18),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2010 = mean(bach2010_share),
    bach2018 = mean(bach2018_share),
    dbach = (sum(bach2018) - sum(bach2010))/sum(bach2010),
    broadband_sub = mean(broadband_sub),
    broadband = sum(f477_maxad_downup_2018dec_25_3_popsum)/sum(pop2018_sum),
    young_firm = sum(firm_10yr)/sum(lodes_all_jobs_2017),
    prop = (sum(prop_emp_2018)-sum(prop_emp_2007))/sum(prop_emp_2007),
    amenity = mean(amenity)
  )

recovery_compare_black_metro <- df_analysis %>%
  group_by(high_black, metro) %>%
  summarize(
    n = n(),
    recession = (sum(emp.2019) - sum(emp.2007))/sum(emp.2007),
    covid = (sum(emp.Apr2020) - sum(emp.Feb2020))/sum(emp.Feb2020),
    recession.covid = (sum(emp.Apr2020) - sum(emp.2007))/sum(emp.2007),
    pop = mean(dpop07_18),
    black2010 = mean(black2010),
    hispanic2010 = mean(hispanic2010),
    bach2010 = mean(bach2010_share),
    bach2018 = mean(bach2018_share),
    dbach = (sum(bach2018) - sum(bach2010))/sum(bach2010),
    broadband_sub = mean(broadband_sub),
    broadband = sum(f477_maxad_downup_2018dec_25_3_popsum)/sum(pop2018_sum),
    young_firm = sum(firm_10yr)/sum(lodes_all_jobs_2017),
    prop = (sum(prop_emp_2018)-sum(prop_emp_2007))/sum(prop_emp_2007),
    amenity = mean(amenity)
  )


write.csv(recovery_compare, "recession_recovery_compare.csv")
write.csv(recovery_compare_nometro, "recession_recovery_compare_nometro.csv")

write.csv(recovery_compare_alt, "recession_recovery_compare_alt.csv")
write.csv(recovery_compare_alt_nometro, "recession_recovery_compare_nometro_alt.csv")

write.csv(recovery_compare_black, "recession_recovery_high_black_share.csv")
write.csv(recovery_compare_black_metro, "recovery_compare_black_metro.csv")
