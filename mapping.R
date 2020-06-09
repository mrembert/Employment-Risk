library(tigris)
library(stringr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(RColorBrewer)

us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "HI")]
us_counties <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      counties(state = x, cb = TRUE)
    }
  )
)
plot(us_counties)

df.emp <- df_emp %>% rename(GEOID = fips)
df.emp$metro <- as.factor(df.emp$metro)
df.emp$recovered <- as.factor(df.emp$recovered)
us_counties$GEOID <- as.numeric(us_counties$GEOID)

counties <- geo_join(us_counties, df.emp, by = "GEOID")

metro <- df.emp %>% filter(metro==1) %>% select(GEOID,metro)
metro <-  geo_join(us_counties, metro, by ="GEOID", how="inner")

recovered <- df.emp %>% filter(recovered ==1) %>% select(GEOID,recovered) 
recovered$recovered <- as.factor(recovered$recovered)
recovered <- geo_join(us_counties, recovered, by="GEOID", how= "inner")

recovering <- df.emp %>% filter(recovered==0) %>% select(GEOID,recovered)
recovering <-  geo_join(us_counties, recovering, by="GEOID", how= "inner")

ggplot(df_emp, aes(demp.2007.2019)) + geom_histogram(binwidth = .005 ) + xlim(-.5, 1)
ggplot(df_emp, aes(demp.covid)) + geom_histogram(binwidth = .005 ) + xlim(-.5, 1)

recession.bins <- c(-100,-.20,-.10,-.05,0,.05,.10,.20,100)
covid.bins <- c(-1,-.20,-.15,-.10,-.05,0,1)

covid.pal <- colorBin("Greys", bins = covid.bins, reverse = TRUE)
recession.pal <- colorBin(c('#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850'), bins = recession.bins)
# '#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026'
# recession.pal <- colorBin(c('#b10026','#e31a1c','#fc4e2a','#fd8d3c','#feb24c','#fed976','#ffffb2'), bins = recession.bins)

metro.pal <- colorFactor("#636363", metro$metro)
recovered.pal <- colorFactor("#636363", recovered$recovered)
recovering.pal <- colorFactor("#636363", recovering$recovering)

county_popup <- paste0("<br><strong>County: </strong>", 
                     counties$area_title, 
                     "<br><strong>Change in Employment Feb20 - Apr20: </strong>", 
                     scales::label_percent(accuracy=.1)(counties$demp.covid),
                     "<br><strong>Change in Employment 2007 - 2019: </strong>", 
                     scales::label_percent(accuracy=.1)(counties$demp.2007.2019),
                     "<br><strong>Total Employment Feb 2020: </strong>", 
                     counties$emp.Feb2020
)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = counties,
              fillColor = ~covid.pal(demp.covid), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = county_popup,
              group = "COVID Impact") %>%
  addPolygons(data = counties,
              fillColor = ~recession.pal(demp.2007.2019), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = county_popup,
              group = "Great Recession") %>%
  addPolygons(data = counties,
              fillColor = ~recession.pal(demp.2007.2019), 
              fillOpacity = 0.3, 
              color = "#BDBDC3", 
              weight = 1,
              popup = county_popup,
              group = "Overlay Great Recession") %>%
  addPolygons(data = metro,
              fillColor = ~metro.pal(metro), 
              fillOpacity = .5, 
              color = "#000000", 
              weight = 1,
              group = "Hide metro") %>%
  addPolygons(data = recovered,
              fillColor = ~recovered.pal(recovered), 
              fillOpacity = .5, 
              color = "#000000", 
              weight = 1,
              group = "Hide recovered") %>%
  addPolygons(data = recovering,
              fillColor = ~recovering.pal(recovered), 
              fillOpacity = .5, 
              color = "#000000", 
              weight = 1,
              group = "Hide recovering") %>%
  addLegend(values = counties$demp.covid, group = "COVID Impact", 
            position = "bottomright", pal =covid.pal) %>%
  addLegend(values = recession.bins, group = "Great Recession",
            position = "bottomleft", pal = recession.pal) %>%
  addLayersControl(
    baseGroups = c("COVID Impact", "Great Recession"),
    overlayGroups = c("Overlay Great Recession", "Hide metro", "Hide recovered", "Hide recovering"),
    options = layersControlOptions(collapsed = FALSE) ) %>% 
  hideGroup(c("Overlay Great Recession","Hide metro", "Hide recovered", "Hide recovering")) 


