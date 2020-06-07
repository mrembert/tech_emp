library(tigris)
library(stringr)
library(dplyr)
library(leaflet)

ipums <- ipums.5.2018
rm(ipums.5.2018)

ipums <- ipums %>% filter(CLASSWKR>0)

us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "HI")]
us_pumas <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      pumas(state = x, cb = TRUE)
    }
  )
)
plot(us_pumas)

ipums.df <-ipums
ipums.df$PUMA <- str_pad(ipums.df$PUMA, 5, pad = "0")
ipums.df$STATEFIP <- str_pad(ipums.df$STATEFIP, 2, pad = "0")
ipums.df <- ipums.df %>% mutate(GEOID10 = paste(STATEFIP,PUMA, sep=""))

network <- read.csv("network.csv")

total.emp <- ipums.df %>%
  group_by(GEOID10) %>%
  summarize(
    total= sum(PERWT)
  )

total.jobs <- ipums.df %>%
  filter(CLASSWKR > 0) %>%
  group_by(GEOID10,jobs) %>%
  summarize(
    emp = sum(PERWT)
  )

tech.total <- total.emp %>%
  left_join(total.jobs, by = "GEOID10") %>%
  mutate(share = emp/total) %>%
  select(GEOID10,jobs,emp,share) %>%
  filter(jobs == "Tech")

tech.wages <- ipums.df %>%
  filter(jobs == "Tech") %>% 
  filter(CLASSWKR == 2) %>%
  group_by(GEOID10) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop 
  ) %>%
  select(GEOID10, mean.wages) 

tech.total.ws <- ipums.df %>%
  filter(INCWAGE > 0 & INCWAGE < 999998 & CLASSWKR == 2) %>%
  filter(jobs == "Tech") %>%
  group_by(GEOID10) %>%
  summarize (
    total.ws = sum(PERWT)
  )

tech.remote <- ipums.df %>%
  filter(INCWAGE > 0 & INCWAGE < 999998 & CLASSWKR == 2) %>%
  filter(jobs == "Tech", commute== "Worked at home") %>%
  group_by(GEOID10) %>%
  summarize (
    remote = sum(PERWT),
    remote.wages = sum(INCWAGE * PERWT) / remote
  ) %>%
  left_join(tech.total.ws, by="GEOID10") %>%
  mutate(remote.share = remote/total.ws)  %>%
  select(GEOID10, remote, remote.share, remote.wages)

tech.diverse <- ipums.df %>%
  filter(jobs == "Tech") %>%
  group_by(GEOID10) %>%
  summarize (
    remote = sum(PERWT),
    remote.wages = sum(INCWAGE * PERWT) / remote
  ) %>%
  left_join(tech.total.ws, by="GEOID10") %>%
  mutate(remote.share = remote/total.ws)  %>%
  select(GEOID10, remote, remote.share, remote.wages)




rural <- ipums.df %>%
  group_by(GEOID10) %>%
  summarize(
    rural = mean(rural)
  )
rural$rural <- as.factor(rural$rural)

tech.final <- tech.total %>%
  left_join(tech.wages, rural, by ="GEOID10") %>%
  left_join(rural, by ="GEOID10")%>%
  left_join(tech.remote, by ="GEOID10")%>%
  select(GEOID10,emp,share, mean.wages, rural,remote, remote.share,remote.wages)

tech.final$remote[is.na(tech.final$remote)] <- 0
tech.final$remote.share[is.na(tech.final$remote.share)] <- 0
tech.final$remote.wages[is.na(tech.final$remote.wages)] <- 0


pumas <- geo_join(us_pumas, tech.final, by = "GEOID10")
pumas$rural[pumas$rural==1] <- NA

share.bins <- c(0, .01, .02, .04, 0.08, 0.16, 1)
emp.bins <- c(0, 250, 500, 1000, 2000, 4000, 1000000)
remote.bins <- c(0,.05, .1, .20, 1)


share.pal <- colorBin("Blues", bins=share.bins)
emp.pal <- colorBin("Greens", bins=emp.bins)
wages.pal <- colorQuantile("Reds", pumas$mean.wages, 5)
remote.pal<- colorBin("Oranges", bins=remote.bins)

rural.pal <- colorFactor(c(rgb(105/255,105/255,105/255), rgb(105/255,105/255,105/255)), na.color = rgb(0, 0, 0, 0), pumas$rural)



puma_popup <- paste0("<br><strong>PUMA: </strong>", 
                     pumas$NAME10, 
                     "<br><strong>Percent Tech Emp: </strong>", 
                     scales::label_percent(accuracy=.1)(pumas$share),
                     "<br><strong>Total Tech Emp: </strong>", 
                     pumas$emp,
                     "<br><strong>Average Tech Wages (W&S only): </strong>",
                     scales::dollar(pumas$mean.wages),
                     "<br><strong>Percent Remote Tech Workers (W&S only): </strong>",
                     scales::label_percent(accuracy=.1)(pumas$remote.share),
                     "<br><strong>Remote Tech Worker Average Wages (W&S only): </strong>",
                     scales::dollar(pumas$remote.wages)
)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = pumas,
              fillColor = ~share.pal(share), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = puma_popup,
              group = "Tech Share") %>%
  addPolygons(data = pumas,
              fillColor = ~emp.pal(emp), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = puma_popup,
              group = "Tech Emp") %>%
  addPolygons(data = pumas,
              fillColor = ~wages.pal(mean.wages), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = puma_popup,
              group = "Tech Wages") %>%
  addPolygons(data = pumas,
              fillColor = ~remote.pal(remote.share), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = puma_popup,
              group = "Remote Tech") %>%
  addPolygons(data = pumas,
              fillColor = ~rural.pal(rural), 
              fillOpacity = 0.30, 
              color = "#BDBDC3", 
              weight = 1,
              popup = puma_popup,
              group = "Metro") %>%
  addMarkers(data=network,
             ~long,
             ~lat,
             popup = ~as.character(city)
  ) %>%
  addLayersControl(
    baseGroups = c("Tech Share", "Tech Emp", "Tech Wages", "Remote Tech"),
    overlayGroups = c("Metro"),
    options = layersControlOptions(collapsed = FALSE) 
)




tech.final %>%
  filter(rural ==1 & is.na(remote.share))
