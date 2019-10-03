# UNL-out-of-state-recruiting
The University of Nebraska-Lincoln, like other public universities, has increased its efforts to recruit students from outside Nebraska in recent years as it seeks to boost its enrollment. In a March 2019 paper published by the Joyce Foundation, "Recruiting the Out-Of-State University: Off-campus recruiting by public universities," Crystal Han and Ozan Jaquette of UCLA and Karina Salazar of the University of Arizona described a process by which they scraped data about college visits from the web, including those by UNL recruiters. Jaquette provided the data to me, which I used as the basis for a story in the Journal Star. 

![image](https://user-images.githubusercontent.com/5634106/66157952-79cb8600-e5ea-11e9-91d4-53cd9d543d7e.png)

### LOAD IN PACKAGES
require(tidyverse)
require(readr)
require(tigris)
require(leaflet)
require(sf)
require(formattable)
require(scales)
require(RColorBrewer)
require(htmlwidgets)

### READ CSV OF WEBSCRAPED COLLEGE RECRUITING VISITS
recruiting_visits = read.csv(("recruiting_visits.csv"), stringsAsFactors = FALSE) %>%
  filter(univ_state == "NE") %>%
  select(8:9, 12:13, 16:22) %>%
  arrange(event_date) %>%
  rename("zip_code" = "determined_zip")

### READ IN MASTER ZIP CODE FILE
master_zip <- read.csv("meta_zipcode.csv")

### JOIN RECRUITING VISIT DATA WITH ZIP CODE DATA FROM U.S. CENSUS BY ZIP CODE
nebrecruiting = left_join(recruiting_visits, master_zip, by = "zip_code") %>%
  rename("address" = "event_address")

### CHANGE AVG MEDIAN INCOME TO CURRENCY
nebrecruiting$avgmedian_inc_2564 = currency(nebrecruiting$avgmedian_inc_2564, digits = 0L)

### READ IN MASTER HIGH SCHOOL FILEs
public_schools = read.csv(("meta_high_school_public.csv"), stringsAsFactors = FALSE) %>%
  select(6, 8, 17:31) %>%
  rename("students" = "total_students", "freshmen" = "total_09", "sophomores" = "total_10", "juniors" = "total_11",
         "seniors" = "total_12", "whites" = "total_white", "blacks" = "total_black", "hispanic" = "total_hispanic",
         "asians" = "total_asian", "natives" = "total_amerindian", "hawaiian" = "total_nativehawaii", "tworaces" = "total_tworaces",
         "fnr" = "free_reduced_lunch")

### JOIN PUBLIC SCHOOL DATA TO NEBRASKA RECRUITING DATA BY ZIP CODE
nebrecruiting = left_join(nebrecruiting, public_schools, by = "zip_code")

### NOW DO THE SAME WITH THE PRIVATE SCHOOLS
private_schools = read.csv(("meta_high_school_private.csv"), stringsAsFactors = FALSE) %>%
  rename("students" = "total_students", "freshmen" = "total_09", "sophomores" = "total_10", "juniors" = "total_11",
         "seniors" = "total_12", "whites" = "total_white", "blacks" = "total_black", "hispanic" = "total_hispanic",
         "asians" = "total_asian", "natives" = "total_amerindian", "hawaiian" = "total_nativehawaii", "tworaces" = "total_tworaces",
         "fnr" = "free_reduced_lunch")

private_schools = private_schools %>%
  select(6, 8, 11:23)
  
### JOIN SCHOOL DATA WITH TABLE BY ADDRESS
nebrecruiting = left_join(nebrecruiting, private_schools, by = "zip_code")

### UPLOAD A SHAPE FILE OF THE U.S.
### Coordinate Reference System:
### EPSG: 4269 
### proj4string: "+proj=longlat +datum=NAD83 +no_defs"
options(tigris_class = "sf")
states = states(class = "sf")
st_crs(states)
st_transform(states, "+proj=longlat +datum=NAD83 +no_defs")

states = subset(states, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))

### JOIN STATE/NONRESIDENT DATA
states_merged = geo_join(states, nonresidents, "NAME", "State")

ggplot(states) +
  geom_sf() +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  labs(title="States")

### TEST MAP
states %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons()

### CREATE MAP AESTHETICS
pal = colorFactor("Set1", 
                  domain = nebrecruiting$event_type, 
                  na.color = NA)

labels <- sprintf("<strong>%s", nebrecruiting$event_location_name) %>% 
  lapply(htmltools::HTML)

pop_percent = percent(nebrecruiting$pop_white/nebrecruiting$pop_total)

### MAP THE DANG THING
recruiting_map = leaflet(states) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  setView(-99.9018, 41,4925, zoom = 5) %>% 
  addCircleMarkers(data = nebrecruiting,
             lng = ~event_longitude, 
             lat = ~event_latitude,
             color = ~pal(nebrecruiting$event_type),
             radius = 5,
             weight = 3,
             fillOpacity = 1,
             stroke = FALSE,
             label = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             popup = paste("<strong>Location:</strong>", nebrecruiting$event_location_name, "<br>",
                           "<strong>City:</strong>", nebrecruiting$event_city, nebrecruiting$event_state, "<br>",
                           "<strong>Date:</strong>", nebrecruiting$event_date, "<br>",
                           "<strong>Avg. household income:</strong>", nebrecruiting$avgmedian_inc_2564, "<br>",
                           "<strong>Percent white population:</strong>", pop_percent, "<br>",
                           "<strong>Student enrollment:</strong>", nebrecruiting$students.x)) %>%
  addLegend(pal = pal, 
            values = nebrecruiting$event_type,
            position = "bottomleft", 
            title = "2017 UNL Recruiting Visits",
            opacity = 1)

recruiting_map
### FIN
