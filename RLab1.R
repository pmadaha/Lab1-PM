install.packages("tidyverse")
install.packages("sf")
library(tidyverse)
library(sf)
library(ggplot2)
p.counties<-"./County_Boundaries.shp"
p.stations<-"./Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"
d.counties<-sf::read_sf(p.counties)
d.stations<-sf::read_sf(p.stations)
glimpse(d.counties)
glimpse(d.stations)

d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()
d.counties<-d.counties %>% sf::st_make_valid()
d.counties %>% sf::st_is_valid()

lab1.1 <- {d.counties %>% dplyr::select(STATEFP10, COUNTYFP10, NAME10, ALAND10, AWATER10)}
lab1.1 %>% mutate (LAND_PCT = ((ALAND10/(ALAND10+AWATER10))*100))

glimpse(lab1.1)
lab1.1<- lab1.1 %>% dplyr::mutate(LAND_PCT = ((ALAND10/(ALAND10+AWATER10))*100))
lab1.1<- lab1.1 %>% mutate (WATER_PCT = 100 - LAND_PCT)
glimpse (lab1.1)

lab1.1 %>% group_by(STATEFP10) %>% dplyr::slice_min(LAND_PCT)
lab1.1 %>% group_by(STATEFP10) %>% count(STATEFP10)

glimpse (d.stations)

task1.4 <- d.stations %>% dplyr::select(STATION_NA)%>% mutate(length = nchar(STATION_NA))
task1.4 %>% dplyr::slice_min(length) # Answer: Abram Creek at Oakmont & Dragon Swamp at Mascot#
glimpse(task1.4)

Task2 <- lab1.1
glimpse (Task2)
Task2 %>% ggplot(., aes(x = ALAND10, y = AWATER10, colour = STATEFP10)) + geom_point() + labs (title = "LAND/WATER AREA RELATIONSHIP", subtitle = "County Scale", x = "Land Area", y = "Water Area")

Task2.2 <- d.stations %>% dplyr::select (STATION_NA, Drainage_A, USGS_STATI)
Task2.2 %>% group_by(USGS_STATI)
Task2.2 %>% ggplot (., aes(x = Drainage_A)) + geom_histogram ( aes (fill = factor(USGS_STATI)))+ labs(title = "Monitoring Stations Drainage Rates", x = "Drainage Area", fill = "Monitoring Stations")

d.counties %>% sf :: st_crs()
d.stations %>% sf :: st_crs()
d.counties %>% sf :: st_crs() == d.stations %>% sf :: st_crs()

Task2.3 <- sf::st_intersection(Task2.2, Task2)
Task2.3 %>% ggplot (., aes(x = Drainage_A)) + geom_histogram (aes (fill = factor(STATEFP10))) + labs(title = "Drainage Rates at State Level", x = "Drainage Area", fill = "STATES")


Task3fun<- function (x,y,z) { if (!is.numeric (x) | !is.numeric(y) | !is.numeric(z)){stop("ERROR: Data is not numeric")} 
  Task3vector<- c(x,y,z)
  A<- list(min=min (Task3vector), max=max (Task3vector), mean=mean (Task3vector), median=median (Task3vector)) 
  B<- sort( Task3vector, decreasing = TRUE)
  return ( list(A=A, B=B)) }

Task3fun(1,0,-1)
Task3fun(10,100,1000)
Task3fun(.1,.001,1e8)
Task3fun("a","b","c")

Task4.1 <- Task2.3 %>% group_by(STATEFP10) %>% summarise(count = n())

Task4.2 <- d.counties %>% select(STATEFP10, ALAND10) %>% filter(STATEFP10 == "36") 
NYsize<- Task4.2 %>% summarise(NYsize = mean(ALAND10)) 

Task4.3 <- Task2.3 %>% group_by(STATEFP10)
AvgDA<- Task4.3 %>% summarize(AvgDA = mean (Drainage_A))


MaxDA<- AvgDA %>% summarise(MaxDA = max(AvgDA)) # 32 = PA. Pennsylvania has the greatest average drainage area










#Q1: The two statements are not equivalent as both functions would give you separate results, as the order of the input matters in this case. For the attribute table, data sets would vary depending on what input comes first as the first input would act as a base and only relevant data from the second input would be to the intersected onto the base. For the spatial data structures the output would be dependent on the first input, as for the first function the results of the spatial data would be in points and for the second function the results of the spatial data would be polygons. If the data type was different my answer would differ in the results of the output. However, if you mean if the data was not spatial that would mean the intersect would be treated differently, for instance it would act as a join find a common value within a data set and use that as a base for intersecting all the values within the data set.#
#Q2: The thing that I found the most challenging was plotting the data, understanding how each variables are placed within the graph, and it took me sometime to understand the output of the data set.#
#Q3: The intersection of R within other scenarios such as finding relationships between data sets and visualizing these relationships, and how R would work if were to map geographical data.#