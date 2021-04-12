# prepare enviroment------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(plotly)
library(scales)
library(shinydashboard)
# library(leaflet)
# library(geojsonio)
# library(htmltools)
# library(htmlwidgets)
# library(sf)
library(dashboardthemes)
library(highcharter)
library(tidyr)
options(scipen = 9999)
library(glue)
library(janitor)



# length(covid19fix$Date)
#import dataset ----------------------------------------------------------------
covid19 <- read.csv("data/covid_19_indonesia_time_series_all.csv")
#inspect data-------------------------------------------------------------------
# head(covid19)
# tail(covid19)
# names(covid19)
# str(covid19)
# colSums(is.na(covid19))

#mengubah tipe data-------------------------------------------------------------
covid19[,c("Location.ISO.Code","Location")] <- lapply(covid19[,c("Location.ISO.Code","Location")],as.factor)

# drop colom yang tidak digunakan-----------------------------------------------
covid19_ <- subset(covid19,select = -c(Location.ISO.Code, New.Cases, New.Deaths, New.Recovered, New.Active.Cases , Province , Country , Continent, Island, Time.Zone, Special.Status, Total.Regencies, Total.Cities, Total.Districts, Total.Urban.Villages, Total.Rural.Villages, New.Cases.per.Million, Total.Cases.per.Million,New.Deaths.per.Million, Total.Deaths.per.Million, Case.Fatality.Rate,Case.Recovered.Rate, Growth.Factor.of.New.Cases,Growth.Factor.of.New.Deaths, City.or.Regency,Area..km2.))

#mengganti nama ----------------------------------------------------------------
covid19_ <- rename(covid19_, Provinsi = Location)
#covid19_ <- rename(covid19_, Date = ï..Date)
#membuang baris indonesia ----------------------------------------------------------------
covid19fix <- covid19_[covid19_$Provinsi != 'Indonesia',]
covid19fix[,"Date"] <- mdy(covid19fix$Date)
# str(covid19fix)
#dataset dipakai
Persen <- covid19fix %>%
  filter(Date == max(Date))

# Persen dalam value box output -------------------
total <- sum(Persen$Total.Cases)
PTAC <-sum(Persen$Total.Active.Cases) / total
PTD <- sum(Persen$Total.Deaths) / total
PTR <- sum(Persen$Total.Recovered) / total

persenter <- function(x){
paste("(",round(x, 3)*100,"%)",sep = "")
}
# persenter(PTD)


# membuat peta --------------------------------------
map_indo <- read.csv("data/map-indo.csv") %>%
  left_join(Persen,by ="Provinsi") %>%
  select(woe_name, kasus = Total.Cases)

map_indoA <- read.csv("data/map-indo.csv") %>%
  left_join(Persen,by ="Provinsi") %>%
  select(woe_name, kasus = Total.Active.Cases)

map_indoR <- read.csv("data/map-indo.csv") %>%
  left_join(Persen,by ="Provinsi") %>%
  select(woe_name, kasus = Total.Recovered)

map_indoD <- read.csv("data/map-indo.csv") %>%
  left_join(Persen,by ="Provinsi") %>%
  select(woe_name, kasus = Total.Deaths)


covid19fix %>%
  filter(Date==max(Date)) %>%
  arrange(desc(Total.Cases)) %>%
  select(Provinsi, Total.Cases, Total.Recovered, Total.Deaths)

name <-c("Total.Cases","Total.Recovered","Total.Deaths","Total.Active.Cases")
names(name) <- c(" Total Cases", "Total Recovered", "Total Deaths", "Total Aktif Cases")
