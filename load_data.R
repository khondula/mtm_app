# load in data from mitigation reports
install_missing <- function(pkgs, repos) {
  missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) install.packages(missing_pkgs, repos = repos)
}

pkg_list <- c("shiny", "RMySQL", "readr", "tidyr", "dplyr", 
              "ggplot2", "rgdal", "leaflet", "shinythemes", "htmltools")

install_missing(pkg_list, repos = "http://cran.us.r-project.org")

library(shiny)
library(RMySQL)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(leaflet)
library(shinythemes)
library(htmltools)

habitat <- read_csv("data/Palmer_EST_habitat.csv")
wq <- read_csv("data/Palmer_EST_waterquality.csv")
biol <- read_csv("data/Palmer_EST_biological.csv")
mitts <- read_csv("data/Palmer_EST_mitigations_short.csv")
ps <- read_csv("data/Palmer_EST_performance_standards.csv")
mitt_points <- read_csv("data/Mitigation_locations.csv")

wq <- wq[,-6]

mitt_points <- left_join(mitt_points, mitts)

hav_gather <- habitat %>% 
  dplyr::select(mittID, permit_year, HAV) %>% 
  left_join(mitts, by = c("mittID" = "Project_Name"))

hav_gather$permit_year <- as.numeric(hav_gather$permit_year)
hav_gather$permit_year_cat <- as.character(paste0("year_",hav_gather$permit_year))

biol_gather <- 
  biol[,c(1,2,4:6,8:13,16:28)] %>% 
  gather(parameter, value, WVSCI:Count_Total_Individuals) %>%
  left_join(mitts, by = c("mittID" = "Project_Name")) 
biol_gather$parameter_category <- "biological"

wq_gather <- 
  wq[,c(1,2,5:48)] %>% gather(parameter, value, Conductivity_uScm:Turbidity_NTU) %>%
  left_join(mitts, by = c("mittID" = "Project_Name"))
wq_gather$parameter_category <- "water_quality"

hab_gather <- 
  habitat[,c(1,4:21)] %>% gather(parameter, value, HAV:riparianWidth) %>%
  left_join(mitts, by = c("mittID" = "Project_Name"))
hab_gather$parameter_category <- "habitat"

monitoring_data <- rbind(hab_gather, biol_gather, wq_gather)




