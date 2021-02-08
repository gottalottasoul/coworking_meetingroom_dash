# ./build.r
# This is the master file for ETL of data needed for marketing/sales reports
# Blake Abbenante
# 10/1/19

if (!require(tidyverse)) {
  install.packages('tidyverse') # load the base tidyverse libraries (dplyr, tidyr, etc.)
  require(tidyverse)
}
if (!require(janitor)) {
  install.packages('janitor') # functions for augmenting dataframes
  require(janitor)
}
if (!require(readr)) {
  install.packages('readr') # enhanced functions for loading data
  require(readr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # enhanced functions for working with dates
  require(lubridate)
}
if (!require(here)) {
  install.packages('here') # file referencing
  require(here)
}
if (!require(httr)) {
  install.packages('httr') # http posts
  require(httr)
}
if (!require(config)) {
  install.packages('config') # read a config file
  require(config)
}
if (!require(RiHana)) {
  devtools::install_github('RiHana') # Hana functions
  require(RiHana)
}


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()


hana_dates<-RiHana::get_relevant_date()

config<-config::get(file="~/OneDrive - CBRE, Inc/data/config/r_config.yaml")


#calculate how many days in this time interval
n_days <- interval(hana_dates$yoy_date ,hana_dates$yesterdays_date)/days(1)
reporting_date_range<- enframe(hana_dates$yoy_date + days(0:n_days)) %>% 
  mutate(start_week=floor_date(value,unit="week",week_start=1)) %>% 
  group_by(start_week) %>% 
  summarise(days=n()) %>% 
  rename(report_week=start_week)


###Build the reports ####

#check if our cached data is less than a day old, otherwise run the ETL script
file_date<-file.info('inc/Meet_data.RData')$mtime

 if(difftime(now(),file_date,units="hours")>24)
 {
  source("meet_explore_load.R")
}

updates_this_week<-""

meet_pub<-rsconnect::deployDoc("meetExplore.Rmd",forceUpdate=TRUE,launch.browser=FALSE)

if (meet_pub){
  POST(url=config$insights_webhook,body=get_slack_payload("Meet Explore Dashboard","https://blake-abbenante-hana.shinyapps.io/meetExplore/", updates_this_week),encode="json")
}

