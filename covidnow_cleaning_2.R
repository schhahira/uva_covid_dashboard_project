
#set working directory and load packages
setwd("C:/Users/jesse/Documents/Coursework/Spring_2023/ClassMaterials")
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)


#URLs from COVID ACT NOW to read in data through API interface
states_now_url <- "https://api.covidactnow.org/v2/states.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"
#county_now_url <- "https://api.covidactnow.org/v2/counties.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"
#cbsa_now_url <- "https://api.covidactnow.org/v2/cbsas.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"

states_history_url <-"https://api.covidactnow.org/v2/states.timeseries.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"
#county_history_url <-"https://api.covidactnow.org/v2/counties.timeseries.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"
#cbsa_history_url <-"https://api.covidactnow.org/v2/cbsas.timeseries.json?apiKey=b1c0e76e18f34c60903a2b0a8311cae4"



##Read in current States data

#get the data from the url
st_result <- GET(states_now_url)
str(st_result)
#pull just the data content we want, no extra internet info
st_content <- content(st_result, as='text')
str(st_content)
#translate JSON data into table format
st <- fromJSON(st_content)
glimpse(st)



##Read in States past data, since start of COVID

#get the data from the url
st_hist_result <- GET(states_history_url)
str(st_hist_result)
#pull just the data content we want, no extra internet info
st_hist_content <- content(st_hist_result, as='text')
str(st_hist_content)
#translate JSON data into table format
st_hist <- fromJSON(st_hist_content)
glimpse(st_hist)

View(st_hist)


########### Important data is coming from variables metrics and actuals. Those variables are data frames themselves in both
########### data sets, and they're the same, current values. For the st_hist data, past data is stored in 
########### metricsTimeseries and actualsTimeseries. These are data frames within the data frame, and each
########### nested data frame has a date variable which tells the date it was collected. 






####Example where daily total deaths are pulled for each state, 
####then aggregated into monthly death rate.

#Run a for-loop, pulling time series daily new deaths from each.
store <- data.frame(matrix(nrow=0,ncol=4))

for(i in 1:length(st_hist$actualsTimeseries)){
  #store time series data from one metro area
  temp <- st_hist$actualsTimeseries[[i]]
  #change date to Date format.  
  #keep deaths and date, add other static variables
  #about the metro area from the cbsanow_join data set. 
  #keep only first days of the month
  temp <- temp %>% mutate(date=as.Date(date)) %>% 
    filter(date %in% unique(floor_date(date,'month'))) %>% 
    select(deaths,date) %>% 
    mutate(population = st$population[i],
           statename = st$state[i])
  store <- rbind(store,temp)
}
View(store)
#aggregate deaths over month
store_sum <- store %>%  
  group_by(population,statename) %>%
  #calculate change month to month from deaths (cumulative deaths)
  mutate(monthlydeaths = deaths - lag(deaths)) %>% 
  mutate(monthlydeathsper = monthlydeaths/population) %>% 
  drop_na(monthlydeaths)

#save cleaned deaths data.
write.csv(store_sum,"sthist_sum.csv")


library(dplyr)

# data for total cases and deaths 
st_hist <- st_hist %>% mutate("total_cases" = st_hist$actuals$cases) %>% 
  mutate("total_deaths" = st_hist$actuals$deaths)
st_hist1 <- st_hist%>%select(state, total_cases, total_deaths)
total_cases_deaths <- st_hist %>% select("state", "lat", "long", "population", "total_cases", "total_deaths")
View(total_cases_deaths)

write.csv(total_cases_deaths, "covid_totals.csv")

# data for monthly per 100k cases, hospital beds, infection rate 
View(st_hist$metricsTimeseries[[1]])

# monthly per 100k cases, infection rate 
cases100k <- data.frame(matrix(nrow=0,ncol=4))

for(i in 1:length(st_hist$metricsTimeseries)){
  temp <- st_hist$metricsTimeseries[[i]]
  temp <- temp %>% mutate(date=as.Date(date)) %>% 
    filter(date %in% unique(floor_date(date,'month'))) %>% 
    select(weeklyNewCasesPer100k, infectionRate, date) %>% 
    mutate(statename = st$state[i])
  cases100k <- rbind(cases100k,temp)
}
View(cases100k)

cases100k <- cases100k %>% replace(is.na(.), 0)
sum(is.na(cases100k$weeklyNewCasesPer100k))
sum(is.na(cases100k$infectionRate))

# get hospital beds 
hospitalbeds <- data.frame(matrix(nrow=0,ncol=4))

for(i in 1:length(st_hist$actualsTimeseries)){
  temp <- st_hist$actualsTimeseries[[i]]
  temp <- temp %>% mutate(date=as.Date(date)) %>% 
   # filter(date %in% unique(floor_date(date,'month'))) %>% 
    select(date) %>% 
    mutate(statename = st$state[i],#) 
           hb_capacity = st_hist$actualsTimeseries[[i]]$hospitalBeds$capacity, 
           hb_usage = st_hist$actualsTimeseries[[i]]$hospitalBeds$currentUsageTotal)
  hospitalbeds <- rbind(hospitalbeds,temp)
}

View(hospitalbeds)

hospitalbeds <- hospitalbeds %>% replace(is.na(.), 0)
sum(is.na(hospitalbeds$hb_capacity))
sum(is.na(hospitalbeds$hb_usage))

hospitalbeds$month <- format(as.Date(hospitalbeds$date, format="%Y-%m-%d"),"%m")
hospitalbeds$year <- format(as.Date(hospitalbeds$date, format="%Y-%m-%d"),"%Y")

hospitalbeds

hp_sum <- hospitalbeds %>% group_by(statename, year, month) %>%
  summarise(mon_hp_capacity = sum(hb_capacity), 
            mon_hp_usage = sum(hb_usage), 
            date = date) %>% 
  filter(date %in% unique(floor_date(date,'month'))) 

hp_sum <- hp_sum %>% mutate(mon_hp_prop = mon_hp_usage/mon_hp_capacity)
hp_sum <- hp_sum %>% replace(is.na(.), 0)

View(hp_sum)


write.csv(hp_sum, "hospitalbeds.csv")
write.csv(cases100k, "cases100k_ir.csv")
