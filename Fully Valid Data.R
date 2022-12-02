
pacman::p_load(tidyr,tidyverse,lubridate, oo)
#Data showing Covid Hospitalizations in the EU
cov <- read.csv("covid.csv")
str(cov)
cov$date <- ymd(cov$date)
cov$week =str_extract(cov$year_week, 'W[:digit:]{1,}') %>% str_remove('W') %>% as.numeric()
#Data showing price of electricity per KWH in the EU
elec <- read.csv("electricprice.csv")
str(elec)
elec$date <- mdy(elec$Date)
elec <- subset(elec,select = -c(Date))
#Data showing price from a European stock exchange
euro <- read.csv("euro100.csv")
str(euro)
euro$Date <-  dmy(euro$Date)
#Data showing change in tempurature with respect to a baseline period from 1951 - 1980
weather <- read.csv("weatherchange.csv")
str(weather)
weather <- subset(weather, select = c(Months, Year, Value, Area))
weather$date <- paste(weather$Year, weather$Months, sep="-") %>% ym() %>% as.Date()
weather$date
cov <- subset(cov,select =  -c(url))
cov <- subset(cov,select =  -c(year_week))#Getting rid of null values in covid dataset
summary(is.na(cov))
summary(is.na(elec))
summary(is.na(euro))
summary(is.na(weather))
weather <- na.omit(weather) #Ommiting null data in weather
summary(is.na(weather))

fulllist <- list(cov, elec, euro, weather)


pacman::p_load(pointblank) #Validation reporting

act = action_levels(warn_at = .01, notify_at = .01)
agent1 = create_agent(tbl = cov, actions = act)

agent1 %>% #(Ctrl + Shift + M) |> take the output on the left and make it as the first input to the next function
  # technically correct checks (you should do that for your entire dataset)
  col_is_date(columns = 'date') %>% 

  # variables can either be a chr vector or passed using the vars function
  col_is_numeric(columns = c('value','week')) %>% 
  # consistency of the data
  col_is_character(columns = c('country', 'indicator', 'source')) ->
  agent1 # overwrite agent based on the rules above

resultscov = interrogate(agent1)
resultscov
str(cov)
resultscov %>% export_report(filename = 'covvalid.html')

agent2 = create_agent(tbl = elec, actions = act)
agent2
str(elec)

agent2 %>%
  col_is_date(columns = 'date') %>%
  col_is_numeric(columns = c('Price..US','Price..EUR.MWhe.')) %>%
  col_is_character(columns = c('Country', 'ISO3.Code')) ->
  agent2
agent2

resultselec = interrogate(agent2)
resultselec
str(elec)
resultselec %>% export_report(filename = 'elecvalid.html')

is.atomic(elec$Price..EUR.MWhe.)
agent3 = create_agent(tbl = euro, actions = act)
agent3

# Step3
agent3 %>% #(Ctrl + Shift + M) |> take the output on the left and make it as the first input to the next function
  # technically correct checks (you should do that for your entire dataset)
  col_is_date(columns = 'Date') %>% 
  col_is_numeric(columns = c('Open', 'High', 'Low', 'Close', 'Adj.Close', 'Volume')) ->
  agent3 # overwrite agent based on the rules above

agent3

# (4) Eval
resultseuro = interrogate(agent3)
resultseuro

resultseuro %>% export_report(filename = 'euro_validation.html')

agent4 = create_agent(tbl = weather, actions = act)
agent4
str(weather)
agent4 %>%
  col_is_date(columns = 'date') %>%
  col_is_numeric(columns = c('Value'))  %>%
  col_is_character( columns = c('Area')) ->
  agent4
agent4

resultsweather = interrogate(agent4)
resultsweather

resultsweather %>% export_report(filename = 'weather_validation.html')
  
  