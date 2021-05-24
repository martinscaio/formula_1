library(tidyverse)
library(data.table)
library(knitr)

# IMPORTAR DADOS


# Dados disponibilizados no kaggle-------


pit_stops <-  fread("C:/Users/mcaio/Desktop/F1/Data/pit_stops.csv")


race <- fread("C:/Users/mcaio/Desktop/F1/Data/races.csv")


drivers <- fread("C:/Users/mcaio/Desktop/F1/Data/drivers.csv")


results <- fread("C:/Users/mcaio/Desktop/F1/Data/results.csv")


constructors <- fread("C:/Users/mcaio/Desktop/F1/Data/constructors.csv")


quali <- fread("C:/Users/mcaio/Desktop/F1/Data/qualifying.csv")



# Dataframe com dados sobre pódio-------


podio <- left_join(results, drivers, by = "driverId") %>% 
  left_join(constructors, by = 'constructorId') %>% 
  left_join(race, by = 'raceId')




# Dataframe com dados para Pit stops-------


dados_pit <- 
  left_join(pit_stops, race, by = 'raceId') %>% 
  left_join(drivers, by = 'driverId') %>% 
  left_join(results, by = c("raceId", "driverId")) %>% 
  left_join(constructors, by = 'constructorId')



# Dataframe dados do qualifying-------


quali <- left_join(quali, drivers, by = 'driverId') %>% 
  left_join(constructors, by = 'constructorId') %>% 
  left_join(race, by = 'raceId')









