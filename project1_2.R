library(tidyverse)
library(plyr)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')
raw.data <- read.fwf(con, widths=c(6, 2, 32, 1, 5, 43), skip=4, stringsAsFactors=FALSE) 
close(con)
  
players.row.1 <- raw.data[seq(1, nrow(raw.data), 3), ] %>% 
  setNames(c('number', 'd1', 'name', 'd2', 'points', 'post')) %>% 
  select(-d1, -d2)
players.row.2 <- raw.data[seq(2, nrow(raw.data), 3), ] %>% 
  setNames(c('location', 'd1', 'pre', 'd2', 'd3', 'd4')) %>% 
  select(-d1, -d2, -d3, -d4) %>% 
  mutate(number = players.row.1$number)

players.combined <- left_join(players.row.1, players.row.2, by='number')

players.combined$pre <- players.combined$pre %>% 
  str_sub(15, 18) %>% 
  as.numeric()
players.combined$post <- players.combined$post %>% 
  lapply(. %>% 
           str_split('\\|') %>% 
           unlist() %>% 
           parse_number() %>% 
           na.omit() %>% 
           as.numeric() %>% 
           mapvalues(1:nrow(players.combined), players.combined$pre) %>% 
           mean() %>% 
           round(digits=0)
         )

numeric.cols <- c('number', 'points', 'games')
character.cols <- c('name', 'location')

players.combined[, numeric.cols] <- players.combined[, numeric.cols] %>% 
  sapply(., as.numeric) 
players.combined[, character.cols] <- players.combined[, character.cols] %>% 
  sapply(., str_trim) 


