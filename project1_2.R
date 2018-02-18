library(tidyverse)
library(plyr)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')
raw.data <- read.fwf(con, widths=c(6, 2, 32, 1, 5, 43), skip=4, stringsAsFactors=FALSE) 
close(con)
  
players.row.1 <- raw.data[seq(1, nrow(raw.data), 3), ] %>% 
  setNames(c('number', 'd1', 'name', 'd2', 'points', 'opponents')) %>% 
  select(-d1, -d2)
players.row.2 <- raw.data[seq(2, nrow(raw.data), 3), ] %>% 
  setNames(c('location', 'd1', 'pre', 'd2', 'd3', 'd4')) %>% 
  select(-d1, -d2, -d3, -d4) %>% 
  mutate(number = players.row.1$number)

players.combined <- left_join(players.row.1, players.row.2, by='number') %>% 
  subset(select=c(2, 5, 3, 6, 4))

players.combined$pre <- players.combined$pre %>% 
  str_sub(15, 18) %>% 
  as.numeric()
players.combined$opponents <- players.combined$opponents %>% 
  lapply(. %>% 
           str_extract_all('\\d+') %>% 
           unlist() %>% 
           as.numeric() %>% 
           mapvalues(1:nrow(players.combined), players.combined$pre) %>% 
           mean() %>% 
           round(digits=0)
  )
  

players.combined[, 3:5] <- players.combined[, 3:5] %>% 
  sapply(., as.numeric) 
players.combined[, 1:2] <- players.combined[, 1:2] %>% 
  sapply(., str_trim) 
write.csv(players.combined, file='ChessData.csv')



