library(tidyverse)
library(plyr)
library(magrittr)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')
raw.data <- read.fwf(con, widths=c(6, 2, 32, 1, 5, 43), skip=4, stringsAsFactors=FALSE) 
close(con)
  
players.row.1 <- raw.data[seq(1, nrow(raw.data), 3), ] %>% 
  setNames(c('number', 'd1', 'name', 'd2', 'points', 'opponents')) %>% 
  select(-d1, -d2)
players.row.2 <- raw.data[seq(2, nrow(raw.data), 3), ] %>% 
  setNames(c('location', 'd1', 'pre', 'd2', 'd3', 'd4')) %>% 
  select(-d1, -d2, -d3, -d4) %>% 
  mutate(number=players.row.1$number)

players.combined <- left_join(players.row.1, players.row.2, by='number') %>% 
  subset(select=c(2, 5, 3, 6, 4)) %>%
  map_df(str_trim)


players.combined$points %<>% 
  as.numeric()
players.combined$pre %<>% 
  str_sub(15, 18) %>% 
  as.numeric()
players.combined$opponents %<>% 
  map_dbl(. %>% str_extract_all('\\d+') %>%
            unlist() %>% 
            as.numeric() %>% 
            mapvalues(seq_along(players.combined$pre), players.combined$pre) %>% 
            mean() %>% 
            round()
         )

write.csv(players.combined, file='ChessData.csv')



