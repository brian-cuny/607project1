library(stringr)
library(tidyverse)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')
raw <- readLines(con)

players <- data.frame(name=character(), location=character(), points=numeric(), pre=numeric(), post=numeric(), stringsAsFactors = FALSE)
games_data <- list()

for(i in seq(5,length(raw),3)){
  first <- setNames(str_match(raw[i], '(\\d+).*?\\| (([:graph:]+ ?){2,}) *?\\|([0-9. ]+)(.*)') %>% as.list(), c('all', 'number', 'name', '_', 'pre', 'games'))
  second <- setNames(str_match(raw[i+1], '(\\w+).*?R: +(\\d+)') %>% as.list(), c('all', 'loc', 'pre'))
  players[first$number,] <- list(name=first$name, location=second$loc, points=first$pre, pre=second$pre, post=0)
  
  games_data[[first$number]] <- str_split(first$games, '\\|\\w* *') %>%
    lapply(as.numeric) %>%
    sapply(na.exclude)
}

players[,c('points','pre','post')] <- sapply(players[,c('points','pre','post')], as.numeric) 
players$post <- sapply(games_data, FUN=function(x){mapvalues(x, 1:nrow(players), players[,'pre']) %>% mean() %>% round(digits=0)})

close(con)