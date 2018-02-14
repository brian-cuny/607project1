library(tidyverse)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')
raw <- readLines(con)

players <- tibble(name=character(), location=character(), points=numeric(), pre=numeric(), post=numeric())
games_data <- list()

for(i in seq(5,length(raw),3)){
  first <- raw[i] %>% 
    str_match('(\\d+).*?\\| (([:graph:]+ ?){2,}) *?\\|([0-9. ]+)(.*)') %>% 
    as.list() %>% 
    setNames(c('all', 'number', 'name', '_', 'pre', 'games'))
  second <- raw[i+1] %>% 
    str_match('(\\w+).*?R: +(\\d+)') %>% 
    as.list() %>% 
    setNames(c('all', 'loc', 'pre'))
  players[first$number,] <- list(name=first$name, location=second$loc, points=first$pre, pre=second$pre, post=0)
  
  games_data[[first$number]] <- first$games %>% 
    str_split('\\|\\w* *') %>%
    lapply(as.numeric) %>%
    sapply(na.exclude)
}

players[,c('points','pre','post')] <- players[,c('points','pre','post')] %>% sapply(. %>% as.numeric)
players[,c('name')] <- players[,c('name')] %>% lapply(. %>% str_trim())
players$post <- games_data %>% 
  sapply(. %>% (function(x){mapvalues(x, 1:nrow(players), players$pre) %>% mean() %>% round(digits=0)}))

close(con)




