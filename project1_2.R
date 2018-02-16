library(tidyverse)
library(plyr)

con <- file('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\tournamentinfo.txt', open='r')

raw <- read.fwf(con, widths=c(6,2,32,1,5,43), skip=4, stringsAsFactors=FALSE)

raw <- raw[1:nrow(raw) %% 3 != 0,]
data1 <- raw[seq(1,nrow(raw),2),] %>% setNames(c('Number', 'd1', 'Name', 'd2', 'Points', 'Post')) %>% select(-d1,-d2)
data2 <- raw[seq(2,nrow(raw),2),] %>% setNames(c('Location', 'd1', 'Pre', 'd2', 'd3','d4')) %>% select(-d1,-d2, -d3, -d4) %>% mutate(Number = data1$Number)


combined <- left_join(data1, data2, by='Number')

combined$Pre <- combined$Pre %>% str_sub(15,18) %>% as.numeric()
combined$Post <- combined$Post %>% lapply(. %>% str_split('\\|') %>% unlist() %>% parse_number() %>% na.omit() %>% as.numeric() %>% mapvalues(1:nrow(combined), combined$Pre) %>% mean() %>% round(digits=0))

combined[,c('Number','Points','Games')] <- combined[,c('Number','Points','Games')] %>% sapply(., as.numeric) 
combined[,c('Name','Location')] <- combined[,c('Name','Location')] %>% sapply(., str_trim) 


close(con)