library(tidyverse)
library(fs)
setwd("Raw Data")

files = dir_ls()


headlines = files %>%
  map_dfr(read_csv,  col_names = c("date", "headline", "type"), .id = "source")


  
headlines = headlines %>%
  group_by(source, headline) %>%
  filter(row_number() == 1)

headlines$date = as.Date(headlines$date, format = "%Y-%m-%d")

headlines$source = gsub("_from_2017101.csv", "", headlines$source)
headlines$source = gsub("_upto_2017101.csv", "", headlines$source)

write.csv(headlines, "headlines.csv", row.names = F)
saveRDS(headlines, "headlines.rds")