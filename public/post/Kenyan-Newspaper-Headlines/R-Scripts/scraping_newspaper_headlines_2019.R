library(rvest)
library(httr)
library(jsonlite)
library(devtools)
library(wayback) # devtools::install_github("jsta/wayback")
library(lubridate)
library(tidyverse)
library(stringr)
library(beepr)

scrape_headlines = function(url, headline_date, filename){
  newspaper = wayback_url(url, headline_date)
  headline_title = wayback_scrape(newspaper$url, "h1 a") # standard
  headline_type = rep("h1", length(headline_title))
  headlines_h1 = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "h2 a")
  headline_type = rep("h2", length(headline_title))
  headlines_h2 = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "h3 a")
  headline_type = rep("h3", length(headline_title))
  headlines_h3 = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "h4 a")
  headline_type = rep("h4", length(headline_title))
  headlines_h4 = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "h5 a")
  headline_type = rep("h5", length(headline_title))
  headlines_h5 = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "li a")
  headline_type = rep("li", length(headline_title))
  headlines_li = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "p a")# standard
  headline_type = rep("p", length(headline_title))
  headlines_p = data.frame(headline_title, headline_type)
  headline_title = wayback_scrape(newspaper$url, "a h3")# standard
  headline_type = rep("a h3", length(headline_title))
  headlines_ah3 = data.frame(headline_title, headline_type)
  headlines = rbind(headlines_h1, headlines_h2, headlines_h3, headlines_h4, headlines_h5, headlines_li, headlines_p, headlines_ah3)
  headlines$headline_title = gsub("[,]", "", headlines$headline_title)
  headlines$headline_title = trimws(headlines$headline_title, which = "both")
  headlines$headline_title = gsub("\\s+", " ", headlines$headline_title)
  headlines = headlines[str_count(headlines$headline_title, "\\S+") > 3,]
  headlines = headlines %>% group_by(headline_title) %>% filter(row_number() == 1)
  headline_date = strptime(headline_date, format = "%Y%m%d")
  headline_date = rep(headline_date, nrow(headlines))
  newspaper_headlines = data.frame(headline_date, headlines)
  write.table(newspaper_headlines, filename, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
}

dates = as.character(seq(from = as.Date("2019-08-19"), to = Sys.Date(), by = 1))
dates = gsub("-", "", dates)

for (i in 1:length(dates)){
  try(scrape_headlines(url = "www.nation.co.ke/", headline_date = dates[i], filename = "daily_nation_from_2017101.csv"))
  try(scrape_headlines(url = "www.businessdailyafrica.com/", headline_date = dates[i], filename = "business_daily_from_2017101.csv"))
  try(scrape_headlines(url = "https://www.standardmedia.co.ke/", headline_date = dates[i], filename = "standard_from_2017101.csv"))
  try(scrape_headlines(url = "https://www.the-star.co.ke/", headline_date = dates[i], filename = "star_from_2017101.csv"))
  
  
}



beep("fanfare")


