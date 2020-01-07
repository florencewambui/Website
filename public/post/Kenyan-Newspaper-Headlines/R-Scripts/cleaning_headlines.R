library(dplyr)
news = readRDS("Raw Data/headlines.rds") %>%
  arrange(date) %>%
  group_by(headline) %>%
  filter(row_number() == 1)

news$headline = gsub("[^A-Za-z1-9[:punct:] ]+", "", news$headline)
news$headline = gsub("\\d+ (hour|hours|day|days|minute|minutes|second|seconds) ago", "", news$headline, ignore.case = TRUE)
news$headline = trimws(news$headline, "both")
news$headline = gsub("(News|LIVE BLOG|Live|LIVE|Economy|EDITORIAL|IN PICTURES|
                     DOWNLOAD|BLOG|lifestyle|Editorials| PICTURES|Opinion|READ|LIVE|
                     Newsplex|news|news|New|NEWS LIVE|PHOTOS|Life|VIDEO|FRONTROW|FACT CHECK)|
                     Financial Time|WATCH LIVE|Columnists|NATION|INTERVIEW|Habari", "", news$headline)



news$headline = gsub("^[:punct:]", "", news$headline)
news$headline = gsub("[:punct:]$", "", news$headline)

news$headline = trimws(news$headline, "both")

news$headline = gsub("-$", "", news$headline)
news$headline = gsub("comments$", "", news$headline, ignore.case = T)
news$headline = gsub("/ Urepor$", "", news$headline, ignore.case = T)
news$headline = gsub("/ Ureport$", "", news$headline, ignore.case = T)

news$headline = trimws(news$headline)


news$four_words = word(news$headline, start = 1, end = 4)

news = news %>%
  group_by(headline) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(year(date), month(date), four_words) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(date, source, headline)

saveRDS(news, "Cleaned Data/headlines.rds")
write.csv(news, "Cleaned Data/headlines.csv", row.names = F)
