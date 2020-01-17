library(readr)
library(dplyr)

news = readRDS("Raw Data/headlines.rds")

corruption_news = news %>%
  filter(grepl(paste(c("corruption", "graft", "stolen", "steal", "stolen", "brib", "embezzl", "misappropriat", "pilfer"), collapse = "|"), headline, ignore.case = T))
#write.csv(corruption_news, "corruption_news0.csv", row.names = F)#cleaned the data in excel by removing rows not related to corruption in public offices
# output is corruption_news0.csv

corruption_news1 = read_csv("corruption_news1.csv")

corruption_news1$headline = gsub("-$", "", corruption_news1$headline)
corruption_news1$headline = gsub("comments$", "", corruption_news1$headline, ignore.case = T)
corruption_news1$headline = gsub("/ Urepor$", "", corruption_news1$headline, ignore.case = T)
corruption_news1$headline = gsub("/ Ureport$", "", corruption_news1$headline, ignore.case = T)

corruption_news1$headline = trimws(corruption_news1$headline)



corruption_news1$headline = gsub("[^A-Za-z1-9[:punct:] ]+", "", corruption_news1$headline)
corruption_news1$headline = gsub("\\d+ (hour|hours|day|days|minute|minutes|second|seconds) ago", "", corruption_news1$headline, ignore.case = TRUE)
corruption_news1$headline = trimws(corruption_news1$headline, "both")

corruption_news1$headline = gsub("^(News|LIVE BLOG|Live|LIVE|Economy|EDITORIAL|IN PICTURES|
                                 DOWNLOAD|BLOG|lifestyle|Editorials| PICTURES|Opinion|READ|LIVE|Newsplex|
                                 POLITICS|politics|New|NEWS LIVE|PHOTOS|Life|VIDEO|FRONTROW|FACT CHECK)|
                                 Financial Time|WATCH LIVE|Columnists|NATION|INTERVIEW|Habari|
                                 Data hub|Ideas & Debate|Data Hub|NEWS INDEPTH|INDEPTH|Counties|Letter|County|Corporate|Technology|Enterprise|Market", "", corruption_news1$headline, ignore.case = FALSE)
corruption_news1$headline = gsub("(News|LIVE BLOG|Live|LIVE|EDITORIAL|IN PICTURES|DOWNLOAD|BLOG|lifestyle|Editorials| 
                                 PICTURES|Opinion|READ|LIVE|Newsplex|POLITICS|politics|New|NEWS LIVE|PHOTOS|Life|VIDEO|FRONTROW|FACT CHECK)|
                                 Financial Time|WATCH LIVE|Columnists|NATION|INTERVIEW|Habari|INTERVIEW|Habari|
                                 Data hub|Ideas & Debate|Data Hub|NEWS INDEPTH|INDEPTH|Counties|Letter|County|Corporate|Technology|Enterprise|Market$", "", corruption_news1$headline, ignore.case = FALSE)

corruption_news1$headline = trimws(corruption_news1$headline, "both")


corruption_news1 = corruption_news1 %>%
  group_by(headline) %>%
  filter(row_number() == 1)
corruption_news1$four_words = word(corruption_news1$headline, start = 1, end = 4)

corruption_news1 = corruption_news1 %>%
  group_by(year(date), month(date), four_words) %>%
  filter(row_number() == 1) %>%
  ungroup()

corruption_news1 = corruption_news1 %>%
  select(date, source, headline) %>%
  arrange(date)


write.csv(corruption_news1, "corruption_news2.csv", row.names = F)
