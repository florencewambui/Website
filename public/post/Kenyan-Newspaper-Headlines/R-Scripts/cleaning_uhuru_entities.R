library(readr)
library(dplyr)
library(stringr)
library(mgsub)



politics_entities = read_csv("politics_with_entities.csv")
politics_entities$entities = gsub("[^A-Za-z, ]", "",politics_entities$entities)
uhuru_entities = politics_entities[politics_entities$uhuru == TRUE,]
write.csv(uhuru_entities, "uhuru_entities.csv", row.names = F) # clean the entities and headlines in excel


uhuru_entities = read_csv("uhuru_entities.csv") %>%
  mutate(uhuru = grepl(paste(c("uhuru", "kenyatta", "uhuruto"), collapse = "|"), headline, ignore.case = T))
uhuru_entities1 = uhuru_entities[uhuru_entities$uhuru == T,] #cleaned in excel

uhuru_entities1 = read_csv("uhuru_entities1.csv")
uhuru_entities1$headline = gsub("-$", "", uhuru_entities1$headline)
uhuru_entities1$headline = gsub("comments$", "", uhuru_entities1$headline, ignore.case = T)
uhuru_entities1$headline = gsub("/ Urepor$", "", uhuru_entities1$headline, ignore.case = T)
uhuru_entities1$headline = gsub("/ Ureport$", "", uhuru_entities1$headline, ignore.case = T)

uhuru_entities1$headline = trimws(uhuru_entities1$headline)


uhuru_entities1 = uhuru_entities1 %>%
  group_by(headline) %>%
  filter(row_number() == 1)
uhuru_entities1$four_words = word(uhuru_entities1$headline, start = 1, end = 4)

uhuru_entities1 = uhuru_entities1 %>%
  group_by(year(date), month(date), four_words) %>%
  filter(row_number() == 1)


write.csv(uhuru_entities1, "uhuru_entities1.csv", row.names = F)  

uhuru_entities = read_csv("uhuru_entities1.csv")


non_entities = paste0(c("accountants", "acre", "activists", "advisors", "africas","african states", "aliyekuwa",  
                        "anglo leasing", "anglo", "anti-doping bill", "anti-doping law", "anti-money laundering bill", 
                        "anti-terrorism bill", "artists", "battle", "bill", "billions", "bite", "brexit", "brokers",
                        "budget",  "cabinet","cabine", "cadets",  "cass", "catch",  "catholics", "catholic",
                        "children", "christians", "christmas", "church leaders", "civil groups", "civil servants",
                        "civil society", "civl servancy", "clerics", "congratulates", "constitution", "counties", 
                        "county bill", "county commissioners", "county representatives", 
                        "county revenue allocation bill", "county revenue bill", "county", "courts", "court","detainees",
                        "devolution", "diaspora", "dissenters", "doctors", "drama festival",  "elders", "elder", "editors", "fly uhuru", "hours",
                        
                        "embassy",  "embrace kenya", "embrace","envoys", "envoy", "eurobond", "farmers", 
                        "fertilisers bill", "finance act", "finance bill", "first", "fishermen", "four", "friday",
                        "gays", "gay", "gender bill", "government", "governor", "hudge", "huduma namba", "hyacinth",
                        "iftaar", "iftar", "immigration", "insurance bill", "interest bill", "islamic", "issue", 
                        "jamhuri day", "jamhuri", "jobs", "journalists", "kenya information and communication bill",  "kenyans", "kenyas","kenyan", "october", "palace",
                        "kenyatta caves", "kenyatta market", "kenyatta stadium", "labour day", "lawyers", "lawyer", "leaders", "lobbies", 
                        "lobby",  "madaraka day", "madaraka speech","madaraka",  "mashujaa day", "mashujaa", "matatu",
                        "matrimonial bill", "mausoleum", "mayor", "mcas", "mca",  "media bill", "mining bill", 
                        "ministers", "ministries", "minister","miss world", "modernday", "monday",  "mugumo tree","mugumo",
                        "musicians", "muslims", "narudi nyumbani", "national prayer breakfast", "national prayer day", "netizens",
                        "ngos", "nyumba kumi", "opposition",  "oppositio", "parastals", "parastatal taskforce", "parastatals", 
                        "parliament buildings", "parliamentary group", "peace meeting", "pentagon", 
                        "house" , "houses","hib/aids", "hiv/aids" ,"petitioners", "petitoners", "poachers", "police", 
                        "politicians", "port", "president", "principal  secretaries", "principal secretaries", "prisons",
                        "private army", "procument officers", "prosecutor", "prosecutos", "provincial commissioners", "psvs", 
                        "pupils", "ramadhan", "rebels", "referendum", "resist", "retirees", "revenue allocation bill",
                        "revenue division bill", "salt bae", "school bus safety bill", "scout", "scribes", "sde", "second", 
                        "security bill", "security chiefs", "senate", "senator", "senators", "shilling", "shipping bill", "six", "mps",
                        "smes", "soldiers",  "state agencies", "summit", "taxpayers", "tea", "teachers", "ten", 
                        "terrorists", "the day", "the interal cri", "the national prayer breakfast", "the year", "three", "today", 
                        "traders", "traffic police", "troops", "universal health coverage", "universities", "vat", "vat law", "voters",
                        "wage bill", "ward rep",  "witnesses", "witness", "women deliver", "workers", "year", "years later", 
                        "youths" ,"ebola"), collapse = "|")


uhuru_entities$entities = tolower(uhuru_entities$entities)
uhuru_entities$entities = gsub(non_entities, "", uhuru_entities$entities)

pattern = c("aden","akombes",  "american", "america", "amina mohammed", "aminas", "annans", "auditorgeneral", "barrack muluka", "bensoudas", "bob collymore", "boinett", "bomet uhuru", "brazilian", "britain", "british", "british army", "building bridges intiatives", "caroline mutokos", "central kenya", "chebukatis", "chinas", "churchill show", "coffee", "commonwealth baton", "commonwealth games", "cotus", "cs echesa", "cuban", "dadaab uhuru", "david kimaiyo", "deputy cj", "dubai sheikh", "eacc bill", "ekuru aukot", "ethiopa", "ethiopian", "fifa world cup", "ford <u+00a1>v kenya", "gatundu south", "githus", "hebesh", "helb bill", "icc kenyatta", "icc maraga", "icc7", "ig police", "igad summit", "igathes", "in god<U+613C><U+3E31><U+613C><U+3E36>s name", "indian", "inspector general", "israeli", "joe kadenges", "johos", "jubilee uhuru", "jubilees", "jubille", "kajiado central", "kakemega", "kalonzo mudavadi", "kalonzos", "kanus", "kanze denas", "kdf el adde", "kiambaa church", "kibaki raila", "kibakis", "kilifi north", "kindiki kithure", "kirinyaga county council", "kithure kindiki", "kiunjuri muriithi", "knhcr", "knhrc", "joshua kutuny", "kurias", "kuttuny", "maara mp", "madiba", "magufulis", "makondes", "mark toos", "kenneth matiba", "mt kenya", "mt. kenya tour", "murkomens", "muthauras", "muthayra", "mwingi west", "nairobi city", "nairobis", "namibias", "nandi bomet", "narc  k", "narcs", "nasa munya", "national shabaab", "ndegwas", "nemas", "ngunjiri sudi", "nigerian", "nigerians", "nigerias", "s nkaisery", "nkaiserry", "ntimamas", "nyayo wards", "nzioka waita", "obama uhuru", "obamas", "odingas", "odms", "op kaa", "opposition atwoli", "otieono", "peugeot assembly", "pks", "raila ruto", "railas", "rais kenyatta", "rais uhuru", "russian", "ruto a", "ruto days", "ruto icc", "ruto nyanza", "ruto raila", "ruto urp", "rutos", "rutouhuru", "s nkaisery", "sing<U+613C><U+3E31><U+613C><U+3E36>oei", "sonkos", "south korean", "statehouse", "supreme courts", "tangatanga kieleweke", "tanzanian", "tanzanias", "the bomas of kenya", "the north rift", "the supreme court", "tom mboya", "tunois", "ugandan", "ugandans", "uhru",  "uhuru addis", "uhuru ali", "uhuru cabinet", "uhuru coast", "uhuru dp", "uhuru macron", "uhuru nyanza", "uhuru. rift", "uhuruchallenge", "uhurus", "uhurus  adc", "uhurus coast", "uhuuru", "ukur", "ukur yatani", "ukur yattani", "uwezo funds", "wamalwas", "western mps", "wiper party", "zumas")
replacement = c("duale", "akombe", "USA", "USA", "amina", "amina", "annan", "auditor general", "barack muluka", "bensouda", "colleymore", "boinnet", "bomet, uhuru", "brazil", "UK", "UK", "UK, army", "building bridges intiative", "caroline mutoko", "central", "chebukati", "china", "churchill", "coffee sector", "commonwealth", "commonwealth", "cotu", "echesa", "cuba", "dadaab, uhuru", "kimaiyo", "dcj", "dubai", "eacc", "aukot", "ethiopia", "ethiopia", "fifa", "ford kenya", "gatundu", "githu", "shebesh", "helb", "icc ,uhuru", "icc, maraga", "icc", "ig", "igad", "igathe", "in god’s name", "india", "ig", "israel", "joe kadenge", "joho", "jubilee, uhuru", "jubilee", "jubilee", "kajiado", "kakamega", "kalonzo, mudavadi", "kalonzo", "kanu", "kanze dena", "kdf, somalia", "kiambaa", "kibaki, raila", "kibaki", "kilifi", "kindiki", "kirinyaga", "kindiki", "kiunjuri", "knchr", "knchr", "kutuny", "kuria", "kutuny", "maara", "mandela", "magufuli", "makonde", "mark too", "matiba", "mt. kenya", "mt. kenya", "murkomen", "muthaura", "muthaura", "mwingi", "nairobi", "nairobi", "namibia", "nandi, bomet", "narc", "narc", "nasa, munya", "alshabaab", "ndegwa", "nema", "ngunjiri, sudi", "nigeria", "nigeria", "nigeria", "nkaissery", "nkaissery", "ntimama", "nyayo", "nzioka", "obama, uhuru", "obama", "odinga", "odm", "op, kaa", "atwoli", "otieno", "peugeot", "pk", "raila, ruto", "raila", "uhuru", "uhuru", "russia", "ruto", "ruto", "ruto, icc", "ruto, nyanza", "ruto, raila", "ruto, urp", "ruto", "ruto, uhuru", "nkaissery", "singoei", "sonko", "south korea", "state house", "supreme court", "tangatanga", "tanzania", "tanzania", "bomas", "north rift", "supreme court", "mboya", "tunoi", "uganda", "uganda", "uhuru", "uhuru, ethiopia", "uhuru, ali", "uhuru", "uhuru, coast", "uhuru, ruto", "uhuru, macron", "uhuru, nyanza", "uhuru, rift valley", "uhuru", "uhuru", "uhuru, adc", "uhuru, coast", "uhuru", "ukur yatani", "ukur yatani", "ukur yatani", "uwezo fund", "wamalwa", "western", "wiper", "zuma")
uhuru_entities$entities = mgsub(uhuru_entities$entities, pattern, replacement)
uhuru_entities$entities = trimws(uhuru_entities$entities, "both")

uhuru_entities$entities = gsub("\\s+", " ", uhuru_entities$entities)
uhuru_entities$entities = gsub(", s$", "", uhuru_entities$entities)
uhuru_entities$entities = gsub("^,", "", uhuru_entities$entities)
uhuru_entities$entities = gsub(",$", "", uhuru_entities$entities)
uhuru_entities$entities = gsub(",,", ",", uhuru_entities$entities)
uhuru_entities$entities = gsub(", ,", ",", uhuru_entities$entities)
uhuru_entities$entities = trimws(uhuru_entities$entities, "both")


uhuru_entities1 = uhuru_entities %>%
  filter(grepl("uhuru", entities), grepl(",", entities)) %>%
  select(source, date, headline, entities)


write.csv(uhuru_entities1, "uhuru_entities2.csv", row.names = F) # cleaned a few words in excel(jap - jubilee, kenyan - kenya, african - africa)

uhuru_entities2 = read_csv("uhuru_entities2.csv")

