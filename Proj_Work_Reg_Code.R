library(tidyverse)
library(rvest)
library(stringr)

# Trying to scrape aggrgate scores
url <- "http://www.gamerankings.com/browse.html?site=&cat=0&year=2017&numrev=1&sort=0&letter=&search="
SCORE_GET <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
SCORE_GET <- as.data.frame(SCORE_GET)
# Rbind for appending the other pages
for (i in 1:7){
  url <- paste("http://www.gamerankings.com/browse.html?page=", i, "&year=2017&numrev=1", sep = "")
  SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
  SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
}

url <- "http://www.gamerankings.com/browse.html?site=&cat=0&year=2016&numrev=1&sort=0&letter=&search="
SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
# Rbind for appending the other pages
for (i in 1:10){
  url <- paste("http://www.gamerankings.com/browse.html?page=", i, "&year=2016&numrev=1", sep = "")
  SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
  SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
}

url <- "http://www.gamerankings.com/browse.html?site=&cat=0&year=2015&numrev=1&sort=0&letter=&search="
SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
# Rbind for appending the other pages
for (i in 1:10){
  url <- paste("http://www.gamerankings.com/browse.html?page=", i, "&year=2015&numrev=1", sep = "")
  SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
  SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
}

url <- "http://www.gamerankings.com/browse.html?site=&cat=0&year=2014&numrev=1&sort=0&letter=&search="
SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
# Rbind for appending the other pages
for (i in 1:9){
  url <- paste("http://www.gamerankings.com/browse.html?page=", i, "&year=2014&numrev=1", sep = "")
  SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
  SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
}

url <- "http://www.gamerankings.com/browse.html?site=&cat=0&year=2013&numrev=1&sort=0&letter=&search="
SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
# Rbind for appending the other pages
for (i in 1:11){
  url <- paste("http://www.gamerankings.com/browse.html?page=", i, "&year=2013&numrev=1", sep = "")
  SUBSCORE_GET <- url %>% read_html %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBSCORE_GET <- as.data.frame(SUBSCORE_GET)
  SCORE_GET <- rbind(SCORE_GET,SUBSCORE_GET)
}

# Start to tidy data set
SCORE_GET$X1 <- NULL
colnames(SCORE_GET)[colnames(SCORE_GET) == "X2"] <- "Platform"
colnames(SCORE_GET)[colnames(SCORE_GET) == "X3"] <- "Game.Untidy"
colnames(SCORE_GET)[colnames(SCORE_GET) == "X4"] <- "Score.Untidy"
# Remove escape sequences
SCORE_GET$Game.Untidy <- str_replace_all(SCORE_GET$Game.Untidy,"\r\n\t\t","$")

# Tidy Game.Untidy to Game, Developer, and Publisher
SCORE_GET <- separate(SCORE_GET, Game.Untidy, into = c("Game", "Info"), sep = "\\$")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"\\, 2017","")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"\\, 2016","")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"\\, 2015","")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"\\, 2014","")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"\\, 2013","")
SCORE_GET$Info <- str_replace(SCORE_GET$Info,"Eighting\\/Raizing", "Eighting")
# Some developers self publish their game which is why a publisher is not listed for those games. Let's fix that. For any row that has N/A in Publisher, copy publisher to developer.
SCORE_GET <- separate(SCORE_GET, Info, into = c("Developer", "Publisher"), sep = "/")
for (i in 1:nrow(SCORE_GET)) {
  if (is.na(SCORE_GET$Publisher[i] == TRUE)) {SCORE_GET$Publisher[i] <- SCORE_GET$Developer[i]}
}
# Tidy Score.Untiy to Score and Number.Of.Reviews
SCORE_GET$Score <- str_extract(SCORE_GET$Score.Untidy,"[0-9]+.[0-9]+")
SCORE_GET$Number.of.Reviews <- str_sub(SCORE_GET$Score.Untidy, 7, 8)
SCORE_GET$Score.Untidy <- NULL

# Getting sales data for software and hardware
url <- "http://www.vgchartz.com/weekly/41581/USA/"
SALES_GET <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
# There is already a table for total sales of software and hardware. We can pull that as well as weekly sales for later use.
SALES_CONSOLE_GET <- SALES_GET[[154]]
SALES_TOTAL_SOFTWARE_GET <- SALES_GET[[155]]
SALES_WEEKLY_SOFTWARE_GET <- SALES_GET[[2]]
# The data doesn't have the actual date. Using the url, which has code that can be used to interpret the date, I can thus get the date.
SALES_WEEKLY_SOFTWARE_GET$Week.Ending <- as.Date(41581 - 3, origin = "1900-01-01")
SALES_CONSOLE_GET$Week.Ending <- as.Date(41581 - 3, origin = "1900-01-01")
SALES_TOTAL_SOFTWARE_GET$Week.Ending <- as.Date(41581 - 3, origin = "1900-01-01")

# With these three data sets initialized, we need a for loop that can extract these three sets for each week until the most current data avaliable. 
for (i in seq(from = 41588, to = 43037, by = 7)) {
  url <- paste("http://www.vgchartz.com/weekly/", i, "/USA/", sep = "")
  SALES_GET <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
  SALES_CONSOLE_GET_PART <- SALES_GET[[154]]
  SALES_CONSOLE_GET_PART$Week.Ending <- as.Date(i - 3, origin = "1900-01-01")
  SALES_TOTAL_SOFTWARE_GET_PART <- SALES_GET[[155]]
  SALES_TOTAL_SOFTWARE_GET_PART$Week.Ending <- as.Date(i - 3, origin = "1900-01-01")
  SALES_WEEKLY_SOFTWARE_GET_PART <- SALES_GET[[2]]
  SALES_WEEKLY_SOFTWARE_GET_PART$Week.Ending <- as.Date(i - 3, origin = "1900-01-01")
  
  SALES_CONSOLE_GET <- rbind(SALES_CONSOLE_GET,SALES_CONSOLE_GET_PART)
  SALES_TOTAL_SOFTWARE_GET <- rbind(SALES_TOTAL_SOFTWARE_GET,SALES_TOTAL_SOFTWARE_GET_PART)
  SALES_WEEKLY_SOFTWARE_GET <- rbind(SALES_WEEKLY_SOFTWARE_GET,SALES_WEEKLY_SOFTWARE_GET_PART)
}

# Tidy software sales
names(SALES_WEEKLY_SOFTWARE_GET) <- make.names(names(SALES_WEEKLY_SOFTWARE_GET))
colnames(SALES_WEEKLY_SOFTWARE_GET)[colnames(SALES_WEEKLY_SOFTWARE_GET) == "Weekly"] <- "Remove"
colnames(SALES_WEEKLY_SOFTWARE_GET)[colnames(SALES_WEEKLY_SOFTWARE_GET) == "Total"] <- "Remove2"
colnames(SALES_WEEKLY_SOFTWARE_GET)[colnames(SALES_WEEKLY_SOFTWARE_GET) == "Week.."] <- "Weekly"
names(SALES_WEEKLY_SOFTWARE_GET)[6] <- "Total"
names(SALES_WEEKLY_SOFTWARE_GET)[7] <- "Week.Number"

# Two games are from 2015 and are labeled as such with parenthesis. Change that to brackets for easier separating later on.
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2015\\)","\\[2015\\]")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2017\\)","")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2016\\)","")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2015\\)","")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2014\\)","")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"\\(2013\\)","")

# More tidy 
SALES_WEEKLY_SOFTWARE_GET <- separate(SALES_WEEKLY_SOFTWARE_GET, Game, into = c("Game", "Platform.Untidy"), sep = "\\(")
SALES_WEEKLY_SOFTWARE_GET <- separate(SALES_WEEKLY_SOFTWARE_GET, Platform.Untidy, into = c("Platform", "Genre.Untidy"), sep = "\\)")
SALES_WEEKLY_SOFTWARE_GET$Genre <- str_extract(SALES_WEEKLY_SOFTWARE_GET$Genre.Untidy, "\\, [A-z]+\\-?[A-z]*")
SALES_WEEKLY_SOFTWARE_GET$Genre <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Genre,"\\, ","")

# From the beginning after scraping the table had a duplicate line for each observation. Need to filter those out. 
SALES_WEEKLY_SOFTWARE_GET[is.na(SALES_WEEKLY_SOFTWARE_GET)] <- 0
SALES_WEEKLY_SOFTWARE_GET <- filter(SALES_WEEKLY_SOFTWARE_GET, Pos != 0)
SALES_WEEKLY_SOFTWARE_GET <- SALES_WEEKLY_SOFTWARE_GET[,c("Pos", "Game", "Platform", "Genre", "Weekly", "Total", "Week.Number","Week.Ending")]

# Convert numbers from character to integer for later use. Pro and N/A signified that membership was required to get those numbers. Put those to zero first to be later noted.
SALES_WEEKLY_SOFTWARE_GET$Weekly <- str_replace_all(SALES_WEEKLY_SOFTWARE_GET$Weekly,"\\,","")
SALES_WEEKLY_SOFTWARE_GET$Weekly <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Weekly,"Pro","0")
SALES_WEEKLY_SOFTWARE_GET$Weekly <- as.integer(SALES_WEEKLY_SOFTWARE_GET$Weekly)
SALES_WEEKLY_SOFTWARE_GET$Total <- str_replace_all(SALES_WEEKLY_SOFTWARE_GET$Total,"\\,","")
SALES_WEEKLY_SOFTWARE_GET$Total <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Total,"N\\/A","0")
SALES_WEEKLY_SOFTWARE_GET$Total <- as.integer(SALES_WEEKLY_SOFTWARE_GET$Total)

# Filter out first week sales only
SALES_WEEKLY_SOFTWARE_GET <- filter(SALES_WEEKLY_SOFTWARE_GET, Week.Number == 1)

# Merging
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game," $","")
SALES_WEEKLY_SOFTWARE_GET$Platform <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Platform,"XOne","XONE")
SALES_WEEKLY_SOFTWARE_GET$Platform <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Platform,"WiiU","WIIU")
SALES_WEEKLY_SOFTWARE_GET$Platform <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Platform,"PSV","VITA")
# Two game has two versions, but sales data doesn't have sales number for each sku. For simplicity, just combine both version into one since their review scores are similar.
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Pokemon Sun","Pokemon Sun/Moon")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Fire Emblem Fates: Special Edition","Fire Emblem Fates")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Pokemon Omega Ruby", "Pokemon Omega Ruby and Alpha Sapphire")

# Miscelaneious Edits for mismatched titles that I noticed
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Odin Sphere: Leifdrasir","Odin Sphere Leifthrasir")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"MLB 16: The Show","MLB The Show 16")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Resident Evil 4 HD","Resident Evil 4")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Doom","DOOM")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Rise of the Tomb Raider","Rise of the Tomb Raider 20 Year Celebration")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Revelator","REVELATOR")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"The Elder Scrolls V Skyrim","The Elder Scrolls V Skyrim Special Edition")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Plants vs. Zombies","Plants vs Zombies")

# Lots of issues with usage of colon in names, and instead of going through each name just remove all of it from both lists. 
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,":","")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,":","")

# Removing unwanted platforms
SCORE_GET <- SCORE_GET %>% filter(Platform != "PC")
SCORE_GET <- SCORE_GET %>% filter(Platform != "IOS")
SCORE_GET <- SCORE_GET %>% filter(Platform != "PS3")
SCORE_GET <- SCORE_GET %>% filter(Platform != "X360")
SALES_WEEKLY_SOFTWARE_GET <- SALES_WEEKLY_SOFTWARE_GET %>% filter(Platform != "PS3")
SALES_WEEKLY_SOFTWARE_GET <- SALES_WEEKLY_SOFTWARE_GET %>% filter(Platform != "X360")

# More edits
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Resident Evil 7 biohazard","Resident Evil VII Biohazard")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Wipeout Omega Collection","WipEout Omega Collection")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Ys VIII Lacrimosa of DANA","Ys VIII Lacrimosa of Dana")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Guilty Gear Xrd Rev 2","Guilty Gear Xrd REV 2")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Kingdom Hearts HD I.5 + II.5 Remix","Kingdom Hearts 1.5 + 2.5 Remix")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Etrian Odyssey V Beyond the Myth","Etrian Odyssey V")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Middle-earth Shadow of War","Middle-Earth Shadow of War")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Animal Crossing amiibo Festival","Animal Crossing Amiibo Festival")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Bravely Default","Bravely Default Flying Fairy")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Call of Duty Black Ops III","Call of Duty Black Ops 3")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Corpse Party","Corpse Party Back to School Edition")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Disney Infinity 2.0 Edition","Disney Infinity 2.0 Marvel Super Heroes")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Disney Infinity 3.0 Edition","Disney Infinity 3.0")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"DmC Devil May Cry Definitive Edition","DmC Definitive Edition")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Dragon Ball Z Extreme Butoden","Dragon Ball Z Extreme Butouden")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Dragon Quest Heroes The World Tree's Woe and the Blight Below","Dragon Quest Heroes")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Etrian Odyssey 2 Untold The Fafnir Knight","Etrian Odyssey 2 Untold Knight of Fafnir")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Final Fantasy X / X-2 HD Remaster","Final Fantasy X-X2 HD")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Gears of War Ultimate Edition","Gears of War")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Hyperdimension Neptunia Re;Birth3 V Generation","Hyperdimension Neptunia Re;Birth 3 V Generation")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"inFamous Second Son","inFAMOUS Second Son")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"J-Stars Victory Vs+","J-Stars Victory Vs.+")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Layton's Mystery Journey Katrielle and The Millionaires' Conspiracy","Layton's Mystery Journey Katrielle and the Millionaire's Conspiracy")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"LEGO Batman 3 Beyond Gotham","Lego Batman 3 Beyond Gotham")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"LEGO Star Wars The Force Awakens","Lego Star Wars The Force Awakens")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Middle-earth Shadow of Mordor","Middle-Earth Shadow of Mordor")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"PlayStation VR WORLDS","PlayStation VR Worlds")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Project X Zone 2","Project X Zone 2 Brave New World")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Professor Layton VS Phoenix Wright Ace Attorney","Professor Layton vs Pheonix Wright Ace Attorney")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Tom Clancy's Rainbow Six Siege","Rainbow Six Siege")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Sniper Elite III","Sniper Elite 3")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Sonic Boom Rise of Lyric","Sonic Boom")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Star Wars Battlefront","Star Wars Battlefront [2015]")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Stranger of Sword City","Stranger of Sword City Black Palace")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Super Mario Maker for Nintendo 3DS","Super Mario Maker")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"The Elder Scrolls V Skyrim Special Edition","The Elder Scrolls V Skyrim")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"The Order 1886","The Order 1866")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Tokyo Twilight Ghost Hunters","Tokyo Twilight Ghosthunters")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Xenoblade Chronicles 3D","Xenoblade Chronicles")
SCORE_GET$Game <- str_replace(SCORE_GET$Game,"Yokai Watch","Yo-kai Watch")
SALES_WEEKLY_SOFTWARE_GET$Game <- str_replace(SALES_WEEKLY_SOFTWARE_GET$Game,"Need for Speed \\[2015\\]","Need for Speed")




# Merge the sales data to review scores
SCORE_SALES_ALL <- left_join(SCORE_GET,SALES_WEEKLY_SOFTWARE_GET, by = c("Game","Platform"))

# Remove unwanted listings
SCORE_SALES_ALL <- SCORE_SALES_ALL %>% filter(Platform != "PC")
SCORE_SALES_ALL <- SCORE_SALES_ALL %>% filter(Week.Number != "NA")
SCORE_SALES_ALL <- SCORE_SALES_ALL %>% filter(Weekly != 0)

# Getting box office weekend numbers
url <- "http://www.boxofficemojo.com/weekend/?yr=2017&p=.htm"
BOX_GET <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
BOX_GET <- BOX_GET[[5]]
BOX_GET <- as.data.frame(BOX_GET)
BOX_GET$Year <- 2017
n <- nrow(BOX_GET)
BOX_GET <- BOX_GET[2:n,]

for (i in 2013:2016) {
  url <- paste("http://www.boxofficemojo.com/weekend/?yr=", i, "&p=.htm", sep = "")
  SUBBOX_GET <- url %>% read_html() %>% html_nodes("table") %>% html_table(fill = TRUE)
  SUBBOX_GET <- SUBBOX_GET[[5]]
  SUBBOX_GET <- as.data.frame(SUBBOX_GET)
  SUBBOX_GET$Year <- i
  n <- nrow(SUBBOX_GET)
  SUBBOX_GET <- SUBBOX_GET[2:n,]
  BOX_GET <- rbind(BOX_GET,SUBBOX_GET)
}
rownames(BOX_GET) <- seq(length=nrow(BOX_GET)) 

names(BOX_GET) <- make.names(names(BOX_GET))
colnames(BOX_GET)[colnames(BOX_GET) == "X1"] <- "Date"
colnames(BOX_GET)[colnames(BOX_GET) == "X4"] <- "Box.office"
colnames(BOX_GET)[colnames(BOX_GET) == "X5"] <- "Change"
BOX_GET <- BOX_GET[,c("Date","Box.office","Year","Change")]
rownames(BOX_GET) <- seq(length=nrow(BOX_GET)) 
BOX_GET$Change <- str_replace(BOX_GET$Change,"-","0")
BOX_GET$Change <- str_replace(BOX_GET$Change,"\\+","")
BOX_GET$Change <- str_replace(BOX_GET$Change,"\\%","")
BOX_GET$Change <- sapply(BOX_GET$Change,as.double)
BOX_GET <- filter(BOX_GET, Change != 0)



BOX_GET <- BOX_GET[8:260,]
rownames(BOX_GET) <- seq(length=nrow(BOX_GET))
BOX_GET <- BOX_GET[-c(200),]
rownames(BOX_GET) <- seq(length=nrow(BOX_GET))
BOX_GET$Week.Ending <- as.Date(43030-3,origin = "1900-01-01")

# Apply date format to data
counter_17 <- 43037
for (i in 1:43) {
  BOX_GET$Week.Ending[i] <- as.Date(counter_17-3,origin = "1900-01-01")
  counter_17 <- counter_17 - 7
}

counter_13 <- 41637
for (i in 44:95) {
  BOX_GET$Week.Ending[i] <- as.Date(counter_13-3,origin = "1900-01-01")
  counter_13 <- counter_13 - 7
}

counter_14 <- 42001
for (i in 96:147) {
  BOX_GET$Week.Ending[i] <- as.Date(counter_14-3,origin = "1900-01-01")
  counter_14 <- counter_14 - 7
}

counter_15 <- 42365
for (i in 148:199) {
  BOX_GET$Week.Ending[i] <- as.Date(counter_15-3,origin = "1900-01-01")
  counter_15 <- counter_15 - 7
}

counter_16 <- 42736
for (i in 200:252) {
  BOX_GET$Week.Ending[i] <- as.Date(counter_16-3,origin = "1900-01-01")
  counter_16 <- counter_16 - 7
}

# Merge
BOX_GET$Box.office <- str_replace(BOX_GET$Box.office,"\\$","")
BOX_GET$Box.office <- str_replace(BOX_GET$Box.office,",","")
BOX_GET$Box.office <- str_replace(BOX_GET$Box.office,",","")
BOX_GET <- BOX_GET[,c("Box.office","Week.Ending")]
SCORE_SALES_ALL <- left_join(SCORE_SALES_ALL,BOX_GET, by = c("Week.Ending"))

# Last minute adjustments
SALES_CONSOLE_GET <- SALES_CONSOLE_GET[,c("Platform","Total","Week.Ending")]
SALES_CONSOLE_GET$Total <- str_replace(SALES_CONSOLE_GET$Total,",","")
SALES_CONSOLE_GET$Total <- str_replace(SALES_CONSOLE_GET$Total,",","")
SALES_CONSOLE_GET$Platform <- str_replace(SALES_CONSOLE_GET$Platform,"XOne","XONE")
SALES_CONSOLE_GET$Platform <- str_replace(SALES_CONSOLE_GET$Platform,"PSV","VITA")
SALES_CONSOLE_GET$Platform <- str_replace(SALES_CONSOLE_GET$Platform,"WiiU","WIIU")
colnames(SALES_CONSOLE_GET)[colnames(SALES_CONSOLE_GET) == "Total"] <- "Console.Total"
SCORE_SALES_ALL <- left_join(SCORE_SALES_ALL,SALES_CONSOLE_GET, by = c("Platform","Week.Ending"))

# Assigning seasons based on date
SCORE_SALES_ALL <- SCORE_SALES_ALL[,c("Platform","Game","Developer","Publisher","Score","Number.of.Reviews","Genre","Weekly","Week.Ending","Box.office","Console.Total")]
SCORE_SALES_ALL$Winter <- 0
SCORE_SALES_ALL$Spring <- 0
SCORE_SALES_ALL$Summer <- 0
SCORE_SALES_ALL$Fall <- 0


for (i in 1:nrow(SCORE_SALES_ALL)) {
  tempo <- as.character(SCORE_SALES_ALL$Week.Ending[i])
  tempo <- str_extract(tempo,"-[0-9]*-")
  tempo <- str_replace(tempo,"-","")
  tempo <- str_replace(tempo,"-","")
  tempo <- as.integer(tempo)
  if (tempo == 12 | tempo == 1 | tempo == 2) {
    SCORE_SALES_ALL$Winter[i] <- 1
  } else if (tempo == 3 | tempo == 4 | tempo == 5) {
    SCORE_SALES_ALL$Spring[i] <- 1
  } else if (tempo == 6 | tempo == 7 | tempo == 8) {
    SCORE_SALES_ALL$Summer[i] <- 1
  } else if (tempo == 9 | tempo == 10 | tempo == 11) {
    SCORE_SALES_ALL$Fall[i] <- 1
  }
}

# Data analysis
SCORE_SALES_ALL[, 5:6] <- sapply(SCORE_SALES_ALL[, 5:6], as.double)
SCORE_SALES_ALL[, 10:11] <- sapply(SCORE_SALES_ALL[, 10:11], as.double)
out <- lm(Weekly~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
boxcox(out)

out <- lm(Weekly~Score+Number.of.Reviews+Box.office+Console.Total+Summer+Fall+Spring,data = SCORE_SALES_ALL)

out <- lm(Weekly~Score+Number.of.Reviews+Box.office+Console.Total+Fall+Spring,data = SCORE_SALES_ALL)

out <- lm(Weekly~Score+Number.of.Reviews+Box.office+Console.Total+Fall,data = SCORE_SALES_ALL)

out <- lm(Weekly~Score+Number.of.Reviews+Box.office+Fall,data = SCORE_SALES_ALL)

plot(x=SCORE_SALES_ALL$Score,y=SCORE_SALES_ALL$Weekly,xlab="Review Score",ylab="First Week Sales")
abline(lm(Weekly~Score,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Number.of.Reviews,y=SCORE_SALES_ALL$Weekly,xlab="Number of Reviews",ylab="First Week Sales")
abline(lm(Weekly~Number.of.Reviews,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Box.office,y=SCORE_SALES_ALL$Weekly,xlab="Box Office",ylab="First Week Sales")
abline(lm(Weekly~Box.office,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Fall,y=SCORE_SALES_ALL$Weekly,xlab="Fall Season",ylab="First Week Sales")
abline(lm(Weekly~Fall,data=SCORE_SALES_ALL))
# None of the plots were good because the data was not normally distributed. Examining the QQ plots showed that the data is actually exponential so use log transformation to normallize the data.

# 
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1553.905
out <- lm(log(Weekly)~Number.of.Reviews+Box.office+Console.Total+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1605.194
out <- lm(log(Weekly)~Score+Box.office+Console.Total+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1621.104
out <- lm(log(Weekly)~Score+Number.of.Reviews+Console.Total+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1557.563
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Winter+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1558.218
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Summer+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1553.905
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1553.905
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Summer+Spring,data = SCORE_SALES_ALL)
BIC(out) #1553.905
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Summer+Fall,data = SCORE_SALES_ALL)
BIC(out) #1553.905


out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1553.905
out <- lm(log(Weekly)~Number.of.Reviews+Box.office+Console.Total+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1605.194
out <- lm(log(Weekly)~Score+Box.office+Console.Total+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1621.104
out <- lm(log(Weekly)~Score+Number.of.Reviews+Console.Total+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1557.563
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Winter+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1558.218
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Fall+Spring,data = SCORE_SALES_ALL)
BIC(out) #1558.738
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Spring,data = SCORE_SALES_ALL)
BIC(out) #1564.116
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1551.071

# BIC can't get lower
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1551.071
out <- lm(log(Weekly)~Number.of.Reviews+Box.office+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1602.547
out <- lm(log(Weekly)~Score+Box.office+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1621.039
out <- lm(log(Weekly)~Score+Number.of.Reviews+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1553.957
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Winter+Fall,data = SCORE_SALES_ALL)
BIC(out) #1556.048
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Fall,data = SCORE_SALES_ALL)
BIC(out) #1552.598
out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter,data = SCORE_SALES_ALL)
BIC(out) #1558.593


out <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Winter+Fall,data = SCORE_SALES_ALL)
out1 <- lm(log(Weekly)~1,data=SCORE_SALES_ALL)
anova(out1,out)

out2 <- lm(log(Weekly)~Score+Number.of.Reviews+Box.office+Console.Total+Fall,data = SCORE_SALES_ALL)
anova(out2,out)

plot(x=SCORE_SALES_ALL$Score,y=log(SCORE_SALES_ALL$Weekly),xlab="Review Score",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Score,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Number.of.Reviews,y=log(SCORE_SALES_ALL$Weekly),xlab="Number of Reviews",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Number.of.Reviews,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Box.office,y=log(SCORE_SALES_ALL$Weekly),xlab="Box Office",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Box.office,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Fall,y=log(SCORE_SALES_ALL$Weekly),xlab="Fall Season",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Fall,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Console.Total,y=log(SCORE_SALES_ALL$Weekly),xlab="Console Total Sales",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Console.Total,data=SCORE_SALES_ALL))
plot(x=SCORE_SALES_ALL$Winter,y=log(SCORE_SALES_ALL$Weekly),xlab="Winter Season",ylab="Log First Week Sales")
abline(lm(log(Weekly)~Winter,data=SCORE_SALES_ALL))