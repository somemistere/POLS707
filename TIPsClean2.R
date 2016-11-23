## load in and do some manipulations to the State Department Human Trafficking tier rating system.



getwd()

rm(list =ls())

setwd("C:/Users/Marcy/Dropbox/POLS 707/Final Project")


##is this package here??
if(!"pscl" %in% installed.packages()) install.packages("pscl", dependencies= TRUE)



##import tier data
dat <- read.csv("TierClean5.csv", header = TRUE) 
total <- read.csv("threeptotal3.csv", header = TRUE) 

##view head of data 
head(total)

##create tier as number 
dat$TierNum <- as.character(dat$Tier)
dat$TierNum <- substr(dat$TierNum, 1, 1)
dat$TierNum <- substr(dat$Tiernum, 2, 2)
dat$tierNum <- substr(dat$Tier, 3, 4)
dat$TierNum <- ifelse(dat$TierNum %in% "S", 0, dat$TierNum)

total$TierNum <- ifelse(total$TierNum %in% "S", NA, total$TierNum)


dat$TierNum <- as.numeric(dat$TierNum)
dat$TierNum[grepl("W", dat$Tier)] <- 3
dat$TierNum[grepl("3", dat$Tier)] <-4
table(dat$Tier, dat$TierNum, useNA = "always")

## Clean Country Names
dat$CountryFix <- gsub("\\s", "", dat$Country)
dat$CountryFix <- gsub("[2]", "", dat$TrimFix)
dat$CountryFix <- gsub("\302\240", "", dat$TrimFix)
dat$CountryFix <- gsub("\342\200\231", "", dat$TrimFix)
dat$CountryFix <- gsub("[/]|[&]|[*]|[.]", "", dat$TrimFix)

##fix country names

dat$CountryFix[grepl("china", dat$CountryFix, ignore.case = T)] <- "China"
dat$CountryFix <- gsub("Columbia", "Colombia", dat$CountryFix)
dat$CountryFix[grepl("bosniaH", dat$CountryFix, ignore.case = T)] <- "BosniaH"
dat$Country[grepl("Congo", dat$CountryFix) & grepl("Dem", dat$CountryFix)] <- "CongoDR"
dat$CountryFix[grepl("Cote", dat$CountryFix, T)] <- "CotedIvoire"
dat$CountryFix[grepl("Kyrgyz", dat$CountryFix, T)] <- "Kyrgyzstan"
dat$CountryFix[grepl("Curac", dat$CountryFix, T)] <- "Curacao"
dat$CountryFix[grepl("Dominican", dat$CountryFix, T)] <- "DominicanRepublic"
dat$CountryFix[grepl("bouti", dat$CountryFix, T)] <- "Djibouti"
dat$CountryFix[grepl("rundi", dat$CountryFix, T)] <- "Burundi"
dat$CountryFix[grepl("newz", dat$CountryFix, T)] <- "NewZealand"
dat$CountryFix[grepl("slovak", dat$CountryFix, T)] <- "Slovakia"
dat$CountryFix[grepl("Bahama", dat$CountryFix, T)] <- "TheBahamas"
dat$CountryFix[grepl("camer", dat$CountryFix, T)] <- "Cameroon" 
dat$Country[grepl("camer", dat$CountryFix, T)] <- "Cameroon" 
dat$Country[grepl("Congo,Dem", dat$CountryFix), T] <- "Congo, Democratic Rep. Of"


## Sort data appropriately
dat <- dat[order(dat$Year), ]
dat <- dat[order(dat$ccow), ]

##change the shit to ISO3 and Cow codes

install.packages("countrycode")
library("countrycode")
?countrycode

##cow codr
Library

dat$ccow <- countrycode(dat$CountryFix, "country.name", "cown")
dat$ccow[grepl("CentralAfr", dat$CountryFix, T)] <- "482" 
dat$ccow[grepl("Yugos", dat$CountryFix, T)] <- "345" 
dat$ccow[grepl("Serbia&", dat$CountryFix, T)] <- "345" 
dat$ccow[grepl("Serbia", dat$CountryFix, T)] <- "345" 

##rename variable column
install.packages("reshape")
library("reshape")
dat <- rename(dat, c(ccow="ccode2"))
dat <- rename(dat, c(Year="year"))

##iso3 code

total$iso3 <- countrycode(total$CountryFix, "country.name", "iso3n")

iso3codes <- table(total$iso3)
iso3codes <- rename(iso3codes, c(.id="id"))



ls(total$iso3)
dat$ISO3[grepl("CentralAfr", dat$CountryFix, T)] <- "140" 
dat$ISO3[grepl("Yugos", dat$CountryFix, T)] <- "891" 
dat$ISO3[grepl("Serbia&", dat$CountryFix, T)] <- "891" 
dat$ISO3[grepl("CentralAf", dat$CountryFix, T)] <- "140" 
dat$ISO3[grepl("koso", dat$CountryFix, T)] <- "901" 
dat$ISO3[grepl("Neth.Antilles", dat$CountryFix, T)] <- "530" 

##delete countries with no cow code
dat <- dat[!is.na(dat$ccow),]

##check for duplications
duplicated(dat)
dat[!duplicated(dat[c("dat$Year","dat$ISO3")]), ]


##write CSV
write.csv(dat, "TierClean4.csv", row.names=FALSE)


##import UN voting data
library(foreign)
voting <- read.dta("C:/Users/Marcy/Dropbox/POLS 707/Final Project/dyadicdata.dta")


##sort UN voting data appropriately
voting <- voting[order(voting$year), ]
voting <- voting[order(voting$ccode1), ]
voting <- voting[order(voting$ccode2), ]

##delete countries with no cow code
dat <- dat[!is.na(dat$ccow),]

##write CSV
write.csv(voting, "voting5.csv", row.names=FALSE)

##separate data to get ratings for US only
voting2 <- subset(voting, ccode1 == 2)

##separate data to get rating for 2001-2014
voting3 <- subset(voting2, year == 2001 
                  | year == 2002
                  | year == 2003
                  | year == 2004
                  | year == 2005
                  | year == 2006
                  | year == 2007
                  | year == 2008
                  | year == 2009
                  | year == 2010
                  | year == 2011
                  | year == 2012
                  | year == 2013
                  | year == 2014)
                  

# merge two data frames by ID and Country
total <- merge(dat,voting3,by=c("ccode2","year")) 



##import 3p data
threep <- read.csv("C:/Users/Marcy/Dropbox/POLS 707/Final Project/3Pindex.csv")


##cow code 3p Data
threep$ccode2 <- countrycode(threep$Country, "country.name", "cown")
threep$ccode2[grepl("Phillipines", threep$Country, T)] <- "840" 
threep$ccode2[grepl("Serbia", threep$Country, T)] <- "345" 

##merge total by threep
total3 <- merge(threep,total,by=c("ccode2","year")) 

##delete countries with no cow code
threep <- threep[!is.na(threep$ccode2),]

##sort
threep <- threep[order(threep$ccode2), ]



###models

##write CSV
write.csv(total3, "threeptotal3.csv", row.names=FALSE)
write.csv(iso3codes, "iso3codes.csv", row.names=FALSE)

##import iso3 data
iso3codes <- read.csv("C:/Users/Marcy/Dropbox/POLS 707/Final Project/iso3codes.csv")

