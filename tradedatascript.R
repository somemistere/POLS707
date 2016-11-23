rm(list =ls())


data1 <- read.table("data1.csv", header = T, sep = ",")

data2 <- read.table("data2.csv", header = T, sep = ",")

allthedata <- rbind(data1, data2

##Package to load trade data from UN COMTRADE API
library(tools)
md5sum("comtrade.tar.gz")
library(comtrade)


s2 <- get.Comtrade(r="842", p="124", ps="2013", 
             fmt="csv")

###convert into data frame ##something like this

df <- do.call(what = "rbind",
                           args = lapply(s2, as.data.frame))



##another way to skin a cat
library(plyr)
iso3codes <- ldply (iso3codes, data.frame)



# LIST OF COMTRADE DATA (LENGTH EQUAL TO INPUT LIST)

comtradedata

comtrade_data <- lapply(iso3codes$id, function(id) {
  Sys.sleep(1)
  tmp <- get.Comtrade(r="842", p=id, ps="2015", 
                         fmt="csv")
  df <- as.data.frame(do.call(rbind, tmp))
  return(df)
})

# NAME EACH ELEMNT OF NEW LIST BY PREFIXING "q"
comtrade_data <- setNames(comtrade_data, paste0("df_q", ls_reporters))

# CREATE df_q1, df_q2, df_q3, ... AS INDIVIDUAL OBJECTS
list2env(comtrade_data, envir=.GlobalEnv)

# EVEN ROW BIND TO DATAFRAME (IF ELEMENTS HAVE SAME EXACT ONE-LEVEL DATA STRUCTURE)
df <- do.call(rbind, comtrade_data)




####          ,"max=",maxrec,"&" #maximum no. of records returned
               ,"type=",type,"&" #type of trade (c=commodities)
               ,"freq=",freq,"&" #frequency
               ,"px=",px,"&" #classification
               ,"ps=",ps,"&" #time period
               ,"r=",r,"&" #reporting area
               ,"p=",p,"&" #partner country
               ,"rg=",rg,"&" #trade flow
               ,"cc=",cc,"&" #classification code
               ,"fmt=",fmt        #Format
               ,sep = "" ####
