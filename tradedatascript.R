rm(list =ls())




##Package to load trade data from UN COMTRADE API
library(tools)
md5sum("comtrade.tar.gz")
library(comtrade)

##or here is the raw code for communication
get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}

##basic search query
s2 <- get.Comtrade(r="842", p="124", ps="2013", 
             fmt="csv")

###convert into data frame ##something like this

df <- do.call(what = "rbind",
                           args = lapply(s2, as.data.frame))



##another way to skin a cat
library(plyr)
iso3codes <- ldply (iso3codes, data.frame)



# comtrade search loop taken from http://stackoverflow.com/questions/39182616/r-looping-through-list-and-passing-values-to-api-of-un-comtrade-programming
# iso3codes are list of codes needed, but I think it might be limiting it to one observation per country which is not 
                    #what I want it to do

      

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




