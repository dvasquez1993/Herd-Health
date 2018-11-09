library(tidyverse)
library(magrittr)

h <- read_csv("Herd Health Disease data.csv")
d <- read_csv("deer_density.csv")

# backfill FIPS code in h using County/Parish and State combo
# correct known spelling mistakes
idx.Choctow <- which(h$`County/Parish`=="Choctow")
h$`County/Parish`[idx.Choctow] <- "Choctaw"
idx.Clairborne <- which(h$`County/Parish`=="Clairborne") 
h$`County/Parish`[idx.Clairborne] <- "Claiborne"

h %<>% mutate(cps=paste(`County/Parish`,State,sep="."),locDate=paste(Location,Date,sep="."))

# remove records where county was not recorded (about 300 out of 15000)
h %<>% drop_na(`County/Parish`)

f <- h %>% select(`FIPS Code`,cps) %>% drop_na() %>% distinct() 
f2 <- h%>% select(`FIPS Code`,locDate) %>% drop_na() %>% distinct()
# internal match: use h data itself to fill in FIPS blanks

# locate FIPS=NA in h & match based on cps
library(stringr)
idx.NA <- which(is.na(h$`FIPS Code`)==T)
for (i in idx.NA){
  newFIPS <- f$`FIPS Code`[which(f$cps==h$cps[i])]
  if (length(newFIPS)>1){newFIPS <- newFIPS[which(str_length(newFIPS)==min(str_length(newFIPS)))]}
  if (length(newFIPS)>0){h$`FIPS Code`[i] <- newFIPS}
}

# locate FIPS=NA in h & match based on locDate
idx.NA <- which(is.na(h$`FIPS Code`)==T)
for (i in idx.NA){
  if (grepl("^NA\\.",h$locDate[i]==F)){newFIPS <- f2$`FIPS Code`[which(f2$locDate==h$locDate[i])]}
  if (length(newFIPS)>0){h$`FIPS Code`[i] <- newFIPS}
  print(c(i,length(newFIPS)))
}

h %<>% drop_na(Date)

# external match: use d data to fill in FIPS blanks in h
# first correct mistakes
h$State[which(h$State==1)]<-"GA"

f3 <- d %>% select(FIPS,StateName,CntyName)
f3 %<>% mutate(cs=paste(CntyName,StateName,sep=".")) 

idx.NA <- which(is.na(h$`FIPS Code`)==T)

s <- data.frame(cbind(c("AL","FL","GA","KS","MS","NC","TX","VA"),c("Alabama","Florida","Georgia","Kansas","Mississippi","North Carolina","Texas","Virginia")))
names(s) <- c("code","name")

for (i in idx.NA){
  d2 <- d %>% filter(StateName==s$name[which(s$code==h$State[i])])
  if (grepl("/",h$`County/Parish`[i])==T){
    multCounties <- unlist(str_split(h$`County/Parish`[i],"/"))
    multFIPS <- NULL
    for (j in multCounties){
      multFIPS <- as.character(c(multFIPS,d2$FIPS[which(grepl(j,d2$CntyName))]))
    }
    FIPS <- paste(multFIPS,collapse="/")
  }
  else{
    FIPS <- as.character(d2$FIPS[which(grepl(h$`County/Parish`[i],d2$CntyName))])
  }
  if (length(FIPS)==0){FIPS <- "PROBLEM"}
  h$`FIPS Code`[i] <- FIPS
}

#alter FIPS-erroneous records
h$`FIPS Code`[which(h$`FIPS Code`=="29091v")] <- "29091"
h$`FIPS Code`[which(h$`FIPS Code`=="2206522107")] <- "22065/22107"
h$`FIPS Code`[which(h$`FIPS Code`=="2404329001")] <- "24043"
h$`FIPS Code`[which(h$`FIPS Code`=="13119\r\r13119")] <- "13119"
#remove garbage records
h %<>% mutate(lenFIPS=str_length(h$`FIPS Code`))
h %<>% filter(lenFIPS!=3)
h %<>% filter(lenFIPS!=9)
h %<>% filter(`FIPS Code`!="4.69581621")

h %<>% mutate(deerDensMax=-1,deerDensMode=-1)

#need to pad 4-digit FIPS in d with leading 0
d %<>% mutate(FIPS=as.character(FIPS))
for (i in 1:dim(d)[1]){
  if (str_length(d$FIPS[i])==4){
    d$FIPS[i]<-paste("0",d$FIPS[i],sep="")
  }  
}

for (i in 1:dim(h)[1]){
  if (h$lenFIPS[i]==5){
    h$deerDensMax[i] <- d$DeerDensityMax[which(d$FIPS==h$`FIPS Code`[i])]    
    h$deerDensMode[i] <- d$DeerDensityMode[which(d$FIPS==h$`FIPS Code`[i])] 
  }
  else{
    theseFIPS <- unlist(strsplit(h$`FIPS Code`[i],split="/"))
    theseMax <- d %>% filter(FIPS %in% theseFIPS) %>% pull(DeerDensityMax)
    thisMax <- max(theseMax)
    theseMode <- d %>% filter(FIPS %in% theseFIPS) %>% pull(DeerDensityMode)
    thisMode <- max(theseMode) #i'm assuming underestimating deer dens more likely than overestimating (could be calc'd other ways)
    h$deerDensMax[i] <- thisMax  
    h$deerDensMode[i] <- thisMode 
  }
}

h %<>% select(-cps,-locDate,-lenFIPS)

write.csv(h, file = "DensityandParasitesPerRecord.csv")
