library(rio)
library(dplyr)

dat<-
  import("Study 1/data/raw/PISA_2015_selected.sav")
cdat<-
  import("Study 1/data/processed/cdat_processed.xlsx")

table(unique(dat$CNT) %in% cdat$ISO3)
table(cdat$ISO3 %in% unique(dat$CNT))

table(unique(dat$CNTRYID) %in% cdat$ISOnum)
unique(dat$Region)

# check with the country what are not found

cdat[!(cdat$ISO3 %in% unique(dat$CNT)),"CLDR"]

unique(dat$CNT)

# these codings are used

#CHN=B-S-J-G (China)
#TWN=TAP
#PRI=QUD

cdat$ISO3

cdat$CNT<-case_when(
  cdat$ISO3=="CHN"~"QCH", #B-S-J-G (China)
  cdat$ISO3=="TWN"~"TAP", #Chinese Taipei (Taiwan)
  cdat$ISO3=="PRI"~"QUD", #Puerto Rico
  TRUE~cdat$ISO3)

table(cdat$CNT %in% unique(dat$CNT))

cdat[!(cdat$CNT %in% unique(dat$CNT)),"CLDR"]

# merge the data

table(dat$ST004D01T,useNA="always") #1 Female, 2 Male

fdat<-dat %>%
  filter(CNT %in% cdat$CNT) %>%
  select(CNT,Sex=ST004D01T,SCIEEFF,INTBRSCI,JOYSCIE)


fdat<-left_join(
  x=fdat,
  y=cdat,
  by="CNT")

str(fdat)

# this strips all the attributes

fdat<- fdat %>% mutate(across(everything(), as.vector))

export(fdat,"Study 1/data/processed/fdat.xlsx")
