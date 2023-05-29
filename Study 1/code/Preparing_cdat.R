# Preparing the country-level data available in S&G (2018) SOM

library(rio)
library(dplyr)

cdat<-import("Study 1/data/raw/cdat.xlsx")

str(cdat)

# convert GGGI to numeric
cdat$GGGI_2015<-as.numeric(cdat$GGGI_2015)

# convert women_in_stem_per to numeric
cdat$women_in_stem_per<-as.numeric(cdat$women_in_stem_per)

# convert LS to numeric
cdat$LS<-as.numeric(cdat$LS)

cdat<-cdat[,c("cntry_region","GGGI_2015","women_in_stem_per","LS")]

# get ISO file

ISO<-
  import("Study 1/data/Raw/ISO.xlsx")
names(ISO)

table(cdat$cntry_region %in% ISO$Country_eng_official)
table(cdat$cntry_region %in% ISO$Country_eng_short)
table(cdat$cntry_region %in% ISO$CLDR)

# CLDR is the best, see which countries do not match

cdat[!(cdat$cntry_region %in% ISO$CLDR),"cntry_region"]

# recode unmatched countries

cdat$CLDR<-case_when(
  cdat$cntry_region=="B-S-J-G (China)"~"China",
  cdat$cntry_region=="Chinese Taipei"~"Taiwan",
  cdat$cntry_region=="Czech Republic"~"Czechia",
  cdat$cntry_region=="Korea"~"South Korea",
  cdat$cntry_region=="Latvia (LSS)"~"Latvia",
  cdat$cntry_region=="Macao"~"Macau",
  cdat$cntry_region=="Macedonia"~"North Macedonia",
  cdat$cntry_region=="Puerto Rico (USA)"~"Puerto Rico",
  cdat$cntry_region=="Russian Federation"~"Russia",
  cdat$cntry_region=="Slovak Republic"~"Slovakia",
  cdat$cntry_region=="Trinidad and Tobago"~"Trinidad & Tobago",
  cdat$cntry_region=="United Kingdom"~"UK",
  cdat$cntry_region=="United States"~"US",
  TRUE~cdat$cntry_region
)

# merge ISO and cdat

cdat<-left_join(
  x=cdat,
  y=ISO,
  by="CLDR")

head(cdat)

export(cdat,
       "Study 1/data/processed/cdat_processed.xlsx")
