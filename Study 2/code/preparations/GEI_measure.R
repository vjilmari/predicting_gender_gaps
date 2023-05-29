# obtaining the different indices used summarized as GEI,
# running the PCA, and obtaining predicted values

# these can be found in the supplementary materials of Falk & Hermle, 2018

library(rio)
library(dplyr)


# GGGI: Global Gender Gap Index

GGGI<-
  import("Study 2/data/Processed/GGGI_2015_processed.xlsx")
str(GGGI)

# GII (Gender Inequality Index)

GII<-
  import("Study 2/data/Processed/GII_2015_processed.xlsx")

str(GII)

# exclude unnecessary information

GII<-GII[,c("ISO3","GII")]

# Labor force participation

LFP<-
  import("Study 2/data/Processed/LFP_2003_2012_processed.xlsx")
str(LFP)

# exclude unneccessary information

LFP<-LFP %>%
  dplyr::select(ISO3="Country Code",LFP)
LFP

# Time since women's suffrage = TSWS

Suffrage<-
  import("Study 2/data/Processed/Suffrage_year.xlsx")
names(Suffrage)

# Calculate from suffrage year, code 0 if not suffrage as of yet
Suffrage$TSWS<-ifelse(Suffrage$Suffrage_year=="not yet",0,
                  2012-as.numeric(Suffrage$Suffrage_year))


Suffrage<-Suffrage[,c("ISO3","TSWS")]
Suffrage

# check how well the ISOCODES match with the specific countries in the sample

dat<-import("Study 2/data/Raw/individual_new.dta")
countries<-unique(dat$isocode)

# GGGI and data

table(countries %in% GGGI$ISO3,useNA="always")

# what is missing

countries[!(countries %in% GGGI$ISO3)]

# there actually was no score for afghanistan in 2015 (0.444)
# but there is one for 2021 https://www3.weforum.org/docs/WEF_GGGR_2021.pdf

# GII and data

table(countries %in% GII$ISO3,useNA="always")

# what is missing

countries[!(countries %in% GII$ISO3)]

# There are no scores for nigeria

# LFP and data

table(countries %in% LFP$ISO3,useNA="always")

# what is missing

countries[!(countries %in% LFP$ISO3)]

# Suffrage and data

table(countries %in% Suffrage$ISO3,useNA="always")

# what is missing

countries[!(countries %in% LFP$ISO3)]

# Combine data

GEI.dat<-data.frame(ISO3=countries)

# left join all the other variables

GEI.dat<-
  left_join(GEI.dat,
            GGGI,
            by="ISO3")

GEI.dat<-
  left_join(GEI.dat,
            GII,
            by="ISO3")

GEI.dat<-
  left_join(GEI.dat,
            LFP,
            by="ISO3")

GEI.dat<-
  left_join(GEI.dat,
            Suffrage,
            by="ISO3")

names(GEI.dat)

# check intercorrelations

cor(GEI.dat[,2:5],use = "pairwise.complete.obs")

# exclude missing for PCA

GEI.fdat<-na.omit(GEI.dat)

# run PCA


library(psych)
prp<-principal(GEI.fdat[,2:5],nfactors = 1,scores = T)
prp
GEI.fdat.scores<-cbind(GEI.fdat,GEI=prp$scores)

#only include the PCA in the data frame
GEI.fdat.scores<-GEI.fdat.scores[,c("ISO3","PC1")]

# join with the frame with missing values

GEI<-left_join(
  x=GEI.dat,
  y=GEI.fdat.scores,
  by=c("ISO3")
)
names(GEI)<-c(names(GEI)[1:5],"GEI")

head(GEI)

# save to a file
export(GEI,"Study 2/data/Processed/GEI.xlsx",
       overwrite=T)

