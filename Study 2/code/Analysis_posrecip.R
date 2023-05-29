#' ---
#' title: "Predicting cross-country differences in economic preferences for posrecip between men and women with country-level gender equality."
#' output:
#'   html_document: 
#'     df_print: default
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Load packages
#' 
## ----message=FALSE, warning=FALSE-----------------------------------------------------
library(multid)
library(lmerTest)
library(rio)
library(dplyr)
library(tibble)
library(ggpubr)
library(ggplot2)
library(MetBrewer)
library(emmeans)
library(finalfit)
source("../../custom_functions.R")


#' 
#' ## Import data
#' 
#' Data (originally used in Falk & Hermle, 2018) is openly available at www.briq-institute.org/global-preferences/home
#' 
## -------------------------------------------------------------------------------------

dat.ex2 <- import("../data/Raw/individual_new.dta")


#' 
#' Import also pre-calculated indices of gender equality index (GEI)
#' 
## -------------------------------------------------------------------------------------
GEI <- 
  import("../data/Processed/GEI.xlsx")

#' 
#' ## Compile country-level data
#' 
## -------------------------------------------------------------------------------------

men<-dat.ex2 %>%
  group_by(isocode) %>%
  filter(gender==0) %>%
  summarise(posrecip.men.mean=mean(posrecip,na.rm=T),
            posrecip.men.sd=sd(posrecip,na.rm=T),
            n.men=n())

women<-dat.ex2 %>%
  group_by(isocode) %>%
  filter(gender==1) %>%
  summarise(posrecip.women.mean=mean(posrecip,na.rm=T),
            posrecip.women.sd=sd(posrecip,na.rm=T),
            n.women=n())


#' 
#' Combine these with each other and with country-level GEI
#' 
## -------------------------------------------------------------------------------------
country.dat.ex2<-left_join(
  x=men,
  y=women,
  by="isocode")

country.dat.ex2<-
  left_join(country.dat.ex2,
            GEI,
            by=c("isocode"="ISO3"))


#' 
#' Use the regression method, process:
#' 
#' 1.  Standardize within-country
#' 
#' 2.  Run regression on the preference while controlling for age, age\^2, subj_math_skills
#' 
#' 3.  Obtain the coefficients for gender as sex difference for each country
#' 
## -------------------------------------------------------------------------------------

countries<-unique(dat.ex2$isocode)

reg.diff.list<-list()

for (i in 1:length(countries)){
  
  temp.dat.ex2<-dat.ex2[dat.ex2$isocode==countries[i],]
  
  # standardize
  
  temp.dat.ex2$posrecip<-
    (temp.dat.ex2$posrecip-mean(temp.dat.ex2$posrecip,na.rm=T))/
    sd(temp.dat.ex2$posrecip,na.rm=T)
  
  # run the model
  
  temp.posrecip.diff<-
    coefficients(lm(posrecip~gender+age+I(age^2)+subj_math_skills,
                    data=temp.dat.ex2))["gender"]
  
  reg.diff.list[[i]]<-
    cbind.data.frame(
    isocode=countries[i],
    posrecip.reg.diff=temp.posrecip.diff)
  
}

reg.diff<-do.call(rbind,reg.diff.list)
rownames(reg.diff)<-NULL
head(reg.diff)

# merge to the country data
country.dat.ex2<-
  left_join(
    x=country.dat.ex2,
    y=reg.diff,
    by="isocode")

# combine to multi-level data

dat.ex2<-
  left_join(dat.ex2,
            country.dat.ex2,
            by=c("isocode"))

# recode sex variable
dat.ex2$sex.c<-(-1)*(dat.ex2$gender-0.5)


#' 
#' ## Data exclusions and transformations
#' 
## -------------------------------------------------------------------------------------
fdat<-dat.ex2 %>%
  dplyr::select(posrecip,sex.c,country,GEI,age,subj_math_skills) %>%
  na.omit() %>%
  mutate(age_sq=age^2)

#' 
#' # Analysis
#' 
#' ## Reliability of the difference score
#' 
## -------------------------------------------------------------------------------------

reliab.posrecip<-
  reliability_dms(
    data=fdat,
    diff_var="sex.c",var = "posrecip",
    diff_var_values = c(0.5,-0.5),
    group_var = "country")


export(t(data.frame(reliab.posrecip)),
       "../results/reliab.posrecip.xlsx",
       overwrite=T)
reliab.posrecip


#' 
#' ## Multi-level model
#' 
#' ### Fit model
#' 
## ----message=FALSE--------------------------------------------------------------------

fit_posrecip<-
  ddsc_ml(data = fdat,predictor = "GEI",
          covariates=c("age","age_sq","subj_math_skills"),
          moderator = "sex.c",moderator_values=c(0.5,-0.5),
          DV = "posrecip",lvl2_unit = "country",re_cov_test = T,
          scaling_sd = "observed")

#' 
#' ### Descriptive statistics
#' 
## -------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_posrecip$descriptives)),
       "../results/posrecip_ml_desc.xlsx",
       overwrite=T)
round(fit_posrecip$descriptives,2)
round(fit_posrecip$SDs,2)

#' 
#' ### Variance heterogeneity test
#' 
## -------------------------------------------------------------------------------------
export(t(data.frame(fit_posrecip$re_cov_test)),
       "../results/posrecip_ml_var_test.xlsx",
       overwrite=T)
round(fit_posrecip$re_cov_test,3)

#' 
#' ### Component correlation
#' 
## -------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_posrecip$ddsc_sem_fit$variance_test)),
       "../results/posrecip_ml_comp_cor.xlsx",
       overwrite=T)
round(fit_posrecip$ddsc_sem_fit$variance_test,3)

#' 
#' ### Deconstructing results
#' 
## -------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_posrecip$results)),
       "../results/posrecip_ml_results.xlsx",
       overwrite=T)
round(fit_posrecip$results,3)

#' 
#' ### Multi-level model output
#' 
## -------------------------------------------------------------------------------------
# cross-level interaction model
summary(fit_posrecip$model)

# reduced model without the predictor
summary(fit_posrecip$reduced_model)

#' 
#' ## Country-level path model
#' 
#' ### Fit the model
#' 
#' The model is already stored within the multi-level model object.
#' 
## -------------------------------------------------------------------------------------
fit_posrecip_sem<-fit_posrecip$ddsc_sem_fit

#' 
#' ### Results
#' 
## -------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_posrecip_sem$results)),
       "../results/posrecip_sem_results.xlsx",
       overwrite=T)
round(fit_posrecip_sem$results,3)

#' 
#' # Plotting the results
#' 
## -------------------------------------------------------------------------------------
# start with obtaining predicted values for means and differences

ml_posrecip<-fit_posrecip$model
ml_posrecip_red<-fit_posrecip$reduced_model
  


# point predictions as function of GEI for components

p<-
  emmip(
    ml_posrecip, 
    sex.c ~ GEI,
    at=list(sex.c = c(-0.5,0.5),
            age=mean(fdat$age,na.rm=T),
            subj_math_skills=mean(fdat$subj_math_skills,na.rm=T),
            GEI=
              seq(from=round(range(fdat$GEI,na.rm=T)[1],2),
                  to=round(range(fdat$GEI,na.rm=T)[2],2),
                  by=0.01)),
    plotit=F,CIs=T,lmerTest.limit = 1e6,disable.pbkrtest=T)

p$sex<-p$tvar
levels(p$sex)<-c("Women","Men")

# obtain min and max for aligned plots
min.y.comp<-min(p$LCL)
max.y.comp<-max(p$UCL)

# Men and Women mean distributions

p3<-coefficients(ml_posrecip_red)$country
p3<-cbind(rbind(p3,p3),weight=rep(c(-0.5,0.5),each=nrow(p3)))

p3$xvar<-p3$`(Intercept)`+
  p3$age*mean(fdat$age,na.rm=T)+
  p3$age_sq*(mean(fdat$age,na.rm=T)^2)+
  p3$subj_math_skills*mean(fdat$subj_math_skills,na.rm=T)+
  p3$sex.c*p3$weight
p3$sex<-as.factor(p3$weight)
levels(p3$sex)<-c("Women","Men")

# obtain min and max for aligned plots
min.y.mean.distr<-min(p3$xvar)
max.y.mean.distr<-max(p3$xvar)


# obtain the coefs for the sex-effect (difference) as function of GGGI

p2<-data.frame(
  emtrends(ml_posrecip,var="+1*sex.c",
           specs="GEI",
           at=list(#Sex = c(-0.5,0.5),
             age=mean(fdat$age,na.rm=T),
             subj_math_skills=mean(fdat$subj_math_skills,na.rm=T),
             GEI=
               seq(from=round(range(fdat$GEI,na.rm=T)[1],2),
                   to=round(range(fdat$GEI,na.rm=T)[2],2),
                   by=0.01)),
           lmerTest.limit = 1e6,disable.pbkrtest=T))

p2$yvar<-p2$X.1.sex.c.trend
p2$xvar<-p2$GEI
p2$LCL<-p2$lower.CL
p2$UCL<-p2$upper.CL

# obtain min and max for aligned plots
min.y.diff<-min(p2$LCL)
max.y.diff<-max(p2$UCL)

# difference score distribution

p4<-coefficients(ml_posrecip_red)$country
p4$xvar=(+1)*p4$sex.c

# obtain mix and max for aligned plots

min.y.diff.distr<-min(p4$xvar)
max.y.diff.distr<-max(p4$xvar)

# define mins and maxs

min.y.pred<-
  ifelse(min.y.comp<min.y.mean.distr,min.y.comp,min.y.mean.distr)

max.y.pred<-
  ifelse(max.y.comp>max.y.mean.distr,max.y.comp,max.y.mean.distr)

min.y.narrow<-
  ifelse(min.y.diff<min.y.diff.distr,min.y.diff,min.y.diff.distr)

max.y.narrow<-
  ifelse(max.y.diff>max.y.diff.distr,max.y.diff,max.y.diff.distr)

# Figures 

# p1

# scaled simple effects to the plot

pvals<-p_coding(c(fit_posrecip$results["b_21","p.value"],
                    fit_posrecip$results["b_11","p.value"]))

ests<-
  round_tidy(c(fit_posrecip$results["b_21","estimate"],
               fit_posrecip$results["b_11","estimate"]),2)

coef1<-paste0("b21 = ",ests[1],", p = ",pvals[1])
coef2<-paste0("b11 = ",ests[2],", p = ",pvals[2])

coef_q<-round_tidy(fit_posrecip$results["q_b11_b21","estimate"],2)
coef_q<-paste0("q_b = ",coef_q,", p ",
               ifelse(fit_posrecip$results["interaction","p.value"]<.001,"","="),
               p_coding(fit_posrecip$results["interaction","p.value"]))

coefs<-data.frame(sex=c("Women","Men"),
                  coef=c(coef1,coef2))


p1.posrecip.GEI<-ggplot(p,aes(y=yvar,x=xvar,color=sex))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Gender Equality Index")+
  #ylim=c(2.3,3.9)+
  ylim(c(min.y.pred,max.y.pred))+
  ylab("Positive reciprocity")+
  scale_color_manual(values=met.brewer("Archambault")[c(6,2)])+
  theme(legend.position = "top",
        legend.title=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ),
        panel.grid.major.x = element_line(size = 0.5, linetype = 2,
                                          colour = "gray"))+
  geom_text(data = coefs,show.legend=F,
            aes(label=coef,x=-1.50,
                y=c(round(min(p$LCL),2)+0.10-0.05
                    ,round(min(p$LCL),2)-0.05),size=14,hjust="left"))+
  geom_text(inherit.aes=F,aes(x=-1.50,y=round(min(p$LCL),2)-0.15,
                              label=coef_q,size=14,hjust="left"),
            show.legend=F)
p1.posrecip.GEI

# prediction plot for difference score


pvals2<-p_coding(fit_posrecip$results["r_xy1y2","p.value"])

ests2<-
  round_tidy(fit_posrecip$results["r_xy1y2","estimate"],2)

coefs2<-paste0("r = ",ests2,
               ", p ",
               ifelse(fit_posrecip$results["r_xy1y2","p.value"]<.001,"","="),
               pvals2)


p2.posrecip.GEI<-ggplot(p2,aes(y=yvar,x=xvar))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Gender Equality Index")+
  ylim(c(min.y.narrow,max.y.narrow))+
  ylab("Difference in Positive reciprocity")+
  #scale_color_manual(values=met.brewer("Archambault")[c(6,2)])+
  theme(legend.position = "right",
        legend.title=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ),
        panel.grid.major.x = element_line(size = 0.5, linetype = 2,
                                          colour = "gray"))+
  #geom_text(coef2,aes(x=0.63,y=min(p2$LCL)))
  geom_text(data = data.frame(coefs2),show.legend=F,
            aes(label=coefs2,x=-1.50,hjust="left",
                y=-0.20,size=14))
p2.posrecip.GEI

# mean-level distributions

p3.posrecip.GEI<-
  ggplot(p3, aes(x=xvar, fill=sex)) + 
  geom_density(alpha=.75) + 
  scale_fill_manual(values=met.brewer("Archambault")[c(6,2)])+
  #scale_fill_manual(values=c("turquoise3","orangered2","black")) + 
  xlab("")+
  ylab("Density")+
  ylim(c(0,8))+
  xlim(c(min.y.pred,max.y.pred))+
  theme_bw()+
  theme(legend.position = "top",
        legend.title=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ),
        panel.grid.major.x = element_line(size = 0.5, linetype = 2,
                                          colour = "gray"))+
  coord_flip()
p3.posrecip.GEI

# distribution for mean differences

p4.posrecip.GEI<-
  ggplot(p4, aes(x=xvar,fill="black")) + 
  geom_density(alpha=.75) + 
  scale_fill_manual(values="black")+
  #scale_fill_manual(values=c("turquoise3","orangered2","black")) + 
  xlab("")+
  ylab("Density")+
  ylim(c(0,8))+
  xlim(c(min.y.narrow,max.y.narrow))+
  theme_bw()+
  theme(legend.position = "none",
        legend.title=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ),
        panel.grid.major.x = element_line(size = 0.5, linetype = 2,
                                          colour = "gray"))+
  coord_flip()
p4.posrecip.GEI

# combine component-specific predictions

p13.posrecip.GEI<-
  ggarrange(p1.posrecip.GEI, p3.posrecip.GEI,common.legend = T,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p13.posrecip.GEI

# combine difference score predictions

p24.posrecip.GEI<-
  ggarrange(p2.posrecip.GEI, p4.posrecip.GEI,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p24.posrecip.GEI


pall.posrecip.GEI<-
  ggarrange(p13.posrecip.GEI,p24.posrecip.GEI,align = "hv",
            ncol=1,nrow=2,heights=c(2,1))
pall.posrecip.GEI

png(filename = 
      "../results/pall.posrecip.GEI.png",
    units = "cm",
    width = 21.0,height=29.7*(4/5),res = 600)
pall.posrecip.GEI
dev.off()

#' 
#' # Session information
#' 
## -------------------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

