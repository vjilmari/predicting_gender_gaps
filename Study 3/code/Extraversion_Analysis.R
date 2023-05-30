#' ---
#' title: "Extraversion"
#' output:
#'   html_document: 
#'     df_print: default
#'     toc: yes
#'     number_sections: yes
#'     keep_md: yes
#' date: "`r Sys.Date()`"
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Preparations
#' 
#' ## Load packages
#' 
## ----message=FALSE-------------------------------------------------------------------------
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
#' 
## ------------------------------------------------------------------------------------------
dat <- import("../data/ipip_processed.rda")
correlates <- import("../data/correlates.xlsx")

#' 
#' 
#' 
#' ## Save variable names of the multivariate set to a vector
#' 
## ------------------------------------------------------------------------------------------
per.facets<-
  names(dat)[which(names(dat)=="A.trust"):
              which(names(dat)=="O.liberalism")]

#' 
#' 
#' ## Calculate trait means
#' 
## ------------------------------------------------------------------------------------------
E.facets<-
  per.facets[grepl("E.",per.facets)]

dat$E<-rowMeans(dat[,E.facets],na.rm=T)

# standardize

dat$E.z<-(dat$E-mean(dat$E,na.rm=T))/sd(dat$E,na.rm=T)


#' 
#' ## Standardize country-level predictors
#' 
## ------------------------------------------------------------------------------------------
# save raw values for plotting
correlates$GenderGapIndex.raw<-correlates$GenderGapIndex
# standardize
correlates$GenderGapIndex<-
  scale(correlates$GenderGapIndex, center = T, scale=T)


#' 
#' 
#' ## Merge correlates to the data files
#' 
## ------------------------------------------------------------------------------------------

fdat<-left_join(x=dat,
                 y=correlates,
                 by=c("COUNTRY"="Country"))

fdat$sex.ratio<-fdat$nMale/(fdat$nMale+fdat$nFemale)

fdat$sex.c<-ifelse(fdat$SEX=="Female",-0.5,
                   ifelse(fdat$SEX=="Male",0.5,NA))

# exclude missing values
fdat <- fdat %>%
  dplyr::select("sex.c","E","E.z","COUNTRY","sex.ratio",
                "GenderGapIndex","GenderGapIndex.raw") %>%
  na.omit()

#' 
#' 
#' # Analysis
#' 
#' ## Reliability of the difference score
#' 
## ------------------------------------------------------------------------------------------

reliab.E.z<-
  reliability_dms(data=fdat,diff_var="sex.c",var = "E.z",
                  diff_var_values = c(0.5,-0.5),group_var = "COUNTRY")

export(t(data.frame(reliab.E.z)),
       "../results/reliab.E.z.xlsx",
       overwrite=T)
reliab.E.z

#' 
#' 
#' ## Multi-level model
#' 
#' ### Fit model
#' 
## ------------------------------------------------------------------------------------------

fit_E.z<-
  ddsc_ml(data = fdat,predictor = "GenderGapIndex",
          covariates="sex.ratio",
          moderator = "sex.c",moderator_values=c(0.5,-0.5),
          DV = "E.z",lvl2_unit = "COUNTRY",re_cov_test = T,
          scaling_sd = "observed")

#' 
#' ### Descriptive statistics
#' 
## ------------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_E.z$descriptives)),
       "../results/E.z_ml_desc.xlsx",
       overwrite=T)
round(fit_E.z$descriptives,2)
round(fit_E.z$SDs,2)

#' 
#' ### Variance heterogeneity test
#' 
## ------------------------------------------------------------------------------------------
export(t(data.frame(fit_E.z$re_cov_test)),
       "../results/E.z_ml_var_test.xlsx",
       overwrite=T)
round(fit_E.z$re_cov_test,3)

#' 
#' ### Component correlation
#' 
## ------------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_E.z$ddsc_sem_fit$variance_test)),
       "../results/E.z_ml_comp_cor.xlsx",
       overwrite=T)
round(fit_E.z$ddsc_sem_fit$variance_test,3)

#' 
#' ### Deconstructing results
#' 
## ------------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_E.z$results)),
       "../results/E.z_ml_results.xlsx",
       overwrite=T)
round(fit_E.z$results,3)

#' 
#' ### Multi-level model output
#' 
## ------------------------------------------------------------------------------------------
# cross-level interaction model
summary(fit_E.z$model)

# reduced model without the predictor
summary(fit_E.z$reduced_model)

#' 
#' ## COUNTRY-level path model
#' 
#' ### Fit the model
#' 
#' The model is already stored within the multi-level model object. 
#' 
## ------------------------------------------------------------------------------------------
fit_E.z_sem<-fit_E.z$ddsc_sem_fit

#' 
#' ### Results
#' 
## ------------------------------------------------------------------------------------------
export(rownames_to_column(data.frame(fit_E.z_sem$results)),
       "../results/E.z_sem_results.xlsx",
       overwrite=T)
round(fit_E.z_sem$results,3)

#' 
#' 
#' # Plotting the results
#' 
## ------------------------------------------------------------------------------------------
# refit reduced and full models with GGGI in original scale

ml_E.z_red<-fit_E.z$reduced_model
  
# refit the model with raw variable
ml_E.z<-
  lmer(E.z~sex.c+
         sex.ratio+
         GenderGapIndex.raw+
         sex.c:GenderGapIndex.raw+
         (sex.c|COUNTRY),data=fdat,
       control = lmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e6)))


# point predictions as function of GGGI for components

p<-
  emmip(
    ml_E.z, 
    sex.c ~ GenderGapIndex.raw,
    at=list(sex.c = c(-0.5,0.5),
            GenderGapIndex.raw=
              seq(from=round(range(fdat$GenderGapIndex.raw)[1],2),
                  to=round(range(fdat$GenderGapIndex.raw)[2],2),
                  by=0.001)),
    plotit=F,CIs=T,lmerTest.limit = 1e6,disable.pbkrtest=T)

p$sex<-p$tvar
levels(p$sex)<-c("Women","Men")

# obtain min and max for aligned plots
min.y.comp<-min(p$LCL)
max.y.comp<-max(p$UCL)

# Men and Women mean distributions

p3<-coefficients(ml_E.z_red)$COUNTRY
p3<-cbind(rbind(p3,p3),weight=rep(c(-0.5,0.5),each=nrow(p3)))
p3$xvar<-p3$`(Intercept)`+p3$sex.ratio*0.5+p3$sex.c*p3$weight
p3$sex<-as.factor(p3$weight)
levels(p3$sex)<-c("Women","Men")

# obtain min and max for aligned plots
min.y.mean.distr<-min(p3$xvar)
max.y.mean.distr<-max(p3$xvar)


# obtain the coefs for the sex-effect (difference) as function of GGGI

p2<-data.frame(
  emtrends(ml_E.z,var="+1*sex.c",
           specs="GenderGapIndex.raw",
           at=list(#sex.c = c(-0.5,0.5),
             GenderGapIndex.raw=
               seq(from=round(range(fdat$GenderGapIndex.raw)[1],2),
                   to=round(range(fdat$GenderGapIndex.raw)[2],2),
                   by=0.001)),
           lmerTest.limit = 1e6,disable.pbkrtest=T))

p2$yvar<-p2$X.1.sex.c.trend
p2$xvar<-p2$GenderGapIndex.raw
p2$LCL<-p2$lower.CL
p2$UCL<-p2$upper.CL

# obtain min and max for aligned plots
min.y.diff<-min(p2$LCL)
max.y.diff<-max(p2$UCL)

# difference score distribution

p4<-coefficients(ml_E.z_red)$COUNTRY
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


pvals<-p_coding(c(fit_E.z$results["b_21","p.value"],
                    fit_E.z$results["b_11","p.value"]))

ests<-
  round_tidy(c(fit_E.z$results["b_21","estimate"],
               fit_E.z$results["b_11","estimate"]),2)

coef1<-paste0("b21 = ",ests[1],", p ",
               ifelse(fit_E.z$results["b_21","p.value"]<.001,
                      "","="),pvals[1])
coef2<-paste0("b11 = ",ests[2],", p ",
               ifelse(fit_E.z$results["b_11","p.value"]<.001,
                      "","="),pvals[2])

coef_q<-round_tidy(fit_E.z$results["q_b11_b21","estimate"],2)
coef_q<-paste0("q_b = ",coef_q,", p ",
               ifelse(fit_E.z$results["interaction","p.value"]<.001,"","="),
               p_coding(fit_E.z$results["interaction","p.value"]))

coefs<-data.frame(sex=c("Women","Men"),
                  coef=c(coef1,coef2))

p1.E.z<-ggplot(p,aes(y=yvar,x=xvar,color=sex))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Global Gender Gap Index")+
  #ylim=c(2.3,3.9)+
  ylim(c(min.y.pred,max.y.pred))+
  ylab("Extraversion Mean-Level")+
  scale_color_manual(values=met.brewer("Archambault")[c(6,2)])+
  theme(legend.position = "top",
        legend.title=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.background = element_rect(fill = "white",
                                        #colour = "black",
                                        #size = 0.5, linetype = "solid"
        ),
        panel.grid.major.x = element_line(linewidth = 0.5, linetype = 2,
                                          colour = "gray"))+
  geom_text(data = coefs,show.legend=F,
            aes(label=coef,x=0.63,
                y=c(-0.3
                    ,-0.4),size=14,hjust="left"))+
  geom_text(inherit.aes=F,aes(x=0.63,y=-0.5,
                              label=coef_q,size=14,hjust="left"),
            show.legend=F)
p1.E.z

# prediction plot for difference score


pvals2<-p_coding(fit_E.z$results["r_xy1y2","p.value"])

ests2<-
  round_tidy(fit_E.z$results["r_xy1y2","estimate"],2)

coefs2<-paste0("r = ",ests2,
               ", p ",
               ifelse(fit_E.z$results["r_xy1y2","p.value"]<.001,"","="),
               pvals2)


p2.E.z<-ggplot(p2,aes(y=yvar,x=xvar))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Global Gender Gap Index")+
  ylim(c(min.y.narrow,max.y.narrow))+
  ylab("Difference in Extraversion")+
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
            aes(label=coefs2,x=0.63,hjust="left",
                y=c(round(min(p2$LCL),2)),size=14))
p2.E.z

# mean-level distributions

p3.E.z<-
  ggplot(p3, aes(x=xvar, fill=sex)) + 
  geom_density(alpha=.75) + 
  scale_fill_manual(values=met.brewer("Archambault")[c(6,2)])+
  #scale_fill_manual(values=c("turquoise3","orangered2","black")) + 
  xlab("")+
  ylab("Density")+
  ylim(c(0,6))+
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
p3.E.z

# distribution for mean differences

p4.E.z<-
  ggplot(p4, aes(x=xvar,fill="black")) + 
  geom_density(alpha=.75) + 
  scale_fill_manual(values="black")+
  #scale_fill_manual(values=c("turquoise3","orangered2","black")) + 
  xlab("")+
  ylab("Density")+
  ylim(c(0,6))+
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
p4.E.z

# combine component-specific predictions

p13.E.z<-
  ggarrange(p1.E.z, p3.E.z,common.legend = T,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p13.E.z

# combine difference score predictions

p24.E.z<-
  ggarrange(p2.E.z, p4.E.z,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p24.E.z


pall.E.z<-
  ggarrange(p13.E.z,p24.E.z,align = "hv",
            ncol=1,nrow=2,heights=c(2,1))
pall.E.z

png(filename = 
      "../results/pall.E.z.png",
    units = "cm",
    width = 21.0,height=29.7*(4/5),res = 600)
pall.E.z
dev.off()

#' 
#' # Session information
#' 
## ------------------------------------------------------------------------------------------
s<-sessionInfo()
print(s,locale=F)

#' 
