---
title: "Predicting cross-country differences in economic preferences for posrecip between men and women with country-level gender equality."
output:
  html_document: 
    df_print: default
    toc: yes
    number_sections: yes
    keep_md: yes
---



# Preparations

## Load packages


```r
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
```

## Import data

Data (originally used in Falk & Hermle, 2018) is openly available at www.briq-institute.org/global-preferences/home


```r
dat.ex2 <- import("../data/Raw/individual_new.dta")
```

Import also pre-calculated indices of gender equality index (GEI)


```r
GEI <- 
  import("../data/Processed/GEI.xlsx")
```

## Compile country-level data


```r
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
```

Combine these with each other and with country-level GEI


```r
country.dat.ex2<-left_join(
  x=men,
  y=women,
  by="isocode")

country.dat.ex2<-
  left_join(country.dat.ex2,
            GEI,
            by=c("isocode"="ISO3"))
```

Use the regression method, process:

1.  Standardize within-country

2.  Run regression on the preference while controlling for age, age\^2, subj_math_skills

3.  Obtain the coefficients for gender as sex difference for each country


```r
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
```

```
##   isocode posrecip.reg.diff
## 1     TUR        0.03037746
## 2     FRA       -0.03141587
## 3     NLD        0.06362745
## 4     ESP        0.06104108
## 5     ITA       -0.14834011
## 6     POL        0.08520578
```

```r
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
```

## Data exclusions and transformations


```r
fdat<-dat.ex2 %>%
  dplyr::select(posrecip,sex.c,country,GEI,age,subj_math_skills) %>%
  na.omit() %>%
  mutate(age_sq=age^2)
```

# Analysis

## Reliability of the difference score


```r
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
```

```
##              r11              r22              r12              sd1 
##      0.985475816      0.986479616      0.959961147      0.340957132 
##              sd2           sd_d12               m1               m2 
##      0.331362088      0.095599506     -0.020810185      0.008904343 
##            m_d12 reliability_dmsa 
##     -0.029714528      0.652815486
```

## Multi-level model

### Fit model


```r
fit_posrecip<-
  ddsc_ml(data = fdat,predictor = "GEI",
          covariates=c("age","age_sq","subj_math_skills"),
          moderator = "sex.c",moderator_values=c(0.5,-0.5),
          DV = "posrecip",lvl2_unit = "country",re_cov_test = T,
          scaling_sd = "observed")
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

### Descriptive statistics


```r
export(rownames_to_column(data.frame(fit_posrecip$descriptives)),
       "../results/posrecip_ml_desc.xlsx",
       overwrite=T)
round(fit_posrecip$descriptives,2)
```

```
##                       M   SD means_y1 means_y1_scaled means_y2 means_y2_scaled
## means_y1          -0.02 0.34     1.00            1.00     0.96            0.96
## means_y1_scaled   -0.06 1.01     1.00            1.00     0.96            0.96
## means_y2           0.01 0.33     0.96            0.96     1.00            1.00
## means_y2_scaled    0.03 0.99     0.96            0.96     1.00            1.00
## GEI                0.00 1.00    -0.08           -0.08    -0.05           -0.05
## GEI_scaled         0.00 1.00    -0.08           -0.08    -0.05           -0.05
## diff_score        -0.03 0.10     0.24            0.24    -0.04           -0.04
## diff_score_scaled -0.09 0.28     0.24            0.24    -0.04           -0.04
##                     GEI GEI_scaled diff_score diff_score_scaled
## means_y1          -0.08      -0.08       0.24              0.24
## means_y1_scaled   -0.08      -0.08       0.24              0.24
## means_y2          -0.05      -0.05      -0.04             -0.04
## means_y2_scaled   -0.05      -0.05      -0.04             -0.04
## GEI                1.00       1.00      -0.11             -0.11
## GEI_scaled         1.00       1.00      -0.11             -0.11
## diff_score        -0.11      -0.11       1.00              1.00
## diff_score_scaled -0.11      -0.11       1.00              1.00
```

```r
round(fit_posrecip$SDs,2)
```

```
##         SD_y1         SD_y2     SD_pooled SD_diff_score            VR 
##          0.34          0.33          0.34          0.10          1.06
```

### Variance heterogeneity test


```r
export(t(data.frame(fit_posrecip$re_cov_test)),
       "../results/posrecip_ml_var_test.xlsx",
       overwrite=T)
round(fit_posrecip$re_cov_test,3)
```

```
## RE_cov RE_cor  Chisq     Df      p 
##  0.003  0.113  0.577  1.000  0.447
```

### Component correlation


```r
export(rownames_to_column(data.frame(fit_posrecip$ddsc_sem_fit$variance_test)),
       "../results/posrecip_ml_comp_cor.xlsx",
       overwrite=T)
round(fit_posrecip$ddsc_sem_fit$variance_test,3)
```

```
##             est    se       z pvalue ci.lower ci.upper
## cov_y1y2  0.946 0.162   5.835  0.000    0.628    1.264
## var_y1    1.014 0.170   5.958  0.000    0.680    1.348
## var_y2    0.958 0.161   5.958  0.000    0.643    1.273
## var_diff  0.056 0.066   0.850  0.395   -0.073    0.186
## var_ratio 1.059 0.070  15.040  0.000    0.921    1.197
## cor_y1y2  0.960 0.009 103.075  0.000    0.942    0.978
```

### Deconstructing results


```r
export(rownames_to_column(data.frame(fit_posrecip$results)),
       "../results/posrecip_ml_results.xlsx",
       overwrite=T)
round(fit_posrecip$results,3)
```

```
##                            estimate    SE     df t.ratio p.value ci.lower
## r_xy1y2                      -0.141 0.118 63.586  -1.192   0.238   -0.378
## w_11                         -0.037 0.040 68.969  -0.907   0.368   -0.117
## w_21                         -0.023 0.039 69.017  -0.585   0.560   -0.102
## r_xy1                        -0.107 0.118 68.969  -0.907   0.368   -0.343
## r_xy2                        -0.070 0.119 69.017  -0.585   0.560   -0.307
## b_11                         -0.109 0.120 68.969  -0.907   0.368   -0.348
## b_21                         -0.069 0.117 69.017  -0.585   0.560   -0.303
## main_effect                  -0.030 0.039 69.045  -0.755   0.453   -0.109
## moderator_effect             -0.056 0.011 68.543  -4.852   0.000   -0.078
## interaction                  -0.014 0.011 63.586  -1.192   0.238   -0.036
## q_b11_b21                    -0.040    NA     NA      NA      NA       NA
## q_rxy1_rxy2                  -0.038    NA     NA      NA      NA       NA
## cross_over_point             -4.110    NA     NA      NA      NA       NA
## interaction_vs_main          -0.016 0.040 68.906  -0.406   0.686   -0.096
## interaction_vs_main_bscale   -0.049 0.120 68.906  -0.406   0.686   -0.287
## interaction_vs_main_rscale   -0.051 0.123 68.912  -0.414   0.680   -0.296
## dadas                        -0.046 0.079 69.017  -0.585   0.720   -0.203
## dadas_bscale                 -0.137 0.235 69.017  -0.585   0.720   -0.605
## dadas_rscale                 -0.139 0.238 69.017  -0.585   0.720   -0.614
## abs_diff                      0.014 0.011 63.586   1.192   0.119   -0.009
## abs_sum                       0.060 0.079 69.045   0.755   0.226   -0.098
## abs_diff_bscale               0.040 0.034 63.586   1.192   0.119   -0.027
## abs_sum_bscale                0.177 0.235 69.045   0.755   0.226   -0.291
## abs_diff_rscale               0.038 0.034 63.725   1.120   0.133   -0.029
## abs_sum_rscale                0.177 0.235 69.045   0.753   0.227   -0.292
##                            ci.upper
## r_xy1y2                       0.095
## w_11                          0.044
## w_21                          0.056
## r_xy1                         0.129
## r_xy2                         0.168
## b_11                          0.131
## b_21                          0.165
## main_effect                   0.049
## moderator_effect             -0.033
## interaction                   0.009
## q_b11_b21                        NA
## q_rxy1_rxy2                      NA
## cross_over_point                 NA
## interaction_vs_main           0.064
## interaction_vs_main_bscale    0.190
## interaction_vs_main_rscale    0.194
## dadas                         0.111
## dadas_bscale                  0.331
## dadas_rscale                  0.336
## abs_diff                      0.036
## abs_sum                       0.217
## abs_diff_bscale               0.108
## abs_sum_bscale                0.646
## abs_diff_rscale               0.105
## abs_sum_rscale                0.646
```

### Multi-level model output


```r
# cross-level interaction model
summary(fit_posrecip$model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: model_formula
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 200818.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8409 -0.6209  0.1334  0.7383  2.8733 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  country  (Intercept) 0.108222 0.32897      
##           sex.c       0.005749 0.07582  0.10
##  Residual             0.863558 0.92928      
## Number of obs: 74443, groups:  country, 71
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)      -4.103e-01  4.502e-02  1.200e+02  -9.115 2.19e-15 ***
## sex.c            -5.551e-02  1.144e-02  6.854e+01  -4.852 7.37e-06 ***
## GEI              -2.983e-02  3.948e-02  6.904e+01  -0.755    0.453    
## age               1.068e-02  9.935e-04  7.439e+04  10.750  < 2e-16 ***
## age_sq           -1.211e-04  1.059e-05  7.438e+04 -11.440  < 2e-16 ***
## subj_math_skills  3.959e-02  1.265e-03  7.435e+04  31.300  < 2e-16 ***
## sex.c:GEI        -1.351e-02  1.133e-02  6.359e+01  -1.192    0.238    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) sex.c  GEI    age    age_sq sbj_m_
## sex.c        0.083                                   
## GEI          0.006  0.002                            
## age         -0.454  0.003 -0.004                     
## age_sq       0.417 -0.004 -0.001 -0.977              
## sbj_mth_skl -0.166 -0.076 -0.010  0.016  0.008       
## sex.c:GEI    0.001  0.014  0.081 -0.002  0.005 -0.002
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
# reduced model without the predictor
summary(fit_posrecip$reduced_model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: posrecip ~ sex.c + age + age_sq + subj_math_skills + (sex.c |  
##     country)
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 200808.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8401 -0.6210  0.1332  0.7385  2.8746 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  country  (Intercept) 0.107556 0.32796      
##           sex.c       0.005832 0.07637  0.11
##  Residual             0.863556 0.92928      
## Number of obs: 74443, groups:  country, 71
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)      -4.101e-01  4.491e-02  1.221e+02  -9.131 1.77e-15 ***
## sex.c            -5.533e-02  1.149e-02  7.008e+01  -4.815 8.20e-06 ***
## age               1.067e-02  9.935e-04  7.439e+04  10.745  < 2e-16 ***
## age_sq           -1.211e-04  1.059e-05  7.438e+04 -11.435  < 2e-16 ***
## subj_math_skills  3.958e-02  1.265e-03  7.436e+04  31.293  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) sex.c  age    age_sq
## sex.c        0.093                     
## age         -0.455  0.003              
## age_sq       0.418 -0.004 -0.977       
## sbj_mth_skl -0.166 -0.076  0.016  0.008
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

## Country-level path model

### Fit the model

The model is already stored within the multi-level model object.


```r
fit_posrecip_sem<-fit_posrecip$ddsc_sem_fit
```

### Results


```r
export(rownames_to_column(data.frame(fit_posrecip_sem$results)),
       "../results/posrecip_sem_results.xlsx",
       overwrite=T)
round(fit_posrecip_sem$results,3)
```

```
##                                    est    se      z pvalue ci.lower ci.upper
## r_xy1_y2                        -0.111 0.118 -0.942  0.346   -0.342    0.120
## r_xy1                           -0.083 0.118 -0.700  0.484   -0.315    0.149
## r_xy2                           -0.053 0.119 -0.448  0.654   -0.285    0.179
## b_11                            -0.084 0.120 -0.700  0.484   -0.319    0.151
## b_21                            -0.052 0.117 -0.448  0.654   -0.281    0.177
## b_10                            -0.062 0.119 -0.520  0.603   -0.295    0.172
## b_20                             0.026 0.116  0.228  0.819   -0.201    0.254
## res_cov_y1_y2                    0.942 0.161  5.836  0.000    0.625    1.258
## diff_b10_b20                    -0.088 0.033 -2.654  0.008   -0.154   -0.023
## diff_b11_b21                    -0.032 0.034 -0.942  0.346   -0.097    0.034
## diff_rxy1_rxy2                  -0.030 0.033 -0.888  0.375   -0.095    0.036
## q_b11_b21                       -0.032 0.034 -0.940  0.347   -0.098    0.034
## q_rxy1_rxy2                     -0.030 0.034 -0.888  0.375   -0.096    0.036
## cross_over_point                -2.799 3.153 -0.887  0.375   -8.979    3.382
## sum_b11_b21                     -0.136 0.234 -0.582  0.561   -0.596    0.323
## main_effect                     -0.068 0.117 -0.582  0.561   -0.298    0.162
## interaction_vs_main_effect      -0.037 0.119 -0.308  0.758   -0.269    0.196
## diff_abs_b11_abs_b21             0.032 0.034  0.942  0.346   -0.034    0.097
## abs_diff_b11_b21                 0.032 0.034  0.942  0.173   -0.034    0.097
## abs_sum_b11_b21                  0.136 0.234  0.582  0.280   -0.323    0.596
## dadas                           -0.105 0.234 -0.448  0.673   -0.563    0.353
## q_r_equivalence                  0.030 0.034  0.888  0.813       NA       NA
## q_b_equivalence                  0.032 0.034  0.940  0.826       NA       NA
## cross_over_point_equivalence     2.799 3.153  0.887  0.813       NA       NA
## cross_over_point_minimal_effect  2.799 3.153  0.887  0.187       NA       NA
```

# Plotting the results


```r
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
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
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
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
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
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```r
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
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-4.png)<!-- -->

```r
# combine component-specific predictions

p13.posrecip.GEI<-
  ggarrange(p1.posrecip.GEI, p3.posrecip.GEI,common.legend = T,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p13.posrecip.GEI
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-5.png)<!-- -->

```r
# combine difference score predictions

p24.posrecip.GEI<-
  ggarrange(p2.posrecip.GEI, p4.posrecip.GEI,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p24.posrecip.GEI
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-6.png)<!-- -->

```r
pall.posrecip.GEI<-
  ggarrange(p13.posrecip.GEI,p24.posrecip.GEI,align = "hv",
            ncol=1,nrow=2,heights=c(2,1))
pall.posrecip.GEI
```

![](Analysis_posrecip_files/figure-html/unnamed-chunk-17-7.png)<!-- -->

```r
png(filename = 
      "../results/pall.posrecip.GEI.png",
    units = "cm",
    width = 21.0,height=29.7*(4/5),res = 600)
pall.posrecip.GEI
dev.off()
```

```
## png 
##   2
```

# Session information


```r
s<-sessionInfo()
print(s,locale=F)
```

```
## R version 4.3.2 (2023-10-31 ucrt)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19045)
## 
## Matrix products: default
## 
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] finalfit_1.0.6    emmeans_1.10.0    MetBrewer_0.2.0   ggpubr_0.6.0     
##  [5] ggplot2_3.4.4     tibble_3.2.1      dplyr_1.1.4       rio_0.5.29       
##  [9] lmerTest_3.1-3    lme4_1.1-35.1     Matrix_1.6-5      multid_1.0.0.9000
## [13] knitr_1.44        rmarkdown_2.25   
## 
## loaded via a namespace (and not attached):
##  [1] mnormt_2.1.1        gridExtra_2.3       sandwich_3.0-2     
##  [4] readxl_1.4.2        rlang_1.1.3         magrittr_2.0.3     
##  [7] multcomp_1.4-25     compiler_4.3.2      vctrs_0.6.5        
## [10] quadprog_1.5-8      pkgconfig_2.0.3     shape_1.4.6        
## [13] crayon_1.5.2        fastmap_1.1.1       backports_1.4.1    
## [16] labeling_0.4.3      pbivnorm_0.6.0      utf8_1.2.4         
## [19] tzdb_0.4.0          haven_2.5.2         nloptr_2.0.3       
## [22] purrr_1.0.2         xfun_0.39           glmnet_4.1-8       
## [25] jomo_2.7-6          cachem_1.0.8        jsonlite_1.8.8     
## [28] pan_1.9             broom_1.0.5         parallel_4.3.2     
## [31] lavaan_0.6-17       R6_2.5.1            bslib_0.5.1        
## [34] stringi_1.8.3       car_3.1-2           boot_1.3-28.1      
## [37] rpart_4.1.21        jquerylib_0.1.4     cellranger_1.1.0   
## [40] numDeriv_2016.8-1.1 estimability_1.4.1  Rcpp_1.0.12        
## [43] iterators_1.0.14    zoo_1.8-12          readr_2.1.4        
## [46] splines_4.3.2       nnet_7.3-19         tidyselect_1.2.0   
## [49] rstudioapi_0.15.0   abind_1.4-5         yaml_2.3.7         
## [52] codetools_0.2-19    curl_5.0.2          lattice_0.21-9     
## [55] withr_3.0.0         coda_0.19-4         evaluate_0.23      
## [58] foreign_0.8-85      survival_3.5-7      zip_2.3.0          
## [61] pillar_1.9.0        carData_3.0-5       mice_3.16.0        
## [64] foreach_1.5.2       stats4_4.3.2        generics_0.1.3     
## [67] hms_1.1.3           munsell_0.5.0       scales_1.3.0       
## [70] minqa_1.2.6         xtable_1.8-4        glue_1.7.0         
## [73] tools_4.3.2         data.table_1.14.8   openxlsx_4.2.5.2   
## [76] ggsignif_0.6.4      forcats_1.0.0       mvtnorm_1.2-4      
## [79] cowplot_1.1.3       grid_4.3.2          tidyr_1.3.1        
## [82] colorspace_2.1-0    nlme_3.1-163        cli_3.6.2          
## [85] fansi_1.0.6         gtable_0.3.4        rstatix_0.7.2      
## [88] sass_0.4.7          digest_0.6.34       TH.data_1.1-2      
## [91] farver_2.1.1        htmltools_0.5.5     lifecycle_1.0.4    
## [94] mitml_0.4-5         MASS_7.3-60
```
