---
title: "Predicting cross-country differences in economic preferences for risktaking between men and women with country-level gender equality."
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
  summarise(risktaking.men.mean=mean(risktaking,na.rm=T),
            risktaking.men.sd=sd(risktaking,na.rm=T),
            n.men=n())

women<-dat.ex2 %>%
  group_by(isocode) %>%
  filter(gender==1) %>%
  summarise(risktaking.women.mean=mean(risktaking,na.rm=T),
            risktaking.women.sd=sd(risktaking,na.rm=T),
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
  
  temp.dat.ex2$risktaking<-
    (temp.dat.ex2$risktaking-mean(temp.dat.ex2$risktaking,na.rm=T))/
    sd(temp.dat.ex2$risktaking,na.rm=T)
  
  # run the model
  
  temp.risktaking.diff<-
    coefficients(lm(risktaking~gender+age+I(age^2)+subj_math_skills,
                    data=temp.dat.ex2))["gender"]
  
  reg.diff.list[[i]]<-
    cbind.data.frame(
    isocode=countries[i],
    risktaking.reg.diff=temp.risktaking.diff)
  
}

reg.diff<-do.call(rbind,reg.diff.list)
rownames(reg.diff)<-NULL
head(reg.diff)
```

```
##   isocode risktaking.reg.diff
## 1     TUR          -0.1082723
## 2     FRA          -0.3496920
## 3     NLD          -0.3902652
## 4     ESP          -0.2070446
## 5     ITA          -0.1657006
## 6     POL          -0.2400633
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
  dplyr::select(risktaking,sex.c,country,GEI,age,subj_math_skills) %>%
  na.omit() %>%
  mutate(age_sq=age^2)
```

# Analysis

## Reliability of the difference score


```r
reliab.risktaking<-
  reliability_dms(
    data=fdat,
    diff_var="sex.c",var = "risktaking",
    diff_var_values = c(0.5,-0.5),
    group_var = "country")


export(t(data.frame(reliab.risktaking)),
       "../results/reliab.risktaking.xlsx",
       overwrite=T)
reliab.risktaking
```

```
##              r11              r22              r12              sd1 
##       0.97943607       0.98620351       0.93488256       0.30891872 
##              sd2           sd_d12               m1               m2 
##       0.33659265       0.11961454       0.11001616      -0.09830914 
##            m_d12 reliability_dmsa 
##       0.20832530       0.75359317
```

## Multi-level model

### Fit model


```r
fit_risktaking<-
  ddsc_ml(data = fdat,predictor = "GEI",
          covariates=c("age","age_sq","subj_math_skills"),
          moderator = "sex.c",moderator_values=c(0.5,-0.5),
          DV = "risktaking",lvl2_unit = "country",re_cov_test = T,
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
export(rownames_to_column(data.frame(fit_risktaking$descriptives)),
       "../results/risktaking_ml_desc.xlsx",
       overwrite=T)
round(fit_risktaking$descriptives,2)
```

```
##                       M   SD means_y1 means_y1_scaled means_y2 means_y2_scaled
## means_y1           0.11 0.31     1.00            1.00     0.93            0.93
## means_y1_scaled    0.34 0.96     1.00            1.00     0.93            0.93
## means_y2          -0.10 0.34     0.93            0.93     1.00            1.00
## means_y2_scaled   -0.30 1.04     0.93            0.93     1.00            1.00
## GEI                0.00 1.00    -0.06           -0.06    -0.14           -0.14
## GEI_scaled         0.00 1.00    -0.06           -0.06    -0.14           -0.14
## diff_score         0.21 0.12    -0.05           -0.05    -0.40           -0.40
## diff_score_scaled  0.64 0.37    -0.05           -0.05    -0.40           -0.40
##                     GEI GEI_scaled diff_score diff_score_scaled
## means_y1          -0.06      -0.06      -0.05             -0.05
## means_y1_scaled   -0.06      -0.06      -0.05             -0.05
## means_y2          -0.14      -0.14      -0.40             -0.40
## means_y2_scaled   -0.14      -0.14      -0.40             -0.40
## GEI                1.00       1.00       0.23              0.23
## GEI_scaled         1.00       1.00       0.23              0.23
## diff_score         0.23       0.23       1.00              1.00
## diff_score_scaled  0.23       0.23       1.00              1.00
```

```r
round(fit_risktaking$SDs,2)
```

```
##         SD_y1         SD_y2     SD_pooled SD_diff_score            VR 
##          0.31          0.34          0.32          0.12          0.84
```

### Variance heterogeneity test


```r
export(t(data.frame(fit_risktaking$re_cov_test)),
       "../results/risktaking_ml_var_test.xlsx",
       overwrite=T)
round(fit_risktaking$re_cov_test,3)
```

```
## RE_cov RE_cor  Chisq     Df      p 
## -0.005 -0.205  2.033  1.000  0.154
```

### Component correlation


```r
export(rownames_to_column(data.frame(fit_risktaking$ddsc_sem_fit$variance_test)),
       "../results/risktaking_ml_comp_cor.xlsx",
       overwrite=T)
round(fit_risktaking$ddsc_sem_fit$variance_test,3)
```

```
##              est    se      z pvalue ci.lower ci.upper
## cov_y1y2   0.918 0.160  5.754  0.000    0.606    1.231
## var_y1     0.902 0.151  5.958  0.000    0.605    1.198
## var_y2     1.070 0.180  5.958  0.000    0.718    1.422
## var_diff  -0.169 0.087 -1.929  0.054   -0.340    0.003
## var_ratio  0.842 0.071 11.869  0.000    0.703    0.981
## cor_y1y2   0.935 0.015 62.522  0.000    0.906    0.964
```

### Deconstructing results


```r
export(rownames_to_column(data.frame(fit_risktaking$results)),
       "../results/risktaking_ml_results.xlsx",
       overwrite=T)
round(fit_risktaking$results,3)
```

```
##                            estimate    SE     df t.ratio p.value ci.lower
## r_xy1y2                       0.140 0.101 66.721   1.383   0.171   -0.062
## w_11                          0.016 0.034 68.715   0.473   0.638   -0.052
## w_21                          0.000 0.036 69.069  -0.012   0.991   -0.073
## r_xy1                         0.053 0.111 68.715   0.473   0.638   -0.169
## r_xy2                        -0.001 0.108 69.069  -0.012   0.991   -0.217
## b_11                          0.050 0.107 68.715   0.473   0.638   -0.162
## b_21                         -0.001 0.113 69.069  -0.012   0.991   -0.227
## main_effect                   0.008 0.035 69.006   0.227   0.821   -0.062
## moderator_effect              0.177 0.012 71.044  14.555   0.000    0.153
## interaction                   0.017 0.012 66.721   1.383   0.171   -0.007
## q_b11_b21                     0.052    NA     NA      NA      NA       NA
## q_rxy1_rxy2                   0.054    NA     NA      NA      NA       NA
## cross_over_point            -10.602    NA     NA      NA      NA       NA
## interaction_vs_main           0.009 0.039 68.987   0.226   0.822   -0.069
## interaction_vs_main_bscale    0.027 0.121 68.987   0.226   0.822   -0.213
## interaction_vs_main_rscale    0.028 0.112 68.970   0.253   0.801   -0.194
## dadas                         0.001 0.073 69.069   0.012   0.495   -0.145
## dadas_bscale                  0.003 0.226 69.069   0.012   0.495   -0.448
## dadas_rscale                  0.003 0.217 69.069   0.012   0.495   -0.430
## abs_diff                      0.017 0.012 66.721   1.383   0.086   -0.007
## abs_sum                       0.016 0.070 69.006   0.227   0.411   -0.123
## abs_diff_bscale               0.052 0.037 66.721   1.383   0.086   -0.023
## abs_sum_bscale                0.049 0.216 69.006   0.227   0.411   -0.383
## abs_diff_rscale               0.054 0.037 66.021   1.458   0.075   -0.020
## abs_sum_rscale                0.051 0.217 68.999   0.237   0.407   -0.381
##                            ci.upper
## r_xy1y2                       0.341
## w_11                          0.085
## w_21                          0.072
## r_xy1                         0.275
## r_xy2                         0.215
## b_11                          0.263
## b_21                          0.224
## main_effect                   0.078
## moderator_effect              0.201
## interaction                   0.041
## q_b11_b21                        NA
## q_rxy1_rxy2                      NA
## cross_over_point                 NA
## interaction_vs_main           0.086
## interaction_vs_main_bscale    0.268
## interaction_vs_main_rscale    0.251
## dadas                         0.146
## dadas_bscale                  0.453
## dadas_rscale                  0.435
## abs_diff                      0.041
## abs_sum                       0.155
## abs_diff_bscale               0.126
## abs_sum_bscale                0.481
## abs_diff_rscale               0.128
## abs_sum_rscale                0.483
```

### Multi-level model output


```r
# cross-level interaction model
summary(fit_risktaking$model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: model_formula
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 197047.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3450 -0.6921 -0.0498  0.6594  3.7305 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  country  (Intercept) 0.084514 0.2907        
##           sex.c       0.007073 0.0841   -0.21
##  Residual             0.832076 0.9122        
## Number of obs: 74066, groups:  country, 71
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       7.224e-02  4.097e-02  1.344e+02   1.764   0.0801 .  
## sex.c             1.771e-01  1.216e-02  7.104e+01  14.555   <2e-16 ***
## GEI               7.921e-03  3.492e-02  6.901e+01   0.227   0.8212    
## age              -1.563e-03  9.801e-04  7.402e+04  -1.595   0.1108    
## age_sq           -1.146e-04  1.046e-05  7.401e+04 -10.959   <2e-16 ***
## subj_math_skills  4.565e-02  1.246e-03  7.401e+04  36.638   <2e-16 ***
## sex.c:GEI         1.670e-02  1.208e-02  6.672e+01   1.383   0.1713    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) sex.c  GEI    age    age_sq sbj_m_
## sex.c       -0.132                                   
## GEI          0.007  0.002                            
## age         -0.491  0.002 -0.005                     
## age_sq       0.452 -0.004 -0.001 -0.977              
## sbj_mth_skl -0.180 -0.070 -0.011  0.017  0.007       
## sex.c:GEI    0.001  0.011 -0.175 -0.002  0.004 -0.002
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
# reduced model without the predictor
summary(fit_risktaking$reduced_model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: risktaking ~ sex.c + age + age_sq + subj_math_skills + (sex.c |  
##     country)
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 197037.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3452 -0.6918 -0.0500  0.6597  3.7305 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  country  (Intercept) 0.083368 0.28873       
##           sex.c       0.007219 0.08497  -0.20
##  Residual             0.832075 0.91218       
## Number of obs: 74066, groups:  country, 71
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       7.207e-02  4.077e-02  1.373e+02   1.768   0.0793 .  
## sex.c             1.768e-01  1.225e-02  7.226e+01  14.438   <2e-16 ***
## age              -1.559e-03  9.801e-04  7.402e+04  -1.591   0.1117    
## age_sq           -1.147e-04  1.046e-05  7.401e+04 -10.965   <2e-16 ***
## subj_math_skills  4.566e-02  1.246e-03  7.402e+04  36.646   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) sex.c  age    age_sq
## sex.c       -0.126                     
## age         -0.494  0.002              
## age_sq       0.454 -0.004 -0.977       
## sbj_mth_skl -0.180 -0.070  0.017  0.007
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

## Country-level path model

### Fit the model

The model is already stored within the multi-level model object.


```r
fit_risktaking_sem<-fit_risktaking$ddsc_sem_fit
```

### Results


```r
export(rownames_to_column(data.frame(fit_risktaking_sem$results)),
       "../results/risktaking_sem_results.xlsx",
       overwrite=T)
round(fit_risktaking_sem$results,3)
```

```
##                                    est    se      z pvalue ci.lower ci.upper
## r_xy1_y2                         0.232 0.115  2.008  0.045    0.006    0.458
## r_xy1                           -0.057 0.118 -0.484  0.628   -0.290    0.175
## r_xy2                           -0.135 0.118 -1.149  0.251   -0.366    0.095
## b_11                            -0.055 0.113 -0.484  0.628   -0.277    0.167
## b_21                            -0.141 0.123 -1.149  0.251   -0.381    0.099
## b_10                             0.341 0.112  3.027  0.002    0.120    0.561
## b_20                            -0.304 0.122 -2.501  0.012   -0.543   -0.066
## res_cov_y1_y2                    0.911 0.158  5.762  0.000    0.601    1.220
## diff_b10_b20                     0.645 0.042 15.194  0.000    0.562    0.728
## diff_b11_b21                     0.086 0.043  2.008  0.045    0.002    0.170
## diff_rxy1_rxy2                   0.078 0.042  1.857  0.063   -0.004    0.160
## q_b11_b21                        0.087 0.044  1.984  0.047    0.001    0.172
## q_rxy1_rxy2                      0.078 0.042  1.855  0.064   -0.004    0.161
## cross_over_point                -7.512 3.773 -1.991  0.046  -14.907   -0.117
## sum_b11_b21                     -0.196 0.232 -0.843  0.399   -0.651    0.259
## main_effect                     -0.098 0.116 -0.843  0.399   -0.325    0.130
## interaction_vs_main_effect      -0.012 0.115 -0.104  0.917   -0.236    0.213
## diff_abs_b11_abs_b21            -0.086 0.043 -2.008  0.045   -0.170   -0.002
## abs_diff_b11_b21                 0.086 0.043  2.008  0.022    0.002    0.170
## abs_sum_b11_b21                  0.196 0.232  0.843  0.200   -0.259    0.651
## dadas                           -0.110 0.227 -0.484  0.686   -0.554    0.334
## q_r_equivalence                  0.078 0.042  1.855  0.968       NA       NA
## q_b_equivalence                  0.087 0.044  1.984  0.976       NA       NA
## cross_over_point_equivalence     7.512 3.773  1.991  0.977       NA       NA
## cross_over_point_minimal_effect  7.512 3.773  1.991  0.023       NA       NA
```

# Plotting the results


```r
# start with obtaining predicted values for means and differences

ml_risktaking<-fit_risktaking$model
ml_risktaking_red<-fit_risktaking$reduced_model
  


# point predictions as function of GEI for components

p<-
  emmip(
    ml_risktaking, 
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

p3<-coefficients(ml_risktaking_red)$country
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
  emtrends(ml_risktaking,var="+1*sex.c",
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

p4<-coefficients(ml_risktaking_red)$country
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

pvals<-p_coding(c(fit_risktaking$results["b_21","p.value"],
                    fit_risktaking$results["b_11","p.value"]))

ests<-
  round_tidy(c(fit_risktaking$results["b_21","estimate"],
               fit_risktaking$results["b_11","estimate"]),2)

coef1<-paste0("b21 = ",ests[1],", p = ",pvals[1])
coef2<-paste0("b11 = ",ests[2],", p = ",pvals[2])

coef_q<-round_tidy(fit_risktaking$results["q_b11_b21","estimate"],2)
coef_q<-paste0("q_b = ",coef_q,", p ",
               ifelse(fit_risktaking$results["interaction","p.value"]<.001,"","="),
               p_coding(fit_risktaking$results["interaction","p.value"]))

coefs<-data.frame(sex=c("Women","Men"),
                  coef=c(coef1,coef2))


p1.risktaking.GEI<-ggplot(p,aes(y=yvar,x=xvar,color=sex))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Gender Equality Index")+
  #ylim=c(2.3,3.9)+
  ylim(c(min.y.pred,max.y.pred))+
  ylab("Risk taking")+
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
p1.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# prediction plot for difference score


pvals2<-p_coding(fit_risktaking$results["r_xy1y2","p.value"])

ests2<-
  round_tidy(fit_risktaking$results["r_xy1y2","estimate"],2)

coefs2<-paste0("r = ",ests2,
               ", p ",
               ifelse(fit_risktaking$results["r_xy1y2","p.value"]<.001,"","="),
               pvals2)


p2.risktaking.GEI<-ggplot(p2,aes(y=yvar,x=xvar))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCL, ymax=UCL),alpha=0.5)+
  xlab("Gender Equality Index")+
  ylim(c(min.y.narrow,max.y.narrow))+
  ylab("Difference in Risk taking")+
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
                y=0.30,size=14))
p2.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
# mean-level distributions

p3.risktaking.GEI<-
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
p3.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```r
# distribution for mean differences

p4.risktaking.GEI<-
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
p4.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-4.png)<!-- -->

```r
# combine component-specific predictions

p13.risktaking.GEI<-
  ggarrange(p1.risktaking.GEI, p3.risktaking.GEI,common.legend = T,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p13.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-5.png)<!-- -->

```r
# combine difference score predictions

p24.risktaking.GEI<-
  ggarrange(p2.risktaking.GEI, p4.risktaking.GEI,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p24.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-6.png)<!-- -->

```r
pall.risktaking.GEI<-
  ggarrange(p13.risktaking.GEI,p24.risktaking.GEI,align = "hv",
            ncol=1,nrow=2,heights=c(2,1))
pall.risktaking.GEI
```

![](Analysis_risktaking_files/figure-html/unnamed-chunk-17-7.png)<!-- -->

```r
png(filename = 
      "../results/pall.risktaking.GEI.png",
    units = "cm",
    width = 21.0,height=29.7*(4/5),res = 600)
pall.risktaking.GEI
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
