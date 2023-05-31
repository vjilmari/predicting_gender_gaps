---
title: "Extraversion"
output:
  html_document: 
    df_print: default
    toc: yes
    number_sections: yes
    keep_md: yes
date: "2023-05-30"
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



```r
dat <- import("../data/ipip_processed.rda")
correlates <- import("../data/correlates.xlsx")
```



## Save variable names of the multivariate set to a vector


```r
per.facets<-
  names(dat)[which(names(dat)=="A.trust"):
              which(names(dat)=="O.liberalism")]
```


## Calculate trait means


```r
E.facets<-
  per.facets[grepl("E.",per.facets)]

dat$E<-rowMeans(dat[,E.facets],na.rm=T)

# standardize

dat$E.z<-(dat$E-mean(dat$E,na.rm=T))/sd(dat$E,na.rm=T)
```

## Standardize country-level predictors


```r
# save raw values for plotting
correlates$GenderGapIndex.raw<-correlates$GenderGapIndex
# standardize
correlates$GenderGapIndex<-
  scale(correlates$GenderGapIndex, center = T, scale=T)
```


## Merge correlates to the data files


```r
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
```


# Analysis

## Reliability of the difference score


```r
reliab.E.z<-
  reliability_dms(data=fdat,diff_var="sex.c",var = "E.z",
                  diff_var_values = c(0.5,-0.5),group_var = "COUNTRY")

export(t(data.frame(reliab.E.z)),
       "../results/reliab.E.z.xlsx",
       overwrite=T)
reliab.E.z
```

```
##              r11              r22              r12              sd1 
##        0.9819759        0.9723050        0.5639358        0.1465266 
##              sd2           sd_d12               m1               m2 
##        0.1131251        0.1247875       -0.1614257       -0.0343040 
##            m_d12 reliability_dmsa 
##       -0.1271217        0.9523886
```


## Multi-level model

### Fit model


```r
fit_E.z<-
  ddsc_ml(data = fdat,predictor = "GenderGapIndex",
          covariates="sex.ratio",
          moderator = "sex.c",moderator_values=c(0.5,-0.5),
          DV = "E.z",lvl2_unit = "COUNTRY",re_cov_test = T,
          scaling_sd = "observed")
```

```
## NOTE: Results may be misleading due to involvement in interactions
```

```
## refitting model(s) with ML (instead of REML)
```

### Descriptive statistics


```r
export(rownames_to_column(data.frame(fit_E.z$descriptives)),
       "../results/E.z_ml_desc.xlsx",
       overwrite=T)
round(fit_E.z$descriptives,2)
```

```
##                           M   SD means_y1 means_y1_scaled means_y2
## means_y1              -0.16 0.15     1.00            1.00     0.56
## means_y1_scaled       -1.23 1.12     1.00            1.00     0.56
## means_y2              -0.03 0.11     0.56            0.56     1.00
## means_y2_scaled       -0.26 0.86     0.56            0.56     1.00
## GenderGapIndex         0.00 1.00    -0.38           -0.38    -0.04
## GenderGapIndex_scaled  0.00 1.00    -0.38           -0.38    -0.04
## diff_score            -0.13 0.12     0.66            0.66    -0.24
## diff_score_scaled     -0.97 0.95     0.66            0.66    -0.24
##                       means_y2_scaled GenderGapIndex GenderGapIndex_scaled
## means_y1                         0.56          -0.38                 -0.38
## means_y1_scaled                  0.56          -0.38                 -0.38
## means_y2                         1.00          -0.04                 -0.04
## means_y2_scaled                  1.00          -0.04                 -0.04
## GenderGapIndex                  -0.04           1.00                  1.00
## GenderGapIndex_scaled           -0.04           1.00                  1.00
## diff_score                      -0.24          -0.41                 -0.41
## diff_score_scaled               -0.24          -0.41                 -0.41
##                       diff_score diff_score_scaled
## means_y1                    0.66              0.66
## means_y1_scaled             0.66              0.66
## means_y2                   -0.24             -0.24
## means_y2_scaled            -0.24             -0.24
## GenderGapIndex             -0.41             -0.41
## GenderGapIndex_scaled      -0.41             -0.41
## diff_score                  1.00              1.00
## diff_score_scaled           1.00              1.00
```

```r
round(fit_E.z$SDs,2)
```

```
##         SD_y1         SD_y2     SD_pooled SD_diff_score            VR 
##          0.15          0.11          0.13          0.12          1.68
```

### Variance heterogeneity test


```r
export(t(data.frame(fit_E.z$re_cov_test)),
       "../results/E.z_ml_var_test.xlsx",
       overwrite=T)
round(fit_E.z$re_cov_test,3)
```

```
## RE_cov RE_cor  Chisq     Df      p 
##  0.004  0.357  5.267  1.000  0.022
```

### Component correlation


```r
export(rownames_to_column(data.frame(fit_E.z$ddsc_sem_fit$variance_test)),
       "../results/E.z_ml_comp_cor.xlsx",
       overwrite=T)
round(fit_E.z$ddsc_sem_fit$variance_test,3)
```

```
##             est    se     z pvalue ci.lower ci.upper
## cov_y1y2  0.535 0.154 3.473  0.001    0.233    0.836
## var_y1    1.228 0.246 5.000  0.000    0.747    1.709
## var_y2    0.732 0.146 5.000  0.000    0.445    1.019
## var_diff  0.496 0.243 2.044  0.041    0.020    0.972
## var_ratio 1.678 0.392 4.281  0.000    0.910    2.446
## cor_y1y2  0.564 0.096 5.847  0.000    0.375    0.753
```

### Deconstructing results


```r
export(rownames_to_column(data.frame(fit_E.z$results)),
       "../results/E.z_ml_results.xlsx",
       overwrite=T)
round(fit_E.z$results,3)
```

```
##                            estimate    SE     df t.ratio p.value
## r_xy1y2                      -0.409 0.121 40.404  -3.395   0.002
## w_11                         -0.056 0.019 45.963  -2.860   0.006
## w_21                         -0.004 0.016 46.695  -0.275   0.784
## r_xy1                        -0.379 0.133 45.963  -2.860   0.006
## r_xy2                        -0.040 0.144 46.695  -0.275   0.784
## b_11                         -0.428 0.150 45.963  -2.860   0.006
## b_21                         -0.034 0.125 46.695  -0.275   0.784
## main_effect                  -0.030 0.016 46.921  -1.846   0.071
## moderator_effect             -0.120 0.015 41.959  -7.937   0.000
## interaction                  -0.051 0.015 40.404  -3.395   0.002
## q_b11_b21                    -0.423    NA     NA      NA      NA
## q_rxy1_rxy2                  -0.359    NA     NA      NA      NA
## cross_over_point             -2.345    NA     NA      NA      NA
## interaction_vs_main           0.021 0.019 43.941   1.084   0.284
## interaction_vs_main_bscale    0.162 0.150 43.941   1.084   0.284
## interaction_vs_main_rscale    0.130 0.179 44.869   0.727   0.471
## dadas                        -0.009 0.033 46.695  -0.275   0.608
## dadas_bscale                 -0.069 0.250 46.695  -0.275   0.608
## dadas_rscale                 -0.079 0.287 46.695  -0.275   0.608
## abs_diff                      0.051 0.015 40.404   3.395   0.001
## abs_sum                       0.060 0.033 46.921   1.846   0.036
## abs_diff_bscale               0.393 0.116 40.404   3.395   0.001
## abs_sum_bscale                0.462 0.250 46.921   1.846   0.036
## abs_diff_rscale               0.340 0.115 39.731   2.959   0.003
## abs_sum_rscale                0.419 0.252 47.019   1.664   0.051
```

### Multi-level model output


```r
# cross-level interaction model
summary(fit_E.z$model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: model_formula
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 2400993
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0379 -0.6521  0.1049  0.7263  2.9885 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  COUNTRY  (Intercept) 0.012279 0.11081      
##           sex.c       0.008795 0.09378  0.27
##  Residual             0.992794 0.99639      
## Number of obs: 848115, groups:  COUNTRY, 50
## 
## Fixed effects:
##                      Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept)          -0.15109    0.10817 46.45221  -1.397  0.16914    
## sex.c                -0.11977    0.01509 41.95950  -7.937  6.9e-10 ***
## GenderGapIndex       -0.03001    0.01626 46.92097  -1.846  0.07116 .  
## sex.ratio             0.11146    0.22426 46.51956   0.497  0.62151    
## sex.c:GenderGapIndex -0.05108    0.01505 40.40401  -3.395  0.00155 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) sex.c  GndrGI sex.rt
## sex.c        0.053                     
## GendrGpIndx -0.061 -0.002              
## sex.ratio   -0.989 -0.018  0.060       
## sx.c:GndrGI  0.002 -0.057  0.231 -0.002
```

```r
# reduced model without the predictor
summary(fit_E.z$reduced_model)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: E.z ~ sex.c + sex.ratio + (sex.c | COUNTRY)
##    Data: data
## Control: lme4::lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: 2400992
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0379 -0.6521  0.1049  0.7263  2.9855 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  COUNTRY  (Intercept) 0.01295  0.1138       
##           sex.c       0.01145  0.1070   0.36
##  Residual             0.99279  0.9964       
## Number of obs: 848115, groups:  COUNTRY, 50
## 
## Fixed effects:
##             Estimate Std. Error       df t value Pr(>|t|)    
## (Intercept) -0.16389    0.10800 47.47041  -1.517    0.136    
## sex.c       -0.12314    0.01677 44.65865  -7.345 3.29e-09 ***
## sex.ratio    0.13754    0.22381 47.45481   0.615    0.542    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##           (Intr) sex.c 
## sex.c      0.067       
## sex.ratio -0.988 -0.019
```

## COUNTRY-level path model

### Fit the model

The model is already stored within the multi-level model object. 


```r
fit_E.z_sem<-fit_E.z$ddsc_sem_fit
```

### Results


```r
export(rownames_to_column(data.frame(fit_E.z_sem$results)),
       "../results/E.z_sem_results.xlsx",
       overwrite=T)
round(fit_E.z_sem$results,3)
```

```
##                                    est    se      z pvalue ci.lower ci.upper
## r_xy1_y2                        -0.412 0.129 -3.194  0.001   -0.664   -0.159
## r_xy1                           -0.382 0.131 -2.921  0.003   -0.638   -0.126
## r_xy2                           -0.040 0.141 -0.286  0.775   -0.317    0.237
## b_11                            -0.427 0.146 -2.921  0.003   -0.714   -0.141
## b_21                            -0.035 0.122 -0.286  0.775   -0.274    0.204
## b_10                            -1.233 0.145 -8.514  0.000   -1.517   -0.949
## b_20                            -0.262 0.121 -2.168  0.030   -0.499   -0.025
## res_cov_y1_y2                    0.520 0.144  3.611  0.000    0.238    0.802
## diff_b10_b20                    -0.971 0.122 -7.985  0.000   -1.210   -0.733
## diff_b11_b21                    -0.392 0.123 -3.194  0.001   -0.633   -0.152
## diff_rxy1_rxy2                  -0.341 0.123 -2.777  0.005   -0.582   -0.100
## q_b11_b21                       -0.422 0.145 -2.911  0.004   -0.706   -0.138
## q_rxy1_rxy2                     -0.362 0.133 -2.718  0.007   -0.623   -0.101
## cross_over_point                -2.474 0.834 -2.966  0.003   -4.110   -0.839
## sum_b11_b21                     -0.462 0.240 -1.927  0.054   -0.932    0.008
## main_effect                     -0.231 0.120 -1.927  0.054   -0.466    0.004
## interaction_vs_main_effect       0.161 0.152  1.064  0.287   -0.136    0.459
## diff_abs_b11_abs_b21             0.392 0.123  3.194  0.001    0.152    0.633
## abs_diff_b11_b21                 0.392 0.123  3.194  0.001    0.152    0.633
## abs_sum_b11_b21                  0.462 0.240  1.927  0.027   -0.008    0.932
## dadas                           -0.070 0.244 -0.286  0.612   -0.548    0.409
## q_r_equivalence                  0.362 0.133  2.718  0.997       NA       NA
## q_b_equivalence                  0.422 0.145  2.911  0.998       NA       NA
## cross_over_point_equivalence     2.474 0.834  2.966  0.998       NA       NA
## cross_over_point_minimal_effect  2.474 0.834  2.966  0.002       NA       NA
```


# Plotting the results


```r
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
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
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
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```r
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
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

```r
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
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-4.png)<!-- -->

```r
# combine component-specific predictions

p13.E.z<-
  ggarrange(p1.E.z, p3.E.z,common.legend = T,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p13.E.z
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-5.png)<!-- -->

```r
# combine difference score predictions

p24.E.z<-
  ggarrange(p2.E.z, p4.E.z,
            ncol=2, nrow=1,widths=c(4,1.4)
  )

p24.E.z
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-6.png)<!-- -->

```r
pall.E.z<-
  ggarrange(p13.E.z,p24.E.z,align = "hv",
            ncol=1,nrow=2,heights=c(2,1))
pall.E.z
```

![](Extraversion_Analysis_files/figure-html/unnamed-chunk-16-7.png)<!-- -->

```r
png(filename = 
      "../results/pall.E.z.png",
    units = "cm",
    width = 21.0,height=29.7*(4/5),res = 600)
pall.E.z
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
## R version 4.3.0 (2023-04-21 ucrt)
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
##  [1] finalfit_1.0.6    emmeans_1.8.6     MetBrewer_0.2.0   ggpubr_0.6.0     
##  [5] ggplot2_3.4.2     tibble_3.2.1      dplyr_1.1.2       rio_0.5.29       
##  [9] lmerTest_3.1-3    lme4_1.1-33       Matrix_1.5-4      multid_0.8.0.9000
## [13] knitr_1.42        rmarkdown_2.21   
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.2.0    farver_2.1.1        fastmap_1.1.1      
##  [4] pROC_1.18.2         digest_0.6.31       estimability_1.4.1 
##  [7] lifecycle_1.0.3     survival_3.5-5      magrittr_2.0.3     
## [10] compiler_4.3.0      rlang_1.1.1         sass_0.4.6         
## [13] tools_4.3.0         utf8_1.2.3          yaml_2.3.7         
## [16] data.table_1.14.8   ggsignif_0.6.4      labeling_0.4.2     
## [19] mnormt_2.1.1        curl_5.0.0          plyr_1.8.8         
## [22] abind_1.4-5         withr_2.5.0         foreign_0.8-84     
## [25] purrr_1.0.1         numDeriv_2016.8-1.1 stats4_4.3.0       
## [28] grid_4.3.0          fansi_1.0.4         lavaan_0.6-15      
## [31] xtable_1.8-4        colorspace_2.1-0    mice_3.15.0        
## [34] scales_1.2.1        iterators_1.0.14    MASS_7.3-58.4      
## [37] cli_3.6.1           mvtnorm_1.1-3       crayon_1.5.2       
## [40] generics_0.1.3      rstudioapi_0.14     readxl_1.4.2       
## [43] minqa_1.2.5         cachem_1.0.8        splines_4.3.0      
## [46] parallel_4.3.0      cellranger_1.1.0    vctrs_0.6.2        
## [49] boot_1.3-28.1       glmnet_4.1-7        jsonlite_1.8.4     
## [52] carData_3.0-5       car_3.1-2           hms_1.1.3          
## [55] rstatix_0.7.2       foreach_1.5.2       tidyr_1.3.0        
## [58] jquerylib_0.1.4     glue_1.6.2          nloptr_2.0.3       
## [61] codetools_0.2-19    cowplot_1.1.1       stringi_1.7.12     
## [64] gtable_0.3.3        shape_1.4.6         quadprog_1.5-8     
## [67] munsell_0.5.0       pillar_1.9.0        htmltools_0.5.5    
## [70] R6_2.5.1            pbivnorm_0.6.0      evaluate_0.21      
## [73] lattice_0.21-8      highr_0.10          haven_2.5.2        
## [76] backports_1.4.1     openxlsx_4.2.5.2    broom_1.0.4        
## [79] bslib_0.4.2         Rcpp_1.0.10         zip_2.3.0          
## [82] gridExtra_2.3       nlme_3.1-162        xfun_0.39          
## [85] forcats_1.0.0       pkgconfig_2.0.3
```
