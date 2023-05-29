#library(rio)
#library(dplyr)
#library(multid)
#library(lmerTest)
#library(emmeans)
library(ggplot2)
library(metafor)
library(MetBrewer)
#library(ggpubr)

# cf=women's mean-level
# rv=men's mean-level
# x=predictor

# correlations between mean-levels
#r_rv_cf<-round(seq(from=-.9,to=.9,by=0.3),1)
r_rv_cf<-round(c(-.95,-.90,-.85,-.80,-.75,-.70,-.60,-.50,-.40,-.30,-.10,
                 .00,.10,.30,.40,.50,.60,.70,.75,.80,.85,.90,.95),2)
# SD of women's mean level
sd_cf_vals<-seq(from=0.10,to=10.0,by=0.01)
# SD of men's mean-level (kept at constant 1)
sd_rv_vals<-1
# component-specific predictions
r_x_rv=.10
r_x_cf=-.10
# this should yield a round number
transf.rtoz(r_x_rv)-transf.rtoz(r_x_cf)

# compile all combinations of these variable values to the same data frame
simd<-expand.grid(r_rv_cf,sd_rv_vals,sd_cf_vals,r_x_rv,r_x_cf)
head(simd)
# name the columns
names(simd)<-c("r_rv_cf","sd_rv_vals","sd_cf_vals","r_x_rv","r_x_cf")
# calculate the standardized (difference score SD)
simd$sd_diff_score<-
  sqrt(-2*simd$r_rv_cf*simd$sd_rv_vals*simd$sd_cf_vals+
         simd$sd_rv_vals^2+simd$sd_cf_vals^2)
# calculate the values in the nominator of the difference score correlation equation
simd$nominator<-(simd$r_x_rv*simd$sd_rv_vals-simd$r_x_cf*simd$sd_cf_vals)
hist(simd$nominator)
# calculate observed difference score correlation
simd$r_diff_score<-simd$nominator/simd$sd_diff_score
# calculate q-estimates (is same for all scenarios)
simd$q=transf.rtoz(simd$r_x_rv)-transf.rtoz(simd$r_x_cf)

met.brewer("Isfahan1")
met.brewer("Isfahan1", n=length(unique(r_rv_cf)))

fig_total_bias<-
  ggplot(simd,aes(y=r_diff_score/q,
                  x=sd_cf_vals/sd_rv_vals,
                  color=as.factor(r_rv_cf)))+
  geom_line(linewidth=2)+
  scale_color_manual(values=met.brewer("Troy",
                                       n=length(unique(r_rv_cf))))+
  geom_hline(yintercept=1,linetype=2)+
  geom_vline(xintercept=1,linetype=2)+
  #scale_y_continuous(trans='log2')+
  scale_x_continuous(trans='log2')+
  scale_y_continuous(breaks=c(0.5,1,1.5,2,2.5,3))+
  ylab("Ratio of difference score correlation to Cohen's q")+
  xlab("Ratio of men's mean-level SD to women's mean-level SD")+
  guides(color=guide_legend(title=
                              "Correlation between\nmean-levels"))+
  theme(#legend.position = "right",
        legend.title.align=0,
        legend.position=c(0.8,0.6))

fig_total_bias
png("illustrations/Figure_diff_r_to_q.png",width = 21,
    height=29.7*(3/4),units = "cm",res = 300)
fig_total_bias
dev.off()