library(ggplot2)
library(metafor)
library(MetBrewer)

# correlations between mean-levels
r_y1_y2<-round(c(-.95,-.90,-.85,-.80,-.75,-.70,-.60,-.50,-.40,-.30,-.10,
                 .00,.10,.30,.40,.50,.60,.70,.75,.80,.85,.90,.95),2)
# SD of women's mean level
sd_y2_vals<-seq(from=0.10,to=10.0,by=0.01)
# SD of men's mean-level (kept at constant 1)
sd_y1_vals<-1
# component-specific predictions
r_x_y1=.10
r_x_y2=-.10

# compile all combinations of these variable values to the same data frame
simd<-expand.grid(r_y1_y2,sd_y1_vals,sd_y2_vals,r_x_y1,r_x_y2)
head(simd)
# name the columns
names(simd)<-c("r_y1_y2","sd_y1_vals","sd_y2_vals","r_x_y1","r_x_y2")
# calculate the standardized (difference score SD)
simd$sd_diff_score<-
  sqrt(-2*simd$r_y1_y2*simd$sd_y1_vals*simd$sd_y2_vals+
         simd$sd_y1_vals^2+simd$sd_y2_vals^2)
# calculate the values in the nominator of the difference score correlation equation
simd$nominator<-(simd$r_x_y1*simd$sd_y1_vals-simd$r_x_y2*simd$sd_y2_vals)

# calculate observed difference score correlation
simd$r_diff_score<-simd$nominator/simd$sd_diff_score

# calculate q-estimates (is same for all scenarios)
simd$q=transf.rtoz(simd$r_x_y1)-transf.rtoz(simd$r_x_y2)

# produce the figure
fig_total_bias<-
  ggplot(simd,aes(y=r_diff_score/q,
                  x=sd_y2_vals/sd_y1_vals,
                  color=as.factor(r_y1_y2)))+
  geom_line(linewidth=2)+
  scale_color_manual(values=met.brewer("Troy",
                                       n=length(unique(r_y1_y2))))+
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


# save the file
ggplot2::ggsave(fig_total_bias,path = "illustrations",
                filename = "Figure_4_VR_and_CC_influence.jpg",
                device = "jpg",units = "cm",
                width = 21,
                height = 29.7*(3/4),dpi = 300)


