# Illustrating the influence of component correlation

library(ggplot2)
library(MetBrewer)
library(metafor)

# define a function for difference score correlation
diff_score_corr<-
  function(SD_Y1,SD_Y2,r_Y1Y2,r_XY1,r_XY2){
    ds_corr<-
      (r_XY1*SD_Y1-r_XY2*SD_Y2)/
      sqrt((SD_Y1^2+SD_Y2^2-2*r_Y1Y2*SD_Y1*SD_Y2))
    ds_corr<-unname(ds_corr)
    
    ds_sd<-
      sqrt((SD_Y1^2+SD_Y2^2-2*r_Y1Y2*SD_Y1*SD_Y2))
    ds_sd<-unname(ds_sd)
    output<-c(ds_corr=ds_corr,ds_sd=ds_sd)
    return(output)
  }

# test if it works
a<-c(SD_Y1=1,SD_Y2=1,r_Y1Y2=-0.99,r_XY1=0.10,r_XY2=(-0.10))


diff_score_corr(SD_Y1=a["SD_Y1"],SD_Y2=a["SD_Y2"],
                r_Y1Y2=a["r_Y1Y2"],
                r_XY1=a["r_XY1"],r_XY2=a["r_XY2"])

# generate a dataset with homogeneous unit variances
# constant slopes and increasing degree of component correlation
# from 0 to .98

df<-
  data.frame(
    SD_Y1=1,
    SD_Y2=1,
    r_Y1Y2=seq(from=0.00,to=0.98,by=0.01),
    r_XY1=0.10,
    r_XY2=(-0.10)
  )

# save difference score correlation estimates for all combinations
estimates<-list()

for (i in 1:nrow(df)){
  estimates[[i]]<-
    diff_score_corr(SD_Y1=df[i,"SD_Y1"],SD_Y2=df[i,"SD_Y2"],
                    r_Y1Y2=df[i,"r_Y1Y2"],
                    r_XY1=df[i,"r_XY1"],r_XY2=df[i,"r_XY2"])
}

# save to a data frame
estimates.df<-do.call(rbind,estimates)
head(estimates.df)

# plot the results
# combine generated data and difference score correlation estimates

plot.df<-
  cbind(df,estimates.df)
head(plot.df)
tail(plot.df,n=10)

# produce the plot
plot<-
  ggplot(plot.df,aes(x=r_Y1Y2,y=ds_corr))+
  geom_line(color=met.brewer("Archambault")[2],linewidth=2)+
  geom_line(aes(y=ds_sd),
            color=met.brewer("Archambault")[1],linewidth=2,linetype=1)+
  geom_line(aes(y=transf.rtoz(r_XY1)-transf.rtoz(r_XY2)),
            color=met.brewer("Archambault")[6],linewidth=1.5,linetype=1)+
  #geom_line(aes(y=ds_corr/(r_XY1-r_XY2)),
  #          color=met.brewer("Archambault")[6],size=1.5,linetype=2)+
  ylab("Estimate")+
  xlab(bquote(italic(r)["Y1,Y2"]))+
  geom_text(aes(x=0.25,y=1.35,label="italic('SD')[Y1-Y2]"),parse = TRUE,
            color=met.brewer("Archambault")[1])+
  geom_text(aes(x=0.9,y=1.05,label="italic('r')['X,Y1-Y2']"),parse = TRUE,
            color=met.brewer("Archambault")[2])+
  #geom_text(aes(x=0.35,y=.30,label="italic('r')['X,Y1']-italic('r')['X,Y2']"),
  #          parse = TRUE,
  #          color=met.brewer("Archambault")[6])+
  geom_text(aes(x=0.15,y=.25,label="Effect~size~italic(q)"),
            parse = TRUE,
            color=met.brewer("Archambault")[6])+
  scale_y_continuous(breaks=c(0.2,0.5,1.0))+
  theme(
        text=element_text(size=12,  family="sans"),
        panel.background = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2,color="black", linetype = 3))
plot  

# save the file
ggplot2::ggsave(plot,path = "illustrations",
                filename = "Figure_3_component_correlation_influence.jpg",
                device = "jpg",units = "cm",
                width = (21.0/2),
                height = (21.0/2),dpi = 300)


