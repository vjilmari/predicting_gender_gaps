library(ggplot2)
library(MetBrewer)

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

sd_a<-sqrt((6/7)+(2*sqrt(2))/7)
sd_b<-sqrt((sd_a^2/2))


# try to find pairs of SDs that hold diff_score_sd/var
# and r1_r2 at constants (1 and .5)

a<-c(SD_Y1=sd_a,SD_Y2=sd_b,
     r_Y1Y2=0.50,r_XY1=0.25,r_XY2=(-0.25))


diff_score_corr(SD_Y1=a["SD_Y1"],SD_Y2=a["SD_Y2"],
                r_Y1Y2=a["r_Y1Y2"],
                r_XY1=a["r_XY1"],r_XY2=a["r_XY2"])

#SD_Y2=seq(from=0.01,to=1,by=0.01)
#SD_Y2=seq(from=0.20,to=5,by=0.01)
SD_Y2=seq(from=0.01,to=2/sqrt(3),by=0.01)
SD_Y1<-ifelse(SD_Y2<=1,
              (.5)*(SD_Y2+sqrt(4-3*SD_Y2^2)),
              (.5)*(SD_Y2-sqrt(4-3*SD_Y2^2)))
SD_Y1
cbind(SD_Y2,SD_Y1)
SD_Y1=(.5)*(sqrt(4-3*SD_Y2^2)+SD_Y2)



df<-
  data.frame(
    SD_Y1=SD_Y1,
    SD_Y2=SD_Y2,
    r_Y1Y2=.5,
    r_XY1=0.25,
    r_XY2=(-0.25)
  )

estimates<-list()

for (i in 1:nrow(df)){
  estimates[[i]]<-
    diff_score_corr(SD_Y1=df[i,"SD_Y1"],SD_Y2=df[i,"SD_Y2"],
                    r_Y1Y2=df[i,"r_Y1Y2"],
                    r_XY1=df[i,"r_XY1"],r_XY2=df[i,"r_XY2"])
}
estimates
estimates.df<-do.call(rbind,estimates)
estimates.df

plot.df<-
  cbind(df,estimates.df)
head(plot.df)
tail(plot.df,n=10)
plot.df$VR=plot.df$SD_Y1^2/plot.df$SD_Y2^2
plot.df$VR
plot.df

plot<-
  ggplot(plot.df[plot.df$VR<15,],aes(x=VR,y=ds_corr))+
  geom_line(color=met.brewer("Archambault")[2],linewidth=2)+
  geom_line(aes(y=ds_sd),
            color=met.brewer("Archambault")[1],linewidth=2,linetype=1)+
  geom_line(aes(y=r_XY1-r_XY2),
            color=met.brewer("Archambault")[6],linewidth=1.5,linetype=2)+
  #geom_line(aes(y=ds_corr/(r_XY1-r_XY2)),
  #          color=met.brewer("Archambault")[6],size=1.5,linetype=2)+
  ylab("Estimate")+
  #xlab(bquote(italic(r)["Y1,Y2"]))+
  xlab(bquote(italic({SD^2})["Y1"]/italic({SD^2})["Y2"]))+
  geom_text(aes(x=8.50,y=0.9,label="italic('SD')[Y1-Y2]"),parse = TRUE,
            color=met.brewer("Archambault")[1])+
  geom_text(aes(x=8.50,y=0.30,label="italic('r')['X,Y1-Y2']"),parse = TRUE,
            color=met.brewer("Archambault")[2])+
  geom_text(aes(x=8.50,y=.60,label="italic('r')['X,Y1']-italic('r')['X,Y2']"),
            parse = TRUE,
            color=met.brewer("Archambault")[6])+
  ylim(c(0.0,1))+
  #scale_x_continuous(trans="log2")+
  theme(
        text=element_text(size=12,  family="sans"),
        panel.background = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2,color="black", linetype = 3))
plot  

png(filename = 
      "illustrations/Figure_VR_influence.png",
    units = "cm",
    height = (21.0/2),width=(21.0/2),res = 600)
plot
dev.off()


