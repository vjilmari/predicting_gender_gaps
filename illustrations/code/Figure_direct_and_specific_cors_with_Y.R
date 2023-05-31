# Demonstrate the patterns that appear as positive correlations with the difference score

# Load packages
library(ggplot2)
library(ggpubr)
library(multid)
library(MASS)
library(MetBrewer)

# Parameters that apply for all scenarios

n=300

# Type A: opposite but equal associations with Y1 and Y2

# correlation matrix (Y1,Y2,X)

cor.A<-matrix(c(
  1.00,0.50,0.25,
  0.50,1.00,-.25,
  0.25,-.25,1.00
),ncol=3,byrow=T)
cor.A

dat.A<-
  data.frame(mvrnorm(n=n,
                     mu = c(1,-1,0),
                     Sigma = cor.A,empirical = T))
dat.A$diff<-dat.A[,1]-dat.A[,2]
names(dat.A)<-c("Y1","Y2","X","diff")
cor(dat.A)


## Visualize

plot.dat.A<-data.frame(
  Predictor=rep(dat.A$X,times=3),
  DV=c(dat.A$Y1,dat.A$Y2,dat.A$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)


plot.A<-
  ggplot(plot.dat.A,
       aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  #scale_color_manual(values=c(met.brewer("Archambault")[c(6,2)],"black"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type",
       linetype = "DV type",
       fill = "DV type")+
  xlab("Gender-equality")+
  coord_cartesian(ylim=c(-2.5,5))
plot.A

# Type B: Boundary condition for Y2, association only between Y1 and X

cor.B<-matrix(c(
  1.00,0.50,0.50,
  0.50,1.00,0.00,
  0.50,0.00,1.00
),ncol=3,byrow=T)
cor.B

dat.B<-
  data.frame(mvrnorm(n=n,
                     mu = c(1,-1,0),
                     Sigma = cor.B,empirical = T))
dat.B$diff<-dat.B[,1]-dat.B[,2]
names(dat.B)<-c("Y1","Y2","X","diff")
cor(dat.B)


## Visualize

plot.dat.B<-data.frame(
  Predictor=rep(dat.B$X,times=3),
  DV=c(dat.B$Y1,dat.B$Y2,dat.B$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)


plot.B<-
  ggplot(plot.dat.B,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type",
       linetype = "DV type",
       fill = "DV type")+
  xlab("Gender-equality")+
  coord_cartesian(ylim=c(-2.5,5))
plot.B


# Type C: Heterogeneity in magnitudes

cor.C<-matrix(c(
  1.00,0.50,0.75,
  0.50,1.00,0.25,
  0.75,0.25,1.00
),ncol=3,byrow=T)
cor.C

dat.C<-
  data.frame(mvrnorm(n=n,
                     mu = c(1,-1,0),
                     Sigma = cor.C,empirical = T))
dat.C$diff<-dat.C[,1]-dat.C[,2]
names(dat.C)<-c("Y1","Y2","X","diff")
cor(dat.C)


## Visualize

plot.dat.C<-data.frame(
  Predictor=rep(dat.C$X,times=3),
  DV=c(dat.C$Y1,dat.C$Y2,dat.C$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)

plot.C<-
  ggplot(plot.dat.C,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type",
       linetype = "DV type",
       fill = "DV type")+
  xlab("Gender-equality")+
  coord_cartesian(ylim=c(-2.5,5))
plot.C


# Type D

cor.D<-matrix(c(
  1.00,0.90,0.111,
  0.90,1.00,-.111,
  0.111,-.111,1.00
),ncol=3,byrow=T)
cor.D

dat.D<-
  data.frame(mvrnorm(n=n,
                     mu = c(1,-1,0),
                     Sigma = cor.D,empirical = T))
dat.D$diff<-dat.D[,1]-dat.D[,2]
names(dat.D)<-c("Y1","Y2","X","diff")
cor(dat.D)
round(c(cor(dat.D)[3,4],
        cor(dat.D)[1,3],
        cor(dat.D)[2,3]),2)

## Visualize

plot.dat.D<-data.frame(
  Predictor=rep(dat.D$X,times=3),
  DV=c(dat.D$Y1,dat.D$Y2,dat.D$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)

plot.D<-
  ggplot(plot.dat.D,
         aes(x=Predictor,y=DV,fill=type,
             color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "top",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type",
       linetype = "DV type",
       fill = "DV type")+
  xlab("Gender-equality")+
  coord_cartesian(ylim=c(-2.5,5))
plot.D

# combined plot from data

plot.dat.comb<-rbind(
  plot.dat.A,
  plot.dat.B,
  plot.dat.C,
  plot.dat.D
)

plot.dat.comb$panel<-
  c(rep(c("Type A","Type B","Type C"),each=3*n),
    rep("Type D",3*n))

plot.comb<-
  ggplot(plot.dat.comb,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type: ",
       linetype = "DV type: ",
       fill = "DV type: ")+
  xlab("Gender-equality")+
  facet_wrap(~panel,nrow = 1)+
  guides(fill="none",linetype="none")
plot.comb


# save the plot
ggplot2::ggsave(plot.comb,path = "illustrations",
                filename = "Figure_Types_ABCD.png",
                device = "png",units = "cm",
                width = 21*(4/4),
                height = 29.7/2,dpi = 300)

# reduced plot (only difference score slopes)

plot.comb.red<-
  ggplot(plot.dat.comb[plot.dat.comb$type=="Difference = Men - Women",],
         aes(x=Predictor,y=DV))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3,color="black")+
  stat_cor(r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),
           label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type: ",
       linetype = "DV type: ",
       fill = "DV type: ")+
  coord_cartesian(ylim=c(-2,5))+
  facet_wrap(~panel,nrow = 1)+
  xlab("Gender-equality")+
  ylab("Difference: Men - Women")+
  guides(fill="none",linetype="none")
plot.comb.red

# Saving commented out

#ggplot2::ggsave(plot.comb.red,path = "illustrations",
#                filename = "Figure_Types_ABCD_no_slopes.png",
#                device = "png",units = "cm",
#                width = 21*(4/4),
#                height = 29.7/2,dpi = 300)
