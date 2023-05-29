# Demonstrate the patterns that appear as correlation with a difference score
# from the point of view of divergence, convergence, and their mix


# Load packages
library(ggplot2)
library(ggpubr)
library(multid)
library(MASS)
library(MetBrewer)

# Parameters that apply for all scenarios

n=300

# Type A1: opposite but equal associations with Y1 and Y2, divergence

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
str(plot.dat.A)

plot.A<-
  ggplot(plot.dat.A,
       aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,size=3)+
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
  coord_cartesian(ylim=c(-4,5))
plot.A



# Type A2: opposite but equal associations with Y1 and Y2, convergence

# correlation matrix (Y1,Y2,X)

cor.A2<-matrix(c(
  1.00,0.50,0.25,
  0.50,1.00,-.25,
  0.25,-.25,1.00
),ncol=3,byrow=T)
cor.A2

dat.A2<-
  data.frame(mvrnorm(n=n,
                     mu = c(-1,1,0),
                     Sigma = cor.A2,empirical = T))
dat.A2$diff<-dat.A2[,1]-dat.A2[,2]
names(dat.A2)<-c("Y1","Y2","X","diff")
cor(dat.A2)


## Visualize

plot.dat.A2<-data.frame(
  Predictor=rep(dat.A2$X,times=3),
  DV=c(dat.A2$Y1,dat.A2$Y2,dat.A2$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)
str(plot.dat.A2)

plot.A2<-
  ggplot(plot.dat.A2,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,size=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("turquoise3","orangered2","black"))+
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
  coord_cartesian(ylim=c(-4,5))
plot.A2

# Type A3: opposite but equal associations with Y1 and Y2, mixture of convergence and divergence

# correlation matrix (Y1,Y2,X)

cor.A3<-matrix(c(
  1.00,0.50,0.25,
  0.50,1.00,-.25,
  0.25,-.25,1.00
),ncol=3,byrow=T)
cor.A3

dat.A3<-
  data.frame(mvrnorm(n=n,
                     mu = c(0,0,0),
                     Sigma = cor.A3,empirical = T))
dat.A3$diff<-dat.A3[,1]-dat.A3[,2]
names(dat.A3)<-c("Y1","Y2","X","diff")
cor(dat.A3)


## Visualize

plot.dat.A3<-data.frame(
  Predictor=rep(dat.A3$X,times=3),
  DV=c(dat.A3$Y1,dat.A3$Y2,dat.A3$diff),
  type=rep(c("Mean for Men","Mean for Women","Difference = Men - Women"),each=n)
)
str(plot.dat.A3)

plot.A3<-
  ggplot(plot.dat.A3,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,size=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("turquoise3","orangered2","black"))+
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
  coord_cartesian(ylim=c(-4,5))
plot.A3



# combined plot from data

plot.dat.comb<-rbind(
  plot.dat.A,
  plot.dat.A2,
  plot.dat.A3
)

plot.dat.comb$panel<-
  c(rep(c("Divergence","Convergence","Mixture"),each=3*n))

# reorder
plot.dat.comb$panel <- factor(plot.dat.comb$panel,
                              levels = c("Divergence","Convergence","Mixture"))

plot.comb<-
  ggplot(plot.dat.comb,
         aes(x=Predictor,y=DV,fill=type,color=type,linetype=type))+
  geom_smooth(method="lm",formula="y~x",se=F,size=3)+
  stat_cor(aes(color = type),r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  scale_color_manual(values=c("black","turquoise3","orangered2"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type: ",
       linetype = "DV type: ",
       fill = "DV type: ")+
  xlab("Gender-equality")+
  #coord_cartesian(ylim=c(-2.5,5))+
  facet_wrap(~panel,nrow = 1)+
  guides(fill="none",linetype="none")#+
  #geom_segment(mapping=aes(x=0, y=-0.75, xend=0, yend=0.75),
  #             arrow=arrow(ends='both'), size=0.75, color="black")
plot.comb


ggplot2::ggsave(plot.comb,path = "illustrations",
                filename = "Figure_Divergence_Convergence.png",
                device = "png",units = "cm",
                width = 21*(4/4),
                height = 29.7/2,dpi = 300)


# produce a reduced plot with only the black lines


plot.comb.red<-
  ggplot(plot.dat.comb[plot.dat.comb$type=="Difference = Men - Women",],
         aes(x=Predictor,y=DV))+
  geom_smooth(method="lm",formula="y~x",se=F,linewidth=3,color="black")+
  stat_cor(r.digits=2,
           p.digits=3,label.x = c(-2.5,-2.5,-2.5),
           label.y = c(5,4.5,4),
           cor.coef.name="r",r.accuracy=0.01,p.accuracy=0.001)+
  #scale_color_manual(values=c("turquoise3","orangered2","black"))+
  #scale_linetype_manual(values=c("solid","solid","solid"))+
  #geom_point(size=1,alpha=0.2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white"))+
  labs(color = "DV type: ",
       linetype = "DV type: ",
       fill = "DV type: ")+
  #coord_cartesian(ylim=c(-2,5))+
  facet_wrap(~panel,nrow = 1)+
  xlab("Gender-equality")+
  ylab("Difference: Men - Women")+
  guides(fill="none",linetype="none")#+
#geom_segment(mapping=aes(x=0, y=-0.75, xend=0, yend=0.75),
#             arrow=arrow(ends='both'), size=0.75, color="black")
plot.comb.red


ggplot2::ggsave(plot.comb.red,path = "illustrations",
                filename = "Figure_Divergence_Convergence_no_slopes.png",
                device = "png",units = "cm",
                width = 21*(4/4),
                height = 29.7/2,dpi = 300)
