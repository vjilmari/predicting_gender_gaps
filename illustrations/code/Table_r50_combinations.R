# exemplary table of constituent combinations leading to r = .50
# specifically for GEP
# this means positive correlations going to extreme values
# illustrate the independence of the components for type B and C

library(metafor)
library(finalfit)
library(stringr)
library(dplyr)
library(flextable)
library(rempsyc)


# Type A patterns
r_y1y2_A_eq=c(.00,.25,.50,.65,.80,.95)
s_y1_A_eq=rep(1,length(r_y1y2_A_eq))
s_y2_A_eq=rep(1,length(r_y1y2_A_eq))

## calculate denominators

s_y1_y2_A_eq=sqrt(s_y1_A_eq^2+s_y2_A_eq^2-2*r_y1y2_A_eq*s_y1_A_eq*s_y2_A_eq)
s_y1_y2_A_eq

## r_xy1 is -1*r_xy2 in Type A patterns
## This gives: r_xy1 = 

r_xy1_A_eq<-
  sqrt((-2)*r_y1y2_A_eq*s_y1_A_eq*s_y2_A_eq+s_y1_A_eq^2+s_y2_A_eq^2)/
  (2*(s_y1_A_eq+s_y2_A_eq))
r_xy1_A_eq
r_xy2_A_eq=-1*r_xy1_A_eq

q_A_eq=transf.rtoz(r_xy1_A_eq)-transf.rtoz(r_xy2_A_eq)
q_A_eq

pre_std_A_eq=(r_xy1_A_eq-r_xy2_A_eq)/
  sqrt(1^2+1^2-2*r_y1y2_A_eq*1*1)
pre_std_A_eq

A_eq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_A_eq,2),
    SD_y2=round_tidy(s_y2_A_eq,2),
    r_y1y2=round_tidy(r_y1y2_A_eq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_A_eq,2),
    r_xy1=round_tidy(r_xy1_A_eq,2),
    r_xy2=round_tidy(r_xy2_A_eq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_A_eq-r_xy2_A_eq,2),
    Cohens_q=round_tidy(q_A_eq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_A_eq)),2),
    pre_std=round_tidy(pre_std_A_eq,2)
  )

A_eq_tab

# confirm that everything went correctly
(r_xy1_A_eq*s_y1_A_eq-r_xy2_A_eq*s_y2_A_eq)/
  sqrt(s_y1_A_eq^2+s_y2_A_eq^2-2*r_y1y2_A_eq*s_y1_A_eq*s_y2_A_eq)


# Type B patterns
# with equal variances (make the one reversed separately)

r_y1y2_B_eq=c(.00,.25,.50,.65,.80,.95)
s_y1_B_eq=rep(1,length(r_y1y2_B_eq))
s_y2_B_eq=rep(1,length(r_y1y2_B_eq))

## calculate denominators

s_y1_y2_B_eq=sqrt(s_y1_B_eq^2+s_y2_B_eq^2-2*r_y1y2_B_eq*s_y1_B_eq*s_y2_B_eq)
s_y1_y2_B_eq

## One of the correlations (r_xy2) is zero in Type B patterns
## This gives: r_xy1 = 

r_xy1_B_eq<-
  sqrt((-2)*r_y1y2_B_eq*s_y1_B_eq*s_y2_B_eq+s_y1_B_eq^2+s_y2_B_eq^2)/
  (2*s_y1_B_eq)
r_xy1_B_eq
r_xy2_B_eq=0*r_xy1_B_eq

q_B_eq=transf.rtoz(r_xy1_B_eq)-transf.rtoz(r_xy2_B_eq)
q_B_eq

pre_std_B_eq=(r_xy1_B_eq-r_xy2_B_eq)/
  sqrt(1^2+1^2-2*r_y1y2_B_eq*1*1)
pre_std_B_eq

B_eq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_B_eq,2),
    SD_y2=round_tidy(s_y2_B_eq,2),
    r_y1y2=round_tidy(r_y1y2_B_eq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_B_eq,2),
    r_xy1=round_tidy(r_xy1_B_eq,2),
    r_xy2=round_tidy(r_xy2_B_eq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_B_eq-r_xy2_B_eq,2),
    Cohens_q=round_tidy(q_B_eq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_B_eq)),2),
    pre_std=round_tidy(pre_std_B_eq,2)
  )

B_eq_tab

# confirm that everything went correctly
(r_xy1_B_eq*s_y1_B_eq-r_xy2_B_eq*s_y2_B_eq)/
  sqrt(s_y1_B_eq^2+s_y2_B_eq^2-2*r_y1y2_B_eq*s_y1_B_eq*s_y2_B_eq)

# Type B reversed patterns
# with equal variances 

r_y1y2_B_reveq=c(.00,.25,.50,.65,.80,.95)
s_y1_B_reveq=rep(1,length(r_y1y2_B_reveq))
s_y2_B_reveq=rep(1,length(r_y1y2_B_reveq))

## calculate denominators

s_y1_y2_B_reveq=sqrt(s_y1_B_reveq^2+s_y2_B_reveq^2-2*r_y1y2_B_reveq*s_y1_B_reveq*s_y2_B_reveq)
s_y1_y2_B_reveq

## One of the correlations (r_xy2) is zero in Type B patterns
## This gives: r_xy1 = 

r_xy2_B_reveq<-
  -1*sqrt((-2)*r_y1y2_B_reveq*s_y1_B_reveq*s_y2_B_reveq+s_y1_B_reveq^2+s_y2_B_reveq^2)/
  (2*s_y1_B_reveq)
r_xy2_B_reveq
r_xy1_B_reveq=0*r_xy2_B_reveq

q_B_reveq=transf.rtoz(r_xy1_B_reveq)-transf.rtoz(r_xy2_B_reveq)
q_B_reveq

pre_std_B_reveq=(r_xy1_B_reveq-r_xy2_B_reveq)/
  sqrt(1^2+1^2-2*r_y1y2_B_reveq*1*1)
pre_std_B_reveq

B_reveq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_B_reveq,2),
    SD_y2=round_tidy(s_y2_B_reveq,2),
    r_y1y2=round_tidy(r_y1y2_B_reveq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_B_reveq,2),
    r_xy1=round_tidy(r_xy1_B_reveq,2),
    r_xy2=round_tidy(r_xy2_B_reveq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_B_reveq-r_xy2_B_reveq,2),
    Cohens_q=round_tidy(q_B_reveq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_B_reveq)),2),
    pre_std=round_tidy(pre_std_B_reveq,2)
  )

B_reveq_tab

# confirm that everything went correctly
(r_xy1_B_reveq*s_y1_B_reveq-r_xy2_B_reveq*s_y2_B_reveq)/
  sqrt(s_y1_B_reveq^2+s_y2_B_reveq^2-2*r_y1y2_B_reveq*s_y1_B_reveq*s_y2_B_reveq)


# Type C patterns
# with equal variances 

r_y1y2_C_eq=c(.25,.50,.65,.80,.95)
s_y1_C_eq=rep(1,length(r_y1y2_C_eq))
s_y2_C_eq=rep(1,length(r_y1y2_C_eq))

## calculate denominators

s_y1_y2_C_eq=sqrt(s_y1_C_eq^2+s_y2_C_eq^2-2*r_y1y2_C_eq*s_y1_C_eq*s_y2_C_eq)
s_y1_y2_C_eq

## One of the correlations (r_xy2) is third of r_xy1 in the exemplary Type C patterns
## This gives: r_xy1 = 

r_xy1_C_eq<-
  3*sqrt((-2)*r_y1y2_C_eq*s_y1_C_eq*s_y2_C_eq+s_y1_C_eq^2+s_y2_C_eq^2)/
  (6*s_y1_C_eq-2*s_y2_C_eq)
r_xy1_C_eq
r_xy2_C_eq=(1/3)*r_xy1_C_eq
r_xy2_C_eq

q_C_eq=transf.rtoz(r_xy1_C_eq)-transf.rtoz(r_xy2_C_eq)
q_C_eq

pre_std_C_eq=(r_xy1_C_eq-r_xy2_C_eq)/
  sqrt(1^2+1^2-2*r_y1y2_C_eq*1*1)
pre_std_C_eq


C_eq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_C_eq,2),
    SD_y2=round_tidy(s_y2_C_eq,2),
    r_y1y2=round_tidy(r_y1y2_C_eq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_C_eq,2),
    r_xy1=round_tidy(r_xy1_C_eq,2),
    r_xy2=round_tidy(r_xy2_C_eq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_C_eq-r_xy2_C_eq,2),
    Cohens_q=round_tidy(q_C_eq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_C_eq)),2),
    pre_std=round_tidy(pre_std_C_eq,2)
  )

C_eq_tab

# confirm that everything went correctly
(r_xy1_C_eq*s_y1_C_eq-r_xy2_C_eq*s_y2_C_eq)/
  sqrt(s_y1_C_eq^2+s_y2_C_eq^2-2*r_y1y2_C_eq*s_y1_C_eq*s_y2_C_eq)


# Type C reversed patterns
# with equal variances 

r_y1y2_C_reveq=c(.25,.50,.65,.80,.95)
s_y1_C_reveq=rep(1,length(r_y1y2_C_reveq))
s_y2_C_reveq=rep(1,length(r_y1y2_C_reveq))

## calculate denominators

s_y1_y2_C_reveq=sqrt(s_y1_C_reveq^2+s_y2_C_reveq^2-2*r_y1y2_C_reveq*s_y1_C_reveq*s_y2_C_reveq)
s_y1_y2_C_reveq

## One of the correlations (r_xy2) is third of r_xy1 in the exemplary Type C patterns
## This gives: r_xy2 = 

r_xy2_C_reveq<-
  -1*3*sqrt((-2)*r_y1y2_C_reveq*s_y1_C_reveq*s_y2_C_reveq+s_y1_C_reveq^2+s_y2_C_reveq^2)/
  (6*s_y1_C_reveq-2*s_y2_C_reveq)
r_xy2_C_reveq
r_xy1_C_reveq=(1/3)*r_xy2_C_reveq
r_xy1_C_reveq

q_C_reveq=transf.rtoz(r_xy1_C_reveq)-transf.rtoz(r_xy2_C_reveq)
q_C_reveq

pre_std_C_reveq=(r_xy1_C_reveq-r_xy2_C_reveq)/
  sqrt(1^2+1^2-2*r_y1y2_C_reveq*1*1)
pre_std_C_reveq


C_reveq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_C_reveq,2),
    SD_y2=round_tidy(s_y2_C_reveq,2),
    r_y1y2=round_tidy(r_y1y2_C_reveq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_C_reveq,2),
    r_xy1=round_tidy(r_xy1_C_reveq,2),
    r_xy2=round_tidy(r_xy2_C_reveq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_C_reveq-r_xy2_C_reveq,2),
    Cohens_q=round_tidy(q_C_reveq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_C_reveq)),2),
    pre_std=round_tidy(pre_std_C_reveq,2)
  )

C_reveq_tab

# confirm that everything went correctly
(r_xy1_C_reveq*s_y1_C_reveq-r_xy2_C_reveq*s_y2_C_reveq)/
  sqrt(s_y1_C_reveq^2+s_y2_C_reveq^2-2*r_y1y2_C_reveq*s_y1_C_reveq*s_y2_C_reveq)

# Type D patterns
# with equal variances

r_y1y2_D_eq=c(.96,.97,.98,.99)
s_y1_D_eq=rep(1,length(r_y1y2_D_eq))
s_y2_D_eq=rep(1,length(r_y1y2_D_eq))

## calculate denominators

s_y1_y2_D_eq=sqrt(s_y1_D_eq^2+s_y2_D_eq^2-2*r_y1y2_D_eq*s_y1_D_eq*s_y2_D_eq)
s_y1_y2_D_eq

## This gives: r_xy1 = 

r_xy1_D_eq<-
  sqrt((-2)*r_y1y2_D_eq*s_y1_D_eq*s_y2_D_eq+s_y1_D_eq^2+s_y2_D_eq^2)/
  (2*(s_y1_D_eq+s_y2_D_eq))
r_xy1_D_eq
r_xy2_D_eq=-1*r_xy1_D_eq

q_D_eq=transf.rtoz(r_xy1_D_eq)-transf.rtoz(r_xy2_D_eq)
q_D_eq

pre_std_D_eq=(r_xy1_D_eq-r_xy2_D_eq)/
  sqrt(1^2+1^2-2*r_y1y2_D_eq*1*1)
pre_std_D_eq


D_eq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_D_eq,2),
    SD_y2=round_tidy(s_y2_D_eq,2),
    r_y1y2=round_tidy(r_y1y2_D_eq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_D_eq,2),
    r_xy1=round_tidy(r_xy1_D_eq,2),
    r_xy2=round_tidy(r_xy2_D_eq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_D_eq-r_xy2_D_eq,2),
    Cohens_q=round_tidy(q_D_eq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_D_eq)),2),
    pre_std=round_tidy(pre_std_D_eq,2)
  )

D_eq_tab

# confirm that everything went correctly
(r_xy1_D_eq*s_y1_D_eq-r_xy2_D_eq*s_y2_D_eq)/
  sqrt(s_y1_D_eq^2+s_y2_D_eq^2-2*r_y1y2_D_eq*s_y1_D_eq*s_y2_D_eq)

# Type D patterns
# with unequal variances

s_y1_D_uneq=rep(1.25,1)
s_y2_D_uneq=rep(0.75,1)
r_y1y2_D_uneq=c(.98)

## calculate denominators

s_y1_y2_D_uneq=sqrt(s_y1_D_uneq^2+s_y2_D_uneq^2-2*r_y1y2_D_uneq*s_y1_D_uneq*s_y2_D_uneq)
s_y1_y2_D_uneq

## This gives: r_xy1 = 
r_xy1_D_uneq<-
  3*sqrt((-2)*r_y1y2_D_uneq*s_y1_D_uneq*s_y2_D_uneq+s_y1_D_uneq^2+s_y2_D_uneq^2)/
  (6*s_y1_D_uneq+10*s_y2_D_uneq)
r_xy1_D_uneq

r_xy2_D_uneq=(-5/3)*r_xy1_D_uneq
r_xy2_D_uneq

q_D_uneq=transf.rtoz(r_xy1_D_uneq)-transf.rtoz(r_xy2_D_uneq)
q_D_uneq

pre_std_D_uneq=(r_xy1_D_uneq-r_xy2_D_uneq)/
  sqrt(1^2+1^2-2*r_y1y2_D_uneq*1*1)
pre_std_D_uneq


D_uneq_tab<-
  data.frame(
    SD_y1=round_tidy(s_y1_D_uneq,2),
    SD_y2=round_tidy(s_y2_D_uneq,2),
    r_y1y2=round_tidy(r_y1y2_D_uneq,2),
    'SD_y1-y2'=round_tidy(s_y1_y2_D_uneq,2),
    r_xy1=round_tidy(r_xy1_D_uneq,2),
    r_xy2=round_tidy(r_xy2_D_uneq,2),
    'r_xy1-r_xy2'=round_tidy(r_xy1_D_uneq-r_xy2_D_uneq,2),
    Cohens_q=round_tidy(q_D_uneq,2),
    r_xy1y2=round_tidy(rep(.50,times=length(s_y1_D_uneq)),2),
    pre_std=round_tidy(pre_std_D_uneq,2)
  )

D_uneq_tab

# confirm that everything went correctly
(r_xy1_D_uneq*s_y1_D_uneq-r_xy2_D_uneq*s_y2_D_uneq)/
  sqrt(s_y1_D_uneq^2+s_y2_D_uneq^2-2*r_y1y2_D_uneq*s_y1_D_uneq*s_y2_D_uneq)

comb_tab<-
  rbind(A_eq_tab,#A_uneq_tab,
      B_eq_tab,B_reveq_tab,
      C_eq_tab,C_reveq_tab,
      D_eq_tab#,D_uneq_tab
      )
names(comb_tab)
comb_tab
r_tabs<-c("r_y1y2","r_xy1","r_xy2","r_xy1y2")

comb_tab<-
  comb_tab %>%
  mutate_at(r_tabs,funs(str_replace(.,"0.", ".")))

comb_tab

comb_tab<-
  cbind(data.frame(Type=rep(LETTERS[1:4],c(6,12,10,4))),
      comb_tab)

save_as_docx(nice_table(comb_tab),
             path="illustrations/Table_r50.docx")
