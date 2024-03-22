# Extract confidence to results reported in text 

library(rio)
library(finalfit)

p_coding <- function(pval) {
  output <- ifelse(pval < 0.001, "<.001", 
                   ifelse(pval > 0.999, ">.999", 
                          substr(round_tidy(pval, 3), 2, 5)))
  return(output)
}

# MF

ml_MF<-import("Study 3/results/MF_ml_results.xlsx")


ml_MF$LL<-ml_MF$estimate+qt(.025,df = ml_MF$df)*ml_MF$SE
ml_MF$UL<-ml_MF$estimate+qt(.975,df = ml_MF$df)*ml_MF$SE

ml_MF$text_report<-paste0(ml_MF$rowname,
                               "(",
                               round_tidy(ml_MF$df,2),
                               ") = ",
                               round_tidy(ml_MF$estimate,2),
                               ", p = ",
                               p_coding(ml_MF$p.value),
                               ", 95% CI [",
                               round_tidy(ml_MF$LL,2),
                               " ,",
                               round_tidy(ml_MF$UL,2),
                               "]")

cc_MF<-import("Study 3/results/MF_ml_comp_cor.xlsx")
vt_MF<-import("Study 3/results/MF_ml_var_test.xlsx")
sem_MF<-import("Study 3/results/MF_sem_results.xlsx")

# N.z

ml_N.z<-import("Study 3/results/N.z_ml_results.xlsx")


ml_N.z$LL<-ml_N.z$estimate+qt(.025,df = ml_N.z$df)*ml_N.z$SE
ml_N.z$UL<-ml_N.z$estimate+qt(.975,df = ml_N.z$df)*ml_N.z$SE

ml_N.z$text_report<-paste0(ml_N.z$rowname,
                               "(",
                               round_tidy(ml_N.z$df,2),
                               ") = ",
                               round_tidy(ml_N.z$estimate,2),
                               ", p = ",
                               p_coding(ml_N.z$p.value),
                               ", 95% CI [",
                               round_tidy(ml_N.z$LL,2),
                               " ,",
                               round_tidy(ml_N.z$UL,2),
                               "]")

cc_N.z<-import("Study 3/results/N.z_ml_comp_cor.xlsx")
vt_N.z<-import("Study 3/results/N.z_ml_var_test.xlsx")
sem_N.z<-import("Study 3/results/N.z_sem_results.xlsx")

# E.z

ml_E.z<-import("Study 3/results/E.z_ml_results.xlsx")


ml_E.z$LL<-ml_E.z$estimate+qt(.025,df = ml_E.z$df)*ml_E.z$SE
ml_E.z$UL<-ml_E.z$estimate+qt(.975,df = ml_E.z$df)*ml_E.z$SE

ml_E.z$text_report<-paste0(ml_E.z$rowname,
                               "(",
                               round_tidy(ml_E.z$df,2),
                               ") = ",
                               round_tidy(ml_E.z$estimate,2),
                               ", p = ",
                               p_coding(ml_E.z$p.value),
                               ", 95% CI [",
                               round_tidy(ml_E.z$LL,2),
                               " ,",
                               round_tidy(ml_E.z$UL,2),
                               "]")

cc_E.z<-import("Study 3/results/E.z_ml_comp_cor.xlsx")
vt_E.z<-import("Study 3/results/E.z_ml_var_test.xlsx")
sem_E.z<-import("Study 3/results/E.z_sem_results.xlsx")

# O.z

ml_O.z<-import("Study 3/results/O.z_ml_results.xlsx")


ml_O.z$LL<-ml_O.z$estimate+qt(.025,df = ml_O.z$df)*ml_O.z$SE
ml_O.z$UL<-ml_O.z$estimate+qt(.975,df = ml_O.z$df)*ml_O.z$SE

ml_O.z$text_report<-paste0(ml_O.z$rowname,
                                "(",
                                round_tidy(ml_O.z$df,2),
                                ") = ",
                                round_tidy(ml_O.z$estimate,2),
                                ", p = ",
                                p_coding(ml_O.z$p.value),
                                ", 95% CI [",
                                round_tidy(ml_O.z$LL,2),
                                " ,",
                                round_tidy(ml_O.z$UL,2),
                                "]")

cc_O.z<-import("Study 3/results/O.z_ml_comp_cor.xlsx")
vt_O.z<-import("Study 3/results/O.z_ml_var_test.xlsx")
sem_O.z<-import("Study 3/results/O.z_sem_results.xlsx")

# A.z

ml_A.z<-import("Study 3/results/A.z_ml_results.xlsx")


ml_A.z$LL<-ml_A.z$estimate+qt(.025,df = ml_A.z$df)*ml_A.z$SE
ml_A.z$UL<-ml_A.z$estimate+qt(.975,df = ml_A.z$df)*ml_A.z$SE

ml_A.z$text_report<-paste0(ml_A.z$rowname,
                             "(",
                             round_tidy(ml_A.z$df,2),
                             ") = ",
                             round_tidy(ml_A.z$estimate,2),
                             ", p = ",
                             p_coding(ml_A.z$p.value),
                             ", 95% CI [",
                             round_tidy(ml_A.z$LL,2),
                             " ,",
                             round_tidy(ml_A.z$UL,2),
                             "]")

cc_A.z<-import("Study 3/results/A.z_ml_comp_cor.xlsx")
vt_A.z<-import("Study 3/results/A.z_ml_var_test.xlsx")
sem_A.z<-import("Study 3/results/A.z_sem_results.xlsx")

# C.z

ml_C.z<-import("Study 3/results/C.z_ml_results.xlsx")


ml_C.z$LL<-ml_C.z$estimate+qt(.025,df = ml_C.z$df)*ml_C.z$SE
ml_C.z$UL<-ml_C.z$estimate+qt(.975,df = ml_C.z$df)*ml_C.z$SE

ml_C.z$text_report<-paste0(ml_C.z$rowname,
                                "(",
                                round_tidy(ml_C.z$df,2),
                                ") = ",
                                round_tidy(ml_C.z$estimate,2),
                                ", p = ",
                                p_coding(ml_C.z$p.value),
                                ", 95% CI [",
                                round_tidy(ml_C.z$LL,2),
                                " ,",
                                round_tidy(ml_C.z$UL,2),
                                "]")

cc_C.z<-import("Study 3/results/C.z_ml_comp_cor.xlsx")
vt_C.z<-import("Study 3/results/C.z_ml_var_test.xlsx")
sem_C.z<-import("Study 3/results/C.z_sem_results.xlsx")




# print the results
# estimates from multilevel models

ml_MF$text_report
ml_N.z$text_report
ml_E.z$text_report
ml_O.z$text_report
ml_A.z$text_report
ml_C.z$text_report

# component correlations and estimates for Variance ratios

cbind(par=cc_MF$rowname,
      est=round_tidy(cc_MF$est,2),
      p=p_coding(cc_MF$pvalue),
      LL=round_tidy(cc_MF$ci.lower,2),
      UL=round_tidy(cc_MF$ci.upper,2))

cbind(par=cc_N.z$rowname,
      est=round_tidy(cc_N.z$est,2),
      p=p_coding(cc_N.z$pvalue),
      LL=round_tidy(cc_N.z$ci.lower,2),
      UL=round_tidy(cc_N.z$ci.upper,2))

cbind(par=cc_E.z$rowname,
      est=round_tidy(cc_E.z$est,2),
      p=p_coding(cc_E.z$pvalue),
      LL=round_tidy(cc_E.z$ci.lower,2),
      UL=round_tidy(cc_E.z$ci.upper,2))


cbind(par=cc_O.z$rowname,
      est=round_tidy(cc_O.z$est,2),
      p=p_coding(cc_O.z$pvalue),
      LL=round_tidy(cc_O.z$ci.lower,2),
      UL=round_tidy(cc_O.z$ci.upper,2))

cbind(par=cc_A.z$rowname,
      est=round_tidy(cc_A.z$est,2),
      p=p_coding(cc_A.z$pvalue),
      LL=round_tidy(cc_A.z$ci.lower,2),
      UL=round_tidy(cc_A.z$ci.upper,2))

cbind(par=cc_C.z$rowname,
      est=round_tidy(cc_C.z$est,2),
      p=p_coding(cc_C.z$pvalue),
      LL=round_tidy(cc_C.z$ci.lower,2),
      UL=round_tidy(cc_C.z$ci.upper,2))

# p-values for variance homogeneity

p_coding(vt_MF$p)
p_coding(vt_N.z$p)
p_coding(vt_E.z$p)
p_coding(vt_O.z$p)
p_coding(vt_A.z$p)
p_coding(vt_C.z$p)


# q-estimate confidence intervals

round_tidy(sem_MF[sem_MF$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_N.z[sem_N.z$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_E.z[sem_E.z$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)

round_tidy(sem_O.z[sem_O.z$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)

round_tidy(sem_A.z[sem_A.z$rowname=="q_b11_b21",
                     c("est","ci.lower","ci.upper")],2)

round_tidy(sem_C.z[sem_C.z$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)
