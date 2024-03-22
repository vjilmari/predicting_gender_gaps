# Extract confidence to results reported in text

library(rio)
library(finalfit)

p_coding <- function(pval) {
  output <- ifelse(pval < 0.001, "<.001", 
                   ifelse(pval > 0.999, ">.999", 
                          substr(round_tidy(pval, 3), 2, 5)))
  return(output)
}

# SCIEEFF

ml_SCIEEFF<-import("Study 1/results/SCIEEFF_ml_results.xlsx")


ml_SCIEEFF$LL<-ml_SCIEEFF$estimate+qt(.025,df = ml_SCIEEFF$df)*ml_SCIEEFF$SE
ml_SCIEEFF$UL<-ml_SCIEEFF$estimate+qt(.975,df = ml_SCIEEFF$df)*ml_SCIEEFF$SE

ml_SCIEEFF$text_report<-paste0(ml_SCIEEFF$rowname,
                               "(",
                               round_tidy(ml_SCIEEFF$df,2),
                               ") = ",
                               round_tidy(ml_SCIEEFF$estimate,2),
                               ", p = ",
                               p_coding(ml_SCIEEFF$p.value),
                               ", 95% CI [",
                               round_tidy(ml_SCIEEFF$LL,2),
                               " ,",
                               round_tidy(ml_SCIEEFF$UL,2),
                               "]")

cc_SCIEEFF<-import("Study 1/results/SCIEEFF_ml_comp_cor.xlsx")
vt_SCIEEFF<-import("Study 1/results/SCIEEFF_ml_var_test.xlsx")
sem_SCIEEFF<-import("Study 1/results/SCIEEFF_sem_results.xlsx")

# INTBRSCI

ml_INTBRSCI<-import("Study 1/results/INTBRSCI_ml_results.xlsx")


ml_INTBRSCI$LL<-ml_INTBRSCI$estimate+qt(.025,df = ml_INTBRSCI$df)*ml_INTBRSCI$SE
ml_INTBRSCI$UL<-ml_INTBRSCI$estimate+qt(.975,df = ml_INTBRSCI$df)*ml_INTBRSCI$SE

ml_INTBRSCI$text_report<-paste0(ml_INTBRSCI$rowname,
                               "(",
                               round_tidy(ml_INTBRSCI$df,2),
                               ") = ",
                               round_tidy(ml_INTBRSCI$estimate,2),
                               ", p = ",
                               p_coding(ml_INTBRSCI$p.value),
                               ", 95% CI [",
                               round_tidy(ml_INTBRSCI$LL,2),
                               " ,",
                               round_tidy(ml_INTBRSCI$UL,2),
                               "]")

cc_INTBRSCI<-import("Study 1/results/INTBRSCI_ml_comp_cor.xlsx")
vt_INTBRSCI<-import("Study 1/results/INTBRSCI_ml_var_test.xlsx")
sem_INTBRSCI<-import("Study 1/results/INTBRSCI_sem_results.xlsx")

# JOYSCIE

ml_JOYSCIE<-import("Study 1/results/JOYSCIE_ml_results.xlsx")


ml_JOYSCIE$LL<-ml_JOYSCIE$estimate+qt(.025,df = ml_JOYSCIE$df)*ml_JOYSCIE$SE
ml_JOYSCIE$UL<-ml_JOYSCIE$estimate+qt(.975,df = ml_JOYSCIE$df)*ml_JOYSCIE$SE

ml_JOYSCIE$text_report<-paste0(ml_JOYSCIE$rowname,
                               "(",
                               round_tidy(ml_JOYSCIE$df,2),
                               ") = ",
                               round_tidy(ml_JOYSCIE$estimate,2),
                               ", p = ",
                               p_coding(ml_JOYSCIE$p.value),
                               ", 95% CI [",
                               round_tidy(ml_JOYSCIE$LL,2),
                               " ,",
                               round_tidy(ml_JOYSCIE$UL,2),
                               "]")

cc_JOYSCIE<-import("Study 1/results/JOYSCIE_ml_comp_cor.xlsx")
vt_JOYSCIE<-import("Study 1/results/JOYSCIE_ml_var_test.xlsx")
sem_JOYSCIE<-import("Study 1/results/JOYSCIE_sem_results.xlsx")


# print the results
# estimates from multilevel models

ml_SCIEEFF$text_report
ml_INTBRSCI$text_report
ml_JOYSCIE$text_report

# component correlations and estimates for Variance ratios

cbind(par=cc_SCIEEFF$rowname,
      est=round_tidy(cc_SCIEEFF$est,2),
      p=p_coding(cc_SCIEEFF$pvalue),
      LL=round_tidy(cc_SCIEEFF$ci.lower,2),
      UL=round_tidy(cc_SCIEEFF$ci.upper,2))

cbind(par=cc_INTBRSCI$rowname,
      est=round_tidy(cc_INTBRSCI$est,2),
      p=p_coding(cc_INTBRSCI$pvalue),
      LL=round_tidy(cc_INTBRSCI$ci.lower,2),
      UL=round_tidy(cc_INTBRSCI$ci.upper,2))

cbind(par=cc_JOYSCIE$rowname,
      est=round_tidy(cc_JOYSCIE$est,2),
      p=p_coding(cc_JOYSCIE$pvalue),
      LL=round_tidy(cc_JOYSCIE$ci.lower,2),
      UL=round_tidy(cc_JOYSCIE$ci.upper,2))

# p-values for variance homogeneity

p_coding(vt_SCIEEFF$p)
p_coding(vt_INTBRSCI$p)
p_coding(vt_JOYSCIE$p)

# q-estimate confidence intervals

round_tidy(sem_SCIEEFF[sem_SCIEEFF$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_INTBRSCI[sem_INTBRSCI$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_JOYSCIE[sem_JOYSCIE$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)
