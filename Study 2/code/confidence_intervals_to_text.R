# Extract confidence to results reported in text

library(rio)
library(finalfit)

p_coding <- function(pval) {
  output <- ifelse(pval < 0.001, "<.001", 
                   ifelse(pval > 0.999, ">.999", 
                          substr(round_tidy(pval, 3), 2, 5)))
  return(output)
}

# altruism

ml_altruism<-import("Study 2/results/altruism_ml_results.xlsx")


ml_altruism$LL<-ml_altruism$estimate+qt(.025,df = ml_altruism$df)*ml_altruism$SE
ml_altruism$UL<-ml_altruism$estimate+qt(.975,df = ml_altruism$df)*ml_altruism$SE

ml_altruism$text_report<-paste0(ml_altruism$rowname,
                               "(",
                               round_tidy(ml_altruism$df,2),
                               ") = ",
                               round_tidy(ml_altruism$estimate,2),
                               ", p = ",
                               p_coding(ml_altruism$p.value),
                               ", 95% CI [",
                               round_tidy(ml_altruism$LL,2),
                               " ,",
                               round_tidy(ml_altruism$UL,2),
                               "]")

cc_altruism<-import("Study 2/results/altruism_ml_comp_cor.xlsx")
vt_altruism<-import("Study 2/results/altruism_ml_var_test.xlsx")
sem_altruism<-import("Study 2/results/altruism_sem_results.xlsx")

# trust

ml_trust<-import("Study 2/results/trust_ml_results.xlsx")


ml_trust$LL<-ml_trust$estimate+qt(.025,df = ml_trust$df)*ml_trust$SE
ml_trust$UL<-ml_trust$estimate+qt(.975,df = ml_trust$df)*ml_trust$SE

ml_trust$text_report<-paste0(ml_trust$rowname,
                               "(",
                               round_tidy(ml_trust$df,2),
                               ") = ",
                               round_tidy(ml_trust$estimate,2),
                               ", p = ",
                               p_coding(ml_trust$p.value),
                               ", 95% CI [",
                               round_tidy(ml_trust$LL,2),
                               " ,",
                               round_tidy(ml_trust$UL,2),
                               "]")

cc_trust<-import("Study 2/results/trust_ml_comp_cor.xlsx")
vt_trust<-import("Study 2/results/trust_ml_var_test.xlsx")
sem_trust<-import("Study 2/results/trust_sem_results.xlsx")

# posrecip

ml_posrecip<-import("Study 2/results/posrecip_ml_results.xlsx")


ml_posrecip$LL<-ml_posrecip$estimate+qt(.025,df = ml_posrecip$df)*ml_posrecip$SE
ml_posrecip$UL<-ml_posrecip$estimate+qt(.975,df = ml_posrecip$df)*ml_posrecip$SE

ml_posrecip$text_report<-paste0(ml_posrecip$rowname,
                               "(",
                               round_tidy(ml_posrecip$df,2),
                               ") = ",
                               round_tidy(ml_posrecip$estimate,2),
                               ", p = ",
                               p_coding(ml_posrecip$p.value),
                               ", 95% CI [",
                               round_tidy(ml_posrecip$LL,2),
                               " ,",
                               round_tidy(ml_posrecip$UL,2),
                               "]")

cc_posrecip<-import("Study 2/results/posrecip_ml_comp_cor.xlsx")
vt_posrecip<-import("Study 2/results/posrecip_ml_var_test.xlsx")
sem_posrecip<-import("Study 2/results/posrecip_sem_results.xlsx")

# negrecip

ml_negrecip<-import("Study 2/results/negrecip_ml_results.xlsx")


ml_negrecip$LL<-ml_negrecip$estimate+qt(.025,df = ml_negrecip$df)*ml_negrecip$SE
ml_negrecip$UL<-ml_negrecip$estimate+qt(.975,df = ml_negrecip$df)*ml_negrecip$SE

ml_negrecip$text_report<-paste0(ml_negrecip$rowname,
                                "(",
                                round_tidy(ml_negrecip$df,2),
                                ") = ",
                                round_tidy(ml_negrecip$estimate,2),
                                ", p = ",
                                p_coding(ml_negrecip$p.value),
                                ", 95% CI [",
                                round_tidy(ml_negrecip$LL,2),
                                " ,",
                                round_tidy(ml_negrecip$UL,2),
                                "]")

cc_negrecip<-import("Study 2/results/negrecip_ml_comp_cor.xlsx")
vt_negrecip<-import("Study 2/results/negrecip_ml_var_test.xlsx")
sem_negrecip<-import("Study 2/results/negrecip_sem_results.xlsx")

# risktaking

ml_risktaking<-import("Study 2/results/risktaking_ml_results.xlsx")


ml_risktaking$LL<-ml_risktaking$estimate+qt(.025,df = ml_risktaking$df)*ml_risktaking$SE
ml_risktaking$UL<-ml_risktaking$estimate+qt(.975,df = ml_risktaking$df)*ml_risktaking$SE

ml_risktaking$text_report<-paste0(ml_risktaking$rowname,
                             "(",
                             round_tidy(ml_risktaking$df,2),
                             ") = ",
                             round_tidy(ml_risktaking$estimate,2),
                             ", p = ",
                             p_coding(ml_risktaking$p.value),
                             ", 95% CI [",
                             round_tidy(ml_risktaking$LL,2),
                             " ,",
                             round_tidy(ml_risktaking$UL,2),
                             "]")

cc_risktaking<-import("Study 2/results/risktaking_ml_comp_cor.xlsx")
vt_risktaking<-import("Study 2/results/risktaking_ml_var_test.xlsx")
sem_risktaking<-import("Study 2/results/risktaking_sem_results.xlsx")

# patience

ml_patience<-import("Study 2/results/patience_ml_results.xlsx")


ml_patience$LL<-ml_patience$estimate+qt(.025,df = ml_patience$df)*ml_patience$SE
ml_patience$UL<-ml_patience$estimate+qt(.975,df = ml_patience$df)*ml_patience$SE

ml_patience$text_report<-paste0(ml_patience$rowname,
                                "(",
                                round_tidy(ml_patience$df,2),
                                ") = ",
                                round_tidy(ml_patience$estimate,2),
                                ", p = ",
                                p_coding(ml_patience$p.value),
                                ", 95% CI [",
                                round_tidy(ml_patience$LL,2),
                                " ,",
                                round_tidy(ml_patience$UL,2),
                                "]")

cc_patience<-import("Study 2/results/patience_ml_comp_cor.xlsx")
vt_patience<-import("Study 2/results/patience_ml_var_test.xlsx")
sem_patience<-import("Study 2/results/patience_sem_results.xlsx")




# print the results
# estimates from multilevel models

ml_altruism$text_report
ml_trust$text_report
ml_posrecip$text_report
ml_negrecip$text_report
ml_risktaking$text_report
ml_patience$text_report

# component correlations and estimates for Variance ratios

cbind(par=cc_altruism$rowname,
      est=round_tidy(cc_altruism$est,2),
      p=p_coding(cc_altruism$pvalue),
      LL=round_tidy(cc_altruism$ci.lower,2),
      UL=round_tidy(cc_altruism$ci.upper,2))

cbind(par=cc_trust$rowname,
      est=round_tidy(cc_trust$est,2),
      p=p_coding(cc_trust$pvalue),
      LL=round_tidy(cc_trust$ci.lower,2),
      UL=round_tidy(cc_trust$ci.upper,2))

cbind(par=cc_posrecip$rowname,
      est=round_tidy(cc_posrecip$est,2),
      p=p_coding(cc_posrecip$pvalue),
      LL=round_tidy(cc_posrecip$ci.lower,2),
      UL=round_tidy(cc_posrecip$ci.upper,2))


cbind(par=cc_negrecip$rowname,
      est=round_tidy(cc_negrecip$est,2),
      p=p_coding(cc_negrecip$pvalue),
      LL=round_tidy(cc_negrecip$ci.lower,2),
      UL=round_tidy(cc_negrecip$ci.upper,2))

cbind(par=cc_risktaking$rowname,
      est=round_tidy(cc_risktaking$est,2),
      p=p_coding(cc_risktaking$pvalue),
      LL=round_tidy(cc_risktaking$ci.lower,2),
      UL=round_tidy(cc_risktaking$ci.upper,2))

cbind(par=cc_patience$rowname,
      est=round_tidy(cc_patience$est,2),
      p=p_coding(cc_patience$pvalue),
      LL=round_tidy(cc_patience$ci.lower,2),
      UL=round_tidy(cc_patience$ci.upper,2))

# p-values for variance homogeneity

p_coding(vt_altruism$p)
p_coding(vt_trust$p)
p_coding(vt_posrecip$p)
p_coding(vt_negrecip$p)
p_coding(vt_risktaking$p)
p_coding(vt_patience$p)


# q-estimate confidence intervals

round_tidy(sem_altruism[sem_altruism$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_trust[sem_trust$rowname=="q_b11_b21",
                       c("est","ci.lower","ci.upper")],2)

round_tidy(sem_posrecip[sem_posrecip$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)

round_tidy(sem_negrecip[sem_negrecip$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)

round_tidy(sem_risktaking[sem_risktaking$rowname=="q_b11_b21",
                     c("est","ci.lower","ci.upper")],2)

round_tidy(sem_patience[sem_patience$rowname=="q_b11_b21",
                        c("est","ci.lower","ci.upper")],2)
