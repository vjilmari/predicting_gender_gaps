# compiling table for economic preferences

library(rio)
library(rempsyc)
library(flextable)
library(finalfit)
library(stringr)
library(dplyr)
library(tibble)

# function for p-value formatting

p_coding <- function(pval) {
  output <- ifelse(pval < 0.001, "<.001", 
                   ifelse(pval > 0.999, ">.999", 
                          substr(round_tidy(pval, 3), 2, 5)))
  return(output)
}


# function for correlation formatting

cor_form<-function(cor){
  str_replace(round_tidy(cor,2),"0.",".")
}


# altruism

altruism_desc<-import("Study 2/results/altruism_ml_desc.xlsx")
altruism_res<-import("Study 2/results/altruism_ml_results.xlsx")
altruism_cc<-import("Study 2/results/altruism_ml_comp_cor.xlsx")
altruism_ml_var_test<-import("Study 2/results/altruism_ml_var_test.xlsx")
altruism_reliab<-import("Study 2/results/reliab.altruism.xlsx")

altruism_desc

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
            "w_11","w_21")

altruism_model.ests<-altruism_res[altruism_res$rowname %in% model.pars,]
altruism_model.ests<-altruism_model.ests[order(match(altruism_model.ests$rowname, model.pars)), ]
altruism_model.ests<-
  altruism_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
altruism_model.ests

# observed variability estimates
altruism_SD<-altruism_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

altruism_SD_test_p<-altruism_ml_var_test$p
altruism_VR<-(altruism_SD[1,"est"]^2)/(altruism_SD[2,"est"]^2)

altruism_SD<-
  rbind(altruism_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,altruism_VR),
                   p=c(altruism_SD_test_p,NA)))
altruism_SD<-
  altruism_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

altruism_slope.ests<-altruism_res[altruism_res$rowname %in% slope.pars,]
altruism_slope.ests<-altruism_slope.ests[order(match(altruism_slope.ests$rowname,
                                                   slope.pars)), ]
altruism_slope.ests<-
  altruism_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
altruism_slope.ests


# component correlation
altruism_ry1y2<-
  altruism_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

altruism_add<-
  altruism_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
altruism_ds_SD<-
  altruism_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

altruism_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(altruism_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
altruism_ml_tab<-
  bind_rows(altruism_model.ests,
          altruism_SD,
          altruism_slope.ests,
          altruism_ry1y2,
          altruism_add,
          altruism_rel_est)

altruism_ml_tab

# trust

trust_desc<-import("Study 2/results/trust_ml_desc.xlsx")
trust_res<-import("Study 2/results/trust_ml_results.xlsx")
trust_cc<-import("Study 2/results/trust_ml_comp_cor.xlsx")
trust_ml_var_test<-import("Study 2/results/trust_ml_var_test.xlsx")
trust_reliab<-import("Study 2/results/reliab.trust.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

trust_model.ests<-trust_res[trust_res$rowname %in% model.pars,]
trust_model.ests<-trust_model.ests[order(match(trust_model.ests$rowname, model.pars)), ]
trust_model.ests<-
  trust_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
trust_model.ests

# observed variability estimates
trust_SD<-trust_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

trust_SD_test_p<-trust_ml_var_test$p
trust_VR<-(trust_SD[1,"est"]^2)/(trust_SD[2,"est"]^2)

trust_SD<-
  rbind(trust_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,trust_VR),
                   p=c(trust_SD_test_p,NA)))
trust_SD<-
  trust_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

trust_slope.ests<-trust_res[trust_res$rowname %in% slope.pars,]
trust_slope.ests<-trust_slope.ests[order(match(trust_slope.ests$rowname,
                                                   slope.pars)), ]
trust_slope.ests<-
  trust_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
trust_slope.ests


# component correlation
trust_ry1y2<-
  trust_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

trust_add<-
  trust_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
trust_ds_SD<-
  trust_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

trust_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(trust_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
trust_ml_tab<-
  bind_rows(trust_model.ests,
            trust_SD,
            trust_slope.ests,
            trust_ry1y2,
            trust_add,
            trust_rel_est)
trust_ml_tab


# posrecip

posrecip_desc<-import("Study 2/results/posrecip_ml_desc.xlsx")
posrecip_res<-import("Study 2/results/posrecip_ml_results.xlsx")
posrecip_cc<-import("Study 2/results/posrecip_ml_comp_cor.xlsx")
posrecip_ml_var_test<-import("Study 2/results/posrecip_ml_var_test.xlsx")
posrecip_reliab<-import("Study 2/results/reliab.posrecip.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

posrecip_model.ests<-posrecip_res[posrecip_res$rowname %in% model.pars,]
posrecip_model.ests<-posrecip_model.ests[order(match(posrecip_model.ests$rowname, model.pars)), ]
posrecip_model.ests<-
  posrecip_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
posrecip_model.ests

# observed variability estimates
posrecip_SD<-posrecip_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

posrecip_SD_test_p<-posrecip_ml_var_test$p
posrecip_VR<-(posrecip_SD[1,"est"]^2)/(posrecip_SD[2,"est"]^2)

posrecip_SD<-
  rbind(posrecip_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,posrecip_VR),
                   p=c(posrecip_SD_test_p,NA)))
posrecip_SD<-
  posrecip_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

posrecip_slope.ests<-posrecip_res[posrecip_res$rowname %in% slope.pars,]
posrecip_slope.ests<-posrecip_slope.ests[order(match(posrecip_slope.ests$rowname,
                                                     slope.pars)), ]
posrecip_slope.ests<-
  posrecip_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
posrecip_slope.ests


# component correlation
posrecip_ry1y2<-
  posrecip_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

posrecip_add<-
  posrecip_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
posrecip_ds_SD<-
  posrecip_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

posrecip_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(posrecip_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
posrecip_ml_tab<-
  bind_rows(posrecip_model.ests,
            posrecip_SD,
            posrecip_slope.ests,
            posrecip_ry1y2,
            posrecip_add,
            posrecip_rel_est)
posrecip_ml_tab


# combine the variables to same frame
ml_tab_1<-bind_cols(altruism_ml_tab,trust_ml_tab[,2:3],posrecip_ml_tab[,2:3])

# export

save_as_docx(nice_table(ml_tab_1),
             path = "Study 2/results/ml_table_1.docx")

# tables for sem models

# altruism
altruism_sem_res<-import("Study 2/results/altruism_sem_results.xlsx")

altruism_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

altruism_model.ests<-altruism_sem_res[altruism_sem_res$rowname %in% model.pars,]
altruism_model.ests<-altruism_model.ests[order(match(altruism_model.ests$rowname, model.pars)), ]
altruism_model.ests<-
  altruism_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
altruism_model.ests

# observed variability estimates
altruism_SD<-altruism_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

altruism_SD_test_p<-altruism_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
altruism_VR<-(altruism_SD[1,"est"]^2)/(altruism_SD[2,"est"]^2)


altruism_SD<-
  rbind(altruism_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,altruism_VR),
                   p=c(altruism_SD_test_p$pvalue[1],NA)))
altruism_SD<-
  altruism_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

altruism_slope.ests<-altruism_sem_res[altruism_sem_res$rowname %in% slope.pars,]
altruism_slope.ests<-altruism_slope.ests[order(match(altruism_slope.ests$rowname,
                                                   slope.pars)), ]
altruism_slope.ests<-
  altruism_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
altruism_slope.ests


# component correlation
altruism_ry1y2<-
  altruism_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

altruism_add<-
  altruism_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
altruism_ds_SD<-
  altruism_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

altruism_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
altruism_sem_tab<-
  bind_rows(altruism_model.ests,
            altruism_SD,
            altruism_slope.ests,
            altruism_ry1y2,
            altruism_add,
            altruism_rel_est)

altruism_sem_tab


# trust
trust_sem_res<-import("Study 2/results/trust_sem_results.xlsx")

trust_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

trust_model.ests<-trust_sem_res[trust_sem_res$rowname %in% model.pars,]
trust_model.ests<-trust_model.ests[order(match(trust_model.ests$rowname, model.pars)), ]
trust_model.ests<-
  trust_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
trust_model.ests

# observed variability estimates
trust_SD<-trust_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

trust_SD_test_p<-trust_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
trust_VR<-(trust_SD[1,"est"]^2)/(trust_SD[2,"est"]^2)


trust_SD<-
  rbind(trust_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,trust_VR),
                   p=c(trust_SD_test_p$pvalue[1],NA)))
trust_SD<-
  trust_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

trust_slope.ests<-trust_sem_res[trust_sem_res$rowname %in% slope.pars,]
trust_slope.ests<-trust_slope.ests[order(match(trust_slope.ests$rowname,
                                                   slope.pars)), ]
trust_slope.ests<-
  trust_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
trust_slope.ests


# component correlation
trust_ry1y2<-
  trust_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
trust_sem_res
trust_add<-
  trust_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
trust_ds_SD<-
  trust_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

trust_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
trust_sem_tab<-
  bind_rows(trust_model.ests,
            trust_SD,
            trust_slope.ests,
            trust_ry1y2,
            trust_add,
            trust_rel_est)

trust_sem_tab


# posrecip
posrecip_sem_res<-import("Study 2/results/posrecip_sem_results.xlsx")

posrecip_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

posrecip_model.ests<-posrecip_sem_res[posrecip_sem_res$rowname %in% model.pars,]
posrecip_model.ests<-posrecip_model.ests[order(match(posrecip_model.ests$rowname, model.pars)), ]
posrecip_model.ests<-
  posrecip_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
posrecip_model.ests

# observed variability estimates
posrecip_SD<-posrecip_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

posrecip_SD_test_p<-posrecip_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
posrecip_VR<-(posrecip_SD[1,"est"]^2)/(posrecip_SD[2,"est"]^2)


posrecip_SD<-
  rbind(posrecip_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,posrecip_VR),
                   p=c(posrecip_SD_test_p$pvalue[1],NA)))
posrecip_SD<-
  posrecip_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

posrecip_slope.ests<-posrecip_sem_res[posrecip_sem_res$rowname %in% slope.pars,]
posrecip_slope.ests<-posrecip_slope.ests[order(match(posrecip_slope.ests$rowname,
                                                   slope.pars)), ]
posrecip_slope.ests<-
  posrecip_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
posrecip_slope.ests


# component correlation
posrecip_ry1y2<-
  posrecip_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
posrecip_sem_res
posrecip_add<-
  posrecip_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
posrecip_ds_SD<-
  posrecip_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

posrecip_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
posrecip_sem_tab<-
  bind_rows(posrecip_model.ests,
            posrecip_SD,
            posrecip_slope.ests,
            posrecip_ry1y2,
            posrecip_add,
            posrecip_rel_est)

posrecip_sem_tab


# combine the variables to same frame
sem_tab_1<-bind_cols(altruism_sem_tab,trust_sem_tab[,2:3],posrecip_sem_tab[,2:3])

# export

save_as_docx(nice_table(sem_tab_1),
             path = "Study 2/results/sem_table_1.docx")



# negrecip

negrecip_desc<-import("Study 2/results/negrecip_ml_desc.xlsx")
negrecip_res<-import("Study 2/results/negrecip_ml_results.xlsx")
negrecip_cc<-import("Study 2/results/negrecip_ml_comp_cor.xlsx")
negrecip_ml_var_test<-import("Study 2/results/negrecip_ml_var_test.xlsx")
negrecip_reliab<-import("Study 2/results/reliab.negrecip.xlsx")

negrecip_desc

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

negrecip_model.ests<-negrecip_res[negrecip_res$rowname %in% model.pars,]
negrecip_model.ests<-negrecip_model.ests[order(match(negrecip_model.ests$rowname, model.pars)), ]
negrecip_model.ests<-
  negrecip_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
negrecip_model.ests

# observed variability estimates
negrecip_SD<-negrecip_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

negrecip_SD_test_p<-negrecip_ml_var_test$p
negrecip_VR<-(negrecip_SD[1,"est"]^2)/(negrecip_SD[2,"est"]^2)

negrecip_SD<-
  rbind(negrecip_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,negrecip_VR),
                   p=c(negrecip_SD_test_p,NA)))
negrecip_SD<-
  negrecip_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

negrecip_slope.ests<-negrecip_res[negrecip_res$rowname %in% slope.pars,]
negrecip_slope.ests<-negrecip_slope.ests[order(match(negrecip_slope.ests$rowname,
                                                     slope.pars)), ]
negrecip_slope.ests<-
  negrecip_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
negrecip_slope.ests


# component correlation
negrecip_ry1y2<-
  negrecip_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

negrecip_add<-
  negrecip_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
negrecip_ds_SD<-
  negrecip_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

negrecip_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(negrecip_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
negrecip_ml_tab<-
  bind_rows(negrecip_model.ests,
            negrecip_SD,
            negrecip_slope.ests,
            negrecip_ry1y2,
            negrecip_add,
            negrecip_rel_est)

negrecip_ml_tab

# risktaking

risktaking_desc<-import("Study 2/results/risktaking_ml_desc.xlsx")
risktaking_res<-import("Study 2/results/risktaking_ml_results.xlsx")
risktaking_cc<-import("Study 2/results/risktaking_ml_comp_cor.xlsx")
risktaking_ml_var_test<-import("Study 2/results/risktaking_ml_var_test.xlsx")
risktaking_reliab<-import("Study 2/results/reliab.risktaking.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

risktaking_model.ests<-risktaking_res[risktaking_res$rowname %in% model.pars,]
risktaking_model.ests<-risktaking_model.ests[order(match(risktaking_model.ests$rowname, model.pars)), ]
risktaking_model.ests<-
  risktaking_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
risktaking_model.ests

# observed variability estimates
risktaking_SD<-risktaking_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

risktaking_SD_test_p<-risktaking_ml_var_test$p
risktaking_VR<-(risktaking_SD[1,"est"]^2)/(risktaking_SD[2,"est"]^2)

risktaking_SD<-
  rbind(risktaking_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,risktaking_VR),
                   p=c(risktaking_SD_test_p,NA)))
risktaking_SD<-
  risktaking_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

risktaking_slope.ests<-risktaking_res[risktaking_res$rowname %in% slope.pars,]
risktaking_slope.ests<-risktaking_slope.ests[order(match(risktaking_slope.ests$rowname,
                                               slope.pars)), ]
risktaking_slope.ests<-
  risktaking_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
risktaking_slope.ests


# component correlation
risktaking_ry1y2<-
  risktaking_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

risktaking_add<-
  risktaking_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
risktaking_ds_SD<-
  risktaking_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

risktaking_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(risktaking_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
risktaking_ml_tab<-
  bind_rows(risktaking_model.ests,
            risktaking_SD,
            risktaking_slope.ests,
            risktaking_ry1y2,
            risktaking_add,
            risktaking_rel_est)
risktaking_ml_tab


# patience

patience_desc<-import("Study 2/results/patience_ml_desc.xlsx")
patience_res<-import("Study 2/results/patience_ml_results.xlsx")
patience_cc<-import("Study 2/results/patience_ml_comp_cor.xlsx")
patience_ml_var_test<-import("Study 2/results/patience_ml_var_test.xlsx")
patience_reliab<-import("Study 2/results/reliab.patience.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

patience_model.ests<-patience_res[patience_res$rowname %in% model.pars,]
patience_model.ests<-patience_model.ests[order(match(patience_model.ests$rowname, model.pars)), ]
patience_model.ests<-
  patience_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
patience_model.ests

# observed variability estimates
patience_SD<-patience_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

patience_SD_test_p<-patience_ml_var_test$p
patience_VR<-(patience_SD[1,"est"]^2)/(patience_SD[2,"est"]^2)

patience_SD<-
  rbind(patience_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,patience_VR),
                   p=c(patience_SD_test_p,NA)))
patience_SD<-
  patience_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

patience_slope.ests<-patience_res[patience_res$rowname %in% slope.pars,]
patience_slope.ests<-patience_slope.ests[order(match(patience_slope.ests$rowname,
                                                     slope.pars)), ]
patience_slope.ests<-
  patience_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
patience_slope.ests


# component correlation
patience_ry1y2<-
  patience_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

patience_add<-
  patience_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
patience_ds_SD<-
  patience_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

patience_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(patience_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
patience_ml_tab<-
  bind_rows(patience_model.ests,
            patience_SD,
            patience_slope.ests,
            patience_ry1y2,
            patience_add,
            patience_rel_est)
patience_ml_tab


# combine the variables to same frame
ml_tab_2<-bind_cols(negrecip_ml_tab,risktaking_ml_tab[,2:3],patience_ml_tab[,2:3])

# export

save_as_docx(nice_table(ml_tab_2),
             path = "Study 2/results/ml_table_2.docx")

# tables for sem models

# negrecip
negrecip_sem_res<-import("Study 2/results/negrecip_sem_results.xlsx")

negrecip_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

negrecip_model.ests<-negrecip_sem_res[negrecip_sem_res$rowname %in% model.pars,]
negrecip_model.ests<-negrecip_model.ests[order(match(negrecip_model.ests$rowname, model.pars)), ]
negrecip_model.ests<-
  negrecip_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
negrecip_model.ests

# observed variability estimates
negrecip_SD<-negrecip_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

negrecip_SD_test_p<-negrecip_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
negrecip_VR<-(negrecip_SD[1,"est"]^2)/(negrecip_SD[2,"est"]^2)


negrecip_SD<-
  rbind(negrecip_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,negrecip_VR),
                   p=c(negrecip_SD_test_p$pvalue[1],NA)))
negrecip_SD<-
  negrecip_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

negrecip_slope.ests<-negrecip_sem_res[negrecip_sem_res$rowname %in% slope.pars,]
negrecip_slope.ests<-negrecip_slope.ests[order(match(negrecip_slope.ests$rowname,
                                                     slope.pars)), ]
negrecip_slope.ests<-
  negrecip_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
negrecip_slope.ests


# component correlation
negrecip_ry1y2<-
  negrecip_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

negrecip_add<-
  negrecip_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
negrecip_ds_SD<-
  negrecip_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

negrecip_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
negrecip_sem_tab<-
  bind_rows(negrecip_model.ests,
            negrecip_SD,
            negrecip_slope.ests,
            negrecip_ry1y2,
            negrecip_add,
            negrecip_rel_est)

negrecip_sem_tab


# risktaking
risktaking_sem_res<-import("Study 2/results/risktaking_sem_results.xlsx")

risktaking_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

risktaking_model.ests<-risktaking_sem_res[risktaking_sem_res$rowname %in% model.pars,]
risktaking_model.ests<-risktaking_model.ests[order(match(risktaking_model.ests$rowname, model.pars)), ]
risktaking_model.ests<-
  risktaking_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
risktaking_model.ests

# observed variability estimates
risktaking_SD<-risktaking_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

risktaking_SD_test_p<-risktaking_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
risktaking_VR<-(risktaking_SD[1,"est"]^2)/(risktaking_SD[2,"est"]^2)


risktaking_SD<-
  rbind(risktaking_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,risktaking_VR),
                   p=c(risktaking_SD_test_p$pvalue[1],NA)))
risktaking_SD<-
  risktaking_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

risktaking_slope.ests<-risktaking_sem_res[risktaking_sem_res$rowname %in% slope.pars,]
risktaking_slope.ests<-risktaking_slope.ests[order(match(risktaking_slope.ests$rowname,
                                               slope.pars)), ]
risktaking_slope.ests<-
  risktaking_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
risktaking_slope.ests


# component correlation
risktaking_ry1y2<-
  risktaking_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
risktaking_sem_res
risktaking_add<-
  risktaking_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
risktaking_ds_SD<-
  risktaking_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

risktaking_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
risktaking_sem_tab<-
  bind_rows(risktaking_model.ests,
            risktaking_SD,
            risktaking_slope.ests,
            risktaking_ry1y2,
            risktaking_add,
            risktaking_rel_est)

risktaking_sem_tab


# patience
patience_sem_res<-import("Study 2/results/patience_sem_results.xlsx")

patience_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

patience_model.ests<-patience_sem_res[patience_sem_res$rowname %in% model.pars,]
patience_model.ests<-patience_model.ests[order(match(patience_model.ests$rowname, model.pars)), ]
patience_model.ests<-
  patience_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
patience_model.ests

# observed variability estimates
patience_SD<-patience_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

patience_SD_test_p<-patience_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
patience_VR<-(patience_SD[1,"est"]^2)/(patience_SD[2,"est"]^2)


patience_SD<-
  rbind(patience_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,patience_VR),
                   p=c(patience_SD_test_p$pvalue[1],NA)))
patience_SD<-
  patience_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

patience_slope.ests<-patience_sem_res[patience_sem_res$rowname %in% slope.pars,]
patience_slope.ests<-patience_slope.ests[order(match(patience_slope.ests$rowname,
                                                     slope.pars)), ]
patience_slope.ests<-
  patience_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
patience_slope.ests


# component correlation
patience_ry1y2<-
  patience_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
patience_sem_res
patience_add<-
  patience_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
patience_ds_SD<-
  patience_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

patience_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
patience_sem_tab<-
  bind_rows(patience_model.ests,
            patience_SD,
            patience_slope.ests,
            patience_ry1y2,
            patience_add,
            patience_rel_est)

patience_sem_tab


# combine the variables to same frame
sem_tab_2<-bind_cols(negrecip_sem_tab,risktaking_sem_tab[,2:3],patience_sem_tab[,2:3])

# export

save_as_docx(nice_table(sem_tab_2),
             path = "Study 2/results/sem_table_2.docx")
