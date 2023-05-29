# compiling table for science attitudes

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


# SCIEEFF

SCIEEFF_desc<-import("Study 1/results/SCIEEFF_ml_desc.xlsx")
SCIEEFF_res<-import("Study 1/results/SCIEEFF_ml_results.xlsx")
SCIEEFF_cc<-import("Study 1/results/SCIEEFF_ml_comp_cor.xlsx")
SCIEEFF_ml_var_test<-import("Study 1/results/SCIEEFF_ml_var_test.xlsx")
SCIEEFF_reliab<-import("Study 1/results/reliab.SCIEEFF.xlsx")

SCIEEFF_desc

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
            "w_11","w_21")

SCIEEFF_model.ests<-SCIEEFF_res[SCIEEFF_res$rowname %in% model.pars,]
SCIEEFF_model.ests<-SCIEEFF_model.ests[order(match(SCIEEFF_model.ests$rowname, model.pars)), ]
SCIEEFF_model.ests<-
  SCIEEFF_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
SCIEEFF_model.ests

# observed variability estimates
SCIEEFF_SD<-SCIEEFF_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

SCIEEFF_SD_test_p<-SCIEEFF_ml_var_test$p
SCIEEFF_VR<-(SCIEEFF_SD[1,"est"]^2)/(SCIEEFF_SD[2,"est"]^2)

SCIEEFF_SD<-
  rbind(SCIEEFF_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,SCIEEFF_VR),
                   p=c(SCIEEFF_SD_test_p,NA)))
SCIEEFF_SD<-
  SCIEEFF_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

SCIEEFF_slope.ests<-SCIEEFF_res[SCIEEFF_res$rowname %in% slope.pars,]
SCIEEFF_slope.ests<-SCIEEFF_slope.ests[order(match(SCIEEFF_slope.ests$rowname,
                                                   slope.pars)), ]
SCIEEFF_slope.ests<-
  SCIEEFF_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
SCIEEFF_slope.ests


# component correlation
SCIEEFF_ry1y2<-
  SCIEEFF_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

SCIEEFF_add<-
  SCIEEFF_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
SCIEEFF_ds_SD<-
  SCIEEFF_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

SCIEEFF_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(SCIEEFF_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
SCIEEFF_ml_tab<-
  bind_rows(SCIEEFF_model.ests,
          SCIEEFF_SD,
          SCIEEFF_slope.ests,
          SCIEEFF_ry1y2,
          SCIEEFF_add,
          SCIEEFF_rel_est)


# INTBRSCI

INTBRSCI_desc<-import("Study 1/results/INTBRSCI_ml_desc.xlsx")
INTBRSCI_res<-import("Study 1/results/INTBRSCI_ml_results.xlsx")
INTBRSCI_cc<-import("Study 1/results/INTBRSCI_ml_comp_cor.xlsx")
INTBRSCI_ml_var_test<-import("Study 1/results/INTBRSCI_ml_var_test.xlsx")
INTBRSCI_reliab<-import("Study 1/results/reliab.INTBRSCI.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

INTBRSCI_model.ests<-INTBRSCI_res[INTBRSCI_res$rowname %in% model.pars,]
INTBRSCI_model.ests<-INTBRSCI_model.ests[order(match(INTBRSCI_model.ests$rowname, model.pars)), ]
INTBRSCI_model.ests<-
  INTBRSCI_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
INTBRSCI_model.ests

# observed variability estimates
INTBRSCI_SD<-INTBRSCI_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

INTBRSCI_SD_test_p<-INTBRSCI_ml_var_test$p
INTBRSCI_VR<-(INTBRSCI_SD[1,"est"]^2)/(INTBRSCI_SD[2,"est"]^2)

INTBRSCI_SD<-
  rbind(INTBRSCI_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,INTBRSCI_VR),
                   p=c(INTBRSCI_SD_test_p,NA)))
INTBRSCI_SD<-
  INTBRSCI_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

INTBRSCI_slope.ests<-INTBRSCI_res[INTBRSCI_res$rowname %in% slope.pars,]
INTBRSCI_slope.ests<-INTBRSCI_slope.ests[order(match(INTBRSCI_slope.ests$rowname,
                                                   slope.pars)), ]
INTBRSCI_slope.ests<-
  INTBRSCI_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
INTBRSCI_slope.ests


# component correlation
INTBRSCI_ry1y2<-
  INTBRSCI_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

INTBRSCI_add<-
  INTBRSCI_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
INTBRSCI_ds_SD<-
  INTBRSCI_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

INTBRSCI_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(INTBRSCI_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
INTBRSCI_ml_tab<-
  bind_rows(INTBRSCI_model.ests,
            INTBRSCI_SD,
            INTBRSCI_slope.ests,
            INTBRSCI_ry1y2,
            INTBRSCI_add,
            INTBRSCI_rel_est)
INTBRSCI_ml_tab


# JOYSCIE

JOYSCIE_desc<-import("Study 1/results/JOYSCIE_ml_desc.xlsx")
JOYSCIE_res<-import("Study 1/results/JOYSCIE_ml_results.xlsx")
JOYSCIE_cc<-import("Study 1/results/JOYSCIE_ml_comp_cor.xlsx")
JOYSCIE_ml_var_test<-import("Study 1/results/JOYSCIE_ml_var_test.xlsx")
JOYSCIE_reliab<-import("Study 1/results/reliab.JOYSCIE.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

JOYSCIE_model.ests<-JOYSCIE_res[JOYSCIE_res$rowname %in% model.pars,]
JOYSCIE_model.ests<-JOYSCIE_model.ests[order(match(JOYSCIE_model.ests$rowname, model.pars)), ]
JOYSCIE_model.ests<-
  JOYSCIE_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
JOYSCIE_model.ests

# observed variability estimates
JOYSCIE_SD<-JOYSCIE_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

JOYSCIE_SD_test_p<-JOYSCIE_ml_var_test$p
JOYSCIE_VR<-(JOYSCIE_SD[1,"est"]^2)/(JOYSCIE_SD[2,"est"]^2)

JOYSCIE_SD<-
  rbind(JOYSCIE_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,JOYSCIE_VR),
                   p=c(JOYSCIE_SD_test_p,NA)))
JOYSCIE_SD<-
  JOYSCIE_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

JOYSCIE_slope.ests<-JOYSCIE_res[JOYSCIE_res$rowname %in% slope.pars,]
JOYSCIE_slope.ests<-JOYSCIE_slope.ests[order(match(JOYSCIE_slope.ests$rowname,
                                                     slope.pars)), ]
JOYSCIE_slope.ests<-
  JOYSCIE_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
JOYSCIE_slope.ests


# component correlation
JOYSCIE_ry1y2<-
  JOYSCIE_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

JOYSCIE_add<-
  JOYSCIE_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
JOYSCIE_ds_SD<-
  JOYSCIE_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

JOYSCIE_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(JOYSCIE_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
JOYSCIE_ml_tab<-
  bind_rows(JOYSCIE_model.ests,
            JOYSCIE_SD,
            JOYSCIE_slope.ests,
            JOYSCIE_ry1y2,
            JOYSCIE_add,
            JOYSCIE_rel_est)
JOYSCIE_ml_tab


# combine the variables to same frame
ml_tab<-bind_cols(SCIEEFF_ml_tab,INTBRSCI_ml_tab[,2:3],JOYSCIE_ml_tab[,2:3])

# export

save_as_docx(nice_table(ml_tab),
             path = "Study 1/results/ml_table.docx")

# tables for sem models

# SCIEEFF
SCIEEFF_sem_res<-import("Study 1/results/SCIEEFF_sem_results.xlsx")

SCIEEFF_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

SCIEEFF_model.ests<-SCIEEFF_sem_res[SCIEEFF_sem_res$rowname %in% model.pars,]
SCIEEFF_model.ests<-SCIEEFF_model.ests[order(match(SCIEEFF_model.ests$rowname, model.pars)), ]
SCIEEFF_model.ests<-
  SCIEEFF_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
SCIEEFF_model.ests

# observed variability estimates
SCIEEFF_SD<-SCIEEFF_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

SCIEEFF_SD_test_p<-SCIEEFF_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
SCIEEFF_VR<-(SCIEEFF_SD[1,"est"]^2)/(SCIEEFF_SD[2,"est"]^2)


SCIEEFF_SD<-
  rbind(SCIEEFF_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,SCIEEFF_VR),
                   p=c(SCIEEFF_SD_test_p$pvalue[1],NA)))
SCIEEFF_SD<-
  SCIEEFF_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

SCIEEFF_slope.ests<-SCIEEFF_sem_res[SCIEEFF_sem_res$rowname %in% slope.pars,]
SCIEEFF_slope.ests<-SCIEEFF_slope.ests[order(match(SCIEEFF_slope.ests$rowname,
                                                   slope.pars)), ]
SCIEEFF_slope.ests<-
  SCIEEFF_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
SCIEEFF_slope.ests


# component correlation
SCIEEFF_ry1y2<-
  SCIEEFF_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
SCIEEFF_sem_res
SCIEEFF_add<-
  SCIEEFF_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
SCIEEFF_ds_SD<-
  SCIEEFF_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

SCIEEFF_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
SCIEEFF_sem_tab<-
  bind_rows(SCIEEFF_model.ests,
            SCIEEFF_SD,
            SCIEEFF_slope.ests,
            SCIEEFF_ry1y2,
            SCIEEFF_add,
            SCIEEFF_rel_est)

SCIEEFF_sem_tab


# INTBRSCI
INTBRSCI_sem_res<-import("Study 1/results/INTBRSCI_sem_results.xlsx")

INTBRSCI_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

INTBRSCI_model.ests<-INTBRSCI_sem_res[INTBRSCI_sem_res$rowname %in% model.pars,]
INTBRSCI_model.ests<-INTBRSCI_model.ests[order(match(INTBRSCI_model.ests$rowname, model.pars)), ]
INTBRSCI_model.ests<-
  INTBRSCI_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
INTBRSCI_model.ests

# observed variability estimates
INTBRSCI_SD<-INTBRSCI_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

INTBRSCI_SD_test_p<-INTBRSCI_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
INTBRSCI_VR<-(INTBRSCI_SD[1,"est"]^2)/(INTBRSCI_SD[2,"est"]^2)


INTBRSCI_SD<-
  rbind(INTBRSCI_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,INTBRSCI_VR),
                   p=c(INTBRSCI_SD_test_p$pvalue[1],NA)))
INTBRSCI_SD<-
  INTBRSCI_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

INTBRSCI_slope.ests<-INTBRSCI_sem_res[INTBRSCI_sem_res$rowname %in% slope.pars,]
INTBRSCI_slope.ests<-INTBRSCI_slope.ests[order(match(INTBRSCI_slope.ests$rowname,
                                                   slope.pars)), ]
INTBRSCI_slope.ests<-
  INTBRSCI_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
INTBRSCI_slope.ests


# component correlation
INTBRSCI_ry1y2<-
  INTBRSCI_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
INTBRSCI_sem_res
INTBRSCI_add<-
  INTBRSCI_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
INTBRSCI_ds_SD<-
  INTBRSCI_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

INTBRSCI_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
INTBRSCI_sem_tab<-
  bind_rows(INTBRSCI_model.ests,
            INTBRSCI_SD,
            INTBRSCI_slope.ests,
            INTBRSCI_ry1y2,
            INTBRSCI_add,
            INTBRSCI_rel_est)

INTBRSCI_sem_tab


# JOYSCIE
JOYSCIE_sem_res<-import("Study 1/results/JOYSCIE_sem_results.xlsx")

JOYSCIE_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

JOYSCIE_model.ests<-JOYSCIE_sem_res[JOYSCIE_sem_res$rowname %in% model.pars,]
JOYSCIE_model.ests<-JOYSCIE_model.ests[order(match(JOYSCIE_model.ests$rowname, model.pars)), ]
JOYSCIE_model.ests<-
  JOYSCIE_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
JOYSCIE_model.ests

# observed variability estimates
JOYSCIE_SD<-JOYSCIE_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

JOYSCIE_SD_test_p<-JOYSCIE_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
JOYSCIE_VR<-(JOYSCIE_SD[1,"est"]^2)/(JOYSCIE_SD[2,"est"]^2)


JOYSCIE_SD<-
  rbind(JOYSCIE_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,JOYSCIE_VR),
                   p=c(JOYSCIE_SD_test_p$pvalue[1],NA)))
JOYSCIE_SD<-
  JOYSCIE_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

JOYSCIE_slope.ests<-JOYSCIE_sem_res[JOYSCIE_sem_res$rowname %in% slope.pars,]
JOYSCIE_slope.ests<-JOYSCIE_slope.ests[order(match(JOYSCIE_slope.ests$rowname,
                                                   slope.pars)), ]
JOYSCIE_slope.ests<-
  JOYSCIE_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
JOYSCIE_slope.ests


# component correlation
JOYSCIE_ry1y2<-
  JOYSCIE_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
JOYSCIE_sem_res
JOYSCIE_add<-
  JOYSCIE_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
JOYSCIE_ds_SD<-
  JOYSCIE_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

JOYSCIE_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
JOYSCIE_sem_tab<-
  bind_rows(JOYSCIE_model.ests,
            JOYSCIE_SD,
            JOYSCIE_slope.ests,
            JOYSCIE_ry1y2,
            JOYSCIE_add,
            JOYSCIE_rel_est)

JOYSCIE_sem_tab


# combine the variables to same frame
sem_tab<-bind_cols(SCIEEFF_sem_tab,INTBRSCI_sem_tab[,2:3],JOYSCIE_sem_tab[,2:3])

# export

save_as_docx(nice_table(sem_tab),
             path = "Study 1/results/sem_table.docx")
