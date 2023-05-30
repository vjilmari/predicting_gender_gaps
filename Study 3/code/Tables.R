# compiling table for personality

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


# FM

FM_desc<-import("Study 3/results/FM_ml_desc.xlsx")
FM_res<-import("Study 3/results/FM_ml_results.xlsx")
FM_cc<-import("Study 3/results/FM_ml_comp_cor.xlsx")
FM_ml_var_test<-import("Study 3/results/FM_ml_var_test.xlsx")
FM_reliab<-import("Study 3/results/reliab.FM.xlsx")

FM_desc

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
            "w_11","w_21")

FM_model.ests<-FM_res[FM_res$rowname %in% model.pars,]
FM_model.ests<-FM_model.ests[order(match(FM_model.ests$rowname, model.pars)), ]
FM_model.ests<-
  FM_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
FM_model.ests

# observed variability estimates
FM_SD<-FM_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

FM_SD_test_p<-FM_ml_var_test$p
FM_VR<-(FM_SD[1,"est"]^2)/(FM_SD[2,"est"]^2)

FM_SD<-
  rbind(FM_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,FM_VR),
                   p=c(FM_SD_test_p,NA)))
FM_SD<-
  FM_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

FM_slope.ests<-FM_res[FM_res$rowname %in% slope.pars,]
FM_slope.ests<-FM_slope.ests[order(match(FM_slope.ests$rowname,
                                                   slope.pars)), ]
FM_slope.ests<-
  FM_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
FM_slope.ests


# component correlation
FM_ry1y2<-
  FM_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

FM_add<-
  FM_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
FM_ds_SD<-
  FM_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

FM_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(FM_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
FM_ml_tab<-
  bind_rows(FM_model.ests,
          FM_SD,
          FM_slope.ests,
          FM_ry1y2,
          FM_add,
          FM_rel_est)

FM_ml_tab

# N.z

N.z_desc<-import("Study 3/results/N.z_ml_desc.xlsx")
N.z_res<-import("Study 3/results/N.z_ml_results.xlsx")
N.z_cc<-import("Study 3/results/N.z_ml_comp_cor.xlsx")
N.z_ml_var_test<-import("Study 3/results/N.z_ml_var_test.xlsx")
N.z_reliab<-import("Study 3/results/reliab.N.z.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

N.z_model.ests<-N.z_res[N.z_res$rowname %in% model.pars,]
N.z_model.ests<-N.z_model.ests[order(match(N.z_model.ests$rowname, model.pars)), ]
N.z_model.ests<-
  N.z_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
N.z_model.ests

# observed variability estimates
N.z_SD<-N.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

N.z_SD_test_p<-N.z_ml_var_test$p
N.z_VR<-(N.z_SD[1,"est"]^2)/(N.z_SD[2,"est"]^2)

N.z_SD<-
  rbind(N.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,N.z_VR),
                   p=c(N.z_SD_test_p,NA)))
N.z_SD<-
  N.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

N.z_slope.ests<-N.z_res[N.z_res$rowname %in% slope.pars,]
N.z_slope.ests<-N.z_slope.ests[order(match(N.z_slope.ests$rowname,
                                                   slope.pars)), ]
N.z_slope.ests<-
  N.z_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
N.z_slope.ests


# component correlation
N.z_ry1y2<-
  N.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

N.z_add<-
  N.z_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
N.z_ds_SD<-
  N.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

N.z_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(N.z_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
N.z_ml_tab<-
  bind_rows(N.z_model.ests,
            N.z_SD,
            N.z_slope.ests,
            N.z_ry1y2,
            N.z_add,
            N.z_rel_est)
N.z_ml_tab


# E.z

E.z_desc<-import("Study 3/results/E.z_ml_desc.xlsx")
E.z_res<-import("Study 3/results/E.z_ml_results.xlsx")
E.z_cc<-import("Study 3/results/E.z_ml_comp_cor.xlsx")
E.z_ml_var_test<-import("Study 3/results/E.z_ml_var_test.xlsx")
E.z_reliab<-import("Study 3/results/reliab.E.z.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

E.z_model.ests<-E.z_res[E.z_res$rowname %in% model.pars,]
E.z_model.ests<-E.z_model.ests[order(match(E.z_model.ests$rowname, model.pars)), ]
E.z_model.ests<-
  E.z_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
E.z_model.ests

# observed variability estimates
E.z_SD<-E.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

E.z_SD_test_p<-E.z_ml_var_test$p
E.z_VR<-(E.z_SD[1,"est"]^2)/(E.z_SD[2,"est"]^2)

E.z_SD<-
  rbind(E.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,E.z_VR),
                   p=c(E.z_SD_test_p,NA)))
E.z_SD<-
  E.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

E.z_slope.ests<-E.z_res[E.z_res$rowname %in% slope.pars,]
E.z_slope.ests<-E.z_slope.ests[order(match(E.z_slope.ests$rowname,
                                                     slope.pars)), ]
E.z_slope.ests<-
  E.z_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
E.z_slope.ests


# component correlation
E.z_ry1y2<-
  E.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

E.z_add<-
  E.z_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
E.z_ds_SD<-
  E.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

E.z_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(E.z_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
E.z_ml_tab<-
  bind_rows(E.z_model.ests,
            E.z_SD,
            E.z_slope.ests,
            E.z_ry1y2,
            E.z_add,
            E.z_rel_est)
E.z_ml_tab


# combine the variables to same frame
ml_tab_1<-bind_cols(FM_ml_tab,N.z_ml_tab[,2:3],E.z_ml_tab[,2:3])

# export

save_as_docx(nice_table(ml_tab_1),
             path = "Study 3/results/ml_table_1.docx")

# tables for sem models

# FM
FM_sem_res<-import("Study 3/results/FM_sem_results.xlsx")

FM_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

FM_model.ests<-FM_sem_res[FM_sem_res$rowname %in% model.pars,]
FM_model.ests<-FM_model.ests[order(match(FM_model.ests$rowname, model.pars)), ]
FM_model.ests<-
  FM_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
FM_model.ests

# observed variability estimates
FM_SD<-FM_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

FM_SD_test_p<-FM_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
FM_VR<-(FM_SD[1,"est"]^2)/(FM_SD[2,"est"]^2)


FM_SD<-
  rbind(FM_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,FM_VR),
                   p=c(FM_SD_test_p$pvalue[1],NA)))
FM_SD<-
  FM_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

FM_slope.ests<-FM_sem_res[FM_sem_res$rowname %in% slope.pars,]
FM_slope.ests<-FM_slope.ests[order(match(FM_slope.ests$rowname,
                                                   slope.pars)), ]
FM_slope.ests<-
  FM_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
FM_slope.ests


# component correlation
FM_ry1y2<-
  FM_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

FM_add<-
  FM_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
FM_ds_SD<-
  FM_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

FM_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
FM_sem_tab<-
  bind_rows(FM_model.ests,
            FM_SD,
            FM_slope.ests,
            FM_ry1y2,
            FM_add,
            FM_rel_est)

FM_sem_tab


# N.z
N.z_sem_res<-import("Study 3/results/N.z_sem_results.xlsx")

N.z_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

N.z_model.ests<-N.z_sem_res[N.z_sem_res$rowname %in% model.pars,]
N.z_model.ests<-N.z_model.ests[order(match(N.z_model.ests$rowname, model.pars)), ]
N.z_model.ests<-
  N.z_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
N.z_model.ests

# observed variability estimates
N.z_SD<-N.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

N.z_SD_test_p<-N.z_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
N.z_VR<-(N.z_SD[1,"est"]^2)/(N.z_SD[2,"est"]^2)


N.z_SD<-
  rbind(N.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,N.z_VR),
                   p=c(N.z_SD_test_p$pvalue[1],NA)))
N.z_SD<-
  N.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

N.z_slope.ests<-N.z_sem_res[N.z_sem_res$rowname %in% slope.pars,]
N.z_slope.ests<-N.z_slope.ests[order(match(N.z_slope.ests$rowname,
                                                   slope.pars)), ]
N.z_slope.ests<-
  N.z_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
N.z_slope.ests


# component correlation
N.z_ry1y2<-
  N.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
N.z_sem_res
N.z_add<-
  N.z_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
N.z_ds_SD<-
  N.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

N.z_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
N.z_sem_tab<-
  bind_rows(N.z_model.ests,
            N.z_SD,
            N.z_slope.ests,
            N.z_ry1y2,
            N.z_add,
            N.z_rel_est)

N.z_sem_tab


# E.z
E.z_sem_res<-import("Study 3/results/E.z_sem_results.xlsx")

E.z_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

E.z_model.ests<-E.z_sem_res[E.z_sem_res$rowname %in% model.pars,]
E.z_model.ests<-E.z_model.ests[order(match(E.z_model.ests$rowname, model.pars)), ]
E.z_model.ests<-
  E.z_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
E.z_model.ests

# observed variability estimates
E.z_SD<-E.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

E.z_SD_test_p<-E.z_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
E.z_VR<-(E.z_SD[1,"est"]^2)/(E.z_SD[2,"est"]^2)


E.z_SD<-
  rbind(E.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,E.z_VR),
                   p=c(E.z_SD_test_p$pvalue[1],NA)))
E.z_SD<-
  E.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

E.z_slope.ests<-E.z_sem_res[E.z_sem_res$rowname %in% slope.pars,]
E.z_slope.ests<-E.z_slope.ests[order(match(E.z_slope.ests$rowname,
                                                   slope.pars)), ]
E.z_slope.ests<-
  E.z_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
E.z_slope.ests


# component correlation
E.z_ry1y2<-
  E.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
E.z_sem_res
E.z_add<-
  E.z_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
E.z_ds_SD<-
  E.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

E.z_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
E.z_sem_tab<-
  bind_rows(E.z_model.ests,
            E.z_SD,
            E.z_slope.ests,
            E.z_ry1y2,
            E.z_add,
            E.z_rel_est)

E.z_sem_tab


# combine the variables to same frame
sem_tab_1<-bind_cols(FM_sem_tab,N.z_sem_tab[,2:3],E.z_sem_tab[,2:3])

# export

save_as_docx(nice_table(sem_tab_1),
             path = "Study 3/results/sem_table_1.docx")



# O.z

O.z_desc<-import("Study 3/results/O.z_ml_desc.xlsx")
O.z_res<-import("Study 3/results/O.z_ml_results.xlsx")
O.z_cc<-import("Study 3/results/O.z_ml_comp_cor.xlsx")
O.z_ml_var_test<-import("Study 3/results/O.z_ml_var_test.xlsx")
O.z_reliab<-import("Study 3/results/reliab.O.z.xlsx")

O.z_desc

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

O.z_model.ests<-O.z_res[O.z_res$rowname %in% model.pars,]
O.z_model.ests<-O.z_model.ests[order(match(O.z_model.ests$rowname, model.pars)), ]
O.z_model.ests<-
  O.z_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
O.z_model.ests

# observed variability estimates
O.z_SD<-O.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

O.z_SD_test_p<-O.z_ml_var_test$p
O.z_VR<-(O.z_SD[1,"est"]^2)/(O.z_SD[2,"est"]^2)

O.z_SD<-
  rbind(O.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,O.z_VR),
                   p=c(O.z_SD_test_p,NA)))
O.z_SD<-
  O.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

O.z_slope.ests<-O.z_res[O.z_res$rowname %in% slope.pars,]
O.z_slope.ests<-O.z_slope.ests[order(match(O.z_slope.ests$rowname,
                                                     slope.pars)), ]
O.z_slope.ests<-
  O.z_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
O.z_slope.ests


# component correlation
O.z_ry1y2<-
  O.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

O.z_add<-
  O.z_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
O.z_ds_SD<-
  O.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

O.z_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(O.z_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
O.z_ml_tab<-
  bind_rows(O.z_model.ests,
            O.z_SD,
            O.z_slope.ests,
            O.z_ry1y2,
            O.z_add,
            O.z_rel_est)

O.z_ml_tab

# A.z

A.z_desc<-import("Study 3/results/A.z_ml_desc.xlsx")
A.z_res<-import("Study 3/results/A.z_ml_results.xlsx")
A.z_cc<-import("Study 3/results/A.z_ml_comp_cor.xlsx")
A.z_ml_var_test<-import("Study 3/results/A.z_ml_var_test.xlsx")
A.z_reliab<-import("Study 3/results/reliab.A.z.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

A.z_model.ests<-A.z_res[A.z_res$rowname %in% model.pars,]
A.z_model.ests<-A.z_model.ests[order(match(A.z_model.ests$rowname, model.pars)), ]
A.z_model.ests<-
  A.z_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
A.z_model.ests

# observed variability estimates
A.z_SD<-A.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

A.z_SD_test_p<-A.z_ml_var_test$p
A.z_VR<-(A.z_SD[1,"est"]^2)/(A.z_SD[2,"est"]^2)

A.z_SD<-
  rbind(A.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,A.z_VR),
                   p=c(A.z_SD_test_p,NA)))
A.z_SD<-
  A.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

A.z_slope.ests<-A.z_res[A.z_res$rowname %in% slope.pars,]
A.z_slope.ests<-A.z_slope.ests[order(match(A.z_slope.ests$rowname,
                                               slope.pars)), ]
A.z_slope.ests<-
  A.z_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
A.z_slope.ests


# component correlation
A.z_ry1y2<-
  A.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

A.z_add<-
  A.z_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
A.z_ds_SD<-
  A.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

A.z_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(A.z_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
A.z_ml_tab<-
  bind_rows(A.z_model.ests,
            A.z_SD,
            A.z_slope.ests,
            A.z_ry1y2,
            A.z_add,
            A.z_rel_est)
A.z_ml_tab


# C.z

C.z_desc<-import("Study 3/results/C.z_ml_desc.xlsx")
C.z_res<-import("Study 3/results/C.z_ml_results.xlsx")
C.z_cc<-import("Study 3/results/C.z_ml_comp_cor.xlsx")
C.z_ml_var_test<-import("Study 3/results/C.z_ml_var_test.xlsx")
C.z_reliab<-import("Study 3/results/reliab.C.z.xlsx")

# estimates from multi-level model
model.pars<-c("main_effect","moderator_effect","interaction",
              "w_11","w_21")

C.z_model.ests<-C.z_res[C.z_res$rowname %in% model.pars,]
C.z_model.ests<-C.z_model.ests[order(match(C.z_model.ests$rowname, model.pars)), ]
C.z_model.ests<-
  C.z_model.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
C.z_model.ests

# observed variability estimates
C.z_SD<-C.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

C.z_SD_test_p<-C.z_ml_var_test$p
C.z_VR<-(C.z_SD[1,"est"]^2)/(C.z_SD[2,"est"]^2)

C.z_SD<-
  rbind(C.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,C.z_VR),
                   p=c(C.z_SD_test_p,NA)))
C.z_SD<-
  C.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

C.z_slope.ests<-C.z_res[C.z_res$rowname %in% slope.pars,]
C.z_slope.ests<-C.z_slope.ests[order(match(C.z_slope.ests$rowname,
                                                     slope.pars)), ]
C.z_slope.ests<-
  C.z_slope.ests %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)
C.z_slope.ests


# component correlation
C.z_ry1y2<-
  C.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

C.z_add<-
  C.z_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main" |
           rowname=="dadas_bscale") %>%
  mutate(est=round_tidy(estimate,2),
         p=p_coding(p.value)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
C.z_ds_SD<-
  C.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

C.z_rel_est<-
  data.frame(rowname="reliability",
             est=round_tidy(unname(C.z_reliab["reliability_dmsa"]),2),
             p=NA)


# bind rows
C.z_ml_tab<-
  bind_rows(C.z_model.ests,
            C.z_SD,
            C.z_slope.ests,
            C.z_ry1y2,
            C.z_add,
            C.z_rel_est)
C.z_ml_tab


# combine the variables to same frame
ml_tab_2<-bind_cols(O.z_ml_tab,A.z_ml_tab[,2:3],C.z_ml_tab[,2:3])

# export

save_as_docx(nice_table(ml_tab_2),
             path = "Study 3/results/ml_table_2.docx")

# tables for sem models

# O.z
O.z_sem_res<-import("Study 3/results/O.z_sem_results.xlsx")

O.z_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

O.z_model.ests<-O.z_sem_res[O.z_sem_res$rowname %in% model.pars,]
O.z_model.ests<-O.z_model.ests[order(match(O.z_model.ests$rowname, model.pars)), ]
O.z_model.ests<-
  O.z_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
O.z_model.ests

# observed variability estimates
O.z_SD<-O.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

O.z_SD_test_p<-O.z_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
O.z_VR<-(O.z_SD[1,"est"]^2)/(O.z_SD[2,"est"]^2)


O.z_SD<-
  rbind(O.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,O.z_VR),
                   p=c(O.z_SD_test_p$pvalue[1],NA)))
O.z_SD<-
  O.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

O.z_slope.ests<-O.z_sem_res[O.z_sem_res$rowname %in% slope.pars,]
O.z_slope.ests<-O.z_slope.ests[order(match(O.z_slope.ests$rowname,
                                                     slope.pars)), ]
O.z_slope.ests<-
  O.z_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
O.z_slope.ests


# component correlation
O.z_ry1y2<-
  O.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics

O.z_add<-
  O.z_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
O.z_ds_SD<-
  O.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

O.z_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
O.z_sem_tab<-
  bind_rows(O.z_model.ests,
            O.z_SD,
            O.z_slope.ests,
            O.z_ry1y2,
            O.z_add,
            O.z_rel_est)

O.z_sem_tab


# A.z
A.z_sem_res<-import("Study 3/results/A.z_sem_results.xlsx")

A.z_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

A.z_model.ests<-A.z_sem_res[A.z_sem_res$rowname %in% model.pars,]
A.z_model.ests<-A.z_model.ests[order(match(A.z_model.ests$rowname, model.pars)), ]
A.z_model.ests<-
  A.z_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
A.z_model.ests

# observed variability estimates
A.z_SD<-A.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

A.z_SD_test_p<-A.z_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
A.z_VR<-(A.z_SD[1,"est"]^2)/(A.z_SD[2,"est"]^2)


A.z_SD<-
  rbind(A.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,A.z_VR),
                   p=c(A.z_SD_test_p$pvalue[1],NA)))
A.z_SD<-
  A.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

A.z_slope.ests<-A.z_sem_res[A.z_sem_res$rowname %in% slope.pars,]
A.z_slope.ests<-A.z_slope.ests[order(match(A.z_slope.ests$rowname,
                                               slope.pars)), ]
A.z_slope.ests<-
  A.z_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
A.z_slope.ests


# component correlation
A.z_ry1y2<-
  A.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
A.z_sem_res
A.z_add<-
  A.z_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
A.z_ds_SD<-
  A.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

A.z_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
A.z_sem_tab<-
  bind_rows(A.z_model.ests,
            A.z_SD,
            A.z_slope.ests,
            A.z_ry1y2,
            A.z_add,
            A.z_rel_est)

A.z_sem_tab


# C.z
C.z_sem_res<-import("Study 3/results/C.z_sem_results.xlsx")

C.z_sem_res

# estimates from sem
model.pars<-c("main_effect","diff_b10_b20","diff_b11_b21")

C.z_model.ests<-C.z_sem_res[C.z_sem_res$rowname %in% model.pars,]
C.z_model.ests<-C.z_model.ests[order(match(C.z_model.ests$rowname, model.pars)), ]
C.z_model.ests<-
  C.z_model.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
C.z_model.ests

# observed variability estimates
C.z_SD<-C.z_desc %>%
  filter(rowname=="means_y1" |rowname=="means_y2")%>%
  select(rowname,SD) %>%
  rename(est=SD) %>%
  mutate(p=NA)

C.z_SD_test_p<-C.z_cc %>% filter(rowname=="var_diff") %>% select(pvalue)
C.z_VR<-(C.z_SD[1,"est"]^2)/(C.z_SD[2,"est"]^2)


C.z_SD<-
  rbind(C.z_SD,
        data.frame(rowname=c("Difference","VR"),
                   est=c(NA,C.z_VR),
                   p=c(C.z_SD_test_p$pvalue[1],NA)))
C.z_SD<-
  C.z_SD %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(p))

# scaled slope and divergence estimates

slope.pars<-c("r_xy1","r_xy2",
              "b_11","b_21",
              "r_xy1y2","q_rxy1_rxy2","q_b11_b21")

C.z_slope.ests<-C.z_sem_res[C.z_sem_res$rowname %in% slope.pars,]
C.z_slope.ests<-C.z_slope.ests[order(match(C.z_slope.ests$rowname,
                                                     slope.pars)), ]
C.z_slope.ests<-
  C.z_slope.ests %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)
C.z_slope.ests


# component correlation
C.z_ry1y2<-
  C.z_cc %>%
  filter(rowname=="cor_y1y2") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  select(rowname,est,p)

# additional test statistics
C.z_sem_res
C.z_add<-
  C.z_sem_res %>%
  filter(rowname=="cross_over_point" |
           rowname=="interaction_vs_main_effect" |
           rowname=="dadas") %>%
  mutate(est=round_tidy(est,2),
         p=p_coding(pvalue)) %>%
  dplyr::select(rowname,est,p)

# difference score variability and reliability
C.z_ds_SD<-
  C.z_desc %>%
  filter(rowname=="diff_score") %>%
  mutate(est=SD,
         p=NA) %>%
  select(est,p)

C.z_rel_est<-
  data.frame(rowname="reliability",
             est=NA,
             p=NA)


# bind rows
C.z_sem_tab<-
  bind_rows(C.z_model.ests,
            C.z_SD,
            C.z_slope.ests,
            C.z_ry1y2,
            C.z_add,
            C.z_rel_est)

C.z_sem_tab


# combine the variables to same frame
sem_tab_2<-bind_cols(O.z_sem_tab,A.z_sem_tab[,2:3],C.z_sem_tab[,2:3])

# export

save_as_docx(nice_table(sem_tab_2),
             path = "Study 3/results/sem_table_2.docx")
