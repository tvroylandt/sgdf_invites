#-----------------#
#### Etude invites - preparation 3 ####
#-----------------#
# 11-07-2018 #
#-----------------#

library(tidyverse)
library(lubridate)

# base des individus ####
invit_data_wed <- readRDS("Data/Augmented/invit_data_wed.rds")

# base sur les WED --> a affiner ####
unites_data <- readRDS(file = "Data/Cleaned/unites_data.rds")

unites_data_s <- unites_data %>%
  mutate(branche = fct_recode(type_un,
                              "Louveteau/Jeannette" = "Unité 8-11 ans",
                              "Scout/Guide" = "Unité 11-14 ans",
                              "Pionnier/Caravelle" = "Unité 14-17 ans",
                              "Compagnon" = "Unité 17-20 ans",
                              "Farfadet" = "Unité Farfadet",
                              "Vent du large" = "Unité Vent du Large")) %>%
  group_by(structure_gr, branche) %>%
  summarise(nb_jeunes_s = sum(nb_jeunes))

invit_data_wed_s <- invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune") %>%
  group_by(structure_gr, branche, ind_invit_wed) %>%
  count() %>%
  spread(key = ind_invit_wed, value = n) %>%
  rename(n_rel_wed = `Invitation reliée au WED`,
         n_norel_wed = `Invitation non reliée au WED`,
         n_no_wed = `Pas de WED`) 

invit_data_unites <- invit_data_wed_s %>%
  right_join(unites_data_s, by = c("structure_gr", "branche")) %>%
  replace(., is.na(.), 0) %>%
  group_by(branche) %>%
  summarise(n_rel_wed = sum(n_rel_wed),
            n_norel_wed = sum(n_rel_wed),
            n_no_wed = sum(n_no_wed),
            n_invit = (sum(n_rel_wed) + sum(n_rel_wed) + sum(n_no_wed)),
            n_jeunes = sum(nb_jeunes_s))

saveRDS(invit_data_unites , "Data/Augmented/invit_data_unites.rds")
