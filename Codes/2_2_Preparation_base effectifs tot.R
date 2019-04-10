#-----------------#
#### Etude invites - preparation 2 ####
#-----------------#
# 11-07-2018 #
#-----------------#

library(tidyverse)

# base des effectifs totaux par branche, fonction et annee ####
effectifs_tot <- tibble(branche = rep(c("Louveteau/Jeannette","Scout/Guide",
                                        "Pionnier/Caravelle","Compagnon",
                                        "Farfadet"), 8),
                        annee = rep(c("2015", "2016", "2017", "2018"), each = 5, times = 2),
                        fonction_group = rep(c("Jeune", "Chef"), each = 20),
                        n = c(18566, 16737, 11844, 3917, 3569,
                              19541, 17469, 11946, 4590, 3883,
                              19508, 17581, 11997, 4938, 4110,
                              19938, 17748, 12000, 5363, 4351,
                              3781, 3486, 2272, 1086, 493,
                              4001, 3665, 2341, 1240, 610,
                              4037, 3675, 2301, 1356, 640,
                              4167, 3310, 2337, 1439, 3711))

# ajout des effectifs responsable par soustraction par rapport au total
effectifs_tot_resp <- tibble(branche = rep("Responsable", 4),
                             annee = c("2015", "2016", "2017", "2018"),
                             fonction_group = rep("Responsable", 4),
                             n = c(7751, 7920, 8242, 8614))

effectifs_tot <- bind_rows(effectifs_tot, effectifs_tot_resp)

# recodage de l'ordre des niveaux des facteurs
effectifs_tot <- effectifs_tot %>%
  mutate(branche = fct_relevel(branche,
                               "Farfadet",
                               "Louveteau/Jeannette",
                               "Scout/Guide",
                               "Pionnier/Caravelle",
                               "Compagnon",
                               "Responsable"),
         fonction_group = fct_relevel(fonction_group,
                                      "Jeune",
                                      "Chef",
                                      "Responsable"))

# sauvegarde
saveRDS(effectifs_tot, "Data/Augmented/effectifs_tot.rds")
