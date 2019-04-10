#-----------------#
#### Etude invites - preparation 1 ####
#-----------------#
# 31-07-2018 #
#-----------------#

library(tidyverse)
library(lubridate)

# base des individus ####
invit_data <- readRDS(file = "Data/Cleaned/invit_data.rds")

# ajout de variables et recodage des codes fonction 
invit_data <- invit_data %>%
  filter(annee %in% c("2018","2017","2016","2015") & str_sub(fonction, 1, 2) != "29" & str_sub(fonction, 1, 2) != "19" ) %>% # filtre Vent du large
  mutate(structure_ter = paste(str_sub(structure_un, 1, 5), "0000", sep = ""), # code territoire
       structure_gr = paste(str_sub(structure_un, 1, 7), "00", sep = ""), # code groupe
       structure_un = paste(str_sub(structure_un, 1, 8), "0", sep = ""), # code unite
       fonction_r = fct_collapse(str_sub(fonction, 1, 2),
                                 "Louveteau/Jeannette" = "11",
                                 "Scout/Guide" = c("12", "15", "16"),
                                 "Pionnier/Caravelle" = "13",
                                 "Compagnon" = "14",
                                 "Farfadet" = "17",
                                 #"Vent du large" = c("19", "29"),
                                 "Chef LJ" = "21",
                                 "Chef SG" = c("22", "25", "26"),
                                 "Chef PK" = "23",
                                 "Accompagnateur compagnon" = "24",
                                 "Animateur farfadet" = "27",
                                 "Membre associé" = "18",
                                 "Responsable" = c("30", "31", "33", "38", "50",
                                                   "60", "62", "63", "65", "66",
                                                   "67", "69", "91", "98")), # recodage code fonction
       fonction_group = fct_collapse(fonction_r,
                                     "Jeune" = c("Louveteau/Jeannette", "Scout/Guide", "Pionnier/Caravelle", "Compagnon",
                                                 "Farfadet"), #, "Vent du large"),
                                     "Chef" = c("Chef LJ", "Chef SG", "Chef PK",
                                                "Accompagnateur compagnon", "Animateur farfadet"),
                                     "Responsable" = c("Responsable", "Membre associé")), # tripartition Jeune/Chef/Responsable
       branche = fct_collapse(fonction_r,
                              "Louveteau/Jeannette" = c("Louveteau/Jeannette", "Chef LJ"),
                              "Scout/Guide" = c("Scout/Guide", "Chef SG"),
                              "Pionnier/Caravelle" = c("Pionnier/Caravelle", "Chef PK"),
                              "Compagnon" = c("Compagnon", "Accompagnateur compagnon"),
                              "Farfadet" = c("Farfadet", "Animateur farfadet"),
                              #"Vent du large" = "Vent du large",
                              "Responsable"= c("Membre associé",
                                               "Responsable"))) # par branche

# indicatrice d'inscription -> par difference avec le 31-08 en date de fin d'inscription 
invit_data <- invit_data %>%
  mutate(date_fin_annee = ymd(paste(annee,"08","31", sep = "-")),
       ind_inscr = cut((as.duration(date_fin_annee %--%
                                      date_fin_inscr) / ddays(1)),
                       breaks = c(-10000, -0.1, 0, 10000)),
       ind_inscr = fct_collapse(ind_inscr,
                                "Non" = c("(-1e+04,-0.1]", "(0,1e+04]"),
                                "Oui" = "(-0.1,0]"))

# duree entre invitation et inscription --> pendant ou après la période d'invitation ?
invit_data <- invit_data %>%
  mutate(ind_inscr_invit = case_when(ind_inscr == "Oui" & date_deb_inscr == date_deb_invit ~ "Inscription durant la période d'invitation",
                                     ind_inscr == "Oui" & date_deb_inscr != date_deb_invit ~ "Inscription après la période d'invitation",
                                     ind_inscr == "Non" ~ "Pas d'inscription"))

# ajout de l information WED par rapport a la date d invitation 
# indicatrice si l invitation est reliee (+/- 1 mois) a un WED 
wed_data <- readRDS(file = "Data/Cleaned/wed_data.rds") %>%
  mutate(annee = "2018")

# fusion avec la base --> faire un filtre 2018 
invit_data_wed <- invit_data %>%
  left_join(wed_data, by = c("structure_gr", "annee"))

# calcul indicatrice
invit_data_wed <- invit_data_wed %>%
  mutate(ind_invit_wed = cut(abs(as.duration(date_deb_wed %--%
                                               date_deb_invit) / ddays(1)),
                             breaks = c(0, 30, 10000)),
         ind_invit_wed = fct_recode(ind_invit_wed,
                                    "Invitation reliée au WED" = "(0,30]",
                                    "Invitation non reliée au WED" = "(30,1e+04]"),
         ind_invit_wed = fct_explicit_na(ind_invit_wed, na_level = "Pas de WED dans le groupe"),
         ind_invit_wed2 = fct_collapse(ind_invit_wed,
                                       "Week-end découverte" = "Invitation reliée au WED",
                                       "Autre" = c("Invitation non reliée au WED", 
                                                   "Pas de WED dans le groupe")))

# calcul semaine
# on construit la semaine de façon comparable pour toutes les annees
# puis on cale sur le mois de septembre en debut
invit_data_wed <- invit_data_wed %>%
  mutate(week_deb_invit = dmy(format(date_deb_invit, "%d-%m-2018")),
       week_deb_invit = case_when(
         week_deb_invit < dmy("31-08-2018") ~ dmy(format(date_deb_invit, "%d-%m-2018")),
         week_deb_invit > dmy("30-08-2018") ~ dmy(format(date_deb_invit, "%d-%m-2017"))),
       week_deb_invit = floor_date(week_deb_invit, unit = "weeks"),
       mois_deb_invit = month(week_deb_invit),
       trim_deb_invit = case_when(
         mois_deb_invit %in% c(9, 10, 11 , 12) ~ "Septembre - décembre",
         mois_deb_invit %in% c(1, 2, 3) ~ "Janvier - mars",
         mois_deb_invit %in% c(4, 5, 6) ~ "Avril - juin"))

# recodage de l'ordre des niveaux des facteurs
invit_data_wed <- invit_data_wed %>%
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
                                      "Responsable"),
         fonction_r = fct_relevel(fonction_r,
                                  "Farfadet",
                                  "Louveteau/Jeannette",
                                  "Scout/Guide",
                                  "Pionnier/Caravelle",
                                  "Compagnon",
                                  "Animateur farfadet",
                                  "Chef LJ",
                                  "Chef SG",
                                  "Chef PK",
                                  "Accompagnateur compagnon",
                                  "Responsable",
                                  "Membre associé"),
         ind_invit_wed = fct_relevel(ind_invit_wed,
                                     "Invitation reliée au WED",
                                     "Invitation non reliée au WED",
                                     "Pas de WED dans le groupe"),
         ind_invit_wed2 = fct_relevel(ind_invit_wed2,
                                      "Week-end découverte",
                                      "Autre"),
         ind_inscr_invit = fct_relevel(ind_inscr_invit,
                                 "Inscription durant la période d'invitation",
                                 "Inscription après la période d'invitation",
                                 "Pas d'inscription"),
         ind_inscr = fct_relevel(ind_inscr,
                                 "Oui",
                                 "Non"),
         trim_deb_invit = fct_relevel(trim_deb_invit,
                                      "Septembre - décembre",
                                      "Janvier - mars",
                                      "Avril - juin")) %>% 
  mutate_at(vars("annee",
            "fonction",
            "fonction_last",
            "fonction_last_nom",
            "fonction_nom",
            "inscr_open",
            "tr_age"), 
            as.factor) # changer le type

# sauvegarde
saveRDS(invit_data_wed, "Data/Augmented/invit_data_wed.rds")
