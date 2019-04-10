#-----------------#
#### Etude invites - analyse ####
#-----------------#
# 27-07-2018 #
#-----------------#

library(tidyverse)
library(lubridate)
library(scales)
library(paletteer)

invit_data_wed <- readRDS("Data/Augmented/invit_data_wed.rds")
invit_data_unites <-  readRDS("Data/Augmented/invit_data_unites.rds")
effectifs_tot <- readRDS("Data/Augmented/effectifs_tot.rds")

# part des invites par branche et annee
invit_data_part <- invit_data_wed %>% 
  group_by(annee, branche, fonction_group) %>%
  count() %>%
  right_join(effectifs_tot, by = c("annee", "branche", "fonction_group")) %>%
  rename(n_invit = n.x,
         n_tot = n.y)

invit_data_part %>%
  filter(annee == "2018") %>%
  mutate(part_invit = (n_invit/n_tot)*100) %>%
  ggplot(aes(x = branche, y = part_invit, fill = fonction_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#0A2F84", "#00A89D", "#6971A4"),
                    name = "Fonction") +
  xlab("Branche") +
  scale_y_continuous(breaks = seq(0,35,5),
                     name = "Part des invités (en %)") +
  ggtitle("Part des invités par branche en 2018")

## Taux d'inscription ##
# taux d'inscription par date
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  ggplot(aes(x = week_deb_invit, fill = ind_inscr)) +
  geom_bar(stat = "count") +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_fill_manual(values = c("#0A2F84", "#00A89D"),
                    name = "Inscription par la suite") +
  scale_y_continuous(breaks = seq(0,2000,100),
                     name = "Nombre d'invités") +
  ggtitle("Taux d'inscription par date en 2018")

# part relative
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(week_deb_invit, ind_inscr) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = week_deb_invit, y=perc*100, fill = ind_inscr)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_fill_manual(values = c("#0A2F84", "#00A89D"),
                    name = "Inscription par la suite") +
  scale_y_continuous(breaks = seq(0,100,10),
                     name = "Part des invités (en %)") +
  ggtitle("Part relative des inscriptions par date en 2018")

# taux d'inscription par branche
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(branche, ind_inscr) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = branche, y=perc*100, fill = ind_inscr)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#0A2F84", "#00A89D"),
                    name = "Inscription par la suite") +
  xlab("Branche") +
  scale_y_continuous(breaks = seq(0,100,10),
                     name = "Taux d'inscription (en %)") +
  ggtitle("Taux d'inscription par branche en 2018")

## Repartition des invitations dans l'annee ##
# repartition dans l'annee par branche
invit_data_wed %>%
  filter(fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  ggplot(aes(x = week_deb_invit, fill = branche)) +
  geom_bar(stat = "count") +
  facet_grid(annee ~ .) + 
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_fill_manual(values = c("#72BA80", "#EE7F00", "#0069B2", "#C22E16", "#006D3E"),
                    name = "") +
  scale_y_continuous(breaks = seq(0,2000,200),
                     name = "Nombre d'invités") +
  ggtitle("Nombre d'invités par branche et par année selon la date")

# repartition par annee
invit_data_wed %>%
  filter(fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(week_deb_invit, annee) %>%
  count() %>%
  mutate(taille = case_when(annee == "2018" ~ "1",
                            TRUE ~ "2")) %>%
  ggplot(aes(x = week_deb_invit, y=n, color = annee, linetype = taille)) +
  geom_smooth(size = 0.8, 
              method = lm, 
              formula = y ~ splines::bs(x, 20),
              se = FALSE) +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_linetype(guide = FALSE) +
  scale_color_paletteer_d(nord, victory_bonds,
                          name = "Année") +
  scale_y_continuous(breaks = seq(0,2000,100),
                     name = "Nombre d'invités") +
  ggtitle("Nombre d'invités selon la date, par année")

# descriptif selon les branches et les fonctions

## Analyse WED ##
# invitations reliees aux WED
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  ggplot(aes(x = week_deb_invit, fill = ind_invit_wed)) +
  geom_bar(stat = "count") +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month") +
  scale_fill_manual(values = c("#0A2F84", "#00A89D", "#72BA80"),
                    name = "") +
  xlab("Date de l'invitation") +
  ylab("Nombre d'invités") +
  ggtitle("Invitations reliées aux week-end découvertes en 2018")

# effet des WED --> faire une regression ? --> il faut neutraliser les effets de date !
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(ind_inscr, ind_invit_wed) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)*100)

# part relative des inscriptions en fonctions de la date et du WED
invit_data_wed %>%
  mutate(ind_inscr_wed = paste(ind_invit_wed2, ind_inscr)) %>%
  filter(annee == "2018" & 
           fonction_group == "Jeune" & 
           (week_deb_invit < dmy("07-07-2018"))) %>%
  group_by(week_deb_invit, ind_inscr_wed) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = week_deb_invit, y=perc*100, fill = ind_inscr_wed)) +
  geom_bar(stat = "identity") +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_fill_paletteer_d(nord, baie_mouton,
                         name = "Inscription par la suite") +
  scale_y_continuous(breaks = seq(0,100,10),
                     name = "Part des invités (en %)") +
  ggtitle("Part relative des inscriptions par date en 2018")

invit_data_wed %>%
  filter(annee == "2018" & 
           fonction_group == "Jeune" & 
           (week_deb_invit < dmy("07-07-2018"))) %>%
  group_by(week_deb_invit, ind_invit_wed2, ind_inscr) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  filter(ind_inscr == "Oui") %>%
  ggplot(aes(x = week_deb_invit, y=perc*100, color = ind_invit_wed2)) +
  geom_line(size = 0.8) +
  scale_x_date(labels = date_format("%b"),
               date_breaks = "1 month",
               name = "Date de l'invitation") +
  scale_color_paletteer_d(nord, lake_superior,
                          name = "")+
  scale_y_continuous(breaks = seq(0,100,10),
                     name = "Part des invités (en %)") +
  ggtitle("Part relative des inscriptions par date en 2018, en fonction du WED")

# calculer la part des invités au WED qui s'inscrivent VS qui ne s'inscrivent pas 
  # --> seuils à ajuster + probleme de definition de l'inscription ? 
invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(ind_invit_wed, ind_inscr) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)*100)

invit_data_wed %>%
  filter(annee == "2018" & fonction_group == "Jeune" & (week_deb_invit < dmy("07-07-2018"))) %>% # filtre pour enlever l'ete
  group_by(ind_inscr) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)*100)

