#-----------------#
#### Etude invites - import ####
#-----------------#
# 03-07-2018 #
#-----------------#

library(tidyverse)
library(readxl)
library(lubridate)

# donnees invites ---------------------------------------------------------
# on charge le chemin puis l'ensemble des feuilles Excel qu'on assemble avec une variable annee
path <- "Data/Origine/Invités par saison_2018 07 24.xlsx"

invit_data <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = path,
                      sheet = .x,
                      range = cell_cols("A:Q"),
                      col_types = c("text", "skip", "skip", "text", 
                                    "skip", "text", "text", "skip",
                                    "date", "date", "text", "skip",
                                    "text", "text", "text", "date",
                                    "date")), 
      .id = "annee")

# nom des variables
colnames(invit_data) <- c("annee", "code_adherent", "structure_un", 
           "fonction", "fonction_nom", "date_deb_invit", 
           "date_fin_invit", "stucture_last",
           "fonction_last", "fonction_last_nom", "statut_last",
           "date_deb_inscr", "date_fin_inscr")

# variables en date
invit_data <- invit_data %>%
  mutate(date_deb_invit = date(date_deb_invit),
         date_fin_invit = date(date_fin_invit),
         date_deb_inscr = date(date_deb_inscr),
         date_fin_inscr = date(date_fin_inscr))

# sauvegarde
saveRDS(invit_data, file = "Data/Cleaned/invit_data.rds")

# donnees we decouverte ---------------------------------------------------------
# chargement des donnees
wed_data <- read_excel(path = "Data/Origine/Groupes_WED.xlsx",
           range = cell_cols("A:Q"),
           col_types = c("skip", "text", "text", "skip",
                         "text", "skip", "skip", "skip",
                         "skip", "skip", "text", "text",
                         "skip", "skip", "skip", "text",
                         "text"),
           col_names = c("date_creation", "date_modification", "structure_gr", 
                         "date_deb_wed", "date_fin_wed", "tr_age", "inscr_open")) %>%
  slice(-1) %>%
  mutate(date_creation = dmy(date_creation),
         date_modification = dmy(date_modification),
         date_deb_wed = dmy(date_deb_wed),
         date_fin_wed = dmy(date_fin_wed)) 

# sauvegarde
saveRDS(wed_data, file = "Data/Cleaned/wed_data.rds")

# donnees groupes --------------------------------------------------------- 
# chargement des donnees
unites_data <- read_excel(path = "Data/Origine/Unités_20180702.xlsx",
                       col_types = c("text", "skip", "text", "skip",
                                     "skip", "text", "skip", "text",
                                     "skip", "numeric"), 
                       col_names = c("structure_ter", "structure_gr", "structure_un", 
                                     "type_un", "nb_jeunes")) %>%
  slice(-1) 

# sauvegarde
saveRDS(unites_data, file = "Data/Cleaned/unites_data.rds")

