#-----------------#
#### Etude invites - methodologie - dictionnaire des codes ####
#-----------------#
# 11-07-2018 #
#-----------------#

### A AFFINER --> package a creer !

library(dataMeta)

invit_data_wed <- readRDS("Data/Augmented/invit_data_wed.rds")

# nom des variables
var_desc_type <- tibble(desc = c("Année", "Code de l'adhérent", "Code de l'unité", "Fonction de l'adhérent", "Nom de la fonction",
  "Date de début de l'invitation", "Date de fin de l'invitation", "Code de la dernière structure", 
  "Dernière fonction de l'adhérent", "Nom de la dernière fonction", "Dernier statut", 
  "Date de début de l'inscription", "Date de fin de l'inscription", "Code du territoire",
  "Code du groupe", "Fonction de l'adhérent agrégée niveau 2", "Fonction de l'adhérent agrégée niveau 1",
  "Branche", "Date de fin de l'année", "Indicatrice de l'inscription", "Indicatrice de l'inscription pendant la période d'invitation",
  "Date de création du WED", "Date de modification du WED", "Date de début du WED", "Date de fin du WED",
  "Tranches d'âge visées par le WED", "Inscriptions ouvertes au WED", "Indicatrice de l'invitation suite à un WED",
  "Date de début de l'inscription (par semaine) calée sur un an"),
  var_type = c(1, 0, 0, 0, 0, 0 ,0 , 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0))

# type et noms des variables
var_description <- invit_data_wed %>% 
  summarise_all(class) %>% 
  gather(col_name, col_type) %>%
  bind_cols(var_desc_type)

# par levels
levels(invit_data_wed$branche)

# extraire les niveaux si demandé
data_frame(variable = c("annee", "branche"), 
           values = map(variable, ~invit_data_wed[[.x]] %>% 
                          unique() %>% 
                          sort())) %>% unlist()
