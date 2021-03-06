#-----------------#
#### Etude invites - methodologie - dictionnaire des codes ####
#-----------------#
# 11-07-2018 #
#-----------------#

### A AFFINER --> package a creer !

library(dataMeta)

invit_data_wed <- readRDS("Data/Augmented/invit_data_wed.rds")

# nom des variables
var_desc_type <- tibble(desc = c("Ann�e", "Code de l'adh�rent", "Code de l'unit�", "Fonction de l'adh�rent", "Nom de la fonction",
  "Date de d�but de l'invitation", "Date de fin de l'invitation", "Code de la derni�re structure", 
  "Derni�re fonction de l'adh�rent", "Nom de la derni�re fonction", "Dernier statut", 
  "Date de d�but de l'inscription", "Date de fin de l'inscription", "Code du territoire",
  "Code du groupe", "Fonction de l'adh�rent agr�g�e niveau 2", "Fonction de l'adh�rent agr�g�e niveau 1",
  "Branche", "Date de fin de l'ann�e", "Indicatrice de l'inscription", "Indicatrice de l'inscription pendant la p�riode d'invitation",
  "Date de cr�ation du WED", "Date de modification du WED", "Date de d�but du WED", "Date de fin du WED",
  "Tranches d'�ge vis�es par le WED", "Inscriptions ouvertes au WED", "Indicatrice de l'invitation suite � un WED",
  "Date de d�but de l'inscription (par semaine) cal�e sur un an"),
  var_type = c(1, 0, 0, 0, 0, 0 ,0 , 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0))

# type et noms des variables
var_description <- invit_data_wed %>% 
  summarise_all(class) %>% 
  gather(col_name, col_type) %>%
  bind_cols(var_desc_type)

# par levels
levels(invit_data_wed$branche)

# extraire les niveaux si demand�
data_frame(variable = c("annee", "branche"), 
           values = map(variable, ~invit_data_wed[[.x]] %>% 
                          unique() %>% 
                          sort())) %>% unlist()
