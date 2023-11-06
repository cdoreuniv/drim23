# Chargement des librairies
library(readr)
library(readxl) # Fichiers au format .xlsx
library(haven) # Fichiers au format .sas7bdat

library(sf)
library(dplyr)
library(tmap)
library(leaflet)
library(ggplot2)
library(spdep)
library(zoo)

library(tidyverse)  

##################################################################################################
# Chargement de la table sur les créations d'entreprise
##################################################################################################

data_cre = read_sas("donnees/table_creation_2017_2023.sas7bdat")
data_cre$date = as.yearqtr(data_cre$trimestre, format = "%q%Y")

#########################################
## Transformer au format court
#########################################
data_crecourt = data_cre %>%
  pivot_wider(
             names_from = Secteur,
             values_from = nombre_creation
             ) %>%
  select(-trimestre) %>%
  arrange(code_region, date)

#########################################
# Somme du nombre de création d'entreprises par région (plusieurs départements
# dans une région et nombre de créations données par département)
#########################################

data_crecourt2 = data_crecourt %>%
  group_by(code_region, date) %>%
  summarize(creaActServiceAdminSoutien = sum(`Activites de services administratifs et de soutien`),
            creaActMenageEmploy = sum(`Activites des menages en tant qu'employeurs ; activi`),
            creaActExtraTerritoriales = sum(`Activites extra-territoriales`),
            creaActFinancierAssurance = sum(`Activites financieres et d'assurance`),
            creaActImmobilieres = sum(`Activites immobilieres`),
            creaActSpecialiseesScientifiquesTechniques = sum(`Activites specialisees, scientifiques et techniques`),
            creaAdministrationPublique = sum(`Administration publique`),
            creaArtsSpectaclesActRecreatives = sum(`Arts, spectacles et activites recreatives`),
            creaAutresActServices = sum(`Autres activites de services`),
            creaCommerceRepaAutoMoto = sum(`Commerce ; reparation d'automobiles et de motocycles`),
            creaConstruction = sum(Construction),
            creaEnseignement = sum(Enseignement),
            creaHebergementRestauration = sum(`Hebergement et restauration`),
            creaIndustrieManufacturiere = sum(`Industrie manufacturiere`),
            creaIndustrieExtractives = sum(`Industries extractives`),
            creaInformationCommunication = sum(`Information et communication`),
            creaProdDistEauAssainissement = sum(`Production et distribution d'eau ; assainissement, g`),
            creaProdDistElectriciteGaz = sum(`Production et distribution d'electricite, de gaz, de`),
            creaSanteHumaineActionSociale = sum(`Sante humaine et action sociale`),
            creaTransportsEntreposage = sum(`Transports et entreposage`))

# colnames(data_crecourt)
#########################################
## Sauvegarde de la base de données travaillée
#########################################
write_csv2(data_crecourt2, "donnees/data_creation.csv")


##################################################################################################
# Chargement de la table sur les défaillances d'entreprise
##################################################################################################
data_def = read_sas("donnees/table_defaut_2017_2023.sas7bdat")
data_def$date = as.yearqtr(data_def$trimestre, format = "%q%Y")

#########################################
# Transformer au format court
#########################################
data_defcourt = data_def %>%
  pivot_wider(
    names_from = Secteur,
    values_from = nombre_defaut ) %>%
  select(-trimestre) %>%
  arrange(code_region, date)

#########################################
# Somme du nombre de défaut d'entreprises par région (plusieurs départements
# dans une région et nombre de défaillances données par département)
#########################################

data_defcourt2 = data_defcourt %>%
  group_by(code_region, date) %>%
  summarize(defActServiceAdminSoutien = sum(`Activites de services administratifs et de soutien`),
            defActFinancieres = sum(`Activites financieres`),
            defActFinancieresAssurance = sum(`Activites financieres et d'assurance`),
            defActImmobilieres = sum(`Activites immobilieres`),
            defActSpecialiseesScientifiquesTechniques = sum(`Activites specialisees, scientifiques et techniques`),
            defAdminPublique = sum(`Administration publique`),
            defAgricultureChasseSylviculture = sum(`Agriculture, chasse, sylviculture`),
            defAgricultureSylviculturePeche = sum(`Agriculture, sylviculture et p&#234;che`),
            defArtsSpectacleActRecreatives = sum(`Arts, spectacles et activites recreatives`),
            defAutresActServices = sum(`Autres activites de services`),
            defCommerce = sum(Commerce),
            defCommerceRepAutoMoto = sum(`Commerce ; reparation d'automobiles et de motocycles`),
            defCommercePrepAutoArtDomestiques = sum(`Commerce ; reparations automobile et d'articles domestiques`),
            defConstruction = sum(`Construction`),
            defEnseignement = sum(`Enseignement`),
            defHotelsRestaurants = sum(`H&#244;tels et restaurants`),
            defHebergementRestauration = sum(`Hebergement et restauration`),
            defImmobilierLocationServices = sum(`Immobilier, location et services aux entreprises`),
            defIndustrieManufacturiere = sum(`Industrie manufacturiere`),
            defIndustrieAgricoleAlimentaire = sum(`Industries agricoles et alimentaires`),
            defIndustrieBatimentGenieCivilAgricole = sum(`Industries de mise en oeuvre du b&#226;timent et du genie civil et agricole`),
            defIndustrieBiensConsommationCourante = sum(`Industries des biens de consommation courante`),
            defIndustrieBiensIntermediaires = sum(`Industries des biens intermediaires`),
            defIndustrieExtractives = sum(`Industries extractives`),
            defInformationCommunication = sum(`Information et communication`),
            defLocationBiensImmobiliers = sum(`Location de biens immobiliers`),
            defProdDistEauAssainissement = sum(`Production et distribution d'eau ; assainissement, gestion des dechets et depollution`),
            defProdDistElectriciteGaz = sum(`Production et distribution d'electricite, de gaz, de vapeur et d'air conditionne`),
            defSanteActionSociale = sum(`Sante et action sociale`),
            defSanteHumaineActionSociale = sum(`Sante humaine et action sociale`),
            defServicesCollectifsSociauxPersonnels = sum(`Services collectifs, sociaux et personnels`),
            defServicesMarchands = sum(`Services marchands`),
            defTransportCommunication = sum(`Transports et communications`),
            defTransportEntreposage = sum(`Transports et entreposage`),
            defTrasnportTelecommunication = sum(`Transports et telecommunications`))

#########################################
## Sauvegarde de la base de données travaillée
#########################################
write_csv2(data_defcourt2, "donnees/data_defaut.csv")

##################################################################################################
# Chargement des données macroéconomiques
##################################################################################################

#####################################################
## Chômage
#####################################################
data_chom = read_excel(path = "donnees/Series macroeconomiques.xlsx",
                       sheet = "Taux de chômage 2012-2021",
                       skip = 3)
data_chom = data_chom[1:19,]
colnames(data_chom)[1] = "code_region"

#########################################
# Transformer au format long
#########################################
data_chomlong = data_chom %>%
  pivot_longer(cols = -c(Libellé, code_region),
               names_to = "date",
               values_to = "TauxChomage")

#########################################
# Changement du format date
#########################################
trim = substr(data_chomlong$date,2,2)
annee = substr(data_chomlong$date, 4,8)
date = paste(trim,annee, by = "")
data_chomlong$date = as.yearqtr(date, format = "%q%Y")

code_libelle = data_chom[,1:2]
data_chomlong = data_chomlong[,-2]

write_csv2(code_libelle, "donnees/code_libelle_regions.csv")
write_csv2(data_chomlong, "donnees/data_chomage.csv")


#####################################################
## PIB en valeur
#####################################################
data_PIBval = read_excel(path = "donnees/Series macroeconomiques.xlsx",
                         sheet = "PIB en valeur 2012-2021",
                         skip = 3)
data_PIBval = data_PIBval[-c(1,24:26),]
colnames(data_PIBval)[1] = "Libellé"

remplacer_caractere = function(chaine, car1, car2) {
  chaine_modifiee = gsub(car1,car2, chaine)
  return(chaine_modifiee)
}

data_PIBval$Libellé = toupper(data_PIBval$Libellé)
data_PIBval$Libellé = remplacer_caractere(data_PIBval$Libellé,"Ô", "O")
data_PIBval$Libellé = remplacer_caractere(data_PIBval$Libellé,"É", "E")
data_PIBval$Libellé = remplacer_caractere(data_PIBval$Libellé,"Î", "I")

data_PIBval1 = inner_join(code_libelle, data_PIBval, by = "Libellé")

#########################################
# Transformer au format long
#########################################
data_PIBvallong = data_PIBval1 %>%
  pivot_longer(cols = -c(Libellé, code_region),
               names_to = "date",
               values_to = "PIBValeur")


## Trimestrialiser les données annuelles pour faire le merge
data_PIBvallong = data_PIBvallong[rep(1:nrow(data_PIBvallong), each = 4), ]
data_PIBvallong$date = paste0( c("01", "02", "03", "04"), data_PIBvallong$date)
data_PIBvallong$date = as.yearqtr(data_PIBvallong$date, format = ("%q%Y")) 


data_PIBvallong = data_PIBvallong[,-2]
write_csv2(data_PIBvallong, "donnees/data_PIBvaleur.csv")



#####################################################
## PIB en volume
#####################################################
data_PIBvol = read_excel(path = "donnees/Series macroeconomiques.xlsx",
                         sheet = "PIB en volume 2012-2021",
                         skip = 3)
data_PIBvol = data_PIBvol[-c(1,24:26),]
colnames(data_PIBvol)[1] = "Libellé"

data_PIBvol$Libellé = toupper(data_PIBvol$Libellé)
data_PIBvol$Libellé = remplacer_caractere(data_PIBvol$Libellé,"Ô", "O")
data_PIBvol$Libellé = remplacer_caractere(data_PIBvol$Libellé,"É", "E")
data_PIBvol$Libellé = remplacer_caractere(data_PIBvol$Libellé,"Î", "I")

data_PIBvol1 = inner_join(code_libelle, data_PIBvol, by = "Libellé")

#########################################
# Transformer au format long
#########################################
data_PIBvollong = data_PIBvol1 %>%
  pivot_longer(cols = -c(Libellé, code_region),
               names_to = "date",
               values_to = "PIBVolume")
data_PIBvollong = data_PIBvollong[rep(1:nrow(data_PIBvollong), each = 4), ]
data_PIBvollong$date = paste0( c("01", "02", "03", "04"), data_PIBvollong$date)
data_PIBvollong$date = as.yearqtr(data_PIBvollong$date, format = ("%q%Y")) 

data_PIBvollong = data_PIBvollong[,-2]
write_csv2(data_PIBvollong, "donnees/data_PIBvolume.csv")


#####################################################
## PIB par habitant
#####################################################
data_PIBhab = read_excel(path = "donnees/Series macroeconomiques.xlsx",
                         sheet = "PIB par hab 2012-2021",
                         skip = 3)
data_PIBhab = data_PIBhab[-c(1,24:26),]
colnames(data_PIBhab)[1] = "Libellé"



data_PIBhab$Libellé = toupper(data_PIBhab$Libellé)
data_PIBhab$Libellé = remplacer_caractere(data_PIBhab$Libellé, "Ô", "O")
data_PIBhab$Libellé = remplacer_caractere(data_PIBhab$Libellé, "É", "E")
data_PIBhab$Libellé = remplacer_caractere(data_PIBhab$Libellé, "Î", "I")

data_PIBhab1 = inner_join(code_libelle, data_PIBhab, by = "Libellé")


#########################################
# Transformer au format long
#########################################
data_PIBhablong = data_PIBhab1 %>%
  pivot_longer(cols = -c(Libellé, code_region),
               names_to = "date",
               values_to = "PIBHabitant")


data_PIBhablong = data_PIBhablong[rep(1:nrow(data_PIBhablong), each = 4), ]
data_PIBhablong$date = paste0( c("01", "02", "03", "04"), data_PIBhablong$date)
data_PIBhablong$date = as.yearqtr(data_PIBhablong$date, format = ("%q%Y")) 
data_PIBhablong = data_PIBhablong[,-2]

write_csv2(data_PIBhablong, "donnees/data_PIBhabitant.csv")


#####################################################
## PIB par emploi
#####################################################
data_PIBempl = read_excel(path = "donnees/Series macroeconomiques.xlsx",
                         sheet = "PIB par emploi 2012-2021",
                         skip = 3)
data_PIBempl = data_PIBempl[-c(1,24:26),]

colnames(data_PIBempl)[1] = "Libellé"

data_PIBempl$Libellé = toupper(data_PIBempl$Libellé)
data_PIBempl$Libellé = remplacer_caractere(data_PIBempl$Libellé,"Ô", "O")
data_PIBempl$Libellé = remplacer_caractere(data_PIBempl$Libellé,"É", "E")
data_PIBempl$Libellé = remplacer_caractere(data_PIBempl$Libellé,"Î", "I")

data_PIBempl1 = inner_join(code_libelle, data_PIBempl, by = "Libellé")

#########################################
# Transformer au format long
#########################################
data_PIBemplong = data_PIBempl1 %>%
  pivot_longer(cols = -c(Libellé, code_region),
               names_to = "date",
               values_to = "PIBHabitant")

data_PIBemplong = data_PIBemplong[rep(1:nrow(data_PIBemplong), each = 4), ]
data_PIBemplong$date = paste0( c("01", "02", "03", "04"), data_PIBemplong$date)
data_PIBemplong$date = as.yearqtr(data_PIBemplong$date, format = ("%q%Y")) 

data_PIBemplong = data_PIBemplong[,-2]
write_csv2(data_PIBemplong, "donnees/data_PIBemploi.csv")


##################################################################################################
# Chargement des données sur les primes exceptionnelles
##################################################################################################
data_primex = read_csv2("donnees/primes-exceptionnelles-du-secteur-prive-par-trimestre-par-region.csv")
date = paste(0,data_primex$Trimestre, data_primex$`﻿Année`, sep = "")
data_primex$date = as.yearqtr(date, format = "%q%Y")

colnames(data_primex)[1] = "Libellé"

data_primex$Libellé = toupper(data_primex$Libellé)
data_primex$Libellé = remplacer_caractere(data_primex$Libellé,"Ô", "O")
data_primex$Libellé = remplacer_caractere(data_primex$Libellé,"É", "E")
data_primex$Libellé = remplacer_caractere(data_primex$Libellé,"Î", "I")

data_primex = inner_join(code_libelle, data_primex, by = "Libellé")
data_primex = data_primex[,-c(2:8)]

##################################################################################################
# Chargement des données sur les privé partage de valeur
##################################################################################################
data_primpart = read_csv2("donnees/prime-partage-valeur-du-secteur-prive-par-trimestre-par-region.csv")
date = paste(0,data_primpart$Trimestre, data_primpart$`﻿Année`, sep = "")
data_primpart$date = as.yearqtr(date, format = "%q%Y")

colnames(data_primpart)[1] = "Libellé"

data_primpart$Libellé = toupper(data_primpart$Libellé)
data_primpart$Libellé = remplacer_caractere(data_primpart$Libellé,"Ô", "O")
data_primpart$Libellé = remplacer_caractere(data_primpart$Libellé,"É", "E")
data_primpart$Libellé = remplacer_caractere(data_primpart$Libellé,"Î", "I")

data_primpart = inner_join(code_libelle, data_primpart, by = "Libellé")
data_primpart = data_primpart[,-c(2:8)]

##################################################################################################
# Chargement des données sur les effectifs
##################################################################################################
data_effe = read_csv2("donnees/effectifs-salaries-et-masse-salariale-du-secteur-prive-par-region-x-na38.csv")
date = paste(0,data_effe$Trimestre, data_effe$Année, sep = "")
data_effe$date = as.yearqtr(date, format = "%q%Y")

colnames(data_effe)[1] = "Libellé"

## Format long
data_effe = data_effe %>%
  pivot_wider(
    names_from  = `Secteur NA28i`,
    values_from = c(`Effectifs salariés (brut)`, `Effectifs salariés (CVS)`, `Masse salariale (brut)`, `Masse salariale (CVS)`))

data_effe$Libellé = toupper(data_effe$Libellé)
data_effe$Libellé = remplacer_caractere(data_effe$Libellé,"Ô", "O")
data_effe$Libellé = remplacer_caractere(data_effe$Libellé,"É", "E")
data_effe$Libellé = remplacer_caractere(data_effe$Libellé,"Î", "I")

data_effe = inner_join(code_libelle, data_effe, by = "Libellé")
data_effe = data_effe[,-c(2:9)]

##################################################################################################
# Chargement des données sur les catastrophes naturelles
##################################################################################################
data_cata = read_excel(path = "donnees/catastrophes_naturelles .xlsx",
                        sheet = "données")

colnames(data_cata)[7] = "code_region"

## Regroupement des données en fonction de la région, l'année et du type de catastrophe
data_cata1 = data_cata %>%
  group_by(code_region, annees, typ_catnat ) %>%
  summarize(nb_catnat = sum(nb_catnat),
            ppr_prescrit = sum(ppr_prescrit),
            ppr_approuve = sum(ppr_approuve),
            ppr_anticip = sum(ppr_anticip))

## Format long
data_catalong = data_cata1 %>%
  pivot_wider(
    names_from  = typ_catnat,
    names_prefix = "Type",
    values_from = c(nb_catnat, ppr_prescrit, ppr_approuve, ppr_anticip))

data_catalong = data_catalong[rep(1:nrow(data_catalong), each = 4), ]
data_catalong$date = paste0( c("01", "02", "03", "04"), data_catalong$annees)
data_catalong$date = as.yearqtr(data_catalong$date, format = ("%q%Y")) 





##################################################################################################
# Regroupement des base de données
##################################################################################################
## Données entreprises
data_entr = merge(data_crecourt2, data_defcourt2, by = c("code_region", "date"))
write_csv2(data_entr, "donnees/data_entreprises.csv")
## Données macro
data_macro = merge(x = data_chomlong, 
                   y = c(data_PIBvallong, data_PIBvollong, data_PIBhablong, data_PIBemplong), 
                   by.x = c("code_region","date"),
                   by.y = c("code_region","date"),
                   sort = TRUE)
write_csv2(data_macro, "donnees/data_macro.csv")
## Données primes
data_prim = merge(x = data_primex, 
                  y = data_primpart, 
                  by.x = c("code_region","date"),
                  by.y = c("code_region","date"),
                  sort = TRUE)

write_csv2(data_prim, "donnees/data_prim.csv")

## Données finales
datafin = left_join(x = data_entr, y = data_macro,
                by = c("code_region","date"))

# datafin = left_join(x = datafin, y = data_prim,
#                 by = c("code_region","date")
#                 )
datafin = left_join(x = datafin, y = data_effe,
                    by = c("code_region","date"))

datafin = left_join(x = datafin, y = data_catalong,
                    by = c("code_region","date"))






write_csv2(datafin, "donnees/final_database.csv")
