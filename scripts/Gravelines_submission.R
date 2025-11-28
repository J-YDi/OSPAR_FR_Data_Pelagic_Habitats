#_______________________________________________________________________________
# Nom               : Gravelines_submission.r
# Date de modif     : 28/11/2025
# Objet             : Mise en forme des donnees IGA Gravelines
# Auteurs           : J-Y. Dias
# Version R         : 4.5.0
#_______________________________________________________________________________

#_______________________________ Packages_______________________________________

# Function to install packages if not present and/or load them
# in state does not work for packages that install outside CRAN
loadpackages <- function(packages){
  for (pkg in packages){
    if (!requireNamespace(pkg,quietly = T)){
      install.packages(pkg)
      library(pkg,character.only = T)
    }else{
      library(pkg,character.only = T)
    }
  }
}
packages_needed <- c("readr","dplyr","ggplot2","cowplot","tidyr","lubridate","readxl")

loadpackages(packages_needed)

#__________________________Loading Original Data________________________________

IGA <-read_delim("data/SEANOE_Gravelines_Hydro_Plancton_2024.csv",delim = ";",col_types = cols(`Passage : Date` = col_date(format = "%d/%m/%Y")),
                 escape_double = FALSE,  trim_ws = TRUE)

# Change columns names
{
  colnames(IGA)[which(names(IGA) == "Passage : Année")] <- "ANNEE"
  colnames(IGA)[which(names(IGA) == "Lieu : Libellé")] <- "SITE"
  colnames(IGA)[which(names(IGA) == "Passage : Date")] <- "DATE"
  colnames(IGA)[which(names(IGA) == "Passage : Heure")] <- "HEURE"
  colnames(IGA)[which(names(IGA) == "Passage : Format UT de l'heure")] <- "FORMAT_HEURE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Niveau de prélèvement : Libellé")] <- "NIVEAU_PROFONDEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Paramètre : Code")] <- "PARAM"
  colnames(IGA)[which(names(IGA) == "Résultat : Taxon référent : Libellé")] <- "TAXON"
  colnames(IGA)[which(names(IGA) == "Résultat : Taxon saisi : Libellé")] <- "TAXON_USER"
  colnames(IGA)[which(names(IGA) == "Résultat : Numéro d'individu")] <- "N_INDIVIDU"
  colnames(IGA)[which(names(IGA) == "Résultat : Paramètre : Libellé")] <- "PARAM_TITRE"
  colnames(IGA)[which(names(IGA) == "Résultat : Valeur quantitative")] <- "VALEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Valeur qualitative : Libellé")] <- "VALEUR_QUANTI"
  colnames(IGA)[which(names(IGA) == "Résultat : Précision : Libellé")] <- "PRECISION"
  colnames(IGA)[which(names(IGA) == "Résultat : Incertitude : Valeur")] <- "INCERTITUDE"
  colnames(IGA)[which(names(IGA) == "Résultat : Incertitude : Type : Libellé")] <- "INCERTITUDE_UNIT"
  colnames(IGA)[which(names(IGA) == "Résultat : Support : Libellé")] <- "SUPPORT"
  colnames(IGA)[which(names(IGA) == "Résultat : Fraction : Libellé")] <- "FRACTION"
  colnames(IGA)[which(names(IGA) == "Résultat : Méthode : Libellé")] <- "METHODE"
  colnames(IGA)[which(names(IGA) == "Résultat : Unité : Symbole")] <- "UNITE"
  colnames(IGA)[which(names(IGA) == "Résultat : Unité : Libellé")] <- "UNITE_TITRE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Libellé")] <- "ENGIN"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Taille")] <- "ENGIN_TAILLE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Engin de prélèvement : Taille : Unité : Symbole")] <- "TAILLE_UNITE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Service préleveur : Libellé")] <- "LABO_PRELEVEMENT"
  colnames(IGA)[which(names(IGA) == "Résultat : Service analyste : Libellé")] <- "LABO_ANALYSTE"
  colnames(IGA)[which(names(IGA) == "Résultat : Service saisisseur : Libellé")] <- "LABO_SAISISSEUR"
  colnames(IGA)[which(names(IGA) == "Résultat : Niveau de qualité : Libellé")] <- "QUALITE"
  colnames(IGA)[which(names(IGA) == "Résultat : Commentaire de qualification")] <- "QUALITE_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Passage : Latitude (Min)")] <- "LON"
  colnames(IGA)[which(names(IGA) == "Passage : Longitude (Min)")] <- "LAT"
  colnames(IGA)[which(names(IGA) == "Passage : Commentaire")] <- "PASSAGE_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Commentaire")] <- "PRELEVEMENT_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Echantillon : Commentaire")] <- "ECHANTILLON_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Résultat : Commentaires")] <- "RESULTAT_COMMENTAIRE"
  colnames(IGA)[which(names(IGA) == "Sonde")] <- "SONDE"
  colnames(IGA)[which(names(IGA) == "Symbole unité de la sonde")] <- "UNITE_SONDE"
  colnames(IGA)[which(names(IGA) == "Lieu : Mnémonique")] <- "ID_SITE"
  colnames(IGA)[which(names(IGA) == "Passage : Identifiant")] <- "ID_PASSAGE"
  colnames(IGA)[which(names(IGA) == "Prélèvement : Identifiant")] <- "ID_PRELEVEMENT"
  colnames(IGA)[which(names(IGA) == "Echantillon : Identifiant")] <- "ID_ECHANTILLON"
  colnames(IGA)[which(names(IGA) == "Résultat : Identifiant")] <- "ID_RESULTAT"
  colnames(IGA)[which(names(IGA) == "Echantillon : Libellé du support")] <- "SUPPORT_ECHANTILLON"
  colnames(IGA)[which(names(IGA) == "Passage : Mois")] <- "MOIS"
}

# Unuseful columns
IGA <- IGA |>
  select(-c(`Passage : Mnémonique`,`Prélèvement : Mnémonique`,`Echantillon : Mnémonique`,`Résultat : Date de contrôle`:`Résultat : Date de qualification`,
            `Prélèvement : Service préleveur : Code`,
            INCERTITUDE,INCERTITUDE_UNIT,PRECISION,ENGIN_TAILLE,TAILLE_UNITE,QUALITE_COMMENTAIRE,PASSAGE_COMMENTAIRE,
            MOIS,ANNEE,`Passage : Delta UT`,`Résultat : Groupe de taxons : Libellé`,`Passage : Latitude (Max)`,`Passage : Coordonnées : Source`,`Résultat : Commentaire`,
            `Passage : Sonde`,`Passage : Sonde : Unité : Symbole`,`Lieu : Identifiant`,`Echantillon : Support : Libellé`:`Date d'extraction de la donnée`))

# Quality filter
IGA <- IGA |>
  filter(QUALITE != "Douteux") |>
  select(-QUALITE)

# Make datetime correct
IGA$HEURE[is.na(IGA$HEURE)] <- as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "UTC")
IGA$DATETIME <- as.POSIXct(paste(IGA$DATE,IGA$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

# PHYTOPLANKTON
IGA_PHYTO <- IGA |>
  filter(PARAM %in% c("CELL","FLORTOT")) |>
  select(-c(N_INDIVIDU,VALEUR_QUANTI,SUPPORT,PARAM_TITRE,FRACTION,METHODE,UNITE,UNITE_TITRE,ENGIN))

# 0 becomes NA
IGA_PHYTO$VALEUR[IGA_PHYTO$VALEUR == 0] <- NA

# Preparation for comparison with process data
PHYTO_Comp <- IGA_PHYTO |>
  select(DATE,HEURE,NIVEAU_PROFONDEUR,TAXON,VALEUR,ID_SITE,ID_ECHANTILLON,ID_PASSAGE,ID_PRELEVEMENT,ID_RESULTAT)

# Count replicates: I follow Maud Lemoine's recommendations for REPHY: we take the maximum value.
# We preserve duplicates if samples are different.
IGA_PHYTO <- IGA_PHYTO |>
  select(-c(ID_SITE,ID_ECHANTILLON,ID_PASSAGE,ID_RESULTAT,LABO_PRELEVEMENT:LABO_SAISISSEUR,TAXON_USER)) |>
  pivot_wider(names_from = "TAXON",values_from = VALEUR,values_fn = max) |>
  pivot_longer(cols = `Nitzschia + Hantzschia`:Plagiolemma,names_to = "TAXON",values_to = "VALEUR") |>
  filter(!is.na(VALEUR))

IGA_PHYTO <- left_join(IGA_PHYTO,PHYTO_Comp)

taxons_PHYTO_IGA <- IGA_PHYTO |>
  select(TAXON) |>
  unique()

Taxonomy_correspondance_IGA <- read_excel("data/Additional_data/Taxonomy_correspondance_IGA.xlsx")
taxons_PHYTO_IGA <- left_join(taxons_PHYTO_IGA,Taxonomy_correspondance_IGA)

# Associate aphiaID
IGA_PHYTO <- left_join(IGA_PHYTO,select(taxons_PHYTO_IGA,TAXON,aphiaID))

# Delete non-phytoplankton taxa
IGA_PHYTO <- filter(IGA_PHYTO,aphiaID != 0)

# Preparation to compare with data processed
# Remove accents
remove_accents <- function(df) {
  # Vérifie que stringi est dispo
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Le package 'stringi' est requis. Installez-le avec install.packages('stringi').")
  }
  
  df_clean <- as.data.frame(
    lapply(df, function(x) {
      if (is.character(x)) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      } else {
        x
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(df_clean)
}

IGA_PHYTO_clean <- remove_accents(IGA_PHYTO) 

# Change of the Phaeocystis denomination in the raw data to match
IGA_PHYTO_clean$TAXON[IGA_PHYTO_clean$TAXON == "Phaeocystis"] <- "Phaeocystis globosa"

# Loading process data
PHYTO_Pro <-read_delim("data/SEANOE_Gravelines_Process_Phyto.csv",delim = ";",
                       escape_double = FALSE,  trim_ws = TRUE,locale = locale(encoding = "Windows-1252"))
# Change columns names
{
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Annee")] <- "ANNEE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Lieu.de.surveillance...Libelle")] <- "SITE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Date")] <- "DATE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Heure")] <- "HEURE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Format.UT.de.l'heure")] <- "FORMAT_HEURE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Prelevement...Niveau")] <- "NIVEAU_PROFONDEUR"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Code.parametre")] <- "PARAM"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Nom.du.taxon.referent")] <- "TAXON"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Nom.du.taxon.saisi")] <- "TAXON_USER"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Numero.d'individu")] <- "N_INDIVIDU"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.parametre")] <- "PARAM_TITRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Valeur.de.la.mesure")] <- "VALEUR"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Valeur.qualitative")] <- "VALEUR_QUANTI"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.precision")] <- "PRECISION"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Incertitude")] <- "INCERTITUDE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Symbole.de.l'unite.incertitude")] <- "INCERTITUDE_UNIT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.support")] <- "SUPPORT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.fraction")] <- "FRACTION"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.methode..11")] <- "METHODE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Symbole.unite.de.mesure.associe.au.quintuplet")] <- "UNITE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Libelle.unite.de.mesure.associe.au.quintuplet")] <- "UNITE_TITRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Libelle.de.l'engin.de.prelevement")] <- "ENGIN"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Taille.de.l'engin.de.prelevement")] <- "ENGIN_TAILLE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Symbole.unite.de.la.taille.de.l'engin.de.prelev.")] <- "TAILLE_UNITE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Prelevement...Service.preleveur...Libelle")] <- "LABO_PRELEVEMENT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Service.analyste...Libelle")] <- "LABO_ANALYSTE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Service.saisisseur...Libelle")] <- "LABO_SAISISSEUR"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Niveau.de.qualite")] <- "QUALITE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Commentaire.de.qualification")] <- "QUALITE_COMMENTAIRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Coordonnees.passage...Coordonnees minx")] <- "LON"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Coordonnees.passage...Coordonnees miny")] <- "LAT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Commentaire")] <- "PASSAGE_COMMENTAIRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Prelevement...Commentaire")] <- "PRELEVEMENT_COMMENTAIRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Echantillon...Commentaire")] <- "ECHANTILLON_COMMENTAIRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Commentaires")] <- "RESULTAT_COMMENTAIRE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Sonde")] <- "SONDE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Symbole.unite.de.la.sonde")] <- "UNITE_SONDE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Lieu.de.surveillance...Mnemonique")] <- "ID_SITE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Identifiant.interne")] <- "ID_PASSAGE"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Prelevement...Identifiant.interne")] <- "ID_PRELEVEMENT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Echantillon...Identifiant.interne")] <- "ID_ECHANTILLON"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Resultat...Identifiant")] <- "ID_RESULTAT"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Echantillon...Libelle.du.support")] <- "SUPPORT_ECHANTILLON"
  colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "Passage...Mois")] <- "MOIS"
}

# Unuseful columns
PHYTO_Pro <- PHYTO_Pro|>
  select(-c(`Coordonnees.passage...Coordonnees.maxx`,`Coordonnees.passage...Coordonnees.maxy`,MOIS,ANNEE,FORMAT_HEURE,Passage...Semaine,FORMAT_HEURE,UNITE_TITRE))

# Compare raw and processed values
colnames(PHYTO_Pro)[which(names(PHYTO_Pro) == "VALEUR")] <- "VALEUR_PRO"

PHYTO_Pro$HEURE[is.na(PHYTO_Pro$HEURE)] <- as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "UTC")
PHYTO_Pro$DATETIME <- as.POSIXct(paste(PHYTO_Pro$DATE,PHYTO_Pro$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

IGA_PHYTO <- left_join(IGA_PHYTO_clean,select(PHYTO_Pro,DATE,HEURE,TAXON,VALEUR_PRO),by = join_by(DATE, HEURE, TAXON))
IGA_PHYTO <- select(IGA_PHYTO,SITE:VALEUR,VALEUR_PRO,ID_SITE:aphiaID)

# Quality flag by BOREA
IGA_PHYTO$QFLAG <-  "A" # Acceptable value - data found acceptable by originators internal quality control checks

# Mark counts that are not present in the processed data and where the counts are less than 100 as questionable. 
# Unverified hypothesis: average then conversion of number of counts to cell/mL --> this is not our approach.
# Then mark counts < 100 as questionable: indicate in the protocol: only if raw and processed counts are <100.
IGA_PHYTO$QFLAG <- ifelse(is.na(IGA_PHYTO$VALEUR_PRO) & IGA_PHYTO$VALEUR < 100,"S",
                          ifelse(IGA_PHYTO$VALEUR < 100 & IGA_PHYTO$VALEUR_PRO < 100,"S",IGA_PHYTO$QFLAG))

# Counts below 100 that are inconsistent with processes are flagged as questionable: conversion to x100 rather than x1000 theoretically.
IGA_PHYTO$QFLAG <- ifelse(is.na(IGA_PHYTO$VALEUR_PRO) & IGA_PHYTO$VALEUR < 100,"S",
                          ifelse(IGA_PHYTO$VALEUR < 100 & IGA_PHYTO$VALEUR_PRO < 100,"S",
                                 ifelse(IGA_PHYTO$VALEUR < 100 & IGA_PHYTO$VALEUR != IGA_PHYTO$VALEUR_PRO,"S",IGA_PHYTO$QFLAG)))

# Transition to doubtful status of raw counts != processed counts
IGA_PHYTO$QFLAG <- ifelse(is.na(IGA_PHYTO$VALEUR_PRO) & IGA_PHYTO$VALEUR < 100,"S",
                          ifelse(IGA_PHYTO$VALEUR < 100 & IGA_PHYTO$VALEUR_PRO < 100,"S",
                                 ifelse(IGA_PHYTO$VALEUR < 100 & IGA_PHYTO$VALEUR != IGA_PHYTO$VALEUR_PRO,"S",
                                        ifelse(IGA_PHYTO$VALEUR != IGA_PHYTO$VALEUR_PRO,"S",IGA_PHYTO$QFLAG))))

# ok for raw taxa that are not in the processes: the initials of IFREMER treatments
IGA_PHYTO$QFLAG[is.na(IGA_PHYTO$QFLAG)] <- "A"

# If a count is flag S the all sampling becomes S

IGA_PHYTO$QFLAG_DATE <- ifelse(IGA_PHYTO$DATE %in% unique(filter(IGA_PHYTO,QFLAG == "S")$DATE),"S",IGA_PHYTO$QFLAG)

# We only report data from 1998
PHYTO <- filter(IGA_PHYTO, DATE >= as.Date("1998-01-01"))
IGA_PHYTO$QFLAG_DATE <- ifelse(IGA_PHYTO$DATE > as.Date("2024-01-01"),"A",IGA_PHYTO$QFLAG)


#_____________________PHYTOPLANKTON DATA TO DOME FORMAT_________________________####

DOME <- PHYTO

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(PHYTO$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- PHYTO$ID_ECHANTILLON

# Latitude
DOME$LATIT <- PHYTO$LAT

# Longitude
DOME$LONGI <- PHYTO$LON

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- PHYTO$SITE

# Sample date
DOME$SDATE <- format(as.Date(PHYTO$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- PHYTO$HEURE

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- PHYTO$ID_ECHANTILLON

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- 0.500

# Minimum depth - surface = 0 metres
DOME$MNDEP <- 0

# Maximum depth in metres
DOME$MXDEP <- 1

# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- PHYTO$aphiaID

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- NA

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- PHYTO$VALEUR

# Measurement unit
DOME$MUNIT <- "nrcells/l"

# Qualifier flag (non mandatory)
DOME$VFLAG <- PHYTO$QFLAG_DATE

# Analytical laboratory
DOME$ALABO <- NA

# Method of analysis
DOME$METOA <- NA

# Method of fixation/preservation
DOME$METFP <- NA

# Sampling laboratry
DOME$SLABO <- NA

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Monitoring Year
DOME$MYEAR <- format(as.Date(PHYTO$DATE),"%Y")

# Data Type
DOME$DTYPE <- "PP"

# Species codelist
DOME$RLIST <- "ERID" # WORMS

# Size Class
DOME$SIZCL <- NA

# Size Class Ref. List
DOME$SIZRF <- NA

# Species flag
DOME$SFLAG <- NA

# Keep only the DOME format columns
DOME_PP_PHYTO <- select(DOME,RLABO:MUNIT,VFLAG,PARAM,ALABO:SFLAG)

# Make the sum of the abundance of aphiaID, because before it was different taxa it needs to be merge to avoid missleading thinking about the data
DOME_PP_PHYTO <- DOME_PP_PHYTO %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")


# Save it
if(nrow(filter(DOME_PP_PHYTO, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, Taxonomy_correspondance_PHYTO_Dias_modif.csv must be updated")
} else {
  write.csv(DOME_PP_PHYTO,file = "output/DOME_PP_IGA_GRAVELINES_Ready_version_2024.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}

#### ZOOPLANKTON ####

IGA_ZOO <- IGA |>
  filter(PARAM %in% c("AINDVSNP","ALARVST2","ALARVST3","ALARVST1","AFEMEOVI","AADULFEM","AADULMAL","ALARVST7","ALARVST6","APLJUSNP","AOEUFSNP","ALARVSNP")) |>
  select(-c(N_INDIVIDU,VALEUR_QUANTI,SUPPORT,PARAM_TITRE,FRACTION,METHODE,UNITE_TITRE,ENGIN))


# Convert measurements to the same unit: everything is expressed in mg.l-1.
IGA_ZOO$VALEUR <- ifelse(
  IGA_ZOO$UNITE == "Nbr.10 m-3",
  IGA_ZOO$VALEUR  * 10,
  IGA_ZOO$VALEUR 
)

IGA_ZOO <- select(IGA_ZOO,-c(UNITE))

IGA_ZOO <- IGA_ZOO |>
  mutate(VALEUR = as.numeric(format(VALEUR, scientific = FALSE)))

IGA_ZOO <- IGA_ZOO |>
  group_by(SITE,DATE,HEURE,NIVEAU_PROFONDEUR,TAXON,LABO_PRELEVEMENT,TAXON_USER,LABO_ANALYSTE,LABO_SAISISSEUR,LON,LAT,ID_PASSAGE,ID_PRELEVEMENT,ID_ECHANTILLON,ID_SITE,DATETIME) |>
  summarise(VALEUR = sum(VALEUR, na.rm = TRUE)) |>
  ungroup()

# replace 0 by NA
IGA_ZOO$VALEUR[IGA_ZOO$VALEUR == 0] <- NA

# Preparation to compare raw and processed data
ZOO_Comp <- IGA_ZOO |>
  select(DATE,HEURE,NIVEAU_PROFONDEUR,TAXON,VALEUR,ID_SITE,ID_ECHANTILLON,ID_PASSAGE,ID_PRELEVEMENT)

# Same as phyto
IGA_ZOO <- IGA_ZOO |>
  select(-c(ID_SITE,ID_ECHANTILLON,ID_PASSAGE,LABO_PRELEVEMENT:LABO_SAISISSEUR,TAXON_USER)) |>
  pivot_wider(names_from = "TAXON",values_from = VALEUR,values_fn = max) |>
  pivot_longer(cols = `Acartia (Acartiura) clausi`:Temora,names_to = "TAXON",values_to = "VALEUR") |>
  filter(!is.na(VALEUR))

IGA_ZOO <- left_join(IGA_ZOO,ZOO_Comp)

# Associate aphiaID
Taxonomy_correspondance_IGA_ZOO <- read_delim("data/Additional_data/Taxonomy_correspondance_IGA_ZOO_rank.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

IGA_ZOO <- left_join(IGA_ZOO,Taxonomy_correspondance_IGA_ZOO)

# Delete Noctiluca scintillans : a dinoflagellate
IGA_ZOO <- filter(IGA_ZOO,aphiaID != 0)


# Preparation to confront raw and processed data
remove_accents <- function(df) {
  # Vérifie que stringi est dispo
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Le package 'stringi' est requis. Installez-le avec install.packages('stringi').")
  }
  
  df_clean <- as.data.frame(
    lapply(df, function(x) {
      if (is.character(x)) {
        stringi::stri_trans_general(x, "Latin-ASCII")
      } else {
        x
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(df_clean)
}

IGA_ZOO_clean <- remove_accents(IGA_ZOO) 

# Loading processed data

ZOO_Pro <-read_delim("data/SEANOE_Gravelines_Process_Zoo.csv",delim = ";",
                     escape_double = FALSE,  trim_ws = TRUE,locale = locale(encoding = "Windows-1252"))
# Change columns names
{
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Annee")] <- "ANNEE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Lieu.de.surveillance...Libelle")] <- "SITE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Date")] <- "DATE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Heure")] <- "HEURE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Format.UT.de.l'heure")] <- "FORMAT_HEURE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Prelevement...Niveau")] <- "NIVEAU_PROFONDEUR"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Code.parametre")] <- "PARAM"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Nom.du.taxon.referent")] <- "TAXON"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Nom.du.taxon.saisi")] <- "TAXON_USER"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Numero.d'individu")] <- "N_INDIVIDU"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.parametre")] <- "PARAM_TITRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Valeur.de.la.mesure")] <- "VALEUR"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Valeur.qualitative")] <- "VALEUR_QUANTI"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.precision")] <- "PRECISION"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Incertitude")] <- "INCERTITUDE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Symbole.de.l'unite.incertitude")] <- "INCERTITUDE_UNIT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.support")] <- "SUPPORT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.fraction")] <- "FRACTION"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.methode..11")] <- "METHODE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Symbole.unite.de.mesure.associe.au.quintuplet")] <- "UNITE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Libelle.unite.de.mesure.associe.au.quintuplet")] <- "UNITE_TITRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Libelle.de.l'engin.de.prelevement")] <- "ENGIN"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Taille.de.l'engin.de.prelevement")] <- "ENGIN_TAILLE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Symbole.unite.de.la.taille.de.l'engin.de.prelev.")] <- "TAILLE_UNITE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Prelevement...Service.preleveur...Libelle")] <- "LABO_PRELEVEMENT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Service.analyste...Libelle")] <- "LABO_ANALYSTE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Service.saisisseur...Libelle")] <- "LABO_SAISISSEUR"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Niveau.de.qualite")] <- "QUALITE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Commentaire.de.qualification")] <- "QUALITE_COMMENTAIRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Coordonnees.passage...Coordonnees minx")] <- "LON"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Coordonnees.passage...Coordonnees miny")] <- "LAT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Commentaire")] <- "PASSAGE_COMMENTAIRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Prelevement...Commentaire")] <- "PRELEVEMENT_COMMENTAIRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Echantillon...Commentaire")] <- "ECHANTILLON_COMMENTAIRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Commentaires")] <- "RESULTAT_COMMENTAIRE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Sonde")] <- "SONDE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Symbole.unite.de.la.sonde")] <- "UNITE_SONDE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Lieu.de.surveillance...Mnemonique")] <- "ID_SITE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Identifiant.interne")] <- "ID_PASSAGE"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Prelevement...Identifiant.interne")] <- "ID_PRELEVEMENT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Echantillon...Identifiant.interne")] <- "ID_ECHANTILLON"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Resultat...Identifiant")] <- "ID_RESULTAT"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Echantillon...Libelle.du.support")] <- "SUPPORT_ECHANTILLON"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "Passage...Mois")] <- "MOIS"
  colnames(ZOO_Pro)[which(names(ZOO_Pro) == "AphiaID")] <- "aphiaID"
}

# Unuseful columns
ZOO_Pro <- ZOO_Pro|>
  select(-c(`Coordonnees.passage...Coordonnees.maxx`,`Coordonnees.passage...Coordonnees.maxy`,MOIS,ANNEE,FORMAT_HEURE,Passage...Semaine,FORMAT_HEURE,UNITE_TITRE))

ZOO_Pro$HEURE[is.na(ZOO_Pro$HEURE)] <- as.POSIXct("00:00:00", format = "%H:%M:%S", tz = "UTC")
ZOO_Pro$DATETIME <- as.POSIXct(paste(ZOO_Pro$DATE,ZOO_Pro$HEURE),format = "%Y-%m-%d %H:%M:%S",tz="UTC")

# Compare raw and processed values
colnames(ZOO_Pro)[which(names(ZOO_Pro) == "VALEUR")] <- "VALEUR_PRO"

ZOO_Pro <- ZOO_Pro |>
  mutate(VALEUR_PRO = as.numeric(format(VALEUR_PRO, scientific = FALSE)))

IGA_ZOO <- left_join(IGA_ZOO_clean,select(ZOO_Pro,DATE,HEURE,TAXON,VALEUR_PRO),by = join_by(DATE, HEURE, TAXON))
IGA_ZOO <- select(IGA_ZOO,SITE:VALEUR,VALEUR_PRO,ID_SITE:aphiaID)

IGA_ZOO <- IGA_ZOO |>
  mutate(VALEUR_PRO = as.numeric(format(VALEUR_PRO, scientific = FALSE))) |>
  mutate(VALEUR = as.numeric(format(VALEUR, scientific = FALSE)))

options(scipen = 999)

# New quality flag BOREA
IGA_ZOO$QFLAG <-  "A" # Acceptable value - data found acceptable by originators internal quality control checks

# If raw and processed data are different it flags S
IGA_ZOO$QFLAG <- ifelse(IGA_ZOO$VALEUR != IGA_ZOO$VALEUR_PRO,"S","A")

# ok if the data is not on the processed data
IGA_ZOO$QFLAG[is.na(IGA_ZOO$QFLAG)] <- "A"

# If there is one taxa flags S all the sampling becomes S

IGA_ZOO$QFLAG_DATE <- ifelse(IGA_ZOO$DATE %in% unique(filter(IGA_ZOO,QFLAG == "S")$DATE),"S",IGA_ZOO$QFLAG)

# We report since 1998
ZOO <- filter(IGA_ZOO, DATE >= as.Date("1998-01-01"))
ZOO$QFLAG_DATE <- ifelse(ZOO$DATE > as.Date("2024-01-01"),"A",ZOO$QFLAG)

DOME <- ZOO

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(ZOO$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- ifelse(is.na(ZOO$ID_ECHANTILLON),paste0("BOREA",rownames(ZOO)),ZOO$ID_ECHANTILLON)

# Latitude
DOME$LATIT <- ZOO$LAT

# Longitude
DOME$LONGI <- ZOO$LON

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- ZOO$SITE

# Sample date
DOME$SDATE <- format(as.Date(ZOO$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- ZOO$HEURE

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- ifelse(is.na(ZOO$ID_ECHANTILLON),paste0("BOREA",rownames(ZOO)),ZOO$ID_ECHANTILLON)

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- NA

# Minimum depth - surface = 0 metres
DOME$MNDEP <- 2

# Maximum depth in metres
DOME$MXDEP <- 5

# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- ZOO$aphiaID

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- NA

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- ZOO$VALEUR

# Measurement unit
DOME$MUNIT <- "nr/m3"

# Qualifier flag (non mandatory)
DOME$VFLAG <- ZOO$QFLAG_DATE

# Analytical laboratory
DOME$ALABO <- NA

# Method of analysis
DOME$METOA <- NA

# Method of fixation/preservation
DOME$METFP <- NA

# Sampling laboratry
DOME$SLABO <- NA

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Monitoring Year
DOME$MYEAR <- format(as.Date(ZOO$DATE),"%Y")

# Data Type
DOME$DTYPE <- "ZP"

# Species codelist
DOME$RLIST <- "ERID" # WORMS

# Size Class
DOME$SIZCL <- NA

# Size Class Ref. List
DOME$SIZRF <- NA

# Species flag
DOME$SFLAG <- NA

# Keep only the DOME format columns
DOME_ZP_ZOO <- select(DOME,RLABO:MUNIT,VFLAG,PARAM,ALABO:SFLAG)

# Make the sum of the abundance of aphiaID, because before it was different taxa it needs to be merge to avoid missleading thinking about the data
DOME_ZP_ZOO <- DOME_ZP_ZOO %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")


# Save it
if(nrow(filter(DOME_PP_ZOO, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, Taxonomy_correspondance_ZOO_Dias_modif.csv must be updated")
} else {
  write.csv(DOME_PP_ZOO,file = "output/DOME_ZP_IGA_GRAVELINES_Ready_version_2024.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}