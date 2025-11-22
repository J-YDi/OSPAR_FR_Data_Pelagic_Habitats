#_______________________________________________________________________________
# Nom               : ROSCOFF_PICONANO_Historic_submission.r
# Date de modif     : 20/11/2025
# Objet             : Mise en forme des donnees historiques de PICONANO des 
#                      stations de Roscoff
# Auteurs           : J-Y. Dias
# Version R         : 4.5.0
#_______________________________________________________________________________

# Donnees fournies par Laetitia Rigaut-Jalabert
# Astan : 2000-2009
# Estacade : 2008-2012
# Suivi integre dans le SOMLIT apres ces dates
# DOIsation en cours

#_______________________________ Packages_______________________________________

# Fonction permettant d'installer les packages si non présents et/ou les charger
# en l etat ne fonctionne pas pour les packages qui s'installent hors CRAN
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
packages_needed <- c("readr","dplyr","ggplot2","cowplot","tidyr","lubridate","stringr","readxl")

loadpackages(packages_needed)

#_____________________Loading and unification Original Data_____________________
Estacade <- read_delim("data/PELAGOS_PICONANO_Estacade_2008_2012.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
Astan <- read_delim("data/PELAGOS_PICONANO_Astan_2000_2009.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Astan[Astan == "no beads"] <- NA
Estacade[Estacade == "no beads"] <- NA

# Conver all columns types 
Astan <- as.data.frame(lapply(Astan, function(x) type.convert(x, as.is = TRUE)))
Estacade <- as.data.frame(lapply(Estacade, function(x) type.convert(x, as.is = TRUE)))

# Get rid off empty lines
Astan <- filter(Astan,!is.na(`station.`))
Estacade <- filter(Estacade,!is.na(`station.`))

# Bind the two sites
ROSCOFF <- bind_rows(Astan,Estacade)

# Change columns names
colnames(ROSCOFF)[which(names(ROSCOFF) == "station.")] <- "OBS" 
colnames(ROSCOFF)[which(names(ROSCOFF) == "site.d.echantillonnage.")] <- "SITE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "jeu.de.donnees.")] <- "DONNEES"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sample_name.")] <- "ID_ECHANTILLON"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_resp.")] <- "RESP"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_date.dd.mm.yyyy..")] <- "DATE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_time..hh.mm.")] <- "TIME"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_depth_min")] <- "PROFONDEUR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_depth_max")] <- "PROFONDEUR_MAX"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_volume")] <- "VOLUME"
colnames(ROSCOFF)[which(names(ROSCOFF) == "tide")] <- "MAREE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "tide_coefficient")] <- "COEFF_MAREE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "sampling_device.")] <- "MET_ECHANTILLON"
colnames(ROSCOFF)[which(names(ROSCOFF) == "bottle_model")] <- "BOTTLE_MODEL"
colnames(ROSCOFF)[which(names(ROSCOFF) == "bottle_volume..liter.")] <- "BOTTLE_VOL"
colnames(ROSCOFF)[which(names(ROSCOFF) == "pump_model")] <- "PUMP_MODEL"
colnames(ROSCOFF)[which(names(ROSCOFF) == "pump_flow..L.per.min.")] <- "PUMP_FLOW"
colnames(ROSCOFF)[which(names(ROSCOFF) == "net_length..m.")] <- "NET_LENGTH"
colnames(ROSCOFF)[which(names(ROSCOFF) == "net_type")] <- "NET_TYPE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "net_area..m2.")] <- "NET_AREA"
colnames(ROSCOFF)[which(names(ROSCOFF) == "net_mesh..Âµm.")] <- "NET_MESH"
colnames(ROSCOFF)[which(names(ROSCOFF) == "net_direction")] <- "NET_DIRECT"
colnames(ROSCOFF)[which(names(ROSCOFF) == "analysis_method.")] <- "METHODE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "analysis_resp.")] <- "RESP_RESULTAT"
colnames(ROSCOFF)[which(names(ROSCOFF) == "analysis_date..dd.mm.yyyy..")] <- "DATE_RESULTAT"
colnames(ROSCOFF)[which(names(ROSCOFF) == "rawdatafile")] <- "RAW_DF"
colnames(ROSCOFF)[which(names(ROSCOFF) == "cytometer")] <- "CYTO"
colnames(ROSCOFF)[which(names(ROSCOFF) == "analysissoftware")] <- "SOFTW"
colnames(ROSCOFF)[which(names(ROSCOFF) == "analysissoftwareversion")] <- "SOFT_V"
colnames(ROSCOFF)[which(names(ROSCOFF) == "fixativemethod")] <- "FIXATION"
colnames(ROSCOFF)[which(names(ROSCOFF) == "fixativefinalconcentration")] <- "FIXATION_CONC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "storagecondition")] <- "STOCK"
colnames(ROSCOFF)[which(names(ROSCOFF) == "otherproduct")] <- "OTH_PROD"
colnames(ROSCOFF)[which(names(ROSCOFF) == "flashfrozen")] <- "FLASHFROZEN"
colnames(ROSCOFF)[which(names(ROSCOFF) == "beadsreferences")] <- "BILLE_REF"
colnames(ROSCOFF)[which(names(ROSCOFF) == "beadssize")] <- "BILLE_TAILLE"
colnames(ROSCOFF)[which(names(ROSCOFF) == "valbeadsbacteria")] <- "BACTC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_LNABac")] <- "LNABACC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_LNABac_SSC")] <- "LNABACSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_LNABac_FL_Green")] <- "LNABACFLV"
colnames(ROSCOFF)[which(names(ROSCOFF) == "valbeadsautotrophs")] <- "AUTOC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_PicoE")] <- "PICOEC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_PicoE_SSC")] <- "PICOESSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_PicoE_FL_Red")] <- "PICOEFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotEuk")] <- "TEUKC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotEuk_SSC")] <- "TEUKSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_HNABac")] <- "HNABACC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_HNABac_SSC")] <- "HNABACSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_HNABac_FL_Green")] <- "HNABACFLV"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotBac")] <- "TBACC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotBac_SSC")] <- "TBACSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotBac_FL_Green")] <- "TBACFLV"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_TotEuk_FL_Red")] <- "TBACFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_NanoE")] <- "NANOEC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_NanoE_SSC")] <- "NANOESSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_NanoE_FL_Red")] <- "NANOEFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Cry")] <- "CRYC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Cry_SSC")] <- "CRYSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Cry_FL_Red")] <- "CRYFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Cry_FL_Orange")] <- "CRYFLO"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Pro")] <- "PROC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Pro_SSC")] <- "PROSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Pro_FL_Red")] <- "PROFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Syn")] <- "SYNC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Syn_SSC")] <- "SYNSSC"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Syn_FL_Red")] <- "SYNFLR"
colnames(ROSCOFF)[which(names(ROSCOFF) == "Val_Syn_FL_Orange")] <- "SYNFLO"

#__________________________________Clean raw data_______________________________####
# Unificate station's names
ROSCOFF[ROSCOFF == "SOMLIT-Astan"] <- "Astan"
ROSCOFF[ROSCOFF == "SOMLIT-Estacade"] <- "Estacade"

# Delete empty and unuseful columns
ROSCOFF <- ROSCOFF |>
  select(-c(OBS,DONNEES,VOLUME,PUMP_MODEL:NET_DIRECT,OTH_PROD,...68,TIME,
            COEFF_MAREE,MET_ECHANTILLON,BOTTLE_MODEL,BOTTLE_VOL,METHODE,RAW_DF,
            SOFTW,SOFT_V,FIXATION,FIXATION_CONC,STOCK,FLASHFROZEN,BILLE_REF,
            DATE_RESULTAT,RESP,PROFONDEUR_MAX))

# Date Formating
ROSCOFF$DATE<- as.Date(ROSCOFF$DATE,format = "%d/%m/%Y",tz="UTC")

# Coordinates
ROSCOFF$LONG <- ifelse(ROSCOFF$SITE == "Astan",-3.968,-3.983)
ROSCOFF$LAT <- ifelse(ROSCOFF$SITE == "Estacade",48.772,48.732)

# Keep only measurements that corresponds to a taxa and are under of scope of COBAM
ROSCOFF_DOME <- ROSCOFF |>
  pivot_longer(cols = BACTC:SYNFLO,names_to = "TAXA",values_to = "VALEUR") |>
  filter(TAXA %in% c("CRYC","SYNC","PROC"))

# Replace 0 by NA 
ROSCOFF_DOME[ROSCOFF_DOME == 0] <- NA

# Delete 0 lines
ROSCOFF_DOME <- ROSCOFF_DOME |>
  filter(!is.na(VALEUR))


#_____________________PHYTOPLANKTON DATA TO DOME FORMAT_________________________####

DOME <- ROSCOFF_DOME

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unspecified research vessel

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(ROSCOFF_DOME$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- ROSCOFF_DOME$ID_ECHANTILLON

# Latitude
DOME$LATIT <- ROSCOFF_DOME$LAT

# Longitude
DOME$LONGI <- ROSCOFF_DOME$LONG

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- ROSCOFF_DOME$SITE

# Sample date
DOME$SDATE <- format(as.Date(ROSCOFF_DOME$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- NA

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- ROSCOFF_DOME$ID_ECHANTILLON

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- 5

# Minimum depth - surface = 0 metres
DOME$MNDEP <- ROSCOFF_DOME$PROFONDEUR
# Maximum depth in metres
DOME$MXDEP <- ROSCOFF_DOME$PROFONDEUR

# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- ifelse(ROSCOFF_DOME$TAXA == "PROC", 345515, #Prochlorochococcus
                     ifelse(ROSCOFF_DOME$TAXA == "CRYC", 17639, #Cryptophytes
                            ifelse(ROSCOFF_DOME$TAXA == "SYNC", 160572, # Synechococcus
                                   NA)))

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- NA

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- ROSCOFF_DOME$VALEUR

# Measurement unit
DOME$MUNIT <- "nrcells/l"

# Qualifier flag (non mandatory)
DOME$QFLAG <- NA

# Analytical laboratory
DOME$ALABO <- NA

# Method of analysis
DOME$METOA <- NA

# Method of fixation/preservation
DOME$METFP <- NA

# Sampling laboratory
DOME$SLABO <- "521"

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Contracting Party
# DOME$CNTRY <- "FR"

# Monitoring Year
DOME$MYEAR <- format(as.Date(ROSCOFF_DOME$DATE),"%Y")

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
DOME_PP_ROSCOFF <- select(DOME,RLABO:SFLAG)

# Make the max of the abundance of aphiaID, as recommanded by Maud Lemoine from the REPHY
DOME_PP_ROSCOFF <- DOME_PP_ROSCOFF %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = max(VALUE, na.rm = TRUE), .groups = "drop")

# Save it
if(nrow(filter(DOME_PP_ROSCOFF, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, SEANOE_PNMI_ROSCOFF_DOME_aphiaID.csv must be updated")
} else {
  write.csv(DOME_PP_ROSCOFF,file = "output/DOME_PP_ROSCOFF_PICONANO_Ready_version_2.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}

