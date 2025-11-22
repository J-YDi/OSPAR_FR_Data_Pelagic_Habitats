#_______________________________________________________________________________
# Title              : PNMI_submission.r
# Date               : 22/11/2025
# Object             : Script for submitting data from the Iroise Marine Park 
#                      for OSPAR Pelagic Habitats
# Authors            : Jean-Yves Dias
# R version          : 4.5.0
# Data DOI           : 10.17882/105465 (from SEANOE)
# Github link        : 
#_______________________________________________________________________________

#_______________________________ Packages_______________________________________####

# Function to install packages if not present and/or load them
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

packages_needed <- c("readr","dplyr","tidyr","lubridate","stringr")

loadpackages(packages_needed)

#__________________________Loading Original Data________________________________####
# Phytoplankton
PNMI_phyto <- read_delim("data/SEANOE_PNMI_phytoplankton_environment.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

PNMI_phyto <- unique(PNMI_phyto) # Remove duplicates (mostly repeated NA)

# WORMS aphiaID for phytoplankton, from SEANOE
aphiaID <- read_csv("data/SEANOE_PNMI_phyto_aphiaID.csv")

# Zooplankton
PNMI_zoo <- read_delim("data/SEANOE_PNMI_zooplankton_environment.csv",
                       delim = ";", escape_double = F, trim_ws = T)


#_________________________PHYTOPLANCTON DATA____________________________________ #### 

# Date Formating
PNMI_phyto$date <- as.Date(PNMI_phyto$date,format = "%d/%m/%Y")

# Keep only phytoplankton data
PHYTO <- select(PNMI_phyto,station:date,colnames(PNMI_phyto)[which(names(PNMI_phyto) == "sal_bottom")+1]:colnames(PNMI_phyto)[length(colnames(PNMI_phyto))])

# Distinguish sampling depth and modify depth labels to match with others data
PHYTO <- PHYTO |>
  pivot_longer(cols = c(colnames(PNMI_phyto)[which(names(PNMI_phyto) == "sal_bottom")+1]:colnames(PNMI_phyto)[length(colnames(PNMI_phyto))]), names_to = "TAXON",values_to = "VALEUR") |>
  separate(TAXON, into = c("PROFONDEUR","TAXON"),sep = "_",extra = "merge",fill = "right") |> # Distinguish depth
  mutate( # uniform with ENV data
    TAXON = str_replace_all(TAXON, "_", " "),
    PROFONDEUR = case_when(
      PROFONDEUR == "surface" ~ "SURFACE",
      PROFONDEUR == "bottom"  ~ "BOTTOM",
      TRUE ~ PROFONDEUR)
  ) |>
  filter(!is.na(VALEUR)) # remove NA's

# Merge taxa with their corresponding aphia ID
colnames(aphiaID)[1] <- "TAXON"  # change column names to match with other data
PHYTO <- left_join(PHYTO,aphiaID)

#_____________________PHYTOPLANKTON DATA TO DOME FORMAT_________________________####

DOME <- PHYTO

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(PHYTO$date),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- paste0(PHYTO$station,"_",format(as.Date(PHYTO$date),"%Y%m%d"),"_",PHYTO$PROFONDEUR)

# Latitude
DOME$LATIT <- PHYTO$lat

# Longitude
DOME$LONGI <- PHYTO$lon

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- paste0("PNMI_",PHYTO$station)

# Sample date
DOME$SDATE <- format(as.Date(PHYTO$date),"%Y%m%d")

# Sample time
DOME$STIME <- NA

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- paste0(PHYTO$station,"_",PHYTO$date,"_",PHYTO$PROFONDEUR)

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- 250 # see data paper Drago et al. in press for details

# Minimum depth - surface = 0 metres
DOME$MNDEP <- ifelse(PHYTO$PROFONDEUR == "SURFACE", 0, 
                     ifelse(PHYTO$PROFONDEUR == "15m", 15,
                            ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B1", 20,
                                   ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B2", 20,
                                          ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B3", 15,
                                                 ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B4", 45,
                                                        ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B5", 80,
                                                               ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B6", 90,
                                                                      ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B7", 90,
                                                                             ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D1", 20,
                                                                                    ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D2", 30,
                                                                                           ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D3", 45,
                                                                                                  ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D4", 75,
                                                                                                         ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D5", 90,
                                                                                                                ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D6", 90,
                                                                                                                       PHYTO$PROFONDEUR))))))))))))))) # see data paper Drago et al. in press for details
# Maximum depth in metres
DOME$MXDEP <- ifelse(PHYTO$PROFONDEUR == "SURFACE", 1, 
                     ifelse(PHYTO$PROFONDEUR == "15m", 15,
                            ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B1", 20,
                                   ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B2", 20,
                                          ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B3", 15,
                                                 ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B4", 45,
                                                        ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B5", 80,
                                                               ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B6", 90,
                                                                      ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "B7", 90,
                                                                             ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D1", 20,
                                                                                    ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D2", 30,
                                                                                           ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D3", 45,
                                                                                                  ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D4", 75,
                                                                                                         ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D5", 90,
                                                                                                                ifelse(PHYTO$PROFONDEUR == "BOTTOM" & PHYTO$station == "D6", 90,
                                                                                                                       PHYTO$PROFONDEUR)))))))))))))))# see data paper Drago et al. in press for details
# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- ifelse(PHYTO$TAXON == "Undetermined nanoflagellates", 1, # Biota
                     ifelse(PHYTO$TAXON == "Noctilucales Undertermined", 109393, #Noctilucales
                            ifelse(PHYTO$TAXON == "Episammic", 1, # Biota
                                   ifelse(PHYTO$TAXON == "Unidentified nanophytoplankton", 1, # Biota
                                          PHYTO$aphia_ID))))

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
DOME$QFLAG <- NA

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
DOME$MYEAR <- format(as.Date(PHYTO$date),"%Y")

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
DOME_PP_PNMI <- select(DOME,RLABO:SFLAG)

# Make the sum of the abundance of aphiaID, because before it was different taxa it needs to be merge to avoid missleading thinking about the data
DOME_PP_PNMI <- DOME_PP_PNMI %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")

# Make sure to submit non-zero values
DOME_PP_PNMI <- filter(DOME_PP_PNMI,VALUE >0)

# Save it
if(nrow(filter(DOME_PP_PNMI, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, SEANOE_PNMI_phyto_aphiaID.csv must be updated")
} else {
  write.csv(DOME_PP_PNMI,file = "output/DOME_PP_PNMI_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}



#__________________________ZOOPLANCTON DATA TO DOME FORMAT______________________#####

# Date Formating
PNMI_zoo$date <- as.Date(PNMI_zoo$date,format = "%d/%m/%Y")

# Keep only zooplankton concentration data "conc"
ZOO <- PNMI_zoo |>
  select(transect:date,colnames(PNMI_zoo)[which(names(PNMI_zoo) == "sal_bottom")+1]:colnames(PNMI_zoo)[length(colnames(PNMI_zoo))])

ZOO <- ZOO |>
  pivot_longer(cols = grep("conc_", colnames(ZOO), value = TRUE), names_to = "TAXON",values_to = "CONC") |>
  select(transect:date,TAXON,CONC) |>
  mutate(TAXON = str_remove(TAXON, "^conc_")) |>
  filter(!is.na(CONC) & CONC > 0) # remove zeros

# Merge taxa with their corresponding aphia ID

# Import data correspondance from WORMS and discussion with Laetitia Jalabert (laetitia.jalabert@imev-mer.fr)
Taxonomy_correspondance_ZOO <- read_delim("data/PNMI_Taxonomy_correspondance_ZOO_Dias_modif.csv", 
                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

ZOO <- left_join(ZOO,Taxonomy_correspondance_ZOO)

# Remove taxa that are not zooplankton or individuals 
ZOO <- filter(ZOO,aphiaID != "SUPPR")


#_______________________ZOOPLANKTON DATA TO DOME FORMAT_________________________####
DOME <- ZOO

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

# Ship or platform code 
DOME$SHIPC <- "AA31" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(ZOO$date),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- paste0(ZOO$station,"_",format(as.Date(ZOO$date),"%Y%m%d"),"_","SURFACE")

# Latitude 
DOME$LATIT <- ZOO$lat

# Longitude
DOME$LONGI <- ZOO$lon

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- paste0("PNMI_",ZOO$station)

# Sample date
DOME$SDATE <- format(as.Date(ZOO$date),"%Y%m%d")

# Sample time
DOME$STIME <- NA

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- paste0(ZOO$station,"_",ZOO$date,"_","SURFACE")

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- NA # Not indicated

# Minimum depth - surface = 0 metres
DOME$MNDEP <- 0 # see data paper Drago et al. in press

# Maximum depth in metres
DOME$MXDEP <- 5 # see data paper Drago et al. in press

# Species from WoRMS
DOME$SPECI <- ZOO$aphiaID

# Species trophic status
DOME$TRPHY <- NA

# Stage of development
# manually indicate stage according to Ecotaxa classification 
DOME$STAGE <-  ifelse(ZOO$TAXON == "larvae<Porcellanidae", "LV",
                      ifelse(ZOO$TAXON == "zoea<Brachyura", "LV-ZO",
                             ifelse(ZOO$TAXON == "nauplii<Cirripedia", "NP",
                                    ifelse(ZOO$TAXON == "egg<Actinopterygii", "EG",
                                           ifelse(ZOO$TAXON == "egg<other", "EG",
                                                  ifelse(ZOO$TAXON == "nauplii<Crustacea", "NP",
                                                         ifelse(ZOO$TAXON == "megalopa<Brachyura", "LV-MG",
                                                                ifelse(ZOO$TAXON == "larvae<Annelida", "LV",
                                                                       ifelse(ZOO$TAXON == "veliger", "LV-VE",
                                                                              ifelse(ZOO$TAXON == "pluteus<Echinoidea", "LV-PU",
                                                                                     ifelse(ZOO$TAXON == "pluteus<Ophiuroidea", "LV-PU",
                                                                                            ifelse(ZOO$TAXON == "zoea<Galatheidae", "LV-ZO",
                                                                                                   ifelse(ZOO$TAXON == "ephyra<Scyphozoa", "LV-EP",
                                                                                                          ifelse(ZOO$TAXON == "larvae<Squillidae", "LV",
                                                                                                                 ifelse(ZOO$TAXON == "larvae<Crustacea", "LV",
                                                                                                                        ifelse(ZOO$TAXON == "body<megalopa", "LV-MG",
                                                                                                                               ifelse(ZOO$TAXON == "invisible membrane<egg", "EG",
                                                                                                                                      ifelse(ZOO$TAXON == "juvenile<Salpida", "JV",
                                                                                                                                             "NS"))))))))))))))))))



# Parameter 
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- ZOO$CONC

# Measurement unit
DOME$MUNIT <- "nr/m3"

# Qualifier flag (not mandatory)
DOME$QFLAG <- NA

# Analytical laboratory
DOME$ALABO <- NA

# Method of analysis
DOME$METOA <- "IMA-ZS"

# Method of fixation/preservation
DOME$METFP <- NA

# Sampling laboratory
DOME$SLABO <- NA

# Sampler type
DOME$SMTYP <- NA

# Monitoring Programme
DOME$MPROG <- "CEMP~NATL" # OSPAR and National reporting

# Purpose of Monitoring
DOME$PURPM <- "T~S" # Temporal trend monitoring and Spatial (geographical) distribution monitoring

# Monitoring Year
DOME$MYEAR <- format(as.Date(ZOO$date),"%Y")

# Data Type
DOME$DTYPE <- "ZP" # Zooplankton

# Species codelist
DOME$RLIST <- "ERID" # WORMS

# Size Class
DOME$SIZCL <- NA

# Size Class Ref. List
DOME$SIZRF <- NA

# Species flag
DOME$SFLAG <- NA

# Keep only the DOME format columns
DOME_ZP_PNMI <- select(DOME,RLABO:SFLAG)

# Make the sum of the abundance of aphiaID, because before it was different taxa it needs to be merge to avoid missleading thinking about the data
DOME_ZP_PNMI <- DOME_ZP_PNMI %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")

# Make sure to submit non-zero values
DOME_ZP_PNMI <- filter(DOME_ZP_PNMI,VALUE >0)

# Save it
if(nrow(filter(DOME_ZP_PNMI, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, Taxonomy_correspondance_ZOO_PNMI_JY_modif.csv must be updated")
} else {
write.csv(DOME_ZP_PNMI,file = "output/DOME_ZP_PNMI_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")
}
