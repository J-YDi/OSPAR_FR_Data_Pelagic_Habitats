#_______________________________________________________________________________
# Title              : PHYTOBS_submission.r
# Date               : 15/09/2025
# Object             : Script for submitting data from the PHYTOBS network 
# Authors            : Jean-Yves Dias
# R version          : 4.5.0
# Data DOI           : 10.17882/85178
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

# Arcachon from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

ARCACHON <- read_delim("data/PELAGOS_Arcachon_Bouee13.csv", 
                       delim = ";", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)

# Clean the data header
column <- as.data.frame(t(ARCACHON[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
ARCACHON_c <- column
ARCACHON <- ARCACHON[-c(1:6),]
colnames(ARCACHON) <- column$keep

# Change columns names
{
colnames(ARCACHON)[which(names(ARCACHON) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(ARCACHON)[which(names(ARCACHON) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(ARCACHON)[which(names(ARCACHON) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(ARCACHON)[which(names(ARCACHON) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(ARCACHON)[which(names(ARCACHON) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(ARCACHON)[which(names(ARCACHON) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(ARCACHON)[which(names(ARCACHON) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(ARCACHON)[which(names(ARCACHON) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(ARCACHON)[which(names(ARCACHON) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(ARCACHON)[which(names(ARCACHON) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(ARCACHON)[which(names(ARCACHON) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(ARCACHON)[which(names(ARCACHON) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(ARCACHON)[which(names(ARCACHON) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(ARCACHON)[which(names(ARCACHON) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(ARCACHON)[which(names(ARCACHON) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(ARCACHON)[which(names(ARCACHON) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(ARCACHON)[which(names(ARCACHON) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(ARCACHON)[which(names(ARCACHON) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(ARCACHON)[which(names(ARCACHON) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(ARCACHON)[which(names(ARCACHON) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(ARCACHON)[which(names(ARCACHON) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(ARCACHON)[which(names(ARCACHON) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(ARCACHON)[which(names(ARCACHON) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(ARCACHON)[which(names(ARCACHON) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(ARCACHON)[which(names(ARCACHON) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(ARCACHON)[which(names(ARCACHON) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
}

ARCACHON <- ARCACHON |>
  pivot_longer(cols = `Achnanthes/Achnanthes sp./149191/NA/NA/NA`:`Pennales/Undetermined pennales/1304629/NA/>=10µm/NA`,names_to = "TAXON",values_to = "VALEUR") |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  ) |>
  select(-`taxon_pelagos/taxon_user/aphiaID/stage/form/sex`)

# Change the site name to match with SOMLIT data
ARCACHON$SITE <- "Bouée 13"

# Portzic from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

STANNE <- read_delim("data/PELAGOS_Brest_St_anne_de_Portzic_point_somlit.csv", 
                     delim = ";", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
# Clean the data header
column <- as.data.frame(t(STANNE[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
STANNE_c <- column
STANNE <- STANNE[-c(1:6),]
colnames(STANNE) <- column$keep

# Change column names
{
colnames(STANNE)[which(names(STANNE) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(STANNE)[which(names(STANNE) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(STANNE)[which(names(STANNE) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(STANNE)[which(names(STANNE) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(STANNE)[which(names(STANNE) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(STANNE)[which(names(STANNE) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(STANNE)[which(names(STANNE) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(STANNE)[which(names(STANNE) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(STANNE)[which(names(STANNE) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(STANNE)[which(names(STANNE) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(STANNE)[which(names(STANNE) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(STANNE)[which(names(STANNE) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(STANNE)[which(names(STANNE) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(STANNE)[which(names(STANNE) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(STANNE)[which(names(STANNE) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(STANNE)[which(names(STANNE) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(STANNE)[which(names(STANNE) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(STANNE)[which(names(STANNE) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(STANNE)[which(names(STANNE) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(STANNE)[which(names(STANNE) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(STANNE)[which(names(STANNE) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(STANNE)[which(names(STANNE) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(STANNE)[which(names(STANNE) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(STANNE)[which(names(STANNE) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(STANNE)[which(names(STANNE) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(STANNE)[which(names(STANNE) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(STANNE)[which(names(STANNE) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(STANNE)[which(names(STANNE) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(STANNE)[which(names(STANNE) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(STANNE)[which(names(STANNE) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(STANNE)[which(names(STANNE) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(STANNE)[which(names(STANNE) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
}

STANNE <- STANNE |>
  pivot_longer(cols = `Achnanthes/Achnanthes/149191/NA/NA/NA`:`Pennales/pennées sp./1304629/NA/NA/NA`,names_to = "TAXON",values_to = "VALEUR") |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  ) |>
  select(-`taxon_pelagos/taxon_user/aphiaID/stage/form/sex`)

# Change the site name to match with SOMLIT data
STANNE$SITE <- "Portzic"

# Antioche from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

ANTIOCHE <- read_delim("data/PELAGOS_LaRochelle_SOMLIT_Antioche.csv", 
                       delim = ";", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)

# Clean the data header
column <- as.data.frame(t(ANTIOCHE[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
ANTIOCHE_c <- column
ANTIOCHE <- ANTIOCHE[-c(1:6),]
colnames(ANTIOCHE) <- column$keep

# Change column names
{
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(ANTIOCHE)[which(names(ANTIOCHE) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
}

ANTIOCHE <- ANTIOCHE |>
  pivot_longer(cols = `Achnanthes longipes/Achnanthes longipes/156533/NA/NA/NA`:`Warnowiaceae/Warnowiaceae/109415/NA/NA/NA`,names_to = "TAXON",values_to = "VALEUR") |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  ) |> select(-`taxon_pelagos/taxon_user/aphiaID/stage/form/sex`)

# Change the site name to match with SOMLIT data
ANTIOCHE$SITE <- "Antioche"

# Luc-sur-Mer SMILE from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

Unicaen <- read_delim("data/PELAGOS_Phyto-Unicaen_Luc.csv", 
                      delim = ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)

# Clean the data header
column <- as.data.frame(t(Unicaen[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
Unicaen_c <- column
Unicaen <- Unicaen[-c(1:6),]
colnames(Unicaen) <- column$keep

# Change column names
{
colnames(Unicaen)[which(names(Unicaen) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(Unicaen)[which(names(Unicaen) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(Unicaen)[which(names(Unicaen) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(Unicaen)[which(names(Unicaen) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(Unicaen)[which(names(Unicaen) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(Unicaen)[which(names(Unicaen) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(Unicaen)[which(names(Unicaen) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(Unicaen)[which(names(Unicaen) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(Unicaen)[which(names(Unicaen) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(Unicaen)[which(names(Unicaen) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(Unicaen)[which(names(Unicaen) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(Unicaen)[which(names(Unicaen) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(Unicaen)[which(names(Unicaen) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(Unicaen)[which(names(Unicaen) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(Unicaen)[which(names(Unicaen) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(Unicaen)[which(names(Unicaen) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(Unicaen)[which(names(Unicaen) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(Unicaen)[which(names(Unicaen) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(Unicaen)[which(names(Unicaen) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(Unicaen)[which(names(Unicaen) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(Unicaen)[which(names(Unicaen) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(Unicaen)[which(names(Unicaen) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(Unicaen)[which(names(Unicaen) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(Unicaen)[which(names(Unicaen) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(Unicaen)[which(names(Unicaen) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(Unicaen)[which(names(Unicaen) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(Unicaen)[which(names(Unicaen) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(Unicaen)[which(names(Unicaen) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(Unicaen)[which(names(Unicaen) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(Unicaen)[which(names(Unicaen) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(Unicaen)[which(names(Unicaen) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(Unicaen)[which(names(Unicaen) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
}

Unicaen <- Unicaen |>
  pivot_longer(cols = `Actinocyclus/Actinocyclus/148944/NA/NA/NA`:`Torodinium/Torodinium/109479/NA/NA/NA`,names_to = "TAXON",values_to = "VALEUR") |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  ) |>
  select(-`taxon_pelagos/taxon_user/aphiaID/stage/form/sex`)

# Change the site name to match with SOMLIT data
Unicaen$SITE <- "Smile"

# Wimereux Point C  from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

Wimereux <- read_delim("data/PELAGOS_Wimereux_SOMLIT_phyto_pointC.csv",
                       delim = ";", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
# Change the site name to match with SOMLIT data
ARCACHON$SITE <- "Bouée 13"# Clean the data header
column <- as.data.frame(t(Wimereux[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
Wimereux_c <- column
Wimereux <- Wimereux[-c(1:6),]
colnames(Wimereux) <- column$keep

# Change column names
{
colnames(Wimereux)[which(names(Wimereux) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(Wimereux)[which(names(Wimereux) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(Wimereux)[which(names(Wimereux) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(Wimereux)[which(names(Wimereux) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(Wimereux)[which(names(Wimereux) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(Wimereux)[which(names(Wimereux) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(Wimereux)[which(names(Wimereux) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(Wimereux)[which(names(Wimereux) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(Wimereux)[which(names(Wimereux) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(Wimereux)[which(names(Wimereux) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(Wimereux)[which(names(Wimereux) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(Wimereux)[which(names(Wimereux) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(Wimereux)[which(names(Wimereux) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(Wimereux)[which(names(Wimereux) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(Wimereux)[which(names(Wimereux) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(Wimereux)[which(names(Wimereux) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(Wimereux)[which(names(Wimereux) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(Wimereux)[which(names(Wimereux) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(Wimereux)[which(names(Wimereux) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(Wimereux)[which(names(Wimereux) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(Wimereux)[which(names(Wimereux) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(Wimereux)[which(names(Wimereux) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(Wimereux)[which(names(Wimereux) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(Wimereux)[which(names(Wimereux) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(Wimereux)[which(names(Wimereux) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(Wimereux)[which(names(Wimereux) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(Wimereux)[which(names(Wimereux) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(Wimereux)[which(names(Wimereux) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(Wimereux)[which(names(Wimereux) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(Wimereux)[which(names(Wimereux) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(Wimereux)[which(names(Wimereux) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(Wimereux)[which(names(Wimereux) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
}

Wimereux <- Wimereux |> pivot_longer(cols = `Achnanthes/Achnanthes/149191/NA/NA/NA`:`Scrippsiella acuminata/scripsiella trochoida/1321853/NA/NA/NA`,names_to = "TAXON",values_to = "VALEUR") |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  ) |> select(-`taxon_pelagos/taxon_user/aphiaID/stage/form/sex`)

# Change the site name to match with SOMLIT data
Wimereux$SITE <- "Point C"

# Roscoff Astan from the PELAGOS database (https://pelagos.sb-roscoff.fr/pelagos/) ####

Astan <- read_delim("data/PELAGOS_Roscoff_SOMLIT_Astan.csv",
                    delim = ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
# Clean the data header
column <- as.data.frame(t(Astan[1:6,]))
column$keep <- paste0(column$V1,"/",column$V2,"/",column$V3,"/",column$V4,"/",column$V5,"/",column$V6)
Astan_c <- column
Astan <- Astan[-c(1:6),]
colnames(Astan) <- column$keep

# Change column names
{
colnames(Astan)[which(names(Astan) == "station*/NA/NA/NA/NA/NA")] <- "OBS" 
colnames(Astan)[which(names(Astan) == "site d'echantillonnage*/NA/NA/NA/NA/NA")] <- "SITE"
colnames(Astan)[which(names(Astan) == "jeu de donnees*/NA/NA/NA/NA/NA")] <- "DONNEES"
colnames(Astan)[which(names(Astan) == "sample_name*/NA/NA/NA/NA/NA")] <- "ID_ECHANTILLON"
colnames(Astan)[which(names(Astan) == "sampling_resp*/NA/NA/NA/NA/NA")] <- "RESP"
colnames(Astan)[which(names(Astan) == "sampling_date(dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE"
colnames(Astan)[which(names(Astan) == "sampling_time (hh:mm)/NA/NA/NA/NA/NA")] <- "TIME"
colnames(Astan)[which(names(Astan) == "sampling_depth_min/NA/NA/NA/NA/NA")] <- "PROFONDEUR"
colnames(Astan)[which(names(Astan) == "sampling_depth_max/NA/NA/NA/NA/NA")] <- "PROFONDEUR_MAX"
colnames(Astan)[which(names(Astan) == "sampling_volume/NA/NA/NA/NA/NA")] <- "VOLUME"
colnames(Astan)[which(names(Astan) == "tide/NA/NA/NA/NA/NA")] <- "MAREE"
colnames(Astan)[which(names(Astan) == "tide_coefficient/NA/NA/NA/NA/NA")] <- "COEFF_MAREE"
colnames(Astan)[which(names(Astan) == "sampling_device*/NA/NA/NA/NA/NA")] <- "MET_ECHANTILLON"
colnames(Astan)[which(names(Astan) == "bottle_model/NA/NA/NA/NA/NA")] <- "BOTTLE_MODEL"
colnames(Astan)[which(names(Astan) == "bottle_volume (liter)/NA/NA/NA/NA/NA")] <- "BOTTLE_VOL"
colnames(Astan)[which(names(Astan) == "pump_model/NA/NA/NA/NA/NA")] <- "PUMP_MODEL"
colnames(Astan)[which(names(Astan) == "pump_flow (L per min)/NA/NA/NA/NA/NA")] <- "PUMP_FLOW"
colnames(Astan)[which(names(Astan) == "net_length (m)/NA/NA/NA/NA/NA")] <- "NET_LENGTH"
colnames(Astan)[which(names(Astan) == "net_type/NA/NA/NA/NA/NA")] <- "NET_TYPE"
colnames(Astan)[which(names(Astan) == "net_area (m2)/NA/NA/NA/NA/NA")] <- "NET_AREA"
colnames(Astan)[which(names(Astan) == "net_mesh (µm)/NA/NA/NA/NA/NA")] <- "NET_MESH"
colnames(Astan)[which(names(Astan) == "net_direction/NA/NA/NA/NA/NA")] <- "NET_DIRECT"
colnames(Astan)[which(names(Astan) == "analysis_method*/NA/NA/NA/NA/NA")] <- "METHODE"
colnames(Astan)[which(names(Astan) == "analysis_resp*/NA/NA/NA/NA/NA")] <- "RESP_RESULTAT"
colnames(Astan)[which(names(Astan) == "analysis_date (dd/mm/yyyy)*/NA/NA/NA/NA/NA")] <- "DATE_RESULTAT"
colnames(Astan)[which(names(Astan) == "preservation/NA/NA/NA/NA/NA")] <- "PRESERVATION"
colnames(Astan)[which(names(Astan) == "volume_analysed(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_ANALYSE"
colnames(Astan)[which(names(Astan) == "filtration_mesh/NA/NA/NA/NA/NA")] <- "FILTRA_MESH"
colnames(Astan)[which(names(Astan) == "sample_fraction/NA/NA/NA/NA/NA")] <- "FRACTION_ECHANTILLON"
colnames(Astan)[which(names(Astan) == "fractioning_method/NA/NA/NA/NA/NA")] <- "FRACTION_METHODE"
colnames(Astan)[which(names(Astan) == "fixing_method/NA/NA/NA/NA/NA")] <- "FIXING_METHODE"
colnames(Astan)[which(names(Astan) == "volume_sedimentation_chamber(ml)/NA/NA/NA/NA/NA")] <- "VOLUME_CHAMBRE"
# To deal with the same column name
colnames(Astan)[which(names(Astan) == "Dinoflagellata/Undetermined Dinoflagellata sp./146203/NA/naked/NA")[1]] <- "Duplicate_Dinoflagellata/Undetermined Dinoflagellata sp./146203/NA/naked/NA"
}

Astan_Phyto <- Astan |> select(OBS:VOLUME_CHAMBRE,`Achnanthes longipes/Achnanthes longipes/156533/NA/NA/NA`:`Warnowia/Warnowia sp./109491/NA/NA/NA`) |>
  pivot_longer(cols = `Achnanthes longipes/Achnanthes longipes/156533/NA/NA/NA`:`Warnowia/Warnowia sp./109491/NA/NA/NA`,names_to = "TAXON",values_to = "VALEUR")

# To deal with the previoulsy duplicated column name
Astan_Phyto[which(Astan_Phyto$TAXON == "Duplicate_Dinoflagellata/Undetermined Dinoflagellata sp./146203/NA/naked/NA"),]$TAXON <- "Dinoflagellata/Undetermined Dinoflagellata sp./146203/NA/naked/NA" 

Astan_Phyto <- Astan_Phyto |>
  separate(
    col = TAXON,       
    into = c("TAXON","TAXON_USER","aphiaID","STAGE","FORM","SEX"),
    sep = "/", 
    remove = FALSE,          
    fill = "right"           
  )

# Change the site name to match with SOMLIT data
Astan_Phyto$SITE <- "Astan"

# /!\ Note : the other stations will be reported through the REPHY database 

# Bind all the station data to a single PHYTOBS dataset for OSPAR ####
# function for multiple data bind_rows
bind_multiple <- function(...) {
  dfs <- list(...)
  bind_rows(dfs)
}

PHYTOBS <- bind_multiple(ANTIOCHE,ARCACHON,Astan_Phyto,STANNE,Unicaen,Wimereux)


#__________________________________Clean raw data_______________________________####

# Encoding NA as it should
PHYTOBS[PHYTOBS == "NA"] <- NA

# We correctly re-encode the columns
PHYTOBS <- type_convert(PHYTOBS)

# Replace zeros by NA's
PHYTOBS[!is.na(PHYTOBS$VALEUR) & PHYTOBS$VALEUR <= 0, "VALEUR"] <- NA

PHYTOBS <- PHYTOBS |>
  filter(!is.na(VALEUR)) |># remove NA's
  filter(METHODE == "Comptage taxinomique phytoplancton") |> # Keep only quantitative counts
  select(-c(MET_ECHANTILLON:NET_DIRECT,FILTRA_MESH:VOLUME_CHAMBRE,DATE_RESULTAT,RESP,OBS)) # remove unuseful columns


# Date Formating
PHYTOBS$DATE<- as.Date(PHYTOBS$DATE,format = "%d/%m/%Y",tz="UTC")
# Hours Formating
PHYTOBS <- PHYTOBS %>%
  mutate(
    TIME_CLEAN = ifelse(is.na(TIME) | TIME == "", NA, TIME),  # On gère les vides
    TIME_CLEAN = gsub("^(\\d{1}):(\\d{1})$", "0\\1:0\\2", TIME_CLEAN), # "1:1" → "01:01"
    TIME_CLEAN = gsub("^(\\d{1}):(\\d{2})$", "0\\1:\\2", TIME_CLEAN),  # "1:25" → "01:25"
    TIME_CLEAN = gsub("^(\\d{2}):(\\d{1})$", "\\1:0\\2", TIME_CLEAN)) |>  # "13:5" → "13:05" : on le sait car est présent des :8 
  mutate(TIME = parse_time(TIME_CLEAN, format = "%H:%M")) |>
  select(-TIME_CLEAN)


# Clean up and reformat the identification columns
# Because some aphia ID are in the STAGE columns...

corriger_aphiaID_depuis_STAGE <- function(data, codes_a_deplacer) {
  data <- data %>%
    mutate(
      aphiaID = if_else(
        STAGE %in% codes_a_deplacer & is.na(aphiaID),
        STAGE,
        aphiaID
      ),
      STAGE = if_else(STAGE %in% codes_a_deplacer, NA_character_, STAGE)
    )
  return(data)
}

codes <- c("149032", "149151", "109540", "148912", "110303", "149619",
           "149074", "109553", "148899", "148985", "341285", "494057",
           "148963", "149069", "109566")

PHYTOBS <- corriger_aphiaID_depuis_STAGE(PHYTOBS, codes) |>
  mutate(aphiaID = if_else(str_detect(aphiaID, "^\\d+$"), aphiaID, NA_character_),
         aphiaID = as.numeric(aphiaID))


# We want to move certain STAGE values to the correct column...
# Function to exchange values from a column to another
deplacer_valeur_colonne <- function(data, colonne_source, colonne_cible, valeurs_a_deplacer) {
  data <- data %>%
    mutate(
      !!sym(colonne_cible) := if_else(
        (!!sym(colonne_source)) %in% valeurs_a_deplacer,
        !!sym(colonne_source),
        !!sym(colonne_cible)
      ),
      !!sym(colonne_source) := if_else(
        (!!sym(colonne_source)) %in% valeurs_a_deplacer,
        NA_character_,
        !!sym(colonne_source)
      )
    )
  
  return(data)
}

Taille <- c("20 µm","small <100µm","large : apical axis>20", "large   ≥20µm", "small <20µm","large ≥100µm","≥20µm","small : apical axis<20", "large   ≥50µm","<20µm",
            "20µm≤ size <50µm", "colony","> 20µm","filament","10-20 µm","large ≥20µm","large ≥50µm","10-20µm","<10µm","≥ 20µm","10- 20µm")
PHYTOBS <- deplacer_valeur_colonne(PHYTOBS,"STAGE","FORM",Taille)

Stade <- c("resting spores","cyst + naked + thecate","naked","spore","Cyst","single cells","thecate","nauplii","adult","cyst")
PHYTOBS <- deplacer_valeur_colonne(PHYTOBS,"FORM","STAGE",Stade)

Stade <- c("cysts")
PHYTOBS <- deplacer_valeur_colonne(PHYTOBS,"SEX","STAGE",Stade) 

Taille <- c("large>20","small<20","<<20 µm" )
PHYTOBS <- deplacer_valeur_colonne(PHYTOBS,"SEX","FORM",Taille)

# Make the STAGE more uniform
PHYTOBS <- PHYTOBS %>%
  mutate(STAGE = if_else(STAGE == "resting spores", "resting spore", STAGE)) |>
  mutate(STAGE = if_else(STAGE == "Cyst", "cyst", STAGE)) |>
  mutate(STAGE = if_else(STAGE == "cysts", "cyst", STAGE)) |>
  mutate(STAGE = if_else(STAGE == "cyste", "cyst", STAGE)) |>
  select(-SEX)



Taxonomy_correspondance_PHYTOBS <- read_delim(
  "data/Additional_data/Taxonomy_correspondance_PHYTOBS_Dias_modif.csv",
  delim = ";",
  escape_double = FALSE,
  col_types = cols(aphiaID = col_double()),
  trim_ws = TRUE,
  locale = locale(encoding = "ISO-8859-1")
)

PHYTOBS <- select(PHYTOBS, -aphiaID)
PHYTOBS <- left_join(PHYTOBS,Taxonomy_correspondance_PHYTOBS)

# Remove taxa without aphiaID, because dead cells or not phytoplankton
PHYTOBS <-  filter(PHYTOBS,aphiaID != 0)

#_____________________PHYTOPLANKTON DATA TO DOME FORMAT_________________________####

DOME <- PHYTOBS

# Ship or platform code 
DOME$SHIPC <- "ZZ99" # Unknown

# Cruise identifier (series of sampling occasions) 
# "Make it up if you don't go on cruises - one name to be used for a year is fine."
DOME$CRUIS <- format(as.Date(PHYTOBS$DATE),"%Y")

# Station identification /Sampling event ID
DOME$STNNO <- PHYTOBS$ID_ECHANTILLON

# Latitude
DOME$LATIT <- ifelse(PHYTOBS$SITE == "Bouée 13",44.633,
                     ifelse(PHYTOBS$SITE == "Antioche",46.084,
                            ifelse(PHYTOBS$SITE == "Astan",48.772,
                                   ifelse(PHYTOBS$SITE == "Portzic",48.552,
                                          ifelse(PHYTOBS$SITE == "Point C",50.679,
                                                 ifelse(PHYTOBS$SITE == "Smile",49.337,
                                                 NA))))))

# Longitude
DOME$LONGI <- ifelse(PHYTOBS$SITE == "Bouée 13",-1.233,
                     ifelse(PHYTOBS$SITE == "Antioche",	-1.308,
                            ifelse(PHYTOBS$SITE == "Astan",-3.968,
                                   ifelse(PHYTOBS$SITE == "Portzic",-4.552,
                                          ifelse(PHYTOBS$SITE == "Point C",1.52,
                                                 ifelse(PHYTOBS$SITE == "Smile",-0.333,
                                                        NA))))))

# Position system "WGS84 assumed if field is blank"
DOME$POSYS <- NA

# Station name
DOME$STATN <- PHYTOBS$SITE

# Sample date
DOME$SDATE <- format(as.Date(PHYTOBS$DATE),"%Y%m%d")

# Sample time
DOME$STIME <- PHYTOBS$TIME

# Sounding depth in metres
DOME$WADEP <- NA

# Sample number / Sample identification 
DOME$SMPNO <- PHYTOBS$ID_ECHANTILLON

# Factors potentially influencing guideline compliance and interpretation of data
DOME$FINFL <- NA

# Total sampled volume in litres
DOME$SMVOL <- 0.250

# Minimum depth - surface = 0 metres
DOME$MNDEP <- PHYTOBS$PROFONDEUR
  
# Maximum depth in metres
DOME$MXDEP <- PHYTOBS$PROFONDEUR_MAX
  
# Species from WoRMS
# manually indicate taxa without aphia ID if needed
DOME$SPECI <- PHYTOBS$aphiaID
  
# Species trophic status
DOME$TRPHY <- NA

# Stage of development
DOME$STAGE <- ifelse(PHYTOBS$STAGE == "cyst","CY",
                     ifelse(PHYTOBS$STAGE == "cyst + naked + thecate",	"MX",
                            "NS"))

# Parameter
DOME$PARAM <- "ABUNDNR" # Abundance number (number counted)

# Value measured
DOME$VALUE <- PHYTOBS$VALEUR

# Measurement unit
DOME$MUNIT <- "cells/l"

# Qualifier flag (non mandatory)
DOME$QFLAG <- NA

# Reporting laboratory
DOME$RLABO <- "BAMN" # BOREA

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

# Contracting Party
DOME$CNTRY <- "FR"

# Monitoring Year
DOME$MYEAR <- format(as.Date(PHYTOBS$DATE),"%Y")

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
DOME_PP_PHYTOBS <- select(DOME,SHIPC:SFLAG)

# Save it
if(nrow(filter(DOME_PP_PHYTOBS, is.na(SPECI))) != 0){
  message("There is a taxa without aphiaID, Taxonomy_correspondance_PHYTOBS_Dias_modif.csv must be updated")
} else {
  write.csv(DOME_PP_PHYTOBS,file = "output/DOME_PP_PHYTOBS_Ready_version.csv",row.names = F,fileEncoding = "UTF-8")
}


