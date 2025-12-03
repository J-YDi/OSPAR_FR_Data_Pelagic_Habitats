#_______________________________________________________________________________
# Nom               : Merge_OSPAR_dataset.r
# Date de modif     : 02/12/2025
# Objet             : Merge OSPAR dataset 
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
packages_needed <- c("readr","dplyr","ggplot2","cowplot","tidyr","lubridate","readxl","stringr")

loadpackages(packages_needed)

#__________________________Loading processed Data________________________________

ZP_Gravelines <- read_csv("output/DOME_ZP_IGA_GRAVELINES_Ready_version_2024.csv")
ZP_PNMI <- read_csv("output/DOME_ZP_PNMI_Ready_version.csv")

#____________________________Make some correction_______________________________
# Change CRUIS data for each dataset
ZP_Gravelines$CRUIS <- paste0("IGA_Gravelines_",ZP_Gravelines$CRUIS)
ZP_PNMI$CRUIS <- paste0("PNMI_",ZP_PNMI$CRUIS)

# Correct STNNO to make them unique, the SMPNNO make them different for each depth 
# but there are only surface sampling for zooplankton

ZP_PNMI$STNNO <- str_remove(ZP_PNMI$STNNO, "_SURFACE$")

#___________________________Merge the dataset___________________________________
ZP_FR <- bind_rows(ZP_PNMI,ZP_Gravelines)

# Replace 00:00 by NA for STIME
ZP_FR$STIME[ZP_FR$STIME == hms::hms(0)] <- NA

# Correct the HPEL construction of STNNO and SMPNO

ZP_FR$STNNO[grep("BOREA",ZP_FR$STNNO)] <- paste0("BOREA",ZP_FR$SDATE[is.na(ZP_FR$STNNO)])

ZP_FR$SMPNO[grep("BOREA",ZP_FR$SMPNO)] <- paste0("BOREA",ZP_FR$SDATE[is.na(ZP_FR$SMPNO)])

# Change CEMP
ZP_FR$MPROG[ZP_FR$MYEAR < 2000] <- "JMP~NATL"

#Move MPROG to the last
ZP_FR <- ZP_FR[c(setdiff(names(ZP_FR), "MPROG"), "MPROG")]

ZP_FR <- ZP_FR %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")

#_______________________Save it_________________________________________________
write.csv(ZP_FR,file = "output/DOME_ZP_FR_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")


################################################################################
##########                    PHYTOPLANKTON                       ##############

#__________________________Loading processed Data________________________________

PP_Gravelines <- read_csv("output/DOME_PP_IGA_GRAVELINES_Ready_version_2024.csv")
PP_PNMI <- read_csv("output/DOME_PP_PNMI_Ready_version.csv")
PP_PHYTOBS <- read_csv("output/DOME_PP_PHYTOBS_Ready_version_2.csv")
PP_ROSCOFF <- read_csv("output/DOME_PP_ROSCOFF_PICONANO_Ready_version_2.csv")
PP_REPHY <- read_csv("output/DOME_PP_REPHY_FLORTOT_Ready_version.csv")
PP_SOMLIT <- read_csv("output/DOME_PP_SOMLIT_PICONANO_Ready_version.csv")


#____________________________Make some correction_______________________________
# Change CRUIS data for each dataset
PP_Gravelines$CRUIS <- paste0("IGA_Gravelines_",PP_Gravelines$CRUIS)
PP_PNMI$CRUIS <- paste0("PNMI_",PP_PNMI$CRUIS)
PP_PHYTOBS$CRUIS <- paste0("PHYTOBS_",PP_PHYTOBS$CRUIS)
PP_ROSCOFF$CRUIS <- paste0("SBR_",PP_ROSCOFF$CRUIS)
PP_REPHY$CRUIS <- paste0("REPHY_",PP_REPHY$CRUIS)
PP_SOMLIT$CRUIS <- paste0("SOMLIT_",PP_SOMLIT$CRUIS)


# Correct STNNO to make them unique, the SMPNNO make them different for each depth 
# but there are only surface sampling for zooplankton

PP_PNMI$STNNO <- str_remove(PP_PNMI$STNNO, "_SURFACE$")
PP_PNMI$STNNO <- str_remove(PP_PNMI$STNNO, "_BOTTOM$")

PP_Gravelines$STNNO <- as.character(PP_Gravelines$STNNO)
PP_Gravelines$SMPNO <- as.character(PP_Gravelines$SMPNO)

PP_REPHY$STNNO <- as.character(PP_REPHY$STNNO)
PP_REPHY$SMPNO <- as.character(PP_REPHY$SMPNO)

#___________________________Merge the dataset___________________________________
PP_FR <- bind_rows(PP_PNMI,PP_Gravelines)
PP_FR <- bind_rows(PP_FR,PP_PHYTOBS)
PP_FR <- bind_rows(PP_FR,PP_ROSCOFF)
PP_FR <- bind_rows(PP_FR,PP_REPHY)
PP_FR <- bind_rows(PP_FR,PP_SOMLIT)

# Replace 00:00 by NA for STIME
PP_FR$STIME[PP_FR$STIME == hms::hms(0)] <- NA

# Change CEMP
PP_FR$MPROG[PP_FR$MYEAR < 2000] <- "JMP~NATL"

# Still some issues with time but it is on the raw data...

# Follow up the corrections
PP_FR <- PP_FR %>%
  group_by(across(-VALUE)) %>%  
  summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop")

#Move MPROG to the last
PP_FR <- PP_FR[c(setdiff(names(PP_FR), "MPROG"), "MPROG")]


#_______________________Save it_________________________________________________
Subdivise_df <- function(data,colonne,filtre,path){
  if (!colonne %in% colnames(data)){ 
    stop("La colonne indique n'existe pas")}
  for (aextraire in filtre){
    subdf <- data[data[[colonne]] %in% aextraire,]
    write.csv(subdf,file = paste0(path,deparse(substitute(data)),"_",aextraire,".csv"),row.names = F,fileEncoding = "UTF-8",na = "")
    message(paste0(deparse(substitute(data)),"_",aextraire,".csv"," created in ",path))
  }
}

Subdivise_df(PP_FR,"MYEAR",unique(PP_FR$MYEAR),"output/PP/")

write.csv(PP_FR,file = "output/DOME_PP_FR_Ready_version.csv",row.names = F,fileEncoding = "UTF-8",na = "")



