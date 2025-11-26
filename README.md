#  R scripts and data for the OSPAR Pelagic Habitats data call 2025

#### Github repository organization

##### Scripts folder :
+ PNMI_submission.R : Script used to submit data from the Iroise Marine National Park. Original data from : [10.17882/105465](https://doi.org/10.17882/105465)
+ PHYTOBS_submission.R : Script used to submit data from the PHYTOBS network. Original data from the [PELAGOS Database](https://pelagos.sb-roscoff.fr/pelagos/?execution=e2s1), with DOI : [10.17882/85178](https://doi.org/10.17882/85178) /!\ it correspond only to stations that is not sampled by IFREMER (part of the REPHY) /!\
+ SOMLIT_submission.R : Script used to submit data from the SOMLIT network. Original data from the [SOMLIT website](https://www.somlit.fr/demande-de-donnees/), with DOI : [10.17882/100323](https://doi.org/10.17882/100323)
+ ROSCOFF_PICONANO_submission.R : Script used to submit data from the historical time series of cytometry collected at the Roscoff Biological Station from Laetitia Rigaut-Jalabert, but now available from SEANOE. DOI : [10.17882/110112](https:doi.org/10.17882/110112)
+ Extraction_REPHY_API_quadrige.R : Script used to extract the REPHY from Quadrige. 

##### data folder : 
+ contains all the original raw datasets needed.

#### output folder :
+ contains the processed datasets

#### submission folder :
+ For DOME format : contains the dataset after the output dataset "Simplified format" were converted to DOME format though https://vocab.ices.dk/DataConversion/home/index and screening with https://dome.ices.dk/datsu/DATSU.aspx
+ For OCEAN format : contains the CSV file as in output folder

#### Main folder :
+ Remontee_OSPAR.Rproj R project file to obtain the relative path.
+ EquivalenceChamp_SEANOE-API_20250617.xlsx Excel files showing the correspondences between the Quadrige website and the Quadrige API.
+ README_Extraction_REPHY_API_quadrige.txt Readme with instructions and explanations for using the Extraction_REPHY_API_quadrige.R script.
+ Licence.
+ General README.


###### Contact : jean-yves.dias@sorbonne-universite.fr
