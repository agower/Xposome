# Testing changes to the Add Project functionality

shinyApps_path <- "/rprojectnb/montilab-p/Xposome/155.41.202.164/home/xposome/Xposome_RC/shinyApps"
www_path <- file.path(shinyApps_path, "www")
data_path <- file.path(shinyApps_path, "data")
setwd(shinyApps_path)

# substitutes for reactive values holding input
intro_file <- function () file.path(www_path, "RMD", "introduction_HEPG2.Rmd")
pro_file <- function () readRDS(file.path(data_path, "HEPG2", "Profile_Annotation.RDS"))
chem_file <- function () readRDS(file.path(data_path, "HEPG2", "Chemical_Annotation.RDS"))
ge_file <- function() readRDS(file.path(data_path, "HEPG2", "Expression_Set.RDS"))
CMapPCL <- function () NULL
CMapPert <- function () NULL
gs_collection_file <- function () NULL

# substitutes for reactive values holding warnings
addProjectBasicInfoWarning <- identity
addProjectIntroFileWarning <- identity
addProjectProfileAnnotationFileWarning <- identity
addProjectGeneExpressionFileWarning <- identity
addProjectCMapPCLFileWarning <- identity
addProjectCMapPertFileWarning <- identity
addProjectCompoundVariableWarning <- identity
addProjectExposureVariablesWarning <- identity
addProjectEnrichmentGSWarning <- identity
addProjectEnrichmentLinkWarning <- identity
addProjectGSCollectionFileWarning <- identity
addProjectExposurePhenotypesWarning <- identity
addProjectInputWarning <- identity

input <- list()
input$addProjectName <- "Liver carcinogenicity"
input$addProjectCellLine <- "HEPG2"
input$addProjectPortal <- "HEPG2"
input$addProjectDescription <- "HEPG2 test description"
input$addProjectLandmark <- TRUE
input$addProjectCompoundVariable <- "test cv"
input$addProjectExposureVariables <- "Dose"
input$addProjectExposurePhenotypes <- c("Carcinogenicity", "Genotoxicity")
input$metavar_testEP <- "testmetavar"
input$addProjectUseExistingGS <- TRUE
input$addProjectIncludeCMap <- FALSE
input$addProjectTAS <- TRUE
input$addProjectModzscores <- TRUE
