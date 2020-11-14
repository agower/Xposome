# Until the basic info (project name, cell line, portal name, and description)
# have all been entered, print a warning message in red
observeEvent(
  {
    input$addProjectName
    input$addProjectCellLine
    input$addProjectPortal
    input$addProjectDescription
  },
  {
    # Obtain the input values
    new_project <- c(
      Project     = trimws(input$addProjectName),
      Cell_Line    = trimws(input$addProjectCellLine),
      Portal      = trimws(input$addProjectPortal),
      Description = trimws(input$addProjectDescription)
    )
    if (any(new_project == "")) {
      addProjectBasicInfoWarning("Please fill in the required (*) fields.")
    } else {
      addProjectBasicInfoWarning("")
    }
  },
  ignoreInit=TRUE
)
output$addProjectBasicInfoWarningHTML <- renderUI({
  req(addProjectBasicInfoWarning())
  p(style="color:red;", HTML(addProjectBasicInfoWarning()))
})

# Introduction File ############################################################

# If an invalid introduction file name is provided, print a warning message
observeEvent(input$addProjectAddButton,
  if (is.null(input$addProjectIntroFile)) {
    addProjectIntroFileWarning("Please choose a file to import.")
    intro_file(NULL)
    return(NULL)
  }
)
observeEvent(input$addProjectIntroFile, {
  intro_file(NULL)
  addProjectIntroFileWarning("")
  if (is.null(input$addProjectIntroFile)) return(NULL)
  tryCatch({
    extension <- tolower(tools::file_ext(input$addProjectIntroFile$datapath))
    if (extension != "rmd") {
      addProjectIntroFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    } else {
      intro_file(input$addProjectIntroFile$datapath)
    }
  }, error=function(err) {
    addProjectIntroFileWarning("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war) {
    addProjectIntroFileWarning("Import failed. Please check your file again.")
    return(NULL)
  })
})
output$addProjectIntroFileWarningHTML <- renderUI({
  req(addProjectIntroFileWarning())
  p(class="fileInputMsg",  HTML(addProjectIntroFileWarning()))
})

# Profile Annotation File ######################################################

# Import profile annotation file; if an invalid file name is provided,
# or the file cannot be read, print a warning message
observeEvent(input$addProjectAddButton, {
  if (is.null(input$addProjectProfileAnnotationFile)) {
    addProjectProfileAnnotationFileWarning("Please choose a file to import.")
    chem_file(NULL)
    pro_file(NULL)
    return(NULL)
  }
})
observeEvent(input$addProjectProfileAnnotationFile, {
  inputfile <- input$addProjectProfileAnnotationFile
  addProjectProfileAnnotationFileWarning("")
  chem_file(NULL)
  pro_file(NULL)
  if (is.null(inputfile)) {
    updateSelectInput(
      session, inputId="addProjectCompoundVariable",
      choices=c("Import a profile annotation" = "")
    )
    updateSelectInput(
      session, inputId="addProjectExposureVariables",
      choices=c("Import a profile annotation" = "")
    )
    updateSelectInput(
      session, inputId="addProjectExposurePhenotypes",
      choices=c("Import a profile annotation" = "")
    )
    return(NULL)
  }
  tryCatch({
    extension <- tolower(tools::file_ext(inputfile$datapath))
    if (extension == "csv") {
      dat <- read.csv(
        inputfile$datapath,
        row.names=1, check.names=FALSE, stringsAsFactors=FALSE
      )
    } else if (extension == "rds") {
      dat <- readRDS(inputfile$datapath)
    } else {
      updateSelectInput(
        session, inputId="addProjectCompoundVariable",
        choices=c("Import a profile annotation" = "")
      )
      updateSelectInput(
        session, inputId="addProjectExposureVariables",
        choices=c("Import a profile annotation" = "")
      )
      updateSelectInput(
        session, inputId="addProjectExposurePhenotypes",
        choices=c("Import a profile annotation" = "")
      )
      addProjectProfileAnnotationFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    }
    variables <- c("Sig_Id", "Chemical_Id", "Chemical_Name", "BUID", "CAS")
    if (all(variables %in% colnames(dat))) {
      exposure_variable <- setdiff(colnames(dat), c(variables, "TAS"))
      updateSelectInput(
        session, inputId="addProjectCompoundVariable",
        choices=c("Please select an option below" = "", "Chemical_Id")
      )
      updateSelectInput(
        session, inputId="addProjectExposureVariables",
        choices=c("Please select an option below" = "", exposure_variable)
      )
      updateSelectInput(
        session, inputId="addProjectExposurePhenotypes",
        choices = c("Please select an option below" = "", exposure_variable)
      )
      chem_file(unique(dat[setdiff(variables, "Sig_Id")]))
      pro_file(dat)
    } else {
      updateSelectInput(
        session, inputId="addProjectCompoundVariable",
        choices=c("Import a profile annotation" = "")
      )
      updateSelectInput(
        session, inputId="addProjectExposureVariables",
        choices=c("Import a profile annotation" = "")
      )
      updateSelectInput(
        session, inputId="addProjectExposurePhenotypes",
        choices=c("Import a profile annotation" = "")
      )
      addProjectProfileAnnotationFileWarning(
        paste(
          "One or more of the required variables:",
          em(paste(variables, collapse = ", ")),
          "are missing from the dataset."
        )
      )
      return(NULL)
    }
  }, error=function(err) {
    addProjectProfileAnnotationFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  }, warning=function(war) {
    addProjectProfileAnnotationFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  })
})
output$addProjectProfileAnnotationFileWarningHTML <- renderUI({
  req(addProjectProfileAnnotationFileWarning())
  p(class="fileInputMsg", HTML(addProjectProfileAnnotationFileWarning()))
})

# Gene Expression File #########################################################

# Import gene expression file; if an invalid file name is provided,
# or the file cannot be read, print a warning message
observeEvent(input$addProjectAddButton, {
  inputfile <- input$addProjectGeneExpressionFile
  pro_ann <- pro_file()
  if (is.null(inputfile)) {
    addProjectGeneExpressionFileWarning("Please choose a file to import.")
    ge_file(NULL)
    return(NULL)
  }
})
observeEvent(input$addProjectGeneExpressionFile, {
  pro_ann <- pro_file()
  ge_file(NULL)
  addProjectGeneExpressionFileWarning("")
  if (is.null(input$addProjectGeneExpressionFile)) return(NULL)
  tryCatch({
    extension <- tolower(
      tools::file_ext(input$addProjectGeneExpressionFile$datapath)
    )
    if (extension == "csv") {
      dat <- read.csv(
        input$addProjectGeneExpressionFile$datapath,
        row.names = 1, check.names = FALSE, stringsAsFactors = FALSE
      )
    } else if (extension == "rds") {
      dat <- readRDS(input$addProjectGeneExpressionFile$datapath)
    } else {
      addProjectGeneExpressionFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    }
    if (is.null(pro_ann)) {
      addProjectGeneExpressionFileWarning(
        "Need a profile annotation file to match."
      )
      return(NULL)
    }
    if (
      all(colnames(dat) %in% pro_ann$Sig_Id) ||
      all(colnames(dat) %in% pro_ann$Chemical_Id)
    ) {
      ge_file(dat)
    } else {
      addProjectGeneExpressionFileWarning(
        paste(
          "The ExpressionSet and profile annotation do not match.",
          "Please check your files again."
        )
      )
      return(NULL)
    }
  }, error=function(err) {
    addProjectGeneExpressionFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  }, warning=function(war) {
    ge_file(dat)
    return(NULL)
  })
})
output$add_addProjectGeneExpressionFileWarningHTML <- renderUI({
  req(addProjectGeneExpressionFileWarning())
  p(class="fileInputMsg", HTML(addProjectGeneExpressionFileWarning()))
})

# Connectivity Map (CMap) Perturbagen Classes (PCL) File #######################

# Import CMap PCL file; if an invalid file name is provided,
# or the file cannot be read, print a warning message
observeEvent(input$addProjectAddButton, {
  req(input$addProjectIncludeCMap)
  pro_ann <- pro_file()
  if (is.null(input$addProjectCMapPCLFile)) {
    addProjectCMapPCLFileWarning("Please choose a file to import.")
    CMapPCL(NULL)
    return(NULL)
  }
})
observeEvent(input$addProjectCMapPCLFile, {
  req(input$addProjectIncludeCMap)
  inputfile <- input$addProjectCMapPCLFile
  pro_ann <- pro_file()
  CMapPCL(NULL)
  addProjectCMapPCLFileWarning("")
  if (is.null(inputfile)) return(NULL)
  tryCatch({
    extension <- tolower(tools::file_ext(inputfile$datapath))
    if (extension == "csv") {
      dat <- read.csv(
        inputfile$datapath,
        row.names = 1, check.names = FALSE, stringsAsFactors = FALSE
      )
    } else if (extension == "rds") {
      dat <- readRDS(inputfile$datapath)
    } else {
      addProjectCMapPCLFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    }
    if (is.null(pro_ann)) {
      addProjectCMapPCLFileWarning("Need a profile annotation file to match.")
      return(NULL)
    }
    if (
      all(colnames(dat) %in% pro_ann$Sig_Id) ||
      all(colnames(dat) %in% pro_ann$Chemical_Id)
    ) {
      CMapPCL(dat)
    } else {
      addProjectCMapPCLFileWarning(
        paste(
          "The connectivity map (perturbagens class) and profile annotation",
          "do not match. Please check your files again."
        )
      )
      return(NULL)
    }
  }, error=function(err) {
    addProjectCMapPCLFileWarning("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war) {
    addProjectCMapPCLFileWarning("Import failed. Please check your file again.")
    return(NULL)
  })
}, ignoreNULL = FALSE
)
output$addProjectCMapPCLFileWarningHTML <- renderUI({
  req(addProjectCMapPCLFileWarning())
  p(class="fileInputMsg",  HTML(addProjectCMapPCLFileWarning()))
})

# Connectivity Map (CMap) Perturbagens File ####################################

# Import CMap perturbagens file; if an invalid file name is provided,
# or the file cannot be read, print a warning message
observeEvent(input$addProjectAddButton, {
  req(input$addProjectIncludeCMap)
  pro_ann <- pro_file()
  if (is.null(input$addProjectCMapPertFile)) {
    addProjectCMapPertFileWarning("Please choose a file to import.")
    CMapPert(NULL)
    return(NULL)
  }
})
observeEvent(input$addProjectCMapPertFile, {
  req(input$addProjectIncludeCMap)
  pro_ann <- pro_file()
  CMapPert(NULL)
  addProjectCMapPertFileWarning("")
  if (is.null(input$addProjectCMapPertFile)) return(NULL)
  tryCatch({
    extension <- tolower(tools::file_ext(input$addProjectCMapPertFile$datapath))
    if (extension == "csv") {
      dat <- read.csv(
        input$addProjectCMapPertFile$datapath,
        row.names=1, check.names=FALSE, stringsAsFactors=FALSE
      )
    } else if (extension == "rds") {
      dat <- readRDS(input$addProjectCMapPertFile$datapath)
    } else {
      addProjectCMapPertFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    }
    if (is.null(pro_ann)) {
      addProjectCMapPertFileWarning("Need a profile annotation file to match.")
      return(NULL)
    }
    if (
      all(colnames(dat) %in% pro_ann$Sig_Id) ||
      all(colnames(dat) %in% pro_ann$Chemical_Id)
    ) {
      CMapPert(dat)
    } else {
      addProjectCMapPertFileWarning(
        paste(
          "The connectivity map (perturbagens) and profile annotation",
          "do not match. Please check your files again."
        )
      )
      return(NULL)
    }
  }, error=function(err) {
    addProjectCMapPertFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  }, warning=function(war) {
    addProjectCMapPertFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  })
}, ignoreNULL = FALSE)
output$addProjectCMapPertFileWarningHTML <- renderUI({
  req(addProjectCMapPertFileWarning())
  p(class="fileInputMsg",  HTML(addProjectCMapPertFileWarning()))
})

# Exposure Variables ###########################################################

# If an invalid entry is provided for the compound variable,
# print a warning message in red
observeEvent(input$addProjectCompoundVariable, {
  req(input$addProjectCompoundVariable)
  addProjectCompoundVariableWarning(NULL)
})
output$addProjectCompoundVariableHTML <- renderUI({
  req(addProjectCompoundVariableWarning())
  p(style="color:red;", HTML(addProjectCompoundVariableWarning()))
})

# If an invalid entry is provided for the exposure variables,
# print a warning message in red
observeEvent(input$addProjectExposureVariables, {
  req(input$addProjectExposureVariables)
  addProjectExposureVariablesWarning(NULL)
})
# Create message for exposure variable selection
output$addProjectExposureVariablesWarningHTML <- renderUI({
  req(addProjectExposureVariablesWarning())
  p(style="color:red;", HTML(addProjectExposureVariablesWarning()))
})

# Gene Set Enrichment ##########################################################

# If an invalid entry is provided for the gene set collection name,
# print a warning message in red
observeEvent(input$addProjectEnrichmentGS, {
  req(input$addProjectEnrichmentGS)
  EnrichmentGS <- trimws(input$addProjectEnrichmentGS)
  if (EnrichmentGS == "") {
    addProjectEnrichmentGSWarning("Please enter a valid name.")
  } else {
    addProjectEnrichmentGSWarning("")
  }
})
output$addProjectEnrichmentGSWarningHTML <- renderUI({
  req(addProjectEnrichmentGSWarning())
  p(style="color:red;", HTML(addProjectEnrichmentGSWarning()))
})

# If an invalid link is provided for the gene set collection,
# print a warning message in red
observeEvent(input$addProjectEnrichmentLink, {
  req(input$addProjectEnrichmentLink)
  EnrichmentLink <- trimws(input$addProjectEnrichmentLink)
  if (EnrichmentLink == "") {
    addProjectEnrichmentLinkWarning("Please enter a valid link.")
  } else {
    addProjectEnrichmentLinkWarning("")
  }
})
output$addProjectEnrichmentLinkWarningHTML <- renderUI({
  req(addProjectEnrichmentLinkWarning())
  p(style="color:red;", HTML(addProjectEnrichmentLinkWarning()))
})

# Import .gmt file; if an invalid file name is provided,
# or the file cannot be read, print a warning message
observeEvent(input$addProjectAddButton, {
  req(input$addProjectUseExistingGS)
  if (is.null(input$addProjectGSCollectionFile)) {
    addProjectGSCollectionFileWarning("Please choose a file to import.")
    gs_collection_file(NULL)
    return(NULL)
  }
})
observeEvent(input$addProjectGSCollectionFile, {
  req(input$addProjectUseExistingGS)
  gs_collection_file(NULL)
  addProjectGSCollectionFileWarning("")
  if (is.null(input$addProjectGSCollectionFile)) return(NULL)
  tryCatch({
    extension <- tolower(
      tools::file_ext(input$addProjectGSCollectionFile$datapath)
    )
    if (extension == "gmt") {
      data <- GSEABase::getGmt(input$addProjectGSCollectionFile$datapath)
      gs_collection_file(
        list(path=input$addProjectGSCollectionFile$datapath, data=data)
      )
    } else {
      addProjectGSCollectionFileWarning(
        "Incorrect file format. Please check your file again."
      )
      return(NULL)
    }
  }, error=function(err) {
    addProjectGSCollectionFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  }, warning=function(war) {
    addProjectGSCollectionFileWarning(
      "Import failed. Please check your file again."
    )
    return(NULL)
  })
})
output$addProjectGSCollectionFileWarningHTML <- renderUI({
  req(addProjectGSCollectionFileWarning())
  p(class="fileInputMsg",  HTML(addProjectGSCollectionFileWarning()))
})

# K2Taxonomer ##################################################################

# If invalid exposure phenotypes are provided,
# print a warning message in red
observeEvent(input$addProjectExposurePhenotypes, {
  req(input$addProjectExposurePhenotypes)
  addProjectExposurePhenotypesWarning(NULL)
})
output$addProjectExposurePhenotypesWarningHTML <- renderUI({
  req(addProjectExposurePhenotypesWarning())
  p(style="color:red;", HTML(addProjectExposurePhenotypesWarning()))
})

# Data table of exposure phenotype statistical tests
output$addProjectMetavarVariableTest <- DT::renderDataTable(
  {
    req(input$addProjectExposurePhenotypes, pro_file())
    pro_ann <- pro_file()
    varlist <- input$addProjectExposurePhenotypes
    test <- suppressWarnings(phenotype_test(pro_ann=pro_ann, varlist=varlist))
    table <- data.frame(
      "Exposure Phenotype"=varlist, "Statistical Test"=test
    )
    return(table)
  },
  rownames=FALSE, server=FALSE, escape=FALSE, selection="none",
  options=list(
    dom="T",
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()) }')
  )
)

# Create cohorts option
observeEvent(input$addProjectExposureVariables, {
  req(pro_file(), chem_file(), ge_file(), input$addProjectExposureVariables)
  pro_ann <- pro_file()
  chem_ann <- chem_file()
  gene_expression <- ge_file()
  var <- ifelse(
    all(colnames(gene_expression) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id"
  )
  exposure <- input$addProjectExposureVariables
  # Getting the number of replicates for each chemical
  if (var == "Sig_Id") {
    pro_ann$unique_ID_by_chem <- unlist(
      lapply(
        1:nrow(pro_ann),
        function(r) paste0(unlist(pro_ann[r,exposure]), collapse="_")
      )
    )
    chem_replicate <- pro_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  } else {
    chem_ann$unique_ID_by_chem <- unlist(
      lapply(
        1:nrow(chem_ann),
        function(r) paste0(unlist(chem_ann[r,exposure]), collapse="_")
      )
    )
    chem_replicate <- chem_ann %>% group_by(Chemical_Id, unique_ID_by_chem) %>% summarise(Frequency=n()) %>% ungroup()
  }
  if (any(chem_replicate$Frequency > 1)) {
    cohorts("Chemical_Id")
  } else {
    cohorts(NULL)
  }
})

# Create Add TAS and ModZ option
output$addProjectTASModz <- renderUI({
  req(cohorts())
  div(
    h4("Calculations:", style="padding-bottom: 10px"),
    checkboxInput(
      inputId="addProjectTAS", label="TAS", value=TRUE
    ),
    checkboxInput(
      inputId="addProjectModzscores", label="Mod-Zscores", value=TRUE
    )
  )
})

# Add Project dialog, 'Add' Button #############################################

observeEvent(input$addProjectAddButton, {
  # Obtain the current project list
  projectlist <- listEntities("Project")
  # Set default MSigDB version
  MSigDB_version <- "7.0"
  # Obtain the input values
  new_project <- list(
    Project     = trimws(input$addProjectName),
    Cell_Line   = trimws(input$addProjectCellLine),
    Portal      = trimws(input$addProjectPortal),
    Description = trimws(input$addProjectDescription)
  )

  # Check all the input variables
  check <- TRUE
  if (any(unlist(new_project) == "")) {
    addProjectBasicInfoWarning("Please fill in the required (*) fields.")
    check <- FALSE
  } else {
    addProjectBasicInfoWarning("")
  }

  new_project$Landmark_Gene <- input$addProjectLandmark

  if (!is.null(pro_file())) {
    CompoundVariable <- input$addProjectCompoundVariable
    if (CompoundVariable == "") {
      addProjectCompoundVariableWarning("Please pick a selection.")
      check <- FALSE
    } else {
      addProjectCompoundVariableWarning("")
    }

    ExposureVariables <- input$addProjectExposureVariables
    if (length(ExposureVariables) == 0) {
      addProjectExposureVariablesWarning("Please pick a selection.")
      check <- FALSE
    } else {
      addProjectExposureVariablesWarning("")
    }

    ExposurePhenotypes <- input$addProjectExposurePhenotypes
    if (length(ExposurePhenotypes) == 0) {
      addProjectExposurePhenotypesWarning("Please pick a selection.")
      check <- FALSE
    } else {
      if (any(sapply(input[paste0("metavar_", ExposurePhenotypes)], is.null))) {
        check <- FALSE
      }
      addProjectExposurePhenotypesWarning("")
    }
  }

  new_project$GS_Collection <- ifelse(
    input$addProjectUseExistingGS,
    "Default", trimws(input$addProjectEnrichmentGS)
  )
  if (new_project$GS_Collection == "") {
    addProjectEnrichmentGSWarning("Please enter a valid name.")
    check <- FALSE
  } else {
    addProjectEnrichmentGSWarning("")
  }

  if (input$addProjectUseExistingGS) {
    new_project$GS_Collection_Links <- c(
      "https://www.gsea-msigdb.org/gsea/msigdb", "https://signalingpathways.org"
    )
  } else {
    new_project$GS_Collection_Links <- unlist(
      lapply(strsplit(input$addProjectEnrichmentLink, ";"), trimws)
    )
  }
  if (length(new_project$GS_Collection_Links) == 0) {
    addProjectEnrichmentLinkWarning("Please enter a valid link.")
    check <- FALSE
  } else {
    addProjectEnrichmentLinkWarning("")
  }

  # Check that all files have been imported
  check <- check && !(
    is.null(intro_file()) || is.null(pro_file()) ||
    is.null(chem_file()) || is.null(ge_file())
  )
  # If CMap is included, check that PCL and perturbagen files have been imported
  check <- check && !(
    input$addProjectIncludeCMap &&
    (is.null(CMapPCL()) || is.null(CMapPert()))
  )
  # If 'Use existing gene set' is set to 'No',
  # check that a gene set collection file has been imported
  check <- check && !(
    !input$addProjectUseExistingGS && is.null(gs_collection_file())
  )
  # If any of the fields have not been completed, print an error message
  if (!check) {
    addProjectInputWarning("Please fill in the required (*) fields.")
    return(NULL)
  }
  # Check whether the project and/or portal already exist,
  # and if so, throw a warning
  project_exists <-
    new_project$Project %in% sapply(projectlist, slot, "Project")
  portal_exists <-
    new_project$Portal %in% sapply(projectlist, slot, "Portal")
  if (project_exists || portal_exists) {
    if (project_exists) {
      addProjectInputWarning(
        "This project name already exists. Please enter another project name."
      )
    }
    if (portal_exists) {
      addProjectInputWarning(
        "This portal name already exists. Please enter another portal name."
      )
    }
    if (project_exists && portal_exists) {
      addProjectInputWarning(
        paste(
          "Both project and portal name already exist.",
          "Please enter another name for project and portal."
        )
      )
    }
    return(NULL)
  }

  shinyjs::disable(id="addProjectAddButton")
  shinyjs::disable(id="addProjectCancelButton")
  # Clear warning messages
  addProjectBasicInfoWarning("")
  addProjectInputWarning("")
  # Get chemical and profile annotation
  chem_ann <- chem_file()
  pro_ann <- pro_file()
  # Get gene expression
  if (is(ge_file(), "ExpressionSet")) {
    gene_expression <- exprs(ge_file())
  } else if (is(ge_file(), "data.frame")) {
    gene_expression <- as.matrix(ge_file())
  }
  # Get variable annotation
  Add_TAS <- isTRUE(input$addProjectTAS)
  Add_Modzscores <- isTRUE(input$addProjectModzscores)
  # Get connectivity map
  if (input$addProjectIncludeCMap) {
    CMap <- list(PCL=CMapPCL(), Pert=CMapPert())
  }
  if (input$addProjectUseExistingGS) {
    # If 'Use existing gene set' set to 'Yes', read gene sets from .gmt files
    gsscores_hallmark <- GSEABase::getGmt(
      file.path(
        "data/Enrichment Gene Set", paste0("h.all.v", MSigDB_version, ".gmt")
      )
    )
    gsscores_c2_reactome <- GSEABase::getGmt(
      file.path(
        "data/Enrichment Gene Set",
        paste0("c2.cp.reactome.v", MSigDB_version, ".gmt")
      )
    )
    gsscores_nursa <- GSEABase::getGmt(
      "data/Enrichment Gene Set/nursa_consensome_Cbyfdrvalue_0.01.gmt"
    )
  } else {
    # Otherwise, save the new gene set file
    gs_collection <- gs_collection_file()[["data"]]
    file.copy(
      from=gs_collection_file()[["path"]],
      to=file.path(
        "data/Enrichment Gene Set", paste0(new_project$GS_Collection, ".gmt")
      ),
      overwrite=TRUE
    )
  }

  # Create new progress bar
  progress <- AsyncProgress$new(message = "Creating new portal:", value=0)

  fut <- future({
    # Shiny packages
    require(K2Taxonomer)
    require(visNetwork)
    require(Biobase)
    require(BiocGenerics)

    # Initialize list to hold values for PortalDataset entity
    portalDataset <- list(
      timestamp = format(Sys.Date(), "%a %b %d %X %Z %Y")
    )

    # INTRODUCTION PAGE ######################################################

    progress$inc(1/10, detail = "Saving introduction page")
    print("Saving introduction page")
    # Save introduction page
    new_project$Introduction <- GeneHive::objectId(
      GeneHive::addWorkFile(intro_file())
    )

    # Chemical And Profile Annotation ########################################

    progress$inc(2/10, detail = "Saving chemical and profile annotation")

    # Get the unique id by chem for profile annotation
    pro_ann$unique_ID_by_chem <- unlist(
      lapply(
        1:nrow(pro_ann),
        function (i) paste(unlist(pro_ann[i, ExposureVariables]), collapse="_")
      )
    )

    # Calculate TAS values and moderated z-scores
    if (Add_TAS || Add_Modzscores) {
      calc_results <- calculate_TAS_Modzscores(
        pro_ann=pro_ann, chem_ann=chem_ann, gene_expression=gene_expression
      )
      if (Add_TAS) {
        chem_ann$TAS <- with(calc_results$TAS,
          TAS[match(chem_ann$Chemical_Id, Chemical_Id)]
        )
        pro_ann$TAS <- with(calc_results$TAS,
          TAS[match(pro_ann$Chemical_Id, Chemical_Id)]
        )
      }
      if (Add_Modzscores) {
        gene_expression <- calc_results$Modzscores
      }
    }
    # Upload profile and chemical annotation to WorkFiles,
    # and add WorkFile IDs to list of values for portal dataset list
    portalDataset$ProfileAnnotationRDS <- GeneHive::objectId(
      GeneHive::storeObjectAsWorkFile(pro_ann)
    )
    portalDataset$ChemicalAnnotationRDS <- GeneHive::objectId(
      GeneHive::storeObjectAsWorkFile(chem_ann)
    )
    print("Saving chemical and profile annotation")

    # Gene Expression ########################################################

    progress$inc(3/10, detail = "Saving ExpressionSet object")

    orig_expressionSet <- ExpressionSet(assayData = gene_expression)
    fData(orig_expressionSet)$Gene <- featureNames(orig_expressionSet)
    var <- ifelse(
      all(sampleNames(orig_expressionSet) %in% pro_ann$Sig_Id),
      "Sig_Id", "Chemical_Id"
    )
    pData(orig_expressionSet) <- pro_ann[
      match(sampleNames(orig_expressionSet), pro_ann[[var]]), var, drop=FALSE
    ]
    sampleNames(orig_expressionSet) <- pData(orig_expressionSet)[[var]]

    expressionSet <- ExpressionSet(assayData = gene_expression)
    pData(expressionSet) <- pData(orig_expressionSet)
    fData(expressionSet) <- fData(orig_expressionSet)
    if (new_project$Landmark_Gene) {
      landmark <- readRDS("data/Landmark/landmark_gene.RDS")
      fData(expressionSet) <- merge(fData(expressionSet), landmark, by="Gene")
    }
    # Add data to portal dataset list
    print("Saving gene expression file")
    portalDataset$GeneExpressionRDS <- GeneHive::objectId(
      GeneHive::storeObjectAsWorkFile(expressionSet)
    )

    # Create Morpheus heatmap
    eset <- expressionSet
    pData(eset) <- if (var == "Sig_Id") pro_ann else chem_ann
    if (!is.null(fData(eset)[["Landmark_Gene"]])) {
      genesetname <- "landmark_gene"
      eset <- eset[fData(eset)[["Landmark_Gene"]] == "Yes", ]
    } else {
      genesetname <- "gene_expression"
    }
    html_filename <- file.path(
      "www", "JSON", Portal, paste0(genesetname, ".html")
    )
    print("Saving gene expression Morpheus heatmap")
    createMorpheusHTML(eset, genesetname, filename=html_filename)

    # Connectivity Map #######################################################

    progress$inc(4/10, detail = "Saving connectivity map")
    if (input$addProjectIncludeCMap) {
      for (i in seq_along(CMap)) {
        # If the object is not an ExpressionSet, coerce it to one
        if (!is(CMap[[i]], "ExpressionSet")) {
          CMap[[i]] <- ExpressionSet(assayData = as.matrix(CMap[[i]]))
        }
        fData(CMap[[i]]) <- data.frame(
          Connectivity_Id = featureNames(CMap[[i]]),
          stringsAsFactors = FALSE
        )
        var <- ifelse(
          all(sampleNames(CMap[[i]]) %in% pro_ann$Sig_Id),
          "Sig_Id", "Chemical_Id"
        )
        pData(CMap[[i]]) <- pro_ann[
          match(sampleNames(CMap[[i]]), pro_ann[[var]]), var, drop=FALSE
        ]
        sampleNames(CMap[[i]]) <- pData(CMap[[i]])[[var]]
        # Create Morpheus heatmap
        eset <- CMap[[i]]
        pData(eset) <- if (var == "Sig_Id") pro_ann else chem_ann
        genesetname <- paste(tolower(names(CMap)[i]), "connectivity", sep="_")
        html_filename <- file.path(
          "www", "JSON", Portal, paste0(genesetname, ".html")
        )
        print("Saving connectivity map Morpheus heatmap")
        createMorpheusHTML(eset, genesetname, filename=html_filename)
      }
      print("Saving connectivity map file")
      # Add list of ExpressionSets to portal dataset list
      portalDataset$ConnectivityRDS <- GeneHive::objectId(
        GeneHive::storeObjectAsWorkFile(CMap)
      )
    }

    # Gene Set Enrichment Analysis ###########################################

    progress$inc(5/10, detail = "Saving gene set enrichment results")
    # Create gene set enrichment
    gsscores <- list()
    gsvamethods <- input$addProjectssGSEAMethod
    if (!input$addProjectUseExistingGS) {
      genesetcollection <- c("gs_collection"=new_project$GS_Collection)
    } else {
      # Run gene set enrichment analysis for hallmark, C2, and NURSA
      genesetcollection <- c(
        "gsscores_hallmark" = paste0("h.all.v", MSigDB_version),
        "gsscores_c2_reactome" = paste0("c2.cp.reactome.v", MSigDB_version),
        "gsscores_nursa" = "nursa_consensome_Cbyfdrvalue_0.01"
      )
    }
    # Getting differential expression
    for (geneset in names(genesetcollection)) {
      for (gsvamethod in gsvamethods) {
        # Run the analysis
        gsva_es <- gsva(
          expr=exprs(expressionSet), gset.idx.list=geneset,
          method=gsvamethod, mx.diff=TRUE
        )
        eset <- ExpressionSet(assayData = gsva_es)
        fData(eset)$Geneset <- featureNames(eset)
        var <- ifelse(
          all(sampleNames(eset) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id"
        )
        pData(eset) <- pro_ann[
          match(sampleNames(eset), pro_ann[[var]]), var, drop=FALSE
        ]
        sampleNames(eset) <- pData(eset)[[var]]

        # Store the expression in a list
        genesetname <- paste(
          "gsscores", genesetcollection[geneset], gsvamethod, sep="_"
        )
        gsscores[[genesetname]] <- eset
        # Create Morpheus heatmap
        pData(eset) <- if (var == "Sig_Id") pro_ann else chem_ann
        html_filename <- file.path(
          "www", "JSON", Portal, paste0(genesetname, ".html")
        )
        print("Saving gene set enrichment Morpheus heatmap")
        createMorpheusHTML(eset, genesetname, filename=html_filename)
      }
    }
    print("Saving gene set enrichment file")
    # Add data to portal dataset list
    portalDataset$GeneSetEnrichmentRDS <- GeneHive::objectId(
      GeneHive::storeObjectAsWorkFile(gsscores)
    )

    # K2Taxonomer Analysis ###################################################

    progress$inc(6/10, detail = "Saving K2Taxonomer results")
    if (!is.null(cohorts())) {
      # Process K2Taxonomer input variables
      AddConnectivityVars <- isTRUE(input$addProjectAddConnectivityVars)
      ConnectivityTest <- ifelse(
        AddConnectivityVars, input$addProjectConnectivityTest, ""
      )
      ExposurePhenotypeTests <- unlist(
        input[paste0("metavar_", ExposurePhenotypes)]
      )
      # Create the info class vector to run the metavariable test
      infoClassVector <- NULL
      # Create an ExpressionSet
      eSet <- orig_expressionSet
      for (i in seq_along(ExposurePhenotypes)) {
        test <- meta_variable_test$method[
          which(
            meta_variable_test$statistical_test %in% ExposurePhenotypeTests[i]
          )
        ]
        if (
          test == "factor1" &&
          all(toupper(c("No", "Yes")) %in% toupper(pData(eSet)[[ExposurePhenotypes[i]]]))
        ) {
          pData(eSet)[[ExposurePhenotypes[i]]] <- factor(
            pData(eSet)[[ExposurePhenotypes[i]]], levels = c("No", "Yes")
          )
        }
        infoClassVector <- c(infoClassVector, test)
      }
      names(infoClassVector) <- ExposurePhenotypes
      # Add connectivity test
      if (AddConnectivityVars) {
        pData <- cbind(
          pData(eSet),
          t(exprs(CMap$PCL))[sampleNames(eSet),],
          t(exprs(CMap$Pert))[sampleNames(eSet),]
        )
        infoClassVector <- c(
          infoClassVector,
          rep(ConnectivityTest, nrow(CMap$PCL)),
          rep(ConnectivityTest, nrow(CMap$Pert))
        )
        names(infoClassVector) <- c(
          ExposurePhenotypes,
          featureNames(CMap$PCL), featureNames(CMap$Pert)
        )
      }
      # Hyperenrichment analysis
      if (input$addProjectUseExistingGS) {
        hallmark_genelist <- lapply(gsscores_hallmark, slot, "geneIds")
        names(hallmark_genelist) <- names(gsscores_hallmark)
        c2_reactome_genelist <- lapply(gsscores_c2_reactome, slot, "geneIds")
        names(c2_reactome_genelist) <- names(gsscores_c2_reactome)
        nursa_genelist <- lapply(gsscores_nursa, slot, "geneIds")
        names(nursa_genelist) <- names(gsscores_nursa)
        # Combine gene set enrichment for hallmark, C2, and NURSA
        geneSetList <- do.call(
          c, list(hallmark_genelist, c2_reactome_genelist, nursa_genelist)
        )
      } else {
        collection_genelist <- lapply(gs_collection, slot, "geneIds")
        names(collection_genelist) <- names(gs_collection)
        geneSetList <- collection_genelist
      }
      RNGkind("L'Ecuyer-CMRG")
      set.seed(12345678)
      K2res <- runK2Taxonomer(
        eSet        = eSet,
        cohorts     = cohorts(),
        featMetric  = input$addProjectFeatureFiltering,
        infoClass   = infoClassVector,
        genesets    = geneSetList,
        ssGSEAalg   = input$addProjectssGSEAMethod,
        ssGSEAcores = 1,
        stabThresh  = 0.67
      )
      # Add K2Taxonomer results to portal dataset list
      portalDataset$K2TaxonomerResultsRDS <- GeneHive::objectId(
        GeneHive::storeObjectAsWorkFile(K2res)
      )
    }
    print("Saving K2Taxonomer results")
  })

  # Show notification on error or user interrupt
  fut <- catch(fut, function(e) print(e$message))

  # When done with analysis, remove progress bar
  fut <- finally(fut, function() {
    progress$close()
    # Add chemical interactions and exposures to project list
    new_project <- c(
      new_project,
      list(
        TAS                      = Add_TAS,
        Modzscores               = Add_Modzscores,
        Exposure_Levels          = ExposureVariables,
        Exposure_Phenotypes      = ExposurePhenotypes,
        Exposure_Phenotype_Tests = ExposurePhenotypeTests,
        Connectivity_Test        = ConnectivityTest,
        Feature_Filtering        = input$addProjectFeatureFiltering
      )
    )
    do.call(addEntity, args=c(.class="Project", new_project))
    do.call(addEntity, args=c(.class="PortalDataset", portalDataset))
    project_table_message(
      paste("Portal", sQuote(Portal), "has been added.")
    )
    removeModal()
    shinyjs::enable(id="addProjectAddButton")
    shinyjs::enable(id="addProjectCancelButton")
  })
  print("Generating new portal....")
})
# If there is a warning message computed from all inputs, print it in red
output$addProjectInputWarningHTML <- renderUI({
  req(addProjectInputWarning())
  p(style="color:red; text-align:center;", HTML(addProjectInputWarning()))
})

# When "Cancel" button is clicked, clear variables and remove modal dialog
observeEvent(input$addProjectCancelButton, {
  # Reset file input
  session$sendCustomMessage("ResetFileInput", "addProjectIntroFile")
  session$sendCustomMessage("ResetFileInput", "addProjectProfileAnnotationFile")
  session$sendCustomMessage("ResetFileInput", "addProjectGeneExpressionFile")
  session$sendCustomMessage("ResetFileInput", "addProjectCMapPCLFile")
  session$sendCustomMessage("ResetFileInput", "addProjectCMapPertFile")
  session$sendCustomMessage("ResetFileInput", "addProjectGSCollectionFile")
  # Clear error messages
  addProjectBasicInfoWarning(NULL)
  addProjectIntroFileWarning(NULL)
  addProjectProfileAnnotationFileWarning(NULL)
  addProjectGeneExpressionFileWarning(NULL)
  addProjectCMapPCLFileWarning(NULL)
  addProjectCMapPertFileWarning(NULL)
  addProjectCompoundVariableWarning(NULL)
  addProjectExposureVariablesWarning(NULL)
  addProjectEnrichmentGSWarning(NULL)
  addProjectEnrichmentLinkWarning(NULL)
  addProjectGSCollectionFileWarning(NULL)
  addProjectExposurePhenotypesWarning(NULL)
  addProjectInputWarning(NULL)
  # Remove data messages
  portal(NULL)
  cohorts(NULL)
  project_table_message(NULL)
  # Remove data
  intro_file(NULL)
  pro_file(NULL)
  chem_file(NULL)
  ge_file(NULL)
  CMapPCL(NULL)
  CMapPert(NULL)
  gs_collection_file(NULL)
  # Close modal log
  removeModal()
})
