library(BiocManager)
library(data.table)
library(ggdendro)
library(password)
library(sendmailR)
library(tidyverse)
library(magrittr)
library(ipc)
library(GeneHive)
library(K2Taxonomer)
library(visNetwork)
library(Biobase)
library(GSVA)
library(GSEABase)
library(limma)
library(dendextend)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(plotly)
library(heatmaply)
library(RColorBrewer)
library(promises)
library(future)
plan(multisession)

# Shiny options
options(repos = BiocManager::repositories())
options(shiny.maxRequestSize=1000*1024^2)

# Define ui logic
ui <- bootstrapPage(
  title=paste0("The Xposome Project"),
  tagList(
    tags$head(
      ###<!-- meta -->####
      tags$meta(charset="UTF-8"),
      tags$meta(
        name="viewport", content="width=device-width, initial-scale=1.0"
      ),
      ###<!-- favicon -->####
      tags$link(href="IMAGES/bu_logo.png", rel="shortcut icon"),
      ####<!-- css style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/LogInStyle.css"),
      tags$link(
        type="text/css", rel="stylesheet", href="CSS/ModeratorStyle.css"
      ),
      ####<!-- overall style -->####
      tags$link(type="text/css", rel="stylesheet", href="CSS/style.css"),
      ####<!-- javascript -->####
      shinyjs::useShinyjs(),
      tags$script(type="text/javascript", src="JS/google-analytics.js"),
      tags$script(type="text/javascript", src="JS/Javascript.js")
    )
  ),
  ## Create an active clock to prevent app from greying out ####
  div(style = "position: absolute; top: -100px;",
      textOutput("clock")
  ),
  ####<!-- start project page -->####
  div(
    class="project-page",
    # Header
    fluidRow(
      class="header-container",
      column(
        width=8,
        div(
          class="text-md-left",
          a(href="?home", h2("The Xposome Project")),
          h4(
            style="font-weight: 200;",
            "Chemical Screening using high-throughput transcriptomics assays"
          )
        )
      ),
      column(
        width=4,
        div(
          class="text-md-right",
          a(
            onclick="curlinkFun('home')", href="?home", id="home",
            class="site-link", "Home"
          ),
          a(
            onclick="curlinkFun('about')", href="?about", id="about",
            class="site-link",  "About"
            ),
          a(
            onclick="curlinkFun('contact')", href="?contact", id="contact",
            class="site-link", "Contact"
          ),
          a(
            onclick="curlinkFun('sign_in')", href="?sign_in", id="sign_in",
            class="site-link", "Sign In"
          )
        )
      )
    ),
    # Main body
    fluidRow(
      class="body-container", style="padding: 0 0 0 0; margin: 0 0 0 0;",
      column(style="padding: 0 0 0 0; margin: 0 0 0 0;",
        width=12,
        uiOutput("pageStub") %>% withSpinner(type=4, color="#0dc5c1")
      )
    ),
    # Footer/copyright
    fluidRow(
      class="copyright-container",
      column(
        width=12,
        div(
          class="text-md-center",
          p("The Xposome Project: Developed by Monti Lab at Boston University"),
          HTML(
            paste0(
              "&copy; Monti Lab &diams; ", format(Sys.Date(), "%Y"),
              " &diams; All Rights Reserved."
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # this prints when a session starts
  cat("Session started.\n")
  # this prints when a session ends
  onSessionEnded(function() { cat("Session ended.\n\n")  })
  # Create a queue object
  queue <- shinyQueue()
  # Keep track of who logs in and output error message
  UserLog <- reactiveValues(Logged=FALSE)
  # Execute signals every 100 milliseconds
  queue$consumer$start(100)
  # To signal STOP to the future
  interruptor <- AsyncInterruptor$new()
  # Update the clock every 5s to prevent app from being inactive and grey out
  output$clock <- renderText({
    invalidateLater(5000)
    Sys.time()
  })
  # UI object files ####
  source("ui_input.R", local=TRUE)
  source("logout.R", local=TRUE)
  # Preprocessing data #####
  source("carcinogenome_startup.R", local=TRUE)
  # Read in helper functions .signatureWrapper() and hyperenrichmentClusters()
  source(
    system.file("dashboard", "DashHelper.R", package="K2Taxonomer"), local=TRUE
  )
  source("taxonomer_startup.R", local=TRUE)
  # Morpheus heatmap ####
  source("morpheus_heatmap.R", local=TRUE)
  # TAS, Modzscores calculation ####
  source("tas_modzscores_calculation.R", local=TRUE)

  # Load server code for page specified in URL
  fname <- isolate(session$clientData$url_search)
  # blank means home page
  if (nchar(fname) == 0 || length(fname) == 0) fname <- "?home"
  # remove leading "?"
  fname <- sub("^\\?", "", fname)
  ## Get the R files for the selected page ####
  if (fname %in% c("home", "about", "contact", "empty")) {
    # Load and run server code for this page
    source(paste0(fname, ".R"), local=TRUE)
  } else if (fname == "sign_in") {
    # Load and run server code for this page
    source("AddProject.R", local=TRUE)
    source("EditProject.R", local=TRUE)
    source("RemoveProject.R", local=TRUE)
    source("SaveProject.R", local=TRUE)
    source("AddUser.R", local=TRUE)
    source("EditUser.R", local=TRUE)
    source("RemoveUser.R", local=TRUE)
    source("SaveUser.R", local=TRUE)
    source("ChangePwd.R", local=TRUE)
    source("ForgotPassword.R", local=TRUE)
    source("sendTemporaryPassword.R", local=TRUE)
    source("sign_in.R", local=TRUE)
    source("add_project_import_files.R", local=TRUE)
    source("add_project.R", local=TRUE)
    source("edit_project_import_files.R", local=TRUE)
    source("edit_project.R", local=TRUE)
  } else {
    # Retrieve list of all PortalDataset entities in hive
    datasets <- listEntities("PortalDataset", portal=fname)
    # Sort by timestamp and extract most recent dataset to convenience object
    datasets <- datasets[order(sapply(datasets, slot, "timestamp"))]
    dataset <- datasets[[length(datasets)]]
    # Read in the profile data
    profile_dat <- reactive(
      future(getWorkFileAsObject(hiveWorkFileID(dataset@ProfileAnnotationRDS)))
    )
    # Read in the chemical data
    chemical_dat <- reactive(
      future(getWorkFileAsObject(hiveWorkFileID(dataset@ChemicalAnnotationRDS)))
    )
    # Read in the expression data
    expression_dat <- reactive(
      future(getWorkFileAsObject(hiveWorkFileID(dataset@GeneExpressionRDS)))
    )
    # Read in the connectivity data
    connectivity_dat <- reactive(
      future(getWorkFileAsObject(hiveWorkFileID(dataset@ConnectivityRDS)))
    )
    # Read in the gs enrichment data
    gs_enrichment_dat <- reactive(
      future(getWorkFileAsObject(hiveWorkFileID(dataset@GeneSetEnrichmentRDS)))
    )
    # Read in K2 Taxonomer data
    taxonomer_results <- reactive({
      future({
        require(K2Taxonomer)
        require(visNetwork)
        require(Biobase)
        require(BiocGenerics)
        # Read in K2 Taxonomer data
        K2summary <- getWorkFileAsObject(
          hiveWorkFileID(dataset@K2TaxonomerResultsRDS)
        )
        # Parse results
        info <- K2info(K2summary)  # Profile information
        infoMat <- as.matrix(info) # Format information
        meta <- K2meta(K2summary) # Get meta data
        # Create variable options
        varOptions <- sort(colnames(info))
        names(varOptions) <- varOptions
        if (!is.null(meta$cohorts)) {
          varOptions <- varOptions[varOptions != meta$cohorts]
        } else {
          varOptions <- varOptions[varOptions != "sampleID"]
        }
        K2res <- K2results(K2summary) # Format K2 results
        # Get IDs of each group
        obsMap <- sapply(K2res, "[[", "obs")
        dataMatrix <- K2data(K2summary) # Format dataMatrix
        genesets <- K2genesets(K2summary) # Get geneset lists
        gene2Pathway <- K2gene2Pathway(K2summary) # Get gene2pathway matching
        eSet <- K2eSet(K2summary) # Get expression set
        gSet <- K2gSet(K2summary) # Get gene set projection expression set
        # Create static dendrogram
        K2dendrogram <- K2dendro(K2summary)
        ## Get sample order
        labs <- get_leaves_attr(K2dendrogram, "label")
        # Create interactive dendrogram
        vNetOut <- K2visNetwork(K2summary)
        # If too many observations in terminal labels, unlabel them
        observations <- lengths(
          regmatches(
            vNetOut$x$nodes$label, gregexpr("\n", vNetOut$x$nodes$label)
          )
        )
        if (max(observations) > 20) {
          # Fix font size
          vNetOut$x$nodes$font.size <- 25
          vNetOut$x$nodes$font.size[vNetOut$x$nodes$shape == "box"] <- 0
          # Change shape
          vNetOut$x$nodes$shape[vNetOut$x$nodes$shape == "box"] <- "square"
        }
        # Get the mod test table
        if (!is.null(K2res[[1]]$modTests)) {
          # Format table
          K2modTestList <- lapply(K2res, function(x) {
            modTests <- x$modTests
            names(modTests) <- c("1", "2")
            do.call(rbind, modTests)
          })
          names(K2modTestList) <- names(K2res)
          K2modTestFram <-
            do.call(rbind, K2modTestList)[,c("value", "pval", "fdr")]
          # Get parent node
          K2modTestFram$Parent <- regmatches(
            rownames(K2modTestFram), regexpr("^[^.]+", rownames(K2modTestFram))
          )
          # Get direction to child
          K2modTestFram$Direction <- gsub(
            "[[:alpha:]]+[.]|[.][[:digit:]]+$", "", rownames(K2modTestFram)
          )
          # Get child
          K2modTestFram$Child <- apply(
            K2modTestFram[, c("Parent", "Direction")], 1,
            function(x) {
              vSub <- vNetOut$x$edges[vNetOut$x$edges$from == x[1],]
              vSub$to[as.numeric(x[2])]
            }
          )
          # Get split
          K2modTestFram$Split <- paste0(
            K2modTestFram$Parent, K2modTestFram$Direction
          )
          # Format p-values
          K2modTestFram <- K2modTestFram[!is.na(K2modTestFram$pval),]
          K2modTestFram <- K2modTestFram[order(K2modTestFram$pval),]
          # Get unrenamed variables of mod test for qvalues formatting
          mvTabSub <- K2modTestFram
          K2modTestFram <-
            K2modTestFram[, c("Split", "Child", "value", "pval", "fdr")]
          colnames(K2modTestFram) <- c(
            "Split", "Node", "Variable", "P Value", "Q Value"
          )
          K2modTestFram$`P Value` <- signif (K2modTestFram$`P Value`, 2)
          K2modTestFram$`Q Value` <- signif (K2modTestFram$`Q Value`, 2)
          # Color breaks for q values
          breaks <- c(1, 0.25, 0.1, 0.05, 0.01, 0.001, 0)
          breakColors <- brewer.pal(7, "Greens")
          mvTabSub$color <- sapply(mvTabSub$pval, function(pval) {
            breakColors[which.min(breaks >= pval)]
          })
          # Size breaks
          breaks <- c(1, 0.1, 0.05, 0.01, 0.001, 0.0001, 0)
          breakSize <- seq(length(breaks)) * 7
          mvTabSub$width <- sapply(mvTabSub$pval, function(pval) {
            breakSize[which.min(breaks >= pval)]
          })
          # Add 2 values
          vNetOut_qvalues <- vNetOut
          # Change width of edges
          mEdge <- mvTabSub[, c("Parent", "Child", "width")][
            !duplicated(mvTabSub[, c("Parent", "Child")]),
          ]
          colnames(mEdge) <- c("from", "to", "width")
          edgeFram <- merge(
            vNetOut_qvalues$x$edges, mEdge, all.x = TRUE, sort = FALSE
          )
          edgeFram$width[is.na(edgeFram$width)] <- 1
          edgeFram$color.inherit <- 'to'
          vNetOut_qvalues$x$edges <- edgeFram
          # Change color of edges
          mNode <- mvTabSub[
            !duplicated(mvTabSub[, c("Child")]), c("Child", "color")
          ]
          colnames(mNode) <- c("id", "color.border")
          nodeFram <- left_join(vNetOut_qvalues$x$nodes, mNode)
          nodeFram$color.border[is.na(nodeFram$color.border)] <-
            brewer.pal(6, "Greens")[1]
          nodeFram$color.background <- nodeFram$color.border
          nodeFram$color.highlight <- 'red'
          vNetOut_qvalues$x$nodes <- nodeFram
        } else {
          K2modTestFram <- NULL
        }
        # Get differential gene expression results
        dgeTable <- getDGETable(K2summary)
        ## Get the gene link
        dgeTable$Plot <- paste0(
          "<label for='PlotRow", seq(nrow(dgeTable)), "'>&#128202;</label>"
        )
        dgeTable$Link <- sapply(as.character(dgeTable$gene), get_dgeTable_link)
        # Reorder columns
        dgeTable <- dgeTable[
          c(
            "gene", "split", "mod", "direction", "pval", "fdr", "coef", "mean",
            "Plot", "Link"
          )
        ]
        # Format numbers to fit in table
        for (i in c("pval", "fdr")) {
          if (i %in% colnames(dgeTable)) {
            dgeTable[,i] <- signif (dgeTable[,i], digits = 2)
          }
        }
        # Format numbers to fit in table
        for (i in c("coef", "mean")) {
          if (i %in% colnames(dgeTable)) {
            dgeTable[,i] <- round(dgeTable[,i], digits = 2)
          }
        }
        # Rename columns
        colnames(dgeTable) <- c(
          "Gene", "Node", "Group", "Direction", "P Value", "FDR", "Diff",
          "Mean", "Plot", "Link"
        )
        # Get gene set enrichment results
        enrTable <- getEnrichmentTable(K2summary)
        # Remove unnecessary columns
        enrTable <- enrTable[, !colnames(enrTable) %in% c("B", "ntot", "t")]
        # Add links to gene sets
        enrTable$Plot <- paste0(
          "<label for='PlotRow", seq(nrow(enrTable)), "'>&#128202;</label>"
        )
        enrTable$Link <- sapply(
          as.character(enrTable$category), get_enrTablelink
        )
        # Format numbers to fit in table
        for (i in c("pval_hyper", "fdr_hyper", "pval_limma", "fdr_limma")) {
          if (i %in% colnames(enrTable)) {
            enrTable[,i] <- signif (enrTable[,i], digits = 2)
          }
        }
        # Format numbers to fit in table
        for (i in c("coef", "mean")) {
          if (i %in% colnames(enrTable)) {
            enrTable[,i] <- round(enrTable[,i], digits = 2)
          }
        }
        colnames(enrTable) <- c(
          "Gene Set", "Node", "Group", "Direction",
          "P Value_Hyper", "FDR_Hyper",
          "N_Overlap", "N_Sig. Genes", "N_Gene Set",
          "P Value_ssGSEA", "FDR_ssGSEA", "Diff_ssGSEA", "Mean_ssGSEA",
          "Hits", "Plot", "Link"
        )
        # return a list of objects
        results <- list(
          info=info,
          infoMat=infoMat,
          meta=meta,
          K2res=K2res,
          dataMatrix=dataMatrix,
          genesets=genesets,
          gene2Pathway=gene2Pathway,
          eSet=eSet,
          gSet=gSet,
          dgeTable=dgeTable,
          enrTable=enrTable,
          K2dendrogram=K2dendrogram,
          vNetOut=vNetOut,
          vNetOut_qvalues=vNetOut_qvalues,
          K2modTestFram=K2modTestFram,
          options=list(
            varOptions=varOptions,
            labs=labs,
            obsMap=obsMap
          )
        )
        return(results)
      }) %...!% { return(NULL) }
    })

    # Create reactive values
    annot_var <- reactive(
      promise_all(pro_ann=profile_dat(), eset=expression_dat()) %...>%
      with(
        ifelse(all(colnames(eset) %in% pro_ann$Sig_Id), "Sig_Id", "Chemical_Id")
      )
    )

    # Get Project from GeneHive
    project <- listEntities("Project", Portal=fname)[[1]]
#    gs_collection <- project@GS_Collection
#    gs_collection_link <- project$GS_Collection_Link
    landmark <- project@Landmark_Gene
    exposure_phenotype_test <- project@Exposure_Phenotype_Tests
    exposure_phenotype <- intersect(
      project@Exposure_Phenotypes,
      c("1-sided Fisher test", "2-sided Fisher test")
    )
    # Get the helptext for different gene set enrichment
    if (project@GS_Collection == "Default") {
      # The default gs enrichment version
      MSigDB_version <- "7.0"
      # Get the gene set scores for diffrent gsva methods
      dsmap <- list(
        Hallmark=paste0("gsscores_h.all.v", MSigDB_version),
        C2=paste0("gsscores_c2.cp.reactome.v", MSigDB_version),
        NURSA=paste0("gsscores_nursa_consensome_Cbyfdrvalue_0.01")
      )
      # Get the helptext
      helptext_geneset <- paste0(
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">",
        "MSigDB Hallmark Pathways (v", MSigDB_version, ")</a><br>",
        "<a href=\"https://www.gsea-msigdb.org/gsea/msigdb\">",
        "MSigDB C2 reactome Pathways (v", MSigDB_version, ")</a><br>",
        "<a href=\"https://signalingpathways.org\">",
        "NURSA: Nuclear Receptor Signaling Atlas, "
        "consensome data for human</a><br>"
      )
    } else {
      # Get the gene set scores for diffrent GSVA methods
      dsmap <- paste0("gsscores_", project@GS_Collection)
      names(dsmap) <- paste0("gsscores_", project@GS_Collection)
      # Get the helptext
      helptext_geneset <- paste0(
        "<a href=\"", project@GS_Collection_Links, "\">", project@GS_Collection,
        "</a><br>"
      )
    }
    # Run the main app
    source("main_app.R", local=TRUE)
    # Server logic
    source("server_annotation.R", local=TRUE)
    source("server_chemical.R", local=TRUE)
    source("server_marker.R", local=TRUE)
    source("server_heatmap.R", local=TRUE)
    source("server_taxonomic_clustering.R", local=TRUE)
    source("server_compare_multiple.R", local=TRUE)
  }
}

shinyApp(ui=ui, server=server)
