AddProject <- function () {
  div(
    id = "addProjectData",
    modalDialog(
      size = "l", title = "Add Project", footer = NULL,
      fluidRow(
        column(
          width=4,
          textInput(
            "addProjectName", label=strong(redAsterisk, "Project name:")
          ),
        ),
        column(
          width=4,
          textInput(
            "addProjectCellLine", label=strong(redAsterisk, "Cell line:")
          )
        ),
        column(
          width=4,
          textInput(
            "addProjectPortal", label=strong(redAsterisk, "Portal name:")
          )
        )
      ),
      fluidRow(
        column(
          width=12,
          strong(redAsterisk, "Description:", style="text-align: left;"),
          HTML(
            "<textarea style='width: 100%; height: 150px; padding: 10px; margin-top: 5px;' id='addProjectDescription' placeholder='Write a short description about the project...'></textarea>"
          ),
          helpText(em("Note: you can use HTML tags inside the description box for styling and formatting text", style="font-size: 9pt;"))
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput(outputId = "addProjectBasicInfoWarningHTML")
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          shinyjs::hidden(
            div(
              id = "add-loading-content",
              div(class="loader"),
              h4("Processing...", id="loading_text")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          # Include L1000 landmark genes?
          radioButtonsWithTooltip(
            inputId="addProjectLandmark", label="L1000 Landmark Gene",
            bId="addProjectLandmarkHelp", helptext="some helptext here",
            choices=c("Yes"=TRUE, "No"=FALSE), selected=FALSE, inline=TRUE
          ),

          h4("Import Files:", style="padding-bottom: 10px;"),

          fileInput(
            inputId = "addProjectIntroFile",
            label = strong(
              redAsterisk, "Choose an introduction file ",
              downloadLink(
                outputId = "introductionTemplateRmd",
                label = em(
                  style="font-size: 11px", "introduction_template.Rmd"
                )
              )
            )
          ),
          uiOutput(outputId = "addProjectIntroFileWarningHTML"),
          br(),

          fileInput(
            inputId = "addProjectProfileAnnotationFile",
            label = strong(redAsterisk, "Choose a profile annotation file")
          ),
          uiOutput(outputId = "addProjectProfileAnnotationFileWarningHTML"),
          br(),

          fileInput(
            inputId = "addProjectGeneExpressionFile",
            label = strong(redAsterisk, "Choose a gene expression file")
          ),
          uiOutput(outputId = "addProjectGeneExpressionFileWarningHTML"),
          br(),

          radioButtons(
            inputId = "addProjectIncludeCMap",
            label = "Include connectivity map?",
            choices=c("Yes"=TRUE, "No"=FALSE), inline = TRUE
          ),
          br(),
          conditionalPanel(
            condition = "input.addProjectIncludeCMap",
            fileInput(
              inputId = "addProjectCMapPCLFile",
              label = strong(
                redAsterisk,
                "Choose a Connectivity Map file (Perturbagen Classes)"
              )
            ),
            uiOutput(outputId = "addProjectCMapPCLFileWarningHTML"),
            br(),
            fileInput(
              inputId = "addProjectCMapPertFile",
              label = strong(
                redAsterisk,
                "Choose a Connectivity Map file (Perturbagens)"
              )
            ),
            uiOutput(outputId = "addProjectCMapPertFileWarningHTML")
          )
        ),
        column(
          width=6,
          h4("Exposure Variables:", style="padding-bottom: 10px;"),

          selectInputWithTooltip(
            inputId = "addProjectCompoundVariable",
            label=strong(redAsterisk, "Select a compound variable:"),
            bId="addProjectCompoundVariableHelp",
            helptext="Some description here",
            choices=c("Import a profile annotation" = ""), multiple = FALSE
          ),
          uiOutput(outputId = "addProjectCompoundVariableWarningHTML"),

          selectInputWithTooltip(
            inputId = "addProjectExposureVariables",
            label=strong(
              redAsterisk, "Select a list of additional exposure variables:"
            ),
            bId="addProjectExposureVariablesHelp",
            helptext="Some description here",
            choices=c("Import a profile annotation" = ""), multiple = TRUE
          ),
          uiOutput(outputId = "addProjectExposureVariablesWarningHTML"),

          # Calculations
          uiOutput(outputId = "addProjectTASModz"),
          br(),

          h4("Gene Set Enrichment:", style="padding-bottom: 10px;"),
          radioButtonsWithTooltip(
            inputId = "addProjectUseExistingGS",
            label = "Use existing gene sets",
            bId = "addProjectUseExistingGSHelp",
            helptext = paste(
              "Gene sets include the MSigDB collections",
              "(Hallmark, C2 reactome pathways),",
              "and gene targets of various nuclear receptors (NURSA)"
            ),
            choices = c("Yes"=TRUE, "No"=FALSE)
          ),
          conditionalPanel(
            condition = "!input.addProjectUseExistingGS",
            textInput(
              inputId = "addProjectEnrichmentGS",
              label = strong(
                redAsterisk, "Enter a name for gene set collection:"
              )
            ),
            uiOutput(outputId = "addProjectEnrichmentGSWarningHTML"),
            textInput(
              inputId = "addProjectEnrichmentLink",
              label = strong(
                redAsterisk, "Provide a link to the gene set collection:"
              )
            ),
            uiOutput(outputId = "addProjectEnrichmentLinkWarningHTML"),
            fileInput(
              inputId = "addProjectGSCollectionFile",
              label = strong(
                redAsterisk, "Choose a Gene Set Collection",
                downloadLink(
                  outputId = "gs_collection_template_gmt",
                  label = em(style="font-size: 11px", "template.gmt")
                )
              )
            ),
            uiOutput(outputId = "addProjectGSCollectionFileWarningHTML")
          ),
          shinyjs::disabled(
            radioButtons(
              inputId="addProjectssGSEAMethod", label="Choose a ssGSEA method:",
              choices=c("gsva", "ssgsea", "zscore", "plage"), inline=TRUE
            )
          ),
          br(),

          h4("K2Taxonomer Analysis:", style="padding-bottom: 10px;"),

          selectInputWithTooltip(
            inputId = "addProjectExposurePhenotypes",
            label=strong(redAsterisk, "Select exposure phenotypes:"),
            bId="addProjectExposurePhenotypesHelp",
            helptext="Some description here",
            choices=c("Import a profile annotation" = ""), multiple = TRUE
          ),
          uiOutput(outputId = "addProjectExposurePhenotypesWarningHTML"),
          DT::dataTableOutput(outputId = "addProjectMetavarVariableTest"),
          br(),

          conditionalPanel(
            condition = "input.addProjectIncludeCMap",
            checkboxInput(
              inputId = "addProjectAddConnectivityVars",
              label = "Add Connectivity Variables", value = FALSE
            ),
            conditionalPanel(
              condition = "input.addProjectAddConnectivityVars",
              selectInput(
                inputId="addProjectConnectivityTest",
                label="Choose a statistical test:",
                choices=c(
                  "1-sided Wilcox RS test", "2-sided Wilcox RS test",
                  "1-sided t test", "2-sided t test"
                )
              )
            )
          ),
          # Additional parameters
          h4("Additional Parameters:"),
          selectInputWithTooltip(
            inputId="addProjectFeatureFiltering", label="Feature filtering:",
            bId="addProjectFeatureFilteringHelp",
            helptext="Some description here", choices=c("sd", "mad"),
            selected="sd"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("addProjectInputWarningHTML")
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(
            "addProjectAddButton", label=strong("Add"),
            class="mybuttons", width="70px"
          ),
          actionButton(
            "addProjectCancelButton", label=strong("Cancel"),
            class="mybuttons", width="70px"
          )
        )
      )
    )
  )
}
