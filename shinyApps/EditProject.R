EditProject <- function (project) {
  div(
    id = "editProjectData",
    modalDialog(
      size = "l", title = "Edit Project", footer = NULL,
      fluidRow(
        column(
          width=4,
          textInput(
            inputId = "Edit_Project_Name",
            label=strong(redAsterisk, "Project name:"),
            value=project@Project
          ),
        ),
        column(
          width=4,
          textInput(
            inputId = "Edit_Cell_Line_Name",
            label=strong(redAsterisk, "Cell line name:"),
            value=project@Cell_Line
          )
        ),
        column(
          width=4,
          textInput(
            inputId = "Edit_Portal_Name",
            label=strong(redAsterisk, "Portal name:"),
            value=project@Portal
          )
        )
      ),
      fluidRow(
        column(
          width=12,
          strong(redAsterisk, "Description:", style="text-align: left;"),
          HTML(
            paste0(
              "<textarea style='width: 100%; height: 150px; padding: 10px;",
              "margin-top: 5px;' id='Edit_Description' ",
              "placeholder='Write a short description about the project...'>",
              project@Description,
              "</textarea>"
            )
          ),
          helpText(
            em(
              "Note: you can use HTML tags inside the description box for styling and formatting text", style="font-size: 9pt;"
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(width=12, uiOutput("editprojectwarningmessage"))
      ),
      br(),
      fluidRow(
        column(
          width=12,
          shinyjs::hidden(
            div(
              id = "edit-loading-content",
              div(class="loader"),
              h4("Processing...", id="loading_text")
            )
          )
        )
      ),
      fluidRow(
        column(
          width=6,
          ## Include L1000 landmark genes
          radioButtonsWithTooltip(
            inputId="Edit_Landmark", label="L1000 Landmark Gene",
            bId="edit_landmark_tooltip", helptext="some helptext here",
            choices=c("Yes"=TRUE, "No"=FALSE),
            selected=isTRUE(project@Landmark_Gene), inline=TRUE
          ),
          ## Modify input files
          h4("Modify Input Files:", style="padding-bottom: 10px;"),
          radioButtons(
            inputId="edit_files", label=NULL,
            choices=c(
              "None", "All", "Introduction Page", "Profile Annotation",
              "Gene Expression", "Connectivity Map", "GS Enrichment",
              "K2Taxonomer"
            )
          ),
          br(),
          conditionalPanel(
            condition = "input.edit_files == 'Introduction Page' || input.edit_files == 'All'",
            fileInput(
              inputId="edit_intro_file",
              label=strong(
                redAsterisk, "Choose an introduction file ",
                downloadButton(
                  outputId="edit_intro_download_file",
                  label=em(style="font-size: 11px", "Introduction_page.Rmd")
                )
              )
            ),
            uiOutput(outputId='edit_intro_file_msg'),
            br()
          ),
          conditionalPanel(
            condition = "input.edit_files == 'Profile Annotation' || input.edit_files == 'All' || input.edit_files == 'Gene Expression' || input.edit_files == 'Connectivity Map'",
            conditionalPanel(
              condition = "input.edit_files == 'Gene Expression'",
              radioButtons(
                inputId="edit_ge_pro_option",
                label="Does profile annotation change?",
                choices=c("Yes"=TRUE, "No"=FALSE), selected=FALSE, inline=TRUE
              )
            ),
            conditionalPanel(
              condition = "input.edit_files == 'Connectivity Map'",
              radioButtons(
                inputId="edit_conn_pro_option",
                label="Does profile annotation change?",
                choices=c("Yes"=TRUE, "No"=FALSE), selected=FALSE, inline=TRUE
              )
            ),
            conditionalPanel(
              condition = "input.edit_files == 'Profile Annotation' || input.edit_files == 'All' || (input.edit_files == 'Gene Expression' && input.edit_ge_pro_option) || (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option)",
              fileInput(
                inputId="edit_pro_file",
                label=strong(
                  redAsterisk, "Choose a profile annotation file",
                  downloadButton(
                    outputId="edit_pro_download_file",
                    label=em(style="font-size: 11px", "Profile_annotation.RDS")
                  )
                )
              ),
              uiOutput(outputId='edit_pro_file_msg'),
              br(),
              conditionalPanel(
                condition = "input.edit_files == 'Profile Annotation'",
                radioButtons(
                  inputId="edit_pro_ge_option",
                  label="Does gene expression change?",
                  choices=c("Yes"=TRUE, "No"=FALSE), inline=TRUE
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.edit_files == 'Gene Expression' || input.edit_files == 'All' || (input.edit_files == 'Profile Annotation' && input.edit_pro_ge_option) || (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option)",
            fileInput(
              inputId = "edit_ge_file",
              label = strong(
                redAsterisk, "Choose a gene expression file",
                downloadButton(
                  outputId="edit_ge_download_file",
                  label=em(style="font-size: 11px", "Expression_Set.RDS")
                )
              )
            )
          ),
          uiOutput(outputId='edit_ge_file_msg'),
          br(),
          conditionalPanel(
            condition = "input.edit_files == 'All' || input.edit_files == 'Connectivity Map' || input.edit_files == 'Profile Annotation' || input.edit_files == 'Gene Expression'",
            conditionalPanel(
              condition = "input.edit_files == 'All' || input.edit_files == 'Profile Annotation' || input.edit_files == 'Gene Expression'",
              radioButtons(
                inputId = "edit_conn_option",
                label = "Does connectivity map change?",
                choices = c("Yes"=TRUE, "No"=FALSE), inline = TRUE
              ),
            ),
            conditionalPanel(
              condition = "(input.edit_files == 'All'  && input.edit_conn_option) || input.edit_files == 'Connectivity Map' || (input.edit_files == 'Profile Annotation' && input.edit_conn_option) || (input.edit_files == 'Gene Expression' && input.edit_conn_option)",
              fileInput(
                inputId = "edit_conn_pcl_file",
                label = strong(
                  redAsterisk,
                  "Choose a connectivity map file (Perturbagen Classes)"
                )
              )
            ),
            uiOutput(outputId='edit_conn_pcl_file_msg'),
            br(),
            conditionalPanel(
              condition = "(input.edit_files == 'All'  && input.edit_conn_option) || input.edit_files == 'Connectivity Map' || (input.edit_files == 'Profile Annotation' && input.edit_conn_option) || (input.edit_files == 'Gene Expression' && input.edit_conn_option)",
              fileInput(
                inputId = "edit_conn_pert_file",
                label = strong(
                  redAsterisk, "Choose a connectivity map file (Perturbagens)"
                )
              )
            ),
            uiOutput(outputId='edit_conn_pert_file_msg')
          )
        ),
        column(
          width=6,
          conditionalPanel(
            condition = "input.edit_files == 'Profile Annotation' || input.edit_files == 'Gene Expression' || input.edit_files == 'All' || input.edit_files == 'GS Enrichment' || input.edit_files == 'K2Taxonomer' || (input.edit_files == 'Connectivity Map' && input.edit_conn_pro_option)",
            # Get the exposure variables
            h4("Exposure Variables:", style="padding-bottom: 10px;"),
            selectInputWithTooltip(
              inputId = "edit_variable_compound",
              label=strong(redAsterisk, "Select a compound variable:"),
              bId="edit_var_compound", helptext="Some description here",
              choices=c("Import a profile annotation" = ""), multiple = FALSE
            ),
            uiOutput(outputId = 'edit_variable_compound_msg'),
            selectInputWithTooltip(
              inputId = "edit_variable_exposure",
              label=strong(
                redAsterisk, "Select a list of additional exposure variables:"
              ),
              bId="edit_var_exposure", helptext="Some description here",
              choices=c("Import a profile annotation" = ""), multiple = TRUE
            ),
            uiOutput(outputId = 'edit_variable_exposure_msg'),
            ## Calculation
            uiOutput(outputId = "Edit_Tas_Modz"),
            br(),
            ## Redo enrichment analysis
            h4("Gene Set Enrichment:", style="padding-bottom: 10px;"),
            radioButtonsWithTooltip(
              inputId = "edit_cur_enrichment_option",
              label = "Use existing gene set",
              bId = "edit_cur_enrichment",
              helptext = "Gene sets include the MSigDB collections (Hallmark, C2 reactome pathways), and gene targets of various nuclear receptors (NURSA)",
              choices = c("Yes"=TRUE, "No"=FALSE),
              selected = project@GS_Collection=="Default"
            ),
            conditionalPanel(
              condition = "!input.edit_cur_enrichment_option",
              textInput(
                inputId = "Edit_New_Enrichment_GS",
                label = strong(
                  redAsterisk, "Enter a Name for Gene Set Collection:"
                ),
                value = ifelse(
                  project@GS_Collection=="Default", "", project@GS_Collection
                )
              ),
              uiOutput(outputId = "edit_enrichment_gs_msg"),
              textInput(
                inputId = "Edit_New_Enrichment_Link",
                label = strong(
                  redAsterisk, "Provide a Link to the GS Collection:"
                ),
                value = ifelse(
                  project@GS_Collection=="Default", "",
                  paste0(project@GS_Collection_Link, collapse=",")
                )
              ),
              uiOutput(outputId = "edit_enrichment_link_msg"),
              if (!project@GS_Collection %in% c("Default", "", NA)) {
                radioButtons(
                  inputId = "edit_gs_collection_file_option",
                  label = strong(redAsterisk, "Change gene set collection?"),
                  choices = c("Yes"=TRUE, "No"=FALSE), selected = FALSE,
                  inline=TRUE
                )
              } else {
                shinyjs::hidden(
                  radioButtons(
                    inputId = "edit_gs_collection_file_option",
                    label = strong(redAsterisk, "Change the collection file:"),
                    choices = c("Yes"=TRUE, "No"=FALSE), selected = TRUE,
                    inline=TRUE
                  )
                )
              }
            ),
            conditionalPanel(
              condition = "!input.edit_cur_enrichment_option && input.edit_gs_collection_file_option",
              fileInput(
                inputId = "edit_gs_collection_file",
                label = strong(
                  redAsterisk, "Choose a Gene Set Collection",
                  downloadLink(
                    outputId = "gs_collection_template_gmt",
                    label = em(style="font-size: 11px", "template.gmt")
                  )
                )
              ),
              uiOutput(outputId = "edit_gs_collection_file_msg")
            ),
            conditionalPanel(
              condition = "!(input.edit_cur_enrichment_option || input.edit_gs_collection_file_option)",
              downloadButton(
                outputId = "download_gs_collection_file",
                label = "Download gene set collection"
              ),
              br(), br()
            ),
            conditionalPanel(
              condition = "input.edit_cur_enrichment_option",
              uiOutput(outputId = "edit_enrichment_cur_version_option")
            ),
            shinyjs::disabled(
              radioButtons(
                inputId="edit_ssGSEA_method", label="Choose a ssGSEA method:",
                choices=c("gsva"="gsva", "ssgsea", "zscore", "plage"),
                inline=TRUE
              )
            ),
            br(),
            ## K2Taxonomer
            h4("K2Taxonomer Analysis:", style="padding-bottom: 10px;"),
            selectInputWithTooltip(
              inputId = "edit_variable_exposure_phenotype",
              label=strong(redAsterisk, "Select a list of exposure phenotype:"),
              bId="edit_var_exposure_phenotype",
              helptext="Some description here",
              choices=c("Import a profile annotation" = ""), multiple = TRUE
            ),
            uiOutput(outputId = 'edit_variable_exposure_phenotype_msg'),
            DT::dataTableOutput(outputId = "edit_metavar_variable_test"),
            br(),
            shinyjs::hidden(
              checkboxInput(
                inputId = "edit_connectivity_var",
                label = "Add Connectivity Variables", value = FALSE
              ),
              conditionalPanel(
                condition = "input.edit_connectivity_var == true",
                selectInput(
                  inputId="edit_connectivity_test",
                  label="Choose a statistical test:",
                  choices=c(
                    "1-sided Wilcox RS test", "2-sided Wilcox RS test",
                    "1-sided t test", "2-sided t test"
                  )
                )
              )
            ),
            ## Additional parameters
            h4("Additional Parameters:"),
            selectInputWithTooltip(
              inputId="edit_feature_metric", label="Feature filtering:",
              bId="edit_feature_filtering_metric",
              helptext="Some description here",
              choices=c("sd"="sd", "mad"="mad"), selected="sd"
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput("edit_input_message")
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(
            "Edit_Project_Add_Button", label=strong("Update"),
            class="mybuttons", width="70px"
          ),
          actionButton(
            "Edit_Project_Cancel_Button", label=strong("Cancel"),
            class="mybuttons", width="70px"
          )
        )
      )
    )
  )
}
