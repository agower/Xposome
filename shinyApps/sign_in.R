redAsterisk <- span(style="color:red;", "*")

## Warning message for main project and login table
LogInMessage <- reactiveVal(NULL)
project_table_message <- reactiveVal(NULL)
login_table_message <- reactiveVal(NULL)
about_file_msg <- reactiveVal(NULL)
contact_file_msg <- reactiveVal(NULL)
forgotpasswordwarningmsg <- reactiveVal(NULL)
changepwdwarningmsg <- reactiveVal(NULL)
## Create reactive import data
reset <- reactiveVal(TRUE)
portal <- reactiveVal(NULL)
user <- reactiveVal(NULL)
cohorts <- reactiveVal(NULL)
intro_file <- reactiveVal(NULL)
pro_file <- reactiveVal(NULL)
chem_file <- reactiveVal(NULL)
ge_file <- reactiveVal(NULL)
conn_pcl_file <- reactiveVal(NULL)
conn_pert_file <- reactiveVal(NULL)
gs_collection_file <- reactiveVal(NULL)
# Warning messages for Add Project
addProjectBasicInfoWarning <- reactiveVal(NULL)
addProjectIntroFileWarning <- reactiveVal(NULL)
addProjectProfileAnnotationFileWarning <- reactiveVal(NULL)
addProjectGeneExpressionFileWarning <- reactiveVal(NULL)
addProjectCMapPCLFileWarning <- reactiveVal(NULL)
addProjectCMapPertFileWarning <- reactiveVal(NULL)
addProjectCompoundVariableWarning <- reactiveVal(NULL)
addProjectExposureVariablesWarning <- reactiveVal(NULL)
addProjectEnrichmentGSWarning <- reactiveVal(NULL)
addProjectEnrichmentLinkWarning <- reactiveVal(NULL)
addProjectGSCollectionFileWarning <- reactiveVal(NULL)
addProjectExposurePhenotypesWarning <- reactiveVal(NULL)
addProjectInputWarning <- reactiveVal(NULL)
## Warning message for edit project function
editprojectwarningmsg <- reactiveVal(NULL)
editinputwarningmsg <- reactiveVal(NULL)
editcompoundvarwarningmsg <- reactiveVal(NULL)
editexposurevarwarningmsg <- reactiveVal(NULL)
editexposurephenotypevarwarningmsg <- reactiveVal(NULL)
editenrichmentgswarningmsg <- reactiveVal(NULL)
editenrichmentlinkwarningmsg <- reactiveVal(NULL)
## Warning message for add and edit user function
adduserwarningmsg <- reactiveVal(NULL)
edituserwarningmsg <- reactiveVal(NULL)

# Sign in page
output$pageStub <- renderUI(
  shiny::div(
    uiOutput(outputId = "uiLogin"),
    uiOutput(outputId = "uiMain")
  )
)

# Call the logout module with reactive trigger to hide/show
logout_init <- callModule(
  logout,
  id = "logout",
  active = reactive(credentials()$user_auth)
)

# Call login module supplying data frame, user and password cols
# and reactive trigger
credentials <- callModule(
  login,
  id = "login",
  log_out = reactive(logout_init())
)

# If the user is not logged in, show the login panel
output$uiLogin <- renderUI({
  req(credentials()$user_auth== FALSE)
  loginUI(id="login")
})

# Password reset ###############################################################

# If "Forgot Password?" button on sign-in page is pressed, show modal dialog
observeEvent(input$ForgetPassword, showModal(ForgotPassword()))

# If "Submit" button on "Forgot your password?" modal dialog is pressed,
# and submitted username checks out, send password reset email
observeEvent(input$Forgot_Submit, {
  Username <- trimws(input$Forgot_Username)
  if (Username == "") {
    forgotpasswordwarningmsg("Please fill in the required (*) fields.")
  } else {
    user <- try(GeneHive::getUser, Username, silent=TRUE)
    if (is(user, "hiveUser")) {
      tmp_pwd <- password(
        n=10, numbers=TRUE, case=TRUE, special=c("?", "!", "&", "%", "$")
      )
      GeneHive::updateUser(user@username, password=tmp_pwd)
      sendTemporaryPassword(
        from_sender       = "montilab@bu.edu",
        to_recipient      = user@email,
        recipient_first   = user@firstName,
        recipient_last    = user@lastName,
        recipient_account = Username,
        tmp_pwd           = tmp_pwd
      )
      forgotpasswordwarningmsg(
        paste(
          "Thank you for your submission!",
          "A temporary password has been sent to your email."
        )
      )
    } else {
      forgotpasswordwarningmsg(
        paste(
          "This username does not exist in our database.",
          "Please enter another username."
        )
      )
    }
  }
})

# If "Back" button is pressed on "Forgot your password?" modal dialog,
# clear warning message and remove modal dialog
observeEvent(input$Forgot_Back, {
  forgotpasswordwarningmsg("")
  removeModal()
})

# Print Forgot password warning message
output$Forgot_Message <- renderUI({
  req(forgotpasswordwarningmsg())
  p(style="color:red;", HTML(forgotpasswordwarningmsg()))
})

# Moderator page ###############################################################

# If user logged in successfully, show the moderator page
output$uiMain <- renderUI({
  req(credentials()$user_auth)
  div(
    class="moderator-page",
    # Header
    fluidRow(
      class="moderator-top-banner",
      column(
        width=6,
        div(class="text-md-left", div(class="button-link", "Moderator Page"))
      ),
      column(
        width=6, div(class="text-md-right", logoutUI(id="logout"))
      )
    ),
    fluidRow(
      class="moderator-body",
      column(
        width=12,
        h3("Project Table"),
        br(),
        DT::dataTableOutput(outputId = "projecttable"),
        br(),
        uiOutput("ProjectTableMessage"),
        br(),
        actionButton(
          inputId="AddProject", label=strong("Add"),
          class="mybuttons", width="70px"
        ),
        actionButton(
          inputId="RemoveProject", label=strong("Remove"),
          class="mybuttons", width="70px"
        ),
        actionButton(
          inputId="EditProject", label=strong("Edit"),
          class="mybuttons", width="70px"
        )
      )
    ),
    fluidRow(
      class="moderator-body",
      column(
        width=6,
        h3("About Page"),
        br(),
        fileInput(
          inputId = "add_about_file",
          label = strong(
            redAsterisk, "Choose an about file ",
            downloadLink(
              outputId = "add_about_template_rmd",
              label = em(style="font-size: 11px", "about_page.Rmd")
            )
          )
        ),
        uiOutput(outputId='add_about_file_msg'),
        actionButton(
          inputId="About_Page_Add_Button", label=strong("Add"),
          class="mybuttons", width="70px"
        )
      ),
      column(
        width=6,
        h3("Contact Page"),
        br(),
        fileInput(
          inputId = "add_contact_file",
          label = strong(
            redAsterisk, "Choose a contact file ",
            downloadLink(
              outputId="add_contact_template_rmd",
              label = em(style="font-size: 11px", "contact_page.Rmd")
            )
          )
        ),
        uiOutput(outputId='add_contact_file_msg'),
        actionButton(
          inputId="Contact_Page_Add_Button", label=strong("Add"),
          class="mybuttons", width="70px"
        ),
      )
    ),
    fluidRow(
      class="moderator-body",
      column(
        width=12,
        h3("Login Table"),
        br(),
        DT::dataTableOutput(outputId = "logintable"),
        br(),
        uiOutput("LoginTableMessage"),
        br(),
        actionButton(
          "AddUser", label=strong("Add"), class="mybuttons", width="70px"
        ),
        actionButton(
          "RemoveUser", label=strong("Remove"), class="mybuttons", width="70px"
        ),
        actionButton(
          "EditUser", label=strong("Edit"), class="mybuttons", width="70px"
        )
      )
    )
  )
})

# Project table ################################################################

output$projecttable <- DT::renderDataTable(
  {
    table_colnames <- c(
      "Project", "Cell_Line", "Portal", "Landmark_Gene", "Exposure_Levels",
      "Exposure_Phenotype", "Description"
    )
    # Get list of Project entities and convert to data frame
    projectlist <- GeneHive::listEntities("Project")
    table <- data.frame(row.names=names(projectlist()))
    for (j in table_colnames) {
      table[[j]] <- sapply(projectlist(), slot, j)
      if (is.list(table[[j]])) {
        table[[j]] <- sapply(table[[j]], paste, collapse=",")
      }
    }
    colnames(table) <- gsub("_", " ", colnames(table))
    return(table)
  },
  escape = FALSE, extensions = 'Buttons', server = TRUE, rownames = FALSE,
  selection = "single",
  options = list(
    columnDefs = list(list(width='400px', targets=-1)),
    deferRender = FALSE,
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    scrollY = 400,
    scrollCollapse = TRUE,
    dom = 'T<"clear">Blfrtip',
    buttons=c('copy','csv','print')
  )
)

# Output project table message
output$ProjectTableMessage <- renderUI({
  req(project_table_message())
  p(style="color:red;", HTML(project_table_message()))
})

# Add Project ##################################################################

# When "Add" project button is pressed,
# initialize variables and show modal dialog
observeEvent(input$AddProject, {
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
  conn_pcl_file(NULL)
  conn_pert_file(NULL)
  gs_collection_file(NULL)
  # Show the Add Project modal dialog
  showModal(AddProject())
}, ignoreInit = TRUE)

# Edit Project #################################################################

# Show pop-up for edit project
observeEvent(input$EditProject, {
  i <- input$projecttable_rows_selected
  if (length(i) == 0) {
    project_table_message("Please select a project to edit.")
  } else {
    # Get list of Project entities and convert to data frame
    projectlist <- GeneHive::listEntities("Project")
    # Reset file input
    session$sendCustomMessage("ResetFileInput", "edit_intro_file")
    session$sendCustomMessage("ResetFileInput", "edit_pro_file")
    session$sendCustomMessage("ResetFileInput", "edit_ge_file")
    session$sendCustomMessage("ResetFileInput", "edit_conn_pcl_file")
    session$sendCustomMessage("ResetFileInput", "edit_conn_pert_file")
    session$sendCustomMessage("ResetFileInput", "edit_gs_collection_file")
    # Remove error messages
    editprojectwarningmsg(NULL)
    editinputwarningmsg(NULL)
    editcompoundvarwarningmsg(NULL)
    editexposurevarwarningmsg(NULL)
    editexposurephenotypevarwarningmsg(NULL)
    editenrichmentgswarningmsg(NULL)
    editenrichmentlinkwarningmsg(NULL)
    # Remove data messages
    cohorts(NULL)
    addProjectIntroFileWarning(NULL)
    addProjectProfileAnnotationFileWarning(NULL)
    addProjectGeneExpressionFileWarning(NULL)
    addProjectCMapPCLFileWarning(NULL)
    addProjectCMapPertFileWarning(NULL)
    addProjectGSCollectionFileWarning(NULL)
    project_table_message(NULL)
    # Remove data
    intro_file(NULL)
    pro_file(NULL)
    chem_file(NULL)
    ge_file(NULL)
    conn_pcl_file(NULL)
    conn_pert_file(NULL)
    gs_collection_file(NULL)
    # Show the edit modal
    portal(projectlist[[i]])
    showModal(EditProject(projectlist[[i]]))
  }
}, ignoreInit = TRUE)

## Output the edit project warning message
output$editprojectwarningmessage <- renderUI({
  req(editprojectwarningmsg())
  p(style="color:red;", HTML(editprojectwarningmsg()))
})

## Observe the enrichment version
observeEvent(input$Edit_New_Enrichment_GS, {
  req(input$Edit_New_Enrichment_GS)
  gs_collection <- trimws(input$Edit_New_Enrichment_GS)
  if (gs_collection=="") {
    editenrichmentgswarningmsg("Please enter a valid name.")
  } else {
    editenrichmentgswarningmsg("")
  }
})

## Output the enrichment version warning message
output$edit_enrichment_gs_msg <- renderUI({
  req(editenrichmentgswarningmsg())
  p(style="color:red;", HTML(editenrichmentgswarningmsg()))
})

## Observe the enrichment version
observeEvent(input$Edit_New_Enrichment_Link, {
  req(input$Edit_New_Enrichment_Link)
  gs_collection_link <- trimws(input$Edit_New_Enrichment_Link)
  if (gs_collection_link=="") {
    editenrichmentlinkwarningmsg("Please enter a valid link.")
  } else {
    editenrichmentlinkwarningmsg("")
  }
})
## Output the enrichment version warning message
output$edit_enrichment_link_msg <- renderUI({
  req(editenrichmentlinkwarningmsg())
  p(style="color:red;", HTML(editenrichmentlinkwarningmsg()))
})
# Observe when variable compound changed
observeEvent(input$edit_variable_compound, {
  req(input$edit_variable_compound)
  editcompoundvarwarningmsg(NULL)
})
# Create message for compound variable selection
output$edit_variable_compound_msg <- renderUI({
  req(editcompoundvarwarningmsg())
  p(style="color:red;", HTML(editcompoundvarwarningmsg()))
})
# Observe when variable exposure changed
observeEvent(input$edit_variable_exposure, {
  req(input$edit_variable_exposure)
  editexposurevarwarningmsg(NULL)
})
# Create message for exposure variable selection
output$edit_variable_exposure_msg <- renderUI({
  req(editexposurevarwarningmsg())
  p(style="color:red;", HTML(editexposurevarwarningmsg()))
})
# Observe when variable exposure phenotype changed
observeEvent(input$edit_variable_exposure_phenotype, {
  req(input$edit_variable_exposure_phenotype)
  editexposurephenotypevarwarningmsg(NULL)
})
# Create message for exposure phenotype variable selection
output$edit_variable_exposure_phenotype_msg <- renderUI({
  req(editexposurephenotypevarwarningmsg())
  p(style="color:red;", HTML(editexposurephenotypevarwarningmsg()))
})
# Create message for all inputs selection
output$edit_input_message <- renderUI({
  req(editinputwarningmsg())
  p(style="color:red; text-align:center;", HTML(editinputwarningmsg()))
})

# Observe when remove data is clicked
observeEvent(input$RemoveProject, {
  projectlist <- GeneHive::listEntities("Project")
  i <- input$projecttable_rows_selected
  if (length(i) == 0) {
    project_table_message("Please select a project to remove.")
  } else {
    portal(projectlist[[i]])
    showModal(RemoveProject())
  }
})

## Observe when yes button is clicked
observeEvent(input$Remove_Project_Yes_Button, {
  projectlist <- GeneHive::listEntities("Project")
  i <- input$projecttable_rows_selected
  project_table_message(
    paste("Project", projectlist[[i]]@Project, "has been removed.")
  )
  GeneHive::deleteEntity(objectId(projectlist[[i]]))
  removeModal()
})

## Observe when no button is clicked
observeEvent(input$Remove_Project_No_Button, {
  project_table_message("")
  portal(NULL)
  removeModal()
})

# Observe when save project yes button is clicked
observeEvent(input$Save_Project_Yes_Button, {
  project_table_message("Project list has been saved.")
  removeModal()
})

# Observe when save project no button is clicked
observeEvent(input$Save_Project_No_Button, {
  project_table_message("")
  removeModal()
})

# USER TABLE

## Output the user log in table
output$logintable <- DT::renderDataTable(
  GeneHive::listUsers(),
  escape = FALSE, extensions = 'Buttons', server = TRUE, rownames = FALSE,
  selection = "single",
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    deferRender = FALSE,
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    scrollY = 400,
    scrollCollapse = TRUE,
    dom = 'T<"clear">Blfrtip',
    buttons=c('copy','csv','print')
  )
)

## Output add user login message
output$AddUserWarningMessage <- renderUI({
  req(adduserwarningmsg())
  p(style="color:red;", HTML(adduserwarningmsg()))
})

## Observe when cancel button is clicked
observeEvent(input$AddUser, showModal(AddUser()))

## Observe the add user button is clicked
observeEvent(input$Add_User_Add_Button, {
  new_user <- list(
    username  = trimws(input$addusername),
    password  = trimws(input$addpassword),
    firstName = trimws(input$addfirstname),
    lastName  = trimws(input$addlastname),
    email     = trimws(input$addemail)
  )
  if (any(unlist(new_user) == "")) {
    adduserwarningmsg("Please fill in the required (*) fields.")
  } else {
    user <- try(GeneHive::getUser(new_user$username), silent=TRUE)
    if (is(user, "hiveUser")) {
      adduserwarningmsg(
        "This username already exists. Please enter another username."
      )
    } else {
      if (new_user$password != trimws(input$addretypepassword)) {
        adduserwarningmsg("Password does not match.")
      } else {
        adduserwarningmsg("")
        do.call(
          GeneHive::addUser,
          args=c(new_user, list(group="xposome", superuser=TRUE))
        )
        login_table_message(
          paste("User", new_user$username, "has been added.")
        )
        removeModal()
      }
    }
  }
})

## Observe the cancel user button is clicked
observeEvent(input$Add_User_Cancel_Button, {
  login_table_message("")
  adduserwarningmsg("")
  removeModal()
})

## Output edit user login message
output$EditUserWarningMessage <- renderUI({
  req(edituserwarningmsg())
  p(style="color:red;", HTML(edituserwarningmsg()))
})

# Pop-up for edit
observeEvent(input$EditUser, {
  i <- input$logintable_rows_selected
  if (length(i) == 0) {
    login_table_message("Please select a user to edit.")
  } else {
    users <- listUsers(simplify=FALSE)
    user(users[[i]])
    showModal(EditUser(users[[i]]))
  }
}, ignoreInit = TRUE
)

## Observe when edit user yes button is clicked
observeEvent(input$Edit_User_Update_Button, {
  edit_user <- list(
    firstName = trimws(input$editfirstname),
    lastName  = trimws(input$editlastname),
    username  = trimws(input$editusername),
    password  = trimws(input$editpassword),
    email     = trimws(input$editemail)
  )
  if (any(unlist(edit_user) == "")) {
    edituserwarningmsg("Please fill in the required (*) fields.")
  } else {
    edituserwarningmsg("")
    do.call(
      GeneHive::updateUser, args=c(list(username=user@username), edit_user)
    )
    login_table_message(paste("User", user@username, "has been modified."))
    removeModal()
  }
})

## Observe when edit user cancel button is clicked
observeEvent(input$Edit_User_Cancel_Button, {
  login_table_message("")
  edituserwarningmsg("")
  user(NULL)
  removeModal()
})

## Output add user login message
output$ChangePwdWarningMessage <- renderUI({
  req(changepwdwarningmsg())
  p(style="color:red;", HTML(changepwdwarningmsg()))
})

# Pop-up for change password
observeEvent(
  input$Edit_Change_Pwd, showModal(ChangePwd()), ignoreInit = TRUE
)

# Pop-up for submit password
observeEvent(input$Submit_Pwd, {
  Password <- trimws(input$newpassword)
  RetypePassword <- trimws(input$retypepassword)
  if (Password == "" || RetypePassword == "") {
    changepwdwarningmsg("Please fill in the required (*) fields.")
  } else {
    if (Password != RetypePassword) {
      changepwdwarningmsg("Password does not match.")
    } else {
      changepwdwarningmsg("")
      i <- input$logintable_rows_selected
      users <- GeneHive::listUsers(simplify=FALSE)
      GeneHive::updateUser(login_dat@username, password=Password)
      user(users[[i]])
      showModal(EditUser(users[[i]]))
    }
  }
}, ignoreInit = TRUE)

# Pop-up for cancel password
observeEvent(input$Cancel_Pwd, {
  changepwdwarningmsg("")
  users <- GeneHive::listUsers(simplify=FALSE)
  i <- input$logintable_rows_selected
  user(users[[i]])
  showModal(EditUser(users[[i]]))
}, ignoreInit = TRUE)

## Observe when remove user button is clicked
observeEvent(input$RemoveUser, {
  i <- input$logintable_rows_selected
  if (length(i) == 0) {
    login_table_message("Please select a user to remove.")
  } else {
    table <- GeneHive::listUsers()
    user(table[i,])
    showModal(RemoveUser())
  }
})

## Observe when remove user yes button is clicked
observeEvent(input$Remove_User_Yes_Button, {
  i <- input$logintable_rows_selected
  login_dat <- GeneHive::listUsers()
  login_table_message(paste("User", login_dat$Username[i], "has been removed."))
  GeneHive::updateUser(login_dat$Username[i], active=FALSE)
  removeModal()
})

## Observe when remove user no button is clicked
observeEvent(input$Remove_User_No_Button, {
  login_table_message("")
  user(NULL)
  removeModal()
})

## Observe when save user yes button is clicked
observeEvent(input$Save_User_Yes_Button, {
  GeneHive::addUser(user@Username)
  login_table_message("Login list has been saved.")
  write.csv(data, "data/User_Login_List.csv", row.names=FALSE)
  logindata(data)
  removeModal()
})

## Observe when save user no button is clicked
observeEvent(input$Save_User_No_Button, {
  login_table_message("")
  removeModal()
})
## Login table warning message
output$LoginTableMessage <- renderUI({
  req(login_table_message())
  p(style="color:red;", HTML(login_table_message()))
})

# Download handlers for template files #########################################

# Download introduction template
output$introductionTemplateRmd <- downloadHandler(
  filename = "introduction_template.Rmd",
  content = function (file) {
    file.copy("data/Template/introduction.Rmd", file)
  },
  contentType = "application"
)
# Download profile annotation template
output$add_pro_template_csv <- downloadHandler(
  filename = "profile_annotation_template.csv",
  content = function (file) {
    file.copy("data/Template/profile_annotation.csv", file)
  },
  contentType = "application"
)
# Download profile annotation template
output$add_pro_template_rds <- downloadHandler(
  filename = "profile_annotation_template.RDS",
  content = function (file) {
    file.copy("data/Template/profile_annotation.RDS", file)
  },
  contentType = "application"
)
# Download gene expression template
output$add_ge_template_csv <- downloadHandler(
  filename = "gene_expression_template.csv",
  content = function (file) {
    file.copy("data/Template/gene_expression.csv", file)
  },
  contentType = "application"
)
# Download gene expression template
output$add_ge_template_rds <- downloadHandler(
  filename = "gene_expression_template.RDS",
  content = function (file) {
    file.copy("data/Template/gene_expression.RDS", file)
  },
  contentType = "application"
)
# Download connectivity template (perturbagens class)
output$add_conn_pcl_template_csv <- downloadHandler(
  filename = "connectivity_perturbagen_class_template.csv",
  content = function (file) {
    file.copy("data/Template/connectivity_pcl.csv", file)
  },
  contentType = "application"
)
output$add_conn_pcl_template_rds <- downloadHandler(
  filename = "connectivity_perturbagen_class_template.RDS",
  content = function (file) {
    file.copy("data/Template/connectivity_pcl.RDS", file)
  },
  contentType = "application"
)
# Download connectivity template (perturbagens)
output$add_conn_pert_template_csv <- downloadHandler(
  filename = "connectivity_perturbagens_template.csv",
  content = function (file) {
    file.copy("data/Template/connectivity_pert.csv", file)
  },
  contentType = "application"
)
output$add_conn_pert_template_rds <- downloadHandler(
  filename = "connectivity_perturbagens_template.RDS",
  content = function (file) {
    file.copy("data/Template/connectivity_pert.RDS", file)
  },
  contentType = "application"
)
# Download gene set collection template
output$gs_collection_template_gmt <- downloadHandler(
  filename = "gs_collection_template.gmt",
  content = function (file) {
    file.copy("data/Template/hallmark.gmt", file)
  },
  contentType = "application"
)

# Download about page template
output$add_about_template_rmd <- downloadHandler(
  filename = "about_page.Rmd",
  content = function (file) file.copy("www/RMD/about_page.Rmd", file),
  contentType = "application"
)
# Download contact page template
output$add_contact_template_rmd <- downloadHandler(
  filename = "contact_page.Rmd",
  content = function (file) file.copy("www/RMD/contact_page.Rmd", file),
  contentType = "application"
)

# Download introduction page
output$edit_intro_download_file <- downloadHandler(
  filename = "Introduction_Page.Rmd",
  content = function (file) {
#    file.copy(
#      file.path("www/RMD", paste0("introduction_", portal()$Portal, ".Rmd")),
#      file
#    )
    GeneHive::getWorkFile(dataset@IntroductionRMD, file)
  },
  contentType = "application"
)

# About page
observeEvent(input$About_Page_Add_Button, {
  inputfile <- input$add_about_file
  if (is.null(inputfile)) {
    about_file_msg("Please choose a file to import.")
    return(NULL)
  }
})

observeEvent(input$add_about_file, {
  inputfile <- input$add_about_file
  about_file_msg("")
  if (is.null(inputfile)) return(NULL)
  tryCatch({
    if (tools::file_ext(inputfile$datapath != "rmd")) {
      about_file_msg("Incorrect file format. Please check your file again.")
      return(NULL)
    } else {
      file.copy(
        from=inputfile$datapath, to="www/RMD/about_page.Rmd", overwrite=TRUE
      )
    }
  }, error=function(err) {
    about_file_msg("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war) {
    about_file_msg("Import failed. Please check your file again.")
    return(NULL)
  })
})

# Output about page warning message
output$add_about_file_msg <- renderUI({
  req(about_file_msg())
  p(class="fileInputMsg",  HTML(about_file_msg()))
})

# contact page
observeEvent(input$Contact_Page_Add_Button,
  if (is.null(input$add_contact_file)) {
    contact_file_msg("Please choose a file to import.")
    return(NULL)
  }
)

observeEvent(input$add_contact_file, {
  inputfile <- input$add_contact_file
  contact_file_msg("")
  if (is.null(inputfile)) return(NULL)
  tryCatch({
    if (tools::file_ext(inputfile$datapath) != "rmd") {
      contact_file_msg("Incorrect file format. Please check your file again.")
      return(NULL)
    } else {
      file.copy(
        from=inputfile$datapath, to="www/RMD/contact_page.Rmd", overwrite=TRUE
      )
    }
  }, error=function(err) {
    contact_file_msg("Import failed. Please check your file again.")
    return(NULL)
  }, warning=function(war) {
    contact_file_msg("Import failed. Please check your file again.")
    return(NULL)
  })
})

# Output contract page warning message
output$add_contact_file_msg <- renderUI({
  req(contact_file_msg())
  p(class="fileInputMsg",  HTML(contact_file_msg()))
})

# Download profile annotation file
output$edit_pro_download_file <- downloadHandler(
  filename = "Profile_Annotation.RDS",
  content = function (file) {
#    file.copy(
#      file.path("data", portal()$Portal, "Profile_Annotation.RDS"), file
#    )
    GeneHive::getWorkFile(dataset@ProfileAnnotationRDS, file)
  },
  contentType = "application"
)

# Download gene expression file
output$edit_ge_download_file <- downloadHandler(
  filename = "Expression_Set.RDS",
  content = function (file) {
#    file.copy(file.path("data", portal()$Portal, "Expression_Set.RDS"), file)
    GeneHive::getWorkFile(dataset@GeneExpressionRDS, file)
  },
  contentType = "application"
)

# Download gs collection file
output$download_gs_collection_file <- downloadHandler(
  filename = paste0(portal()$GS_Collection, ".gmt"),
  content = function (file) {
    file.copy(
      file.path(
        "data", paste0("Enrichment Gene Set", portal()$GS_Collection, ".gmt")
      ),
      file
    )
  },
  contentType = "application"
)
