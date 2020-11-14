AddUser <- function() {
  div(
    id = "addUserData",
    modalDialog(
      size = "l", title = "Add User", footer = NULL,
      fluidRow(
        column(
          width=3,
          textInput("addfirstname", label=strong(redAsterisk, "First name:"))
        ),
        column(
          width=3,
          textInput("addlastname", label=strong(redAsterisk, "Last name:"))
        ),
        column(
          width=3,
          textInput("addusername", label=strong(redAsterisk, "Username:"))
        ),
        column(
          width=3,
          textInput("addemail", label=strong(redAsterisk, "Email:"))
        ),
        column(
          width=3,
          passwordInput("addpassword", label=strong(redAsterisk, "Password:"))
        ),
        column(
          width=3,
          passwordInput(
            "addretypepassword", label=strong(redAsterisk, "Re-type Password:")
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width=12,
          uiOutput(outputId="AddUserWarningMessage")
        )
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(
            inputId="Add_User_Add_Button", label=strong("Add"),
            class="mybuttons", width="70px"
          ),
          actionButton(
            inputId="Add_User_Cancel_Button", label=strong("Cancel"),
            class="mybuttons", width="70px"
          )
        )
      )
    )
  )
}
