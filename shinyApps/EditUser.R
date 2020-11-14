EditUser <- function (user) {
  div(
    id = "editUserData",
    modalDialog(
      size="l", title=paste("Edit User", sQuote(user@username)), footer=NULL,
      fluidRow(
        column(
          width=3,
          textInput(
            inputId="editfirstname", label=strong(redAsterisk, "First name:"),
            value=user@firstName
          )
        ),
        column(
          width=3,
          textInput(
            "editlastname", label=strong(redAsterisk, "Last name:"),
            value=user@lastName
          )
        ),
        column(
          width=3,
          shinyjs::disabled(
            passwordInput(
              "editpassword", label=strong(redAsterisk, "Password:"),
              value=user@password
            )
          )
        ),
        column(
          width=3,
          textInput(
            "editemail", label=strong(redAsterisk, "Email:"), value=user@email
          )
        )
      ),
      br(),
      fluidRow(
        column(width=12, uiOutput("EditUserWarningMessage"))
      ),
      br(),
      fluidRow(
        column(
          width=12,
          actionButton(
            inputId="Edit_User_Update_Button", label=strong("Update"),
            class="mybuttons", width="70px"
          ),
          actionButton(
            inputId="Edit_User_Cancel_Button", label=strong("Cancel"),
            class="mybuttons", width="70px"
          ),
          actionButton(
            inputId="Edit_Change_Pwd", label=strong("Change Password?"),
            class="mybuttons"
          )
        )
      )
    )
  )
}
