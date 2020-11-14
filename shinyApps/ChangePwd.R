# Function to change password
ChangePwd <- function () {
  div(
    id="changePwd",
    modalDialog(
      size="s", title="Enter New Password", footer=NULL,
      fluidRow(
        column(
          width=12,
          passwordInput(
            "newpassword", label=strong(redAsterisk, "New Password:")
          ),
          passwordInput(
            "retypepassword", label=strong(redAsterisk, "Re-type Password:")
          )
        ),
        column(width=12, uiOutput("ChangePwdWarningMessage")),
        column(
          width=12,
          actionButton(
            inputId="Submit_Pwd", label=strong("Submit"), class="mybuttons"
          ),
          actionButton(
            inputId="Cancel_Pwd", label=strong("Cancel"), class="mybuttons"
          )
        )
      )
    )
  )
}
