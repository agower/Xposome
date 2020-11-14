ForgotPassword <- function ()
{
  div(
    id = "Forgot_Password",
    modalDialog(
      size = "s", title = "Forgot your password?", footer = NULL,
      fluidRow(
        column(
          width=12,
          p(strong("To access your account, please enter your user name")),
          textInput("Forgot_Username", label=strong(redAsterisk, "Username")),
          uiOutput("Forgot_Message"),
          br(),
          actionButton(
            "Forgot_Submit", label=strong("Submit"), class="mybuttons"
          ),
          actionButton("Forgot_Back", label=strong("Back"), class="mybuttons")
        )
      )
    )
  )
}
