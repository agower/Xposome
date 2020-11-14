loginUI <- function(
  id, title="Sign In", user_title="User Name", pass_title="Password",
  login_title="Sign in", error_message="Invalid username or password!"
)
{
  ns <- shiny::NS(id)
  fluidRow(
    class="signin-page", id=ns("panel"),
    column(
      width=4, offset=4, class="login-container",
      div(class="login-form", div(class="login-form-title", strong('Sign In'))),
      div(
        class="login-validate-form",
        shiny::textInput(
          inputId=ns("user_name"),
          label=shiny::tagList(shiny::icon("user"), user_title), width="auto"
        ),
        tags$script('document.getElementById("user_name").focus();'),
        shiny::passwordInput(
          inputId=ns("password"),
          label=shiny::tagList(shiny::icon("unlock-alt"), pass_title),
          width="auto"
        ),
        shinyjs::hidden(
          shiny::div(
            id = ns("error"),
            shiny::tags$p(
              error_message,
              style = "color: red; font-weight: bold; padding-top: 5px;",
              class = "text-center"
            )
          )
        ),
        br(),
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(
            inputId=ns("button"), class="login-btn",
            label=strong(login_title), onkeypress="loginfunction(event)",
            width="auto"
          )
        ),
        tags$script(
          paste0(
            'function loginfunction(e){',
              'if (e.which === 13) {',
                'document.getElementById("button").click();',
              '}',
            '}'
          )
        ),
        br(), br(),
        shiny::tags$p(
          style="text-align: center",
          actionLink(
            inputId="ForgetPassword", label=strong("Forgot Password?"),
            width="auto"
          )
        )
      )
    )
  )
}

login <- function (input, output, session, log_out=NULL)
{
  credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL)

  shiny::observeEvent(log_out(), {
    credentials$user_auth <- FALSE
    credentials$info <- NULL
    shiny::updateTextInput(session, "password", value = "")
  })

  shiny::observeEvent(credentials$user_auth,
    ignoreInit = TRUE,
    shinyjs::toggle(id = "panel")
  )

  shiny::observeEvent(input$button, {
    # Check that the username/password combination is valid
    password_match <- GeneHive::checkPassword(
      username=input$user_name, password=input$password
    )
    if (password_match) {
      shinyjs::hide(id = "error")
      credentials$user_auth <- TRUE
      credentials$info <- GeneHive::getUser(input$user_name)
    } else {
      # if not valid temporarily show error message to user
      shinyjs::show(id = "error")
    }
  })

  # return reactive list containing auth boolean and user information
  shiny::reactive(shiny::reactiveValuesToList(credentials))
}
