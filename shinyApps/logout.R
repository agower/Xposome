logoutUI <- function(
  id, label="Log out", icon=NULL, class="btn-danger", style="color: white;"
)
{
  ns <- shiny::NS(id)
  shiny::actionLink(
    inputId=ns("button"), class="button-link",
    icon=tags$i(class="fa fa-user-circle"), label="Log out"
  )
}

logout <- function (input, output, session, active)
{
  shiny::observeEvent(
    active(), ignoreInit = TRUE,
    {
      # Remove warning message
      project_table_message("")
      login_table_message("")
      shinyjs::toggle(id = "button", anim = TRUE, time = 1, animType = "fade")
    }
  )
  # return reactive logout button tracker
  shiny::reactive(input$button)
}
