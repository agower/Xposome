SaveUser <- function() {
  div(
    id = "saveUserData",
    modalDialog(
      footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(width=12, h4("Are you sure you want to save the changes?")),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(
            "Save_User_Yes_Button", label=strong("Yes"),
            class="mybuttons", width="50px"
          ),
          actionButton(
            "Save_User_No_Button", label=strong("No"),
            class="mybuttons", width="50px"
          )
        )
      )
    )
  )
}
