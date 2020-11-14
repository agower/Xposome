RemoveUser <- function () {
  div(
    id = "removeUserData",
    modalDialog(
      footer = NULL, size= "l", title = NULL,
      fluidRow(
        column(width=12, h4("Are you sure you want to remove this user?")),
      ),
      br(),
      fluidRow(
        column(
          width=4,
          actionButton(
            "Remove_User_Yes_Button", label=strong("Yes"),
            class="mybuttons", width="50px"
          ),
          actionButton(
            "Remove_User_No_Button", label=strong("No"),
            class="mybuttons", width="50px"
          )
        )
      )
    )
  )
}
