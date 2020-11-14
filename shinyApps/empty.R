## home page ####
output$pageStub <- renderUI({
  fluidRow(
    class="home-page",
    column(
      width=12,
      h4(style="text-align:center;", "This portal is empty.")
    )
  )
})
