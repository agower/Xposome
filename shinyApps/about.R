output$pageStub <- renderUI({
  fluidRow(
    class="about-page",
    column(
      width=12,
      includeMarkdown(paste0("www/RMD/about_page.Rmd")) 
    )
  )
})
