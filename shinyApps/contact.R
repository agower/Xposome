# Contact page
output$pageStub <- renderUI({
  fluidRow(
    class="contact-page",
    column(
      width=12,
      includeMarkdown("www/RMD/contact_page.Rmd")
    )
  )
})
