# Portal About Page
fluidRow(
  class="portal-about",
  column(
    width=12,
    includeMarkdown(
      file.path("www/RMD", paste0("introduction_", fname, ".Rmd"))
    )
  )
)
