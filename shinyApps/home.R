# Home page
output$pageStub <- renderUI({
  fluidRow(
    class="home-page",
    column(
      width=12,
      DT::dataTableOutput(outputId = "main_table")
    )
  )
})

# Output main table
output$main_table <- DT::renderDataTable(
  {
    if (length(projectlist))) {
      portal_names <- sapply(projectlist, slot, "Portal")
      data.frame(
        "Project"     = paste0(
          '<a onclick="curlinkFun(&#39;', portal_names,
          '&#39;)" href="?', portal_names,
          '" class="portal-link" id="', portal_names,
          '" value="', portal_names, '">', portal_names,
          '</a>'
        ),
        "Cell line"   = sapply(projectlist, slot, "Cell_Line"),
        "Description" = sapply(projectlist, slot, "Description")
      )
    } else {
      data.frame(
        "Project"     = "<br>",
        "Cell line"   = "<br>",
        "Description" = "<br>"
      )
    }
  },
  escape = FALSE, server = TRUE, rownames = FALSE, selection = "none",
  options = list(
    columnDefs = list(list(className = 'dt-center', targets = "_all")),
    deferRender = FALSE,
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = 20,
    scrollX = TRUE,
    scrollY = 400,
    scrollCollapse = TRUE,
    dom = 'T<"clear">Blfrtip',
    buttons=c('copy','csv','print')
  )
)
