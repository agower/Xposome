# The main page layout ####
output$pageStub <- renderUI(
  div(
    class="main-page",
    navbarPage(
      title=actionLink(inputId="main_link", label=strong(fname, "Portal")),
      id="main_page", position=c("static-top"), collapsible=TRUE,
      selected="About",
      # About
      tabPanel(
        title="About", value="About",
        source("ui_about.R", local=TRUE)$value
      ),
      # Annotation
      tabPanel(
        title="Annotation", value="Annotation",
        source("ui_annotation.R", local=TRUE)$value
      ),
      # Chemical Explorer
      tabPanel(
        title = "Chemical Explorer", value = "Chemical Explorer",
        source("ui_chemical.R", local=TRUE)$value
      ),
      # Marker Explorer
      tabPanel(
        title = "Marker Explorer", value = "Marker Explorer",
        source("ui_marker.R", local=TRUE)$value
      ),
      # Heatmap Explorer
      tabPanel(
        title = "Heatmap Explorer", value = "Heatmap Explorer",
        source("ui_heatmap.R", local=TRUE)$value
      ),
      # Taxonomic Clustering
      navbarMenu(
        title = "Taxonomic Clustering",
        tabPanel(
          title = "K2 Taxonomer Results", value = "K2 Taxonomer Results",
          source("ui_taxonomic_clustering.R", local=TRUE)$value
        ),
        tabPanel(
          title = "Compare Multiple", value = "Compare Multiple",
          source("ui_compare_multiple.R", local=TRUE)$value
        )
      )
    )
  )
)

# Go back to home page when the logo link is clicked on ####
observeEvent(input$main_link,
  updateNavbarPage(session, inputId="main_page", selected = "About"),
  ignoreInit=TRUE
)

# Go back to home page when the logo link is clicked on ####
observeEvent(input$main_page,
  if (input$main_page == "K2 Taxonomer Results") {
    session$sendCustomMessage("ResizeK2Table", "clusteringTable")
  }
)
