#ui.R

shinyUI(fluidPage(
  titlePanel("Datatube V2"),
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("plot_selection" , "Choose data source", choices = c("select contigs" , "view ranked contigs")),
      conditionalPanel(
        condition = "input.plot_selection == 'select contigs'",
          selectizeInput("contigs", "Select up to 50 contigs", 
                     choices = NULL, #need to be able to parse the entire list without crashing
                     options=list(maxOptions = 100),
                     multiple = TRUE)),
      downloadButton(outputId = "dld", label = "Export as tiff")),
    mainPanel(
      conditionalPanel(
        condition = "input.plot_selection == 'view ranked contigs'",
        actionButton("prevButton" , "Previous"),
        actionButton("nextButton" , "Next")),
      plotOutput("heatmap")
    )
  )
)
)