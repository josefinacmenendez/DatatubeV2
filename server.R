#server.R
shinyServer(function(input, output, session) {
  updateSelectizeInput(session, 'contigs', choices = contigs, server = TRUE, options = list(maxItems=50))
  indexes <- reactiveValues(
    from = 1,
    to   = 50)
  observeEvent(input$plot_selection, {
    if(input$plot_selection == 'select contigs'){
      output$heatmap <- renderPlot({
        validate(need(length(input$contigs)>1,message = "Select at least 2 contigs and 50 contigs at most")) #input validation
        plot <- plot_heatmap_subset(input$contigs)
        plot(plot)
      }, height = 700, width = 1000)
      output$dld <- downloadHandler(
        filename = function(){
          paste("heatmap", "tiff", sep = ".")
        },
        content = function(filename) {
          tiff(filename, height = 700, width = 1000)
          plot <- plot_heatmap_subset(input$contigs)
          plot(plot)
          dev.off()
        }
      )
    }
    else if (input$plot_selection == 'view ranked contigs'){ 
      observeEvent(input$prevButton, {
        if(indexes$from > 1){
          indexes$from <- indexes$from - 50
          indexes$to   <- indexes$to   - 50
          }
        else{
          indexes$from <- indexes$from
          indexes$to   <- indexes$to     
          }
        })
      observeEvent(input$nextButton, {
        if(indexes$to == max(dim(dge_dataframe))/3 - 36 ){
          indexes$from <- indexes$from + 50
          indexes$to   <- indexes$to   + 36        
          }
        else{
          indexes$from <- indexes$from + 50
          indexes$to   <- indexes$to   + 50
          }
        })
      output$heatmap <- renderPlot({
        plot <- plot_heatmap_subset(ranked_contigs[indexes$from:indexes$to])
        plot(plot)
        }, height = 700, width = 1000)
      output$dld <- downloadHandler(
        filename = function(){
          paste("heatmap", "tiff", sep = ".")
        },
        content = function(filename) {
          tiff(filename, height = 700, width = 1000)
          plot <- plot_heatmap_subset(ranked_contigs[indexes$from:indexes$to])
          plot(plot)
          dev.off()
        }
      )
      }
    })

})