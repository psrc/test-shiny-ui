# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    div(plotlyOutput(ns('plot')), style = 'margin-top: 1rem')
  )
  
}

trends_plot_server <- function(id, trendtable, alias) {
  
  moduleServer(id, function(input, output, session) { 
    
    output$plot <- renderPlotly({
      interactive_column_chart(trendtable, 
                          x = alias, 
                          y = 'Total', 
                          fill = 'Survey',
                          title = paste('Estimate by', alias))
    })
    
    
  }) # end moduleServer
  
}