# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}

trends_plot_server <- function(id, go, trendtable, alias) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$plotui <- renderUI({
      go
      
      div(
        withSpinner(
          plotlyOutput(ns('plot')),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
          ),
        style = 'margin-top: 1rem'
        )
      
    })
    
    output$plot <- renderPlotly({
      interactive_column_chart(trendtable, 
                          x = alias, 
                          y = 'Total', 
                          fill = 'Survey',
                          title = paste('Estimate by', alias))
    })
    
    
  }) # end moduleServer
  
}