# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}

trends_plot_server <- function(id, go, trendtable, trend_var, alias, geography, visoption) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$plotui <- renderUI({
      # go
      
      div(
        withSpinner(
          plotOutput(ns('plot')),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    clean_table <- reactive({
      
      trendtable()[, survey := str_replace_all(survey, '_', '/')]
    })
    
    settings <- reactive({
     
      primary_col <- switch(visoption(),
                            'share' = 'share',
                            'estimate' = 'estimate',
                            "share_with_MOE" = 'share',
                            "estimate_with_MOE" = 'estimate',
                            "sample_count" = 'sample_count')

      moe_col <- switch(visoption(),
                        'share' = NULL,
                        'estimate' = NULL,
                        "share_with_MOE" = 'MOE',
                        "estimate_with_MOE" = 'estMOE',
                        "sample_count" = NULL)

      est <- switch(visoption(),
                    'share' = 'percent',
                    'estimate' = 'number',
                    "share_with_MOE" = 'percent',
                    "estimate_with_MOE" = 'number',
                    "sample_count" = 'number')
      
      return(list(p = primary_col, m = moe_col, e = est))
    })
    
    text <- reactive({
      desc <- switch(visoption(),
                     'share' = 'Share',
                     'estimate' = 'Estimate',
                     "share_with_MOE" = 'Share',
                     "estimate_with_MOE" = 'Estimate',
                     "sample_count" = 'Sample count')
      
      title <- paste(desc, 'of', alias())
      g <- ifelse(isolate(geography()) != 'Region', paste(isolate(geography()), 'County'), 'Regional')

      subtitle <- paste(g, 'results')
      
      return(list(title = title, subtitle = subtitle))
    })
    
    output$plot <- renderPlot({
      static_column_chart(t = clean_table(),
                          x = isolate(trend_var()),
                          y = settings()$p,
                          moe = settings()$m,
                          est = settings()$e,
                          fill = 'survey',
                          title = text()$title,
                          subtitle = text()$subtitle,
                          source = 'Puget Sound Regional Household Travel Survey')
    })
    
    
  }) # end moduleServer
  
}