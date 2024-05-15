# Display trends as a plot

current_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}

current_plot_server <- function(id, go, crosstab_table, var_one, var_two, visoption) {
  # id, go, trendtable, trend_var, alias, geography, subgeography = NULL, visoption, valsvar
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$plotui <- renderUI({
      
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

      crosstab_table() %>% 
        rename('share' = 'prop',  'estimate' = 'est', 'sample_count' = 'count')
      
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
     
    })
    
    output$plot <- renderPlot({

      # static_bar_chart(mode_income_summary, y='mode_simple', x='prop', fill='hhincome_broad', color='pgnobgy_10')

      # static_bar_chart(t = clean_table(),
      #                  x = 'prop',
      #                  y = var_one,
      #                  fill = var_two,
      #                  color = 'pgnobgy_10',
      #                  source = 'Puget Sound Regional Household Travel Survey')
      # browser()
      static_column_chart(t = clean_table(),
                          x = var_one,
                          y = settings()$p,
                          moe = settings()$m,
                          est = settings()$e,
                          fill = var_two,
                          color = 'pgnobgy_10',
                          source = 'Puget Sound Regional Household Travel Survey')
      
      # static_column_chart(t = clean_table(),
      #                     x = valsvar(),
      #                     y = settings()$p,
      #                     moe = settings()$m,
      #                     est = settings()$e,
      #                     fill = 'survey',
      #                     title = text()$title,
      #                     subtitle = text()$subtitle,
      #                     source = 'Puget Sound Regional Household Travel Survey')
    })
    
    
  }) # end moduleServer
  
}