# Display trends as a plot

trends_plot_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    # render static psrcplot
    uiOutput(ns('plotui'))
  )
  
}

trends_plot_server <- function(id, go, trendtable, trend_var, visoption) {#, valsvar
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$plotui <- renderUI({
      
      div(
        withSpinner(
          echarts4rOutput(ns('plot'), width = "100%", height = "50rem"),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })
    
    clean_table <- reactive({
      trendtable()
    })
    
    settings <- reactive({
     
      primary_col <- switch(visoption(),
                            'share' = 'share',
                            'estimate' = 'estimate',
                            "share_moe" = 'share',
                            "estimate_moe" = 'estimate',
                            "sample_count" = 'sample_count')
      
      moe_col <- switch(visoption(),
                        'share' = NULL,
                        'estimate' = NULL,
                        "share_moe" = 'share_moe', 
                        "estimate_moe" = 'estimate_moe',
                        "sample_count" = NULL)
      
      est <- switch(visoption(),
                    'share' = 'percent',
                    'estimate' = 'number',
                    "share_moe" = 'percent',
                    "estimate_moe" = 'number',
                    "sample_count" = 'number')
      
      return(list(p = primary_col, m = moe_col, e = est))
    })
    
    output$plot <- renderEcharts4r({
      # analyze unique # of x var_labels
      df <- clean_table()

      # adjust labels and rotation
      num_x_labels <- length(unique(df[["var_label"]]))

      if(num_x_labels <= 2) {
        setting_x_label_str_wrap <- NULL
        setting_x_label_rotate <- 0
      } else {
        setting_x_label_str_wrap <- 10
        setting_x_label_rotate <- 90
      }

      echart_bar_chart(
        t = df,
        x = "var_label",
        y = settings()$p,
        est = settings()$e,
        fill = "year",
        moe = settings()$m,
        pos = NULL,
        column_vs_bar = "column",
        color = psrc_colors$pgnobgy_10,
        legend_str_wrap = 30,
        x_label_str_wrap = setting_x_label_str_wrap,
        egrid_left = "10%",
        egrid_bottom = "20%",
        x_label_rotate = setting_x_label_rotate
      )
      
    })
    
    
  }) # end moduleServer
  
}