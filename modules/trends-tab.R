# Display UI for trends tab

trends_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('trendsBanner', 
              photo_filename = psrc_photos[sample.int(length(psrc_photos), 1)], 
              banner_title = "Travel Survey Trends", 
              banner_subtitle = "Compare survey results across time"),
    
    div(style = 'margin: 3rem 5rem;',
        fluidRow(
          column(width = 3,
                 trends_widgets_ui(ns('trendsWidgets')),
                 
                 # if visual tab is clicked, display radio button selection (share, share moe, count, count moe, etc.)
                 conditionalPanel(paste0("input['", ns("tabset"), "'] == 'v'"),
                                  div(style = 'margin: 3rem 0',
                                      radioButtons(ns('visopt'),
                                                   label = 'Visual Options',
                                                   choices = dtype_choice_vis
                                      ))
                 )
                 
          ), # end column
          column(width = 9,
                 tabsetPanel(id = ns('tabset'),
                             type = 'pills',
                             tabPanel('Table',
                                      value = 't',
                                      trends_table_ui(ns('table'))
                             ),
                             tabPanel('Visual',
                                      value = 'v',
                                      trends_plot_ui(ns('plot'))
                                      
                             )
                             
                 ) # end tabsetPanel
                 
          ) # end column
        ) # end fluidRow
    ) # end div
    
  )
  
}

trends_tab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # vals <- reactiveValues(var = NULL)
    # 
    # observeEvent(input$`trends-go`, {
    #   vals$var <- input$`trends-variable`
    # })
    
    trends_widgets_server('trendsWidgets')
    
    d <- eventReactive(input$`trendsWidgets-go`, {
      trends_data_server('trendsData',
                         trend_var = input$`trendsWidgets-variable`
                         )
    })
    
    observeEvent(input$`trendsWidgets-go`, {
      trends_table_server('table', 
                          go = input$`trendsWidgets-go`, 
                          trendtable = d()$table
      )
    })
    
    trends_plot_server('plot',
                       go = input$`trendsWidgets-go`,
                       trendtable = reactive(d()$table), 
                       trend_var = reactive(input$`trendsWidgets-variable`), 
                       visoption = reactive(input$visopt)
                       )
                          
    
  }) # end moduleServer
  
}