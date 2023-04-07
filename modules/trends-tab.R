# Display UI for trends tab

trends_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('trendsBanner', 
              photo_filename = "street-intersection.jpeg", 
              banner_title = "Travel Survey Trends", 
              banner_subtitle = "Something Something"),
    
    div(style = 'margin: 3rem 0;',
        fluidRow(
          column(width = 3,
                 trends_widgets_ui(ns('trends')),
                 
                 # if visual tab is clicked, display radio button selection (share, share moe, count, count moe, etc.)
                 conditionalPanel(paste0("input['", ns("tabset"), "'] == 'v'"),
                                  div(style = 'margin: 3rem 0',
                                      radioButtons(ns('radio'),
                                                   label = 'Radio Buttons',
                                                   choices = c('a', 'b', 'c')))
                 )
                 
          ), # end column
          column(width = 9,
                 tabsetPanel(id = ns('tabset'),
                             type = 'pills',
                             tabPanel('Table',
                                      value = 't',
                                      div(DTOutput(ns('table')), style = 'margin-top: 1rem')
                                      
                                      
                             ),
                             tabPanel('Visual',
                                      value = 'v'
                                      
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
    
    trends_widgets_server('trends')

    d <- eventReactive(input$`trends-go`, {
      trends_data_server('trendsData', go = input$`trends-go`, trend_var = input$`trends-variable`)
    })
    
    output$table <- renderDT({
      if(input$`trends-go`) {
        browser()
        d <- d()
      }
        
    })

    
    
    
  }) # end moduleServer
  
}



## old ----
# tabPanel(title = "Trends",
#          banner_ui('trendsBanner'),
#          div(style = 'margin: 3rem 0;',
#              fluidRow(
#                column(width = 3,
#                       trends_widgets_ui('trends')
#                      
#                ),
#                column(width = 9,
#                       trends_plot_table_ui('trendsContent') # display DT
#                )
#              ) # end fluidRow
#          ) # end div
# ), # end tabPanel