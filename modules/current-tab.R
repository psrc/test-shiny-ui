# Display UI for current demographic travel tab

current_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('currentBanner', 
              photo_filename = psrc_photos[sample.int(length(psrc_photos), 1)], 
              banner_title = "Current Demographic Travel", 
              banner_subtitle = "Cross-tabulate 2023 Survey Results"),
    
    div(style = 'margin: 3rem 5rem;',
        fluidRow(
          column(width = 3,
                
                 current_widgets_ui(ns('current')),
                 
                 # if visual tab is clicked, display radio button selection (share, share moe, count, count moe, etc.)
                 conditionalPanel(paste0("input['", ns("tabset"), "'] == 'v'"),
                                  div(style = 'margin: 3rem 0',
                                      radioButtons(ns('visopt'),
                                                   label = 'Visual Options',
                                                   choices = dtype.choice.stab.vis
                                      )))
                 
                 
          ), # end column
          column(width = 9,
                 tabsetPanel(id = ns('tabset'),
                             type = 'pills',
                             tabPanel('Table',
                                      value = 't',
                                      # trends_table_ui(ns('table'))
                             ),
                             tabPanel('Visual',
                                      value = 'v',
                                      current_plot_ui(ns('plot'))
                                      
                             )
                             
                 ) # end tabsetPanel
                 
          ) # end column
        ) # end fluidRow
    ) # end div
    
    

    
  )
  
}

current_tab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(var1 = NULL, var2 = NULL)
    
    observeEvent(input$`current-go`, {
      vals$var1 <- input$`current-var_one`
      vals$var2 <- input$`current-var_two`
    })
    
    current_widgets_server('current')
    
    d <- eventReactive(input$`current-go`, {
      # query table that match var 1 and var 2 

      current.vars.subset %>% 
        filter(var_1_name == input$`current-var_one` & var_2_name == input$`current-var_two`)

    })
    
    current_plot_server(id = 'plot', 
                        go = input$`current-go`, 
                        crosstab_table = reactive(d()),
                        var_one = 'var_1_value',
                        var_two = 'var_2_value',
                        visoption = reactive(input$visopt)
    )
    
  }) # end moduleServer
  
}