# Display UI for current demographic travel tab -- a cross-tabulation of variables for the current survey year

current_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    banner_ui('currentBanner', 
              photo_filename = psrc_photos[sample.int(length(psrc_photos), 1)], 
              banner_title = "Current Demographic Travel", 
              banner_subtitle = "Cross-tabulate 2023 Survey Results"),
    
    div(style = 'margin: 3rem 5rem;',
        fluidRow(
          column(width = 4,
                 
                 current_widgets_ui(ns('current')),
                 
                 # if visual tab is clicked, display radio button selection (share, share moe, count, count moe, etc.)
                 conditionalPanel(paste0("input['", ns("tabset"), "'] == 'v'"),
                                  div(style = 'margin: 3rem 0',
                                      radioButtons(ns('visopt'),
                                                   label = 'Visual Options',
                                                   choices = dtype_choice_vis #dtype.choice.stab.vis
                                      )))
                 
                 
          ), # end column
          column(width = 8,
                 tabsetPanel(id = ns('tabset'),
                             type = 'pills',
                             tabPanel('Table',
                                      value = 't',
                                      current_table_ui(ns('table'))
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
      # clean colnames

      current.vars.subset %>%
        filter(var1 == input$`current-var_one` & var2 == input$`current-var_two`) |>
        filter(geography == input$`current-geog`) |>
        rename(data_colnames)
      
    })
    
    current_plot_server(id = 'plot', 
                        go = input$`current-go`, 
                        crosstab_table = reactive(d()),
                        var_one = 'val1',
                        var_two = 'val2',
                        visoption = reactive(input$visopt)
    )
    
    observeEvent(input$`current-go`, {
      
      current_table_server(id = 'table', 
                           go = input$`current-go`, 
                           current_table = d() # for tables, don't wrap with reactive(). I don't know why!
                           # variable1 = input$`current-var_one`, 
                           # variable2 = input$`current-var_two`
      )
      
    })
    
  }) # end moduleServer
  
}