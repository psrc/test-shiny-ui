# Display widgets for the trends tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

trends_widgets_ui <- function(id) {
  ns <- NS(id)
  cats <- c('Travel') # test only with Travel category
  vars.cat <- vars.cat[which(vars.cat %in% cats)]
  
  tagList(
    selectInput(ns('category'),
                label = 'Category',
                choices = vars.cat, # list all categories available from variables table (regardless of survey)
                ),
    uiOutput(ns('var')),
    actionButton(ns('go'),
                 label = 'Enter')
  )
  
}

trends_widgets_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    variables <- reactive({
      # variable and alias list
      
      t <- variables.lu[category %in% input$category, ][variable %in% vars.subset$variable]
      v.raw <- as.list(unique(t$variable))
      v.list <- setNames(v.raw, as.list(unique(t$variable_name)))
    })
    
    output$var <- renderUI({
      selectInput(ns('variable'),
                  label = 'Variable', 
                  choices = variables())
    })
    

  }) # end moduleServer
  
}