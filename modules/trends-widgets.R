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
    uiOutput(ns('icon')),
    checkboxInput(ns('filter'),
                  label = "Seattle households only",
                  value = FALSE),
    actionButton(ns('go'),
                 label = 'Enter')
  )
  
}

trends_widgets_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    variables <- reactive({
      # variable and alias list
      # vars.subset is read in global.R
      t <- variables.lu[category %in% input$category, ][variable %in% vars.subset$variable]
      v.raw <- as.list(unique(t$variable))
      v.list <- setNames(v.raw, as.list(unique(t$variable_name)))
    })
    
    output$var <- renderUI({
     div(style = "width: 90%; float:left;",
      selectInput(ns('variable'),
                  label = 'Variable',
                  choices = variables(),
                  selected = variables()[2])
     )
    })
    
    output$icon <- renderUI({
      
      tooltip(icon('circle-info', style="padding: 0px; font-size: 15px"),
              title = variable_desc())
      
    })
    
    variable_desc <- eventReactive(input$variable, {
      if(is.null(input$variable)) return(NULL)
      unique(variables.lu[variable == input$variable, .(detail)])
    })
    
    
  }) # end moduleServer
  
}