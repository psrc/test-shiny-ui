# Display widgets for the trends tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

trends_widgets_ui <- function(id) {
  ns <- NS(id)
  cats <- unique(trends_vars_subset$var_category)
  
  tagList(
    selectInput(ns('category'),
                label = 'Category',
                choices = cats, 
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
      vars <- trends_vars_subset |> 
        filter(var_category == input$category) |> 
        select(var_nice_name, var_name) |> 
        distinct() |> 
        deframe()
    })
    
    output$var <- renderUI({
     div(style = "width: 90%; float:left;",
      selectInput(ns('variable'),
                  label = 'Variable',
                  choices = variables())
     )
    })
    
    
  }) # end moduleServer
  
}