# Display widgets for the crosstab (current demgographic travel) tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

current_widgets_ui <- function(id) {
  ns <- NS(id)
  
  vars.cat <- current.vars.subset %>% 
    select(category_1, category_2) %>% 
    distinct()

  tagList(
    
    # Variable One
    selectInput(ns('cat_one'),
                label = 'Category One',
                choices = unique(vars.cat$category_1)), 
    uiOutput(ns('var_one_ui')),
    
    # Variable Two
    selectInput(ns('cat_two'),
                label = 'Category Two',
                choices = unique(vars.cat$category_2)), 
    uiOutput(ns('var_two_ui')),

    actionButton(ns('go'),
                 label = 'Enter')
  )
  
}

current_widgets_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    variables <- reactive({
      # variable and alias list for both dropdowns
      # current.vars.subset is read in global.R
      t <- current.vars.subset %>% 
        filter(var_1_nice_name != 'Survey data collection year')
      
      v_one <- t %>% filter(category_1 == input$cat_one) %>% select(var_1_name, var_1_nice_name) %>% distinct()
      vars_one <- as.vector(v_one$var_1_name)
      names(vars_one) <- as.vector(v_one$var_1_nice_name)
      
      v_two <- t %>% filter(category_2 == input$cat_two) %>% select(var_2_name, var_2_nice_name) %>% distinct()
      vars_two <- as.vector(v_two$var_2_name)
      names(vars_two) <- as.vector(v_two$var_2_nice_name)
      
      return(list(one = vars_one, two = vars_two))
      
    })
 
    output$var_one_ui <- renderUI({
      
      selectInput(ns('var_one'),
                  label = 'Variable One',
                  choices = variables()$one)
      
    })
    
    output$var_two_ui <- renderUI({
      
      selectInput(ns('var_two'),
                  label = 'Variable Two',
                  choices = variables()$two)
      
    })
    
  }) # end moduleServer
  
}