# Display widgets for the crosstab (current demgographic travel) tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

current_widgets_ui <- function(id) {
  ns <- NS(id)
  
  vars.cat <- current.vars.subset %>% 
    select(category_1, category_2) %>% 
    distinct()

  geogs <- current.vars.subset$geography |> unique()

  tagList(
    
    div(style = "background-color: #BCBEC0; padding: 2rem; margin-bottom: .75rem; border-radius: 10px;",
        
        # Variable One
        selectInput(ns('cat_one'),
                    label = 'Category One',
                    choices = unique(vars.cat$category_1)), 
        uiOutput(ns('var_one_ui')),
    ),
    
    div(style = "background-color: #BCBEC0; padding: 2rem; margin-bottom: .75rem; border-radius: 10px;",
        # Variable Two
        selectInput(ns('cat_two'),
                    label = 'Category Two',
                    choices = unique(vars.cat$category_2)), 
        uiOutput(ns('var_two_ui')),
    ),
    
    selectInput(ns('geog'),
                label = 'Geography',
                choices = geogs),
    
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
      t <- current.vars.subset
      
      v_one <- t %>% filter(category_1 == input$cat_one) %>% select(var1) %>% distinct()
      vars_one <- as.vector(v_one$var1)
      names(vars_one) <- as.vector(v_one$var1)
      
      v_two <- t %>% filter(category_2 == input$cat_two) %>% select(var2) %>% distinct()
      vars_two <- as.vector(v_two$var2)
      names(vars_two) <- as.vector(v_two$var2)
      
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