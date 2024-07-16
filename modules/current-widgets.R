# Display widgets for the crosstab (current demgographic travel) tab. Display categories and render the appropriate variables for chosen category.
# !categories can differ across surveys!

current_widgets_ui <- function(id) {
  ns <- NS(id)
  
  vars.cat <- current.vars.subset %>% 
    select(var_category, grouping_category) %>% 
    distinct()

  tagList(
    div(style = "background-color: #E6E6E6; padding: 2rem; margin-bottom: .75rem; border-radius: 10px;",
        fluidRow(
          
          column(width = 6,
                 div(
                   # Variable (one)
                   selectInput(ns('cat_one'),
                               label = 'Category',
                               choices = unique(vars.cat$var_category)),
                   uiOutput(ns('var_one_ui'))
                 )
          ),
          column(width = 6,
                 div(
                   # Grouping Variable (two)
                   uiOutput(ns('cat_two_ui')),
                   uiOutput(ns('var_two_ui'))
                 )
          )
        ) # end fluidrow
    ),
    
    actionButton(ns('go'),
                 label = 'Enter')
  )
  
}

current_widgets_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    ## category/variable and alias list for primary variable ----
    variable_one <- reactive({
      # current.vars.subset is read in global.R
      
      # variable
      v_one <- current.vars.subset |>  
        filter(var_category == input$cat_one) |>  
        select(var_nice_name, var_name) |>  
        distinct() |> 
        deframe()
    })
    
    output$var_one_ui <- renderUI({
      
      selectInput(ns('var_one'),
                  label = 'Variable',
                  choices = variable_one())
      
    })
    
    ## category/variable and alias list for grouping variable ----
    ### selection of primary variable influences choices for grouping category/variable
    category_vars_two <- reactive({

      # grouping options
      if(is.null(input$var_one)) return(NULL)
      
      current.vars.subset |>  
        filter(var_name == input$var_one) |>  
        select(grouping_category, grouping_nice_name, grouping) |>  
        distinct()
      
    })
    
    categories_two <- reactive({
      
      if(is.null(input$var_one)) return(NULL)
      
      category_vars_two() |> 
        select(grouping_category) |> 
        distinct() |> 
        deframe()
    })
    
    output$cat_two_ui <- renderUI({
      
      if(is.null(input$var_one)|is.null(categories_two())) return(NULL)
      
      selectInput(ns('cat_two'),
                  label = 'Grouping Category',
                  choices = categories_two())
    })
    
    variables_two <- reactive({
      
      if(is.null(input$var_one)|is.null(input$cat_two)) return(NULL)
      
      category_vars_two() |> 
        filter(grouping_category == input$cat_two) |> 
        select(grouping_nice_name, grouping) |> 
        distinct() |> 
        deframe()
    })
    
    output$var_two_ui <- renderUI({
      
      if(is.null(input$var_one)|is.null(variables_two())) return(NULL)
      
      selectInput(ns('var_two'),
                  label = 'Grouping Variable',
                  choices = variables_two())
    })
 
   
  }) # end moduleServer
  
}