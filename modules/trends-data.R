# Returns trend data (to be used with Trends plot and table)

trends_data_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList()
  
}

trends_data_server <- function(id, trend_var) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    trendtable <- reactive({
      df <- trends_vars_subset[var_name == trend_var, ]
      setorderv(df, c("var_label_order"))
      setnames(df, data_colnames, names(data_colnames)) 
      return(df)
    })
    
    return(list(table = trendtable()))

  }) # end moduleServer
  
}