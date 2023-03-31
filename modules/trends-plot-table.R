# Display trends as a data table

trends_plot_table_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    DT::dataTableOutput(ns('table'))
  )
  
}

trends_plot_table_server <- function(id, go, trend_var) {
  
  moduleServer(id, function(input, output, session) { 
    
    varyears <- eventReactive(go, {
      # identify which years the variable is available
      
      v <- variables.lu[variable %in% trend_var, ]
      y <- unique(v$survey_year)
      y <- as.character(y[which(y != 2017)]) # 2019 will represent 2017_2019
      str_replace(y, "2019", "2017_2019")
    })
    
    alias <- eventReactive(go, {
      # return the variable's alias (all available years)
      
      alias <- variables.lu[variable %in% trend_var, .(variable_name)]
      unique(alias$variable_name)
    })
    
    values <- eventReactive(go, {
      # return variable's values (all available years)
      
      v <- values.lu[variable %in% trend_var, ][order(value_order)] # return a dt
      unique(v[, .SD, .SDcols = !c('value_id', 'survey_year')])
      # used in stabTableType, (xvals) for table display
    })
    
    tabletype <- eventReactive(go, {
      # identify table type (t, p, h), table name
      
      v <- variables.lu[variable %in% trend_var, ]
      t <- unique(v$table_name)
      table_name <- table_names[[t]]
      dtypes <- as.vector(unique(v$dtype))
      
      ifelse('fact' %in% dtypes,  type <- 'fact', type <- 'dimension') 
      # weight_name <- select.vars$weight_name # weight_name doesn't exist in db!!!
      # return(list(Weight_Name=weight_name, Type=type, Table_Name=table_name))
      return(list(Type=type, Table_Name=table_name))
    } )
    
    trendtable <- eventReactive(go, {
      # return list of tables subsetted by value types
      
      wt_field <- tabletype()$Weight_Name
      table_name <- tabletype()$Table_Name
      type <- tabletype()$Type
      
      # collect data for available years
      survey_years <- varyears()
      data <- map(survey_years, ~get_hhts(survey = .x, level = table_name, vars = c("seattle_home", trend_var)))
      walk(data, ~setDT(.x))
      
      ### iterate when checkbox is included in UI
      # if(input$stab_fltr_sea == T) data <- data[seattle_home == 'Home in Seattle',]

      xa <- alias()

      if (type == 'fact') {
        data <- map(data, ~.x[eval(parse(text = trend_var)) > min_float])
        data <- map(data, ~.x[eval(parse(text = trend_var)) < max_float])
        data <- map(data, ~.x[, (trend_var) := cut(eval(parse(text = trend_var)), hist_breaks, labels = hist_breaks_labels, order_result = TRUE)])
      }
      
      trendtab <- map(data, ~hhts_count(.x, group_vars = trend_var, incl_na = FALSE))

      new.colnames <- c("estimate", "estMOE", "share", "MOE", 'sample_count')
      old.colnames <- c('count', 'count_moe', 'share', 'share_moe', 'sample_size')
      walk(trendtab, ~setnames(.x, old = old.colnames, new = new.colnames))
      # browser()
      new.colorder <- c("survey", trend_var, new.colnames)
      trendtab <- map(trendtab, ~.x[, ..new.colorder][share != 1, ])
      
      xvals <- values()[, .(value_order, value_text)][]

      # # check input type and xvals. sometimes xvals doesn't exist for some variables
      # if((typeof(input$stab_xcol) == 'character') & (nrow(xvals) > 0)){
      #   simpletab <- base::merge(simpletab, xvals, by.x=input$stab_xcol, by.y='value_text')
      #   setorder(simpletab, value_order)
      # }
      # 
      # dtypes <- dtype.choice.stab
      # selcols <- c(xa, names(dtypes))
      # 
      # setnames(simpletab, c(input$stab_xcol, dtypes), selcols)
      # setcolorder(simpletab, selcols)
      # 
      # dt <- simpletab[!(base::get(eval(xa)) %in% "")][, ..selcols]
    })
    
    output$table <- DT::renderDataTable({
      trendtable()
      # colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
      # dt <- stabTable.DT()
      # 
      # fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
      # fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
      # DT::datatable(dt,
      #               options = list(bFilter=0, 
      #                              # pageLength = 10,
      #                              autoWidth = FALSE,
      #                              columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
      #               )
      # ) %>%
      #   formatPercentage(fmt.per, 1) %>%
      #   formatRound(fmt.num, 0) %>%
      #   formatStyle(columns = 2:ncol(dt),
      #               valueColumns = ncol(dt), 
      #               color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
      
      
    })
    
    
  }) # end moduleServer
  
}