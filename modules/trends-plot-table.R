# Display trends as a data table

trends_plot_table_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList( 
    br(),
    tabsetPanel(id = ns('tabset'),
                type = 'pills',
                tabPanel('Table',
                         DT::dataTableOutput(ns('table'))
                ),
                tabPanel('Visual'
                         
                )
                
    ) # end tabsetPanel
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

      a <- alias()

      if (type == 'fact') {
        data <- map(data, ~.x[eval(parse(text = trend_var)) > min_float])
        data <- map(data, ~.x[eval(parse(text = trend_var)) < max_float])
        data <- map(data, ~.x[, (trend_var) := cut(eval(parse(text = trend_var)), hist_breaks, labels = hist_breaks_labels, order_result = TRUE)])
      }
      
      trendtab <- map(data, ~hhts_count(.x, group_vars = trend_var, incl_na = FALSE))

      new.colnames <- c("estimate", "estMOE", "share", "MOE", 'sample_count')
      old.colnames <- c('count', 'count_moe', 'share', 'share_moe', 'sample_size')
      walk(trendtab, ~setnames(.x, old = old.colnames, new = new.colnames))
      new.colorder <- c("survey", trend_var, new.colnames)
      trendtab <- map(trendtab, ~.x[, ..new.colorder][share != 1, ])
      
      # rbind all dataframes
      trendtab <- rbindlist(trendtab)
      
      xvals <- values()[, .(value_order, value_text)][]

      # check input type and xvals. sometimes xvals doesn't exist for some variables
      if((typeof(trend_var) == 'character') & (nrow(xvals) > 0)){
        trendtab <- base::merge(trendtab, xvals, by.x = trend_var, by.y = 'value_text')
        setorder(trendtab, value_order)
      }

      dtypes <- dtype.choice.stab
      selcols <- c(a, names(dtypes))

      setnames(trendtab, c('survey', trend_var, dtypes), c('Survey', selcols))
      setcolorder(trendtab, c('Survey', a, selcols[which(selcols != a)]))

      dt <- trendtab[!(base::get(eval(a)) %in% "")][, !('value_order')]
    })
    
    
    trendtable.DT <- reactive({
      # clean Margin of Error columns and column/row reorder for DT
      
      xa <- alias()
      dt <- copy(trendtable())

      col <- names(dtype.choice[dtype.choice %in% "MOE"])
      col2 <- names(dtype.choice[dtype.choice %in% "estMOE"])
      
      # round columns
      dt[, (col) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = col
      ][, (col2) := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = col2]
      
      # add symbols
      dt[, (col) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = col
      ][, (col2) := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = col2]
      
      # format survey year column, reorder rows
      dt[, Survey := str_replace_all(Survey, "_", "/")]
      dt <- dt[order(Survey)]

      new.colorder <- c('Survey',
                        xa,
                        names(dtype.choice[dtype.choice %in% c("share")]),
                        col,
                        names(dtype.choice[dtype.choice %in% c("estimate")]),
                        col2,
                        names(dtype.choice[dtype.choice %in% c("sample_count")]))
      
      setcolorder(dt,  new.colorder)
      return(dt)
    })
    
    output$table <- DT::renderDataTable({
      # render DT with some additional column formatting
      
      colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
      
      dt <- trendtable.DT()

      fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
      fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
      DT::datatable(dt,
                    options = list(autoWidth = FALSE,
                                   columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
                    )
      ) %>%
        formatPercentage(fmt.per, 1) %>%
        formatRound(fmt.num, 0) %>%
        formatStyle(columns = 2:ncol(dt),
                    valueColumns = ncol(dt),
                    color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
    })
    
    
  }) # end moduleServer
  
}