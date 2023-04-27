# Returns trend data (to be used with Trends plot and table)

trends_data_ui <- function(id) {
  ns <- NS(id) # ns is namespace
  
  tagList()
  
}

trends_data_server <- function(id, go, trend_var, geography, subgeography = NULL) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
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
      data <- map(survey_years, ~get_hhts(survey = .x, level = table_name, vars = c("sample_county", "final_home_jurisdiction", "seattle_home", trend_var)))
      walk(data, ~setDT(.x))
      
      # filter for home county when county is selected
      if(geography != 'Region') data <- map(data, ~.x[sample_county == geography,])
      
      if(geography %in% c('Region', 'Kitsap', 'Snohomish')) subgeography <- NULL
      
      if(!is.null(subgeography) & geography != 'Region') {
        if(subgeography == 'Seattle') {
          data <- map(data, ~.x[seattle_home == 'Home in Seattle',])
        } else if(subgeography == 'Bellevue-Kirkland-Redmond') {
          data <- map(data, ~.x[final_home_jurisdiction %in% c('Bellevue', 'Kirkland', 'Redmond'),])
        } else if(subgeography == 'Tacoma') {
          data <- map(data, ~.x[final_home_jurisdiction == 'Tacoma',])
        }
      }
        
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

      return(trendtab)
    })
    
    trendtable_dt <- eventReactive(go, {
      # create a version with prepped column headers for DT
      
      t <- copy(trendtable())
      a <- alias()
      
      dtypes <- dtype.choice.stab
      selcols <- c(a, names(dtypes))

      setnames(t, c('survey', trend_var, dtypes), c('Survey', selcols))
      setcolorder(t, c('Survey', a, selcols[which(selcols != a)]))
      
      dt <- t[!(base::get(eval(a)) %in% "")][, !('value_order')]
    })
      
    return(list(table = trendtable_dt(), tablevis = trendtable(), tabletype = tabletype(), val = values(), alias = alias()))

  }) # end moduleServer
  
}