# Display trends table as DT

current_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('tableui'))
  )
  
}

current_table_server <- function(id, go, current_table) {
  # id, go, trendtable, alias, geography, subgeography = NULL
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$tableui <- renderUI({
      go
      
      div(
        withSpinner(
          DTOutput(ns('table')),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    clean_table <- reactive({
      # clean Margin of Error columns and column/row reorder for DT
      
      cols <- c("label1", "label2", "survey_year",  "geography", "val1", "val2", dtype_choice_tbl)
      dt <- current_table[, ..cols]
      
      col <- names(dtype_choice_tbl[dtype_choice_tbl %in% 'share_moe'])
      col2 <- names(dtype_choice_tbl[dtype_choice_tbl %in% 'estimate_moe'])
     
      # round columns
      dt[, share_moe := lapply(.SD, function(x) round(x*100, 1)), .SDcols = 'share_moe'
      ][, estimate_moe := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = 'estimate_moe']
      
      # add symbols
      dt[, share_moe := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = 'share_moe'
      ][, estimate_moe := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = 'estimate_moe']

      # format survey year column, reorder rows
      dt <- dt[, survey_year := str_replace_all(survey_year, "_", "/")][order(survey_year)]
      
      return(dt)
    })
    
    description <- reactive({
      # if(geography %in% c('Region', 'Kitsap', 'Snohomish')) subgeography <- NULL
      # 
      # if(is.null(subgeography)) {
      #   if(geography == 'Region') g <- 'Regional Results'
      #   if(geography == 'Kitsap' | geography == 'Snohomish') g <- paste(geography, 'County Results')
      # } else {
      #   if(geography != 'Region' & (subgeography != 'Region' && !is.null(subgeography))) g <- paste(geography, 'County:', subgeography, 'Results')
      #   if(geography != 'Region' & subgeography == 'Region') g <- paste(geography, 'County Results')
      # }
      # 
      # return(g)
    })
    
    output$table <- renderDT({
      # render DT with some additional column formatting
      
      colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
      dt <- clean_table()

      # colnames for DT
      cols <- c("Year", "Geography", unique(dt$label1), unique(dt$label2), names(dtype_choice_tbl))
      dt <- dt[, 3:ncol(dt)]
      
      DT::datatable(dt,
                    # caption = description(),
                    colnames = cols,
                    options = list(autoWidth = FALSE,
                                   columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
                    )
      ) %>%
        formatPercentage('share', 1) %>%
        formatRound(c('estimate', 'sample_count'), 0) %>%
        formatStyle(columns = 1:ncol(dt),
                    valueColumns = ncol(dt),
                    color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
    })
    
  }) # end moduleServer
  
}