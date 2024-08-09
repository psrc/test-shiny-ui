# Display trends table as DT

trends_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('tableui'))
  )
  
}

trends_table_server <- function(id, go, trendtable) {
 
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
      
      cols <- c("var_nice_name", "year", "var_label", "var_label_order", dtype_choice_tbl)

      dt <- trendtable[, ..cols]
      
      col <- names(dtype_choice_tbl[dtype_choice_tbl %in% 'share_moe'])
      col2 <- names(dtype_choice_tbl[dtype_choice_tbl %in% 'estimate_moe'])
      
      # round columns
      dt[, share_moe := lapply(.SD, function(x) round(x*100, 1)), .SDcols = 'share_moe'
      ][, estimate_moe := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = 'estimate_moe']
      
      # add symbols
      dt[, share_moe := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = 'share_moe'
      ][, estimate_moe := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = 'estimate_moe']
      
      # format survey year column, reorder rows
      setnames(dt, "year", "survey_year")
      
      return(dt)

    })
    
    output$table <- renderDT({
      # render DT with some additional column formatting
   
      colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
      
      dt <- clean_table()
      
      fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
      fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])

      # colnames for DT
      cols <- c("var_nice_name", "Year", unique(dt$var_nice_name), names(dtype_choice_tbl))
      
      # order by label order, survey_year
      setorderv(dt, c("var_label_order", "survey_year"))
      dt <- dt[, `:=` (var_label_order = NULL)]
      
      datatable(dt,
                # caption = description(),
                colnames = cols,
                options = list(autoWidth = FALSE,
                               columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))),
                                                 list(visible = FALSE, targets = c(1)))
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