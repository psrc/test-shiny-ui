
navbarPage(
  id = "my_id",
  tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
  
  # logo
  title = a(div(tags$img(src='psrc-logo.png',
                         style="margin-top: -30px; padding-left: 40px;",
                         height = "80")), 
            href="https://www.psrc.org", target="_blank"),
  
  # navbar height
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:25px !important; 
                            padding-bottom:0 !important;
                            height: 75px;
                            }
                           .navbar {min-height:25px !important;}'))
  ),
  
  windowTitle = "My UI test", 
  theme = "styles.css",
  position = "fixed-top",
  
  # tabs ----
  tabPanel(title = "Trends",
           banner_ui('trendsBanner'),
           div(style = 'margin: 3rem 0;',
               fluidRow(
                 column(width = 3,
                        trends_widgets_ui('trends')
                       
                 ),
                 column(width = 9,
                        trends_plot_table_ui('trendsContent') # display DT
                 )
               ) # end fluidRow
           ) # end div
  ), # end tabPanel
  
  tabPanel(title = "Something Else"),
  
  tags$footer(footer_ui('myFooter'))
  
) # end navbarpage

