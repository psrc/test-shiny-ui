server <- function(input, output, session) {
  footer_server('myFooter')
  
  trends_tab_server('trends')
  current_tab_server('current')
  
}
