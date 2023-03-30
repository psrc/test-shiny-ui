server <- function(input, output, session) {
    footer_server('myFooter')
  
  # trends tab ----
    trends_widgets_server('trends')
    banner_server('trendsBanner', 
                  photo_filename = "street-intersection.jpeg", 
                  banner_title = "Travel Survey Trends", 
                  banner_subtitle = "Something Something")
  
}
