# Display banner (picture and jumbotron at the top of each tab)

banner_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('banner'))
  )
  
}

banner_server <- function(id, photo_filename, banner_title, banner_subtitle = NULL) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$banner <- renderUI({
      fluidRow(column(4, style = 'padding-left:50px; padding-right:0px;',
                      div(
                        img(src = photo_filename, 
                            width = "100%", 
                            height = "100%", 
                            style = "padding-top: 0px; border-radius:0 0 30px 0;", 
                            alt = "Street Intersection with housing building in the background")
                        )
                      ),
               column(8, style = 'padding-left:0px; padding-right:50px;',
                      bs4Jumbotron(
                        title = strong(div(class="mainpage_title", banner_title)),
                        status = "success",
                        btnName = strong(div(class="mainpage_subtitle", banner_subtitle))
                        )
                      )
      )
      
    })
    
  }) # end moduleServer
  
}