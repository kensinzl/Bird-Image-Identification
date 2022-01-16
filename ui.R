#https://forloopsandpiepkicks.wordpress.com/
ui <- dashboardPage(
    skin="blue",
    title = "Bird Image",
    dashboardHeader(title=tags$h1("Bird Image", style="font-size: 140%; font-weight: bold; color: white; margin-top: 3%;"),
                    titleWidth = 350,
                    dropdownMenu(type = "notifications", icon = icon("question-circle", "fa-1x"), badgeStatus = "success", headerText="",
                                 tags$li(a(href = "https://www.facebook.com/cheyneleo.kueh", "Author Facebook"))
                    )),
    
    
    dashboardSidebar(
        width=350,
        fileInput(inputId = "input_image", label = "Upload Image", accept = c('.jpg', '.jpeg'), buttonLabel = "Browse..."), 
        tags$br()
    ),
    
    
    dashboardBody(
        h4("Instruction:"),
        tags$br(),tags$p("1. Take a picture of a bird."),
        tags$p("2. Crop image so that bird fills out most of the image."),
        tags$p("3. Upload image with menu on the left."),
        tags$br(),
        fluidRow(
            column(h4("Image:"), 
                   imageOutput(outputId = "output_image"), 
                   width = 6
                   ),
            column(h4("Result:"), 
                   tags$br(), 
                   textOutput(outputId = "warntext"), 
                   tags$br(),
                   tags$p("This bird is probably a:"), 
                   tableOutput(outputId = "text"), 
                   width=6
                   )
        ),
        tags$br()
    )
)


