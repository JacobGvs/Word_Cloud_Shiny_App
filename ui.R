library(shiny)
shinyUI(fluidPage(
        titlePanel("Word Cloud"),
        
        sidebarLayout(
          #sidebar with a slider and update input
          sidebarPanel(
          fileInput("wcfile","Upload a text file for wordcloud", multiple = F, accept = "text/plain"),
          actionButton("Update","Create Word Cloud"),
        hr(),
        sliderInput("freq",
                    "Minium Frequency:",
                     min = 10, max = 200, value = 15),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1, max = 300, value = 100)
        ),
        mainPanel(
          plotOutput("wcplot")
        )
     )
 )
)