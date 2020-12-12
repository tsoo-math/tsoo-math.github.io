#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Poisson point process on a disc"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("intensity",
                        "Intensity",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library("plotrix")
    output$distPlot <- renderPlot({
        
        point <- function(){
            z=2
            while(length(z)==1){
                x = 2*runif(1) -1
                y = 2*runif(1) -1
                if (x^2 + y^2 <1) { z<-c(x,y)}
            }
            z
        }
        re=replicate(rpois(1,(input$intensity)*pi), point())
        x=re[1,]
        y=re[2,]
        plot(x, y, xlim=c(-1.1,1.1), ylim=c(-1.1,1.1), asp=1, pch = 20)
        draw.circle(0, 0, 1, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
