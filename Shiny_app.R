#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Prediction of the probabilties for every Emotion"),
    
    
    sidebarPanel(
        selectizeInput(inputId = "Sex", label = "Gender", choices = c("Male", "Female")),
        sliderInput(inputId = "Age", min = 0, max = 100, value = 40, label = "Age in years"),
        sliderInput(inputId = "Vasvalue", min = 0, max = 100, value = 60, label = "VASvalue"),
        sliderInput(inputId = "Trial", min = 0, max = 104, value = 10, label = "Trial"),
        sliderInput(inputId = "AQ", min = 0, max = 50, value = 10, label = "AQ"),
        sliderInput(inputId = "EQ", min = 0, max = 144, value = 10, label = "EQ"),
        sliderInput(inputId = "LSAS", min = 0, max = 144, value = 10, label = "LSAS")
        ),
    
    sidebarPanel(
        numericInput(inputId = "ISCOR", label = "Intercept Stimulus COR", value = 0.02, min = NA, max = NA, step = 0.001,
                   width = NULL),
        numericInput(label = "Slope Stimulus COR", inputId = "SSCOR", value = -0.000227, min = NA, max = NA, step = 0.0001,
                     width = NULL),
        numericInput(label = "Intercept Blank COR", inputId = "IBCOR", value = -0.0178, min = NA, max = NA, step = 0.0001,
                     width = NULL),
        numericInput(label= "Slope Blank COR", inputId = "SBCOR", value = 5.339015e-06, min = NA, max = NA, step = 0.0000001,
                     width = NULL)
    ),
    
    sidebarPanel(
      numericInput(label = "Intercept Stimulus ZYG", inputId = "ISZYG", value = 0.001534616, min = NA, max = NA, step = 0.0001,
                   width = NULL),
      numericInput(label = "Slope Stimulus ZYG", inputId = "SSZYG", value = 2.631058e-05, min = NA, max = NA, step = 0.000001,
                   width = NULL),
      numericInput(label = "Intercept Blank ZYG", inputId = "IBZYG", value = -0.02733962 , min = NA, max = NA, step = 0.001,
                   width = NULL),
      numericInput(label = "Slope Blank ZYG", inputId = "SBZYG", value = 0.0004305061, min = NA, max = NA, step = 0.00001,
                   width = NULL)
    ),
    
    sidebarPanel(
      numericInput(label = "Intercept Stimulus SC", inputId = "ISSC", value = 0.144175, min = NA, max = NA, step = 0.0001,
                   width = NULL),
      numericInput(label = "Slope Stimulus SC", inputId = "SSSC", value = -0.003431481, min = NA, max = NA, step = 0.000001,
                   width = NULL),
      numericInput(label = "Intercept Blank SC", inputId = "IBSC", value = 0.146717 , min = NA, max = NA, step = 0.001,
                   width = NULL),
      numericInput(label = "Slope Blank SC", inputId = "SBSC", value = -0.003984374, min = NA, max = NA, step = 0.00001,
                   width = NULL)
    ),
    
    sidebarPanel(
      numericInput(label = "Intercept Stimulus SKT", inputId = "ISSKT", value = -0.06073867, min = NA, max = NA, step = 0.0001,
                   width = NULL),
      numericInput(label = "Slope Stimulus SKT", inputId = "SSSKT", value = 0.001697445, min = NA, max = NA, step = 0.000001,
                   width = NULL),
      numericInput(label = "Intercept Blank SKT", inputId = "IBSKT", value = -0.0409399 , min = NA, max = NA, step = 0.001,
                   width = NULL),
      numericInput(label = "Slope Blank SKT", inputId = "SBSKT", value = 0.001217014, min = NA, max = NA, step = 0.00001,
                   width = NULL)
    ),
    
    sidebarPanel(
      numericInput(label = "Intercept Stimulus DIA", inputId = "ISDIA", value = -0.226454, min = NA, max = NA, step = 0.0001,
                   width = NULL),
      numericInput(label = "Slope Stimulus DIA", inputId = "SSDIA", value = 0.012385, min = NA, max = NA, step = 0.000001,
                   width = NULL),
      numericInput(label = "Intercept Blank DIA", inputId = "IBDIA", value = 0.2747783 , min = NA, max = NA, step = 0.001,
                   width = NULL),
      numericInput(label = "Slope Blank DIA", inputId = "SBDIA", value = -0.005330103, min = NA, max = NA, step = 0.00001,
                   width = NULL)
    ),
    
    mainPanel(textOutput("pred"),
              textOutput("summary"),
              plotOutput("hist"))
    
)


  
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$hist <- renderPlot({
                        title <- "Histogram of the probabilities for each emotion"
                        
                        if (input$Sex == "Male"){
                            Sex <- 1
                        } else{
                          Sex <- 0
                        }
                        data_angry <- readRDS("data_angry.Rdata")
                        data_happy <- readRDS("data_happy.Rdata")
                        data_scared <- readRDS("data_scared.Rdata")
                        data_neu <- readRDS("data_neu.Rdata")
                        data_sad <- readRDS("data_sad.Rdata")
                        
                        Angry <- (data_angry[1, "Estimate"] +
                                                       data_angry[2, "Estimate"] * input$Trial +
                                                       data_angry[3, "Estimate"] * input$Age +
                                                       data_angry[4, "Estimate"] * input$Vasvalue +
                                                       data_angry[5, "Estimate"] * Sex +
                                                       data_angry[6, "Estimate"] * input$AQ +
                                                       data_angry[7, "Estimate"] * input$EQ +
                                                       data_angry[8, "Estimate"] * input$LSAS +
                                                       data_angry[9, "Estimate"] * input$ISCOR +
                                                       data_angry[10, "Estimate"] * input$SSCOR +
                                                       data_angry[11, "Estimate"] * input$IBCOR +
                                                       data_angry[12, "Estimate"] * input$SBCOR +
                                                       data_angry[13, "Estimate"] * input$ISZYG +
                                                       data_angry[14, "Estimate"] * input$SSZYG +
                                                       data_angry[15, "Estimate"] * input$IBZYG +
                                                       data_angry[16, "Estimate"] * input$SBZYG +
                                                       data_angry[17, "Estimate"] * input$ISSC +
                                                       data_angry[18, "Estimate"] * input$SSSC +
                                                       data_angry[19, "Estimate"] * input$IBSC +
                                                       data_angry[20, "Estimate"] * input$SBSC +
                                                       data_angry[21, "Estimate"] * input$ISSKT +
                                                       data_angry[22, "Estimate"] * input$SSSKT +
                                                       data_angry[23, "Estimate"] * input$IBSKT +
                                                       data_angry[24, "Estimate"] * input$SBSKT +
                                                       data_angry[25, "Estimate"] * input$ISDIA +
                                                       data_angry[26, "Estimate"] * input$SSDIA +
                                                       data_angry[27, "Estimate"] * input$IBDIA +
                                                       data_angry[28, "Estimate"] * input$SBDIA )
                        Angry <- round((1/(1+exp(-Angry))) * 100, 2)
                        
                        
                        Happy <- (data_happy[1, "Estimate"] +
                                    data_happy[2, "Estimate"] * input$Trial +
                                    data_happy[3, "Estimate"] * input$Age +
                                    data_happy[4, "Estimate"] * input$Vasvalue +
                                    data_happy[5, "Estimate"] * Sex +
                                    data_happy[6, "Estimate"] * input$AQ +
                                    data_happy[7, "Estimate"] * input$EQ +
                                    data_happy[8, "Estimate"] * input$LSAS +
                                    data_happy[9, "Estimate"] * input$ISCOR +
                                    data_happy[10, "Estimate"] * input$SSCOR +
                                    data_happy[11, "Estimate"] * input$IBCOR +
                                    data_happy[12, "Estimate"] * input$SBCOR +
                                    data_happy[13, "Estimate"] * input$ISZYG +
                                    data_happy[14, "Estimate"] * input$SSZYG +
                                    data_happy[15, "Estimate"] * input$IBZYG +
                                    data_happy[16, "Estimate"] * input$SBZYG +
                                    data_happy[17, "Estimate"] * input$ISSC +
                                    data_happy[18, "Estimate"] * input$SSSC +
                                    data_happy[19, "Estimate"] * input$IBSC +
                                    data_happy[20, "Estimate"] * input$SBSC +
                                    data_happy[21, "Estimate"] * input$ISSKT +
                                    data_happy[22, "Estimate"] * input$SSSKT +
                                    data_happy[23, "Estimate"] * input$IBSKT +
                                    data_happy[24, "Estimate"] * input$SBSKT +
                                    data_happy[25, "Estimate"] * input$ISDIA +
                                    data_happy[26, "Estimate"] * input$SSDIA +
                                    data_happy[27, "Estimate"] * input$IBDIA +
                                    data_happy[28, "Estimate"] * input$SBDIA )
                        Happy <- round((1/(1+exp(-Happy))) * 100, 2)
                        
                        Scared <- (data_scared[1, "Estimate"] +
                                    data_scared[2, "Estimate"] * input$Trial +
                                     data_scared[3, "Estimate"] * input$Age +
                                     data_scared[4, "Estimate"] * input$Vasvalue +
                                     data_scared[5, "Estimate"] * Sex +
                                     data_scared[6, "Estimate"] * input$AQ +
                                     data_scared[7, "Estimate"] * input$EQ +
                                     data_scared[8, "Estimate"] * input$LSAS +
                                     data_scared[9, "Estimate"] * input$ISCOR +
                                     data_scared[10, "Estimate"] * input$SSCOR +
                                     data_scared[11, "Estimate"] * input$IBCOR +
                                     data_scared[12, "Estimate"] * input$SBCOR +
                                     data_scared[13, "Estimate"] * input$ISZYG +
                                     data_scared[14, "Estimate"] * input$SSZYG +
                                     data_scared[15, "Estimate"] * input$IBZYG +
                                     data_scared[16, "Estimate"] * input$SBZYG +
                                     data_scared[17, "Estimate"] * input$ISSC +
                                     data_scared[18, "Estimate"] * input$SSSC +
                                     data_scared[19, "Estimate"] * input$IBSC +
                                     data_scared[20, "Estimate"] * input$SBSC +
                                     data_scared[21, "Estimate"] * input$ISSKT +
                                    data_scared[22, "Estimate"] * input$SSSKT +
                                     data_scared[23, "Estimate"] * input$IBSKT +
                                     data_scared[24, "Estimate"] * input$SBSKT +
                                    data_scared[25, "Estimate"] * input$ISDIA +
                                     data_scared[26, "Estimate"] * input$SSDIA +
                                     data_scared[27, "Estimate"] * input$IBDIA +
                                     data_scared[28, "Estimate"] * input$SBDIA )
                        Scared <- round((1/(1+exp(-Scared))) * 100, 2)
                        
                        Neutral <- (data_neu[1, "Estimate"] +
                                      data_neu[2, "Estimate"] * input$Trial +
                                      data_neu[3, "Estimate"] * input$Age +
                                      data_neu[4, "Estimate"] * input$Vasvalue +
                                      data_neu[5, "Estimate"] * Sex +
                                      data_neu[6, "Estimate"] * input$AQ +
                                      data_neu[7, "Estimate"] * input$EQ +
                                      data_neu[8, "Estimate"] * input$LSAS +
                                      data_neu[9, "Estimate"] * input$ISCOR +
                                      data_neu[10, "Estimate"] * input$SSCOR +
                                      data_neu[11, "Estimate"] * input$IBCOR +
                                     data_neu[12, "Estimate"] * input$SBCOR +
                                      data_neu[13, "Estimate"] * input$ISZYG +
                                      data_neu[14, "Estimate"] * input$SSZYG +
                                      data_neu[15, "Estimate"] * input$IBZYG +
                                      data_neu[16, "Estimate"] * input$SBZYG +
                                      data_neu[17, "Estimate"] * input$ISSC +
                                      data_neu[18, "Estimate"] * input$SSSC +
                                      data_neu[19, "Estimate"] * input$IBSC +
                                      data_neu[20, "Estimate"] * input$SBSC +
                                      data_neu[21, "Estimate"] * input$ISSKT +
                                      data_neu[22, "Estimate"] * input$SSSKT +
                                      data_neu[23, "Estimate"] * input$IBSKT +
                                      data_neu[24, "Estimate"] * input$SBSKT +
                                      data_neu[25, "Estimate"] * input$ISDIA +
                                      data_neu[26, "Estimate"] * input$SSDIA +
                                      data_neu[27, "Estimate"] * input$IBDIA +
                                      data_neu[28, "Estimate"] * input$SBDIA )
                        Neutral <- round((1/(1+exp(-Neutral))) * 100, 2)
                        
                        Sad <- (data_sad[1, "Estimate"] +
                                  data_sad[2, "Estimate"] * input$Trial +
                                  data_sad[3, "Estimate"] * input$Age +
                                  data_sad[4, "Estimate"] * input$Vasvalue +
                                  data_sad[5, "Estimate"] * Sex +
                                  data_sad[6, "Estimate"] * input$AQ +
                                  data_sad[7, "Estimate"] * input$EQ +
                                  data_sad[8, "Estimate"] * input$LSAS +
                                  data_sad[9, "Estimate"] * input$ISCOR +
                                  data_sad[10, "Estimate"] * input$SSCOR +
                                  data_sad[11, "Estimate"] * input$IBCOR +
                                  data_sad[12, "Estimate"] * input$SBCOR +
                                  data_sad[13, "Estimate"] * input$ISZYG +
                                  data_sad[14, "Estimate"] * input$SSZYG +
                                  data_sad[15, "Estimate"] * input$IBZYG +
                                  data_sad[16, "Estimate"] * input$SBZYG +
                                  data_sad[17, "Estimate"] * input$ISSC +
                                  data_sad[18, "Estimate"] * input$SSSC +
                                  data_sad[19, "Estimate"] * input$IBSC +
                                  data_sad[20, "Estimate"] * input$SBSC +
                                  data_sad[21, "Estimate"] * input$ISSKT +
                                  data_sad[22, "Estimate"] * input$SSSKT +
                                  data_sad[23, "Estimate"] * input$IBSKT +
                                  data_sad[24, "Estimate"] * input$SBSKT +
                                  data_sad[25, "Estimate"] * input$ISDIA +
                                  data_sad[26, "Estimate"] * input$SSDIA +
                                  data_sad[27, "Estimate"] * input$IBDIA +
                                  data_sad[28, "Estimate"] * input$SBDIA )
                        Sad <- round((1/(1+exp(-Sad))) * 100, 2)
                        
                        
                        prob <- c(Angry, Happy, Scared, Neutral, Sad)
         
                
                        emotions <- c("Angry", "Happy", "Scared", "Neutral", "Sad")
                        
                        col=c("red","orange","yellow","blue","green")
                        pie(prob, labels = prob, main = title, col = col)
                        legend("topright", emotions , cex = 0.8, fill = col)
                               
                        
                
                        })
}

   
  
    #output$pred <- renderText({paste("The estimation of the sumscore for angry is", calc())})
    

# Run the application 
shinyApp(ui = ui, server = server)
