################# PROJECT 2 - TEXT MINING USING WOMENS CLOTHING E-COMMERCE DATA ##############
##******************************************************************************************##


library(shiny)

ui <- fluidPage(
  

  titlePanel("Text Mining on Women's Clothing Reviews"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("selection", "Choose a Department:",
                  choices = departments),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 30, value = 4),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 250,  value = 24),
      sliderInput("clusters",
                  "Number of Clusters:",
                  min = 1,  max = 9,  value = 3),
      actionButton("update", "Go"),
      hr()
    ),
    
   
    mainPanel(
      plotOutput("freqPlot"),
      plotOutput("plot"),
      plotOutput(outputId = 'plot3')
      
    )
  )
)


