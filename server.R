################# PROJECT 2 - TEXT MINING USING WOMENS CLOTHING E-COMMERCE DATA ##############
##******************************************************************************************##


server <- function(input, output, session) {
 
  terms <- reactive({
       input$update
    isolate({
      withProgress({
        setProgress(message = "Please wait ...corpus being processed...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  
  terms_2 <- reactive({
    input$update
    isolate({
      getConditionedDataFrame(terms())
    })
  })
  
  terms_3 <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Please wait ...corpus being processed...")
        model(input$selection)
      })
    })
  })
  
  terms_4 <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Please wait ...corpus being processed...")
        hierarchical(input$selection)
      })
    })
  })
  
  
  
  

  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(3.5,0.25),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  ########word frequency barplot##########
  hues <- c(60:330)
  output$freqPlot <- renderPlot({
    v <- terms_2()
    ggplot(v, aes(x=reorder(term, freq), y=freq, fill = as.factor(term))) +
      geom_bar(stat = "identity", position = "dodge", col= "black") + xlab("Terms") + ylab("Count") + 
      scale_fill_hue(c = sample(hues, 1)) + 
      ggtitle("Word Frequency") + 
      theme(axis.text.x = element_text(angle = 90)) +
      guides(fill=FALSE)
  })
  
 
  
  #classification plot
  
  output$plot3 <- renderPlot({
    v <- terms_4()
    plot(v)
    rect.hclust(v, k = input$clusters) # cut tree into 9 clusters
    
  })
  
  
}

#shinyApp(ui, server)
