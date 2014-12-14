shinyServer(function(input, output) {
  
  values <- reactiveValues()
  tmp <- oe[0,2:8]
  tmp$delta <- integer(0)
  tmp$QALY <- integer(0)
  values$current <- tmp
  values$queue <- tmp
  
  # You can access the value of the widget with input$num, e.g.
  output$value <- renderPrint({ 
    # input$num 
    # AGE MINS_SINCE_INJURY GCS_EYE GCS_MOTOR GCS_VERBAL PUPIL_REACT_LEFT PUPIL_REACT_RIGHT
    nd <- data.frame(
                     AGE=c(input$age),
                     MINS_SINCE_INJURY=c(input$mins),
                     GCS_EYE=c(input$eye),
                     GCS_MOTOR=c(input$motor),
                     GCS_VERBAL=c(input$verbal),
                     PUPIL_REACT_LEFT=c(input$left),
                     PUPIL_REACT_RIGHT=c(input$right),
                     delta=c(0),
                     QALY=c(0))
    # [dim(values$queue)[1]+1,]
    values$current <- nd
    predict(r.glm2,newdata=nd,type="response")[[1]]
    })
  
    output$table <- renderDataTable({

      #knn(nd)
      input$action
      curr <- isolate(values$current)
      curr$delta <- sensitivity(curr,60)
      curr$QALY <- qaly(curr)
      values$queue <- rbind(curr,isolate(values$queue))
      # Order by sensitivity
      values$queue[order(isolate(values$queue$delta),decreasing=TRUE),]
    })

  
})