shinyServer(function(input, output) {
  
  values <- reactiveValues()
  values$current <- oe[0,2:8]
  values$queue <- oe[0,2:8]
  
  # You can access the value of the widget with input$num, e.g.
  output$value <- renderPrint({ 
    # input$num 
    # AGE MINS_SINCE_INJURY GCS_EYE GCS_MOTOR GCS_VERBAL PUPIL_REACT_LEFT PUPIL_REACT_RIGHT
    nd <- data.frame(AGE=c(input$age),
                     MINS_SINCE_INJURY=c(input$mins),
                     GCS_EYE=c(input$eye),
                     GCS_MOTOR=c(input$motor),
                     GCS_VERBAL=c(input$verbal),
                     PUPIL_REACT_LEFT=c(input$left),
                     PUPIL_REACT_RIGHT=c(input$right))
    # [dim(values$queue)[1]+1,]
    values$current <- nd
    predict(r.glm2,newdata=nd,type="response")[[1]]
    })
  
    output$table <- renderDataTable({

      #knn(nd)
      input$action
      values$queue <- rbind(isolate(values$current),isolate(values$queue))
      values$queue
    })

  
})