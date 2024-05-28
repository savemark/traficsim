library(shiny)
source("functions.R")

shinyServer(
  function(input, output) {
    simInputA <- reactive({
      sim1 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = 1,
                         probability.type = "reverse",
                         travel.time.type = "linear")
      sim2 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = 1, 
                         probability.type = input$probability,
                         sigma = input$sigma,
                         travel.time.type = "linear")
      return(list(sim1 = sim1, sim2 = sim2))
    })
    output$simfails1 <- renderText({
      return(paste("Percentage of unacceptable travel times (blue scenario): ", round(100*length(simInputA()$sim2$time[simInputA()$sim2$time>60])/(2*input$T), 2), "%"))
    })
    output$simplot1 <- renderPlot({
      sim1 <- simInputA()$sim1
      sim2 <- simInputA()$sim2
      simulation.plot(sim1$time, sim2$time, T1 = input$T, 
                      main = "Maximum and minimum percieved travel time for two simulations",
                      leg = c("reverse", input$probability))
    })
    simInputB <- reactive({
      sim1 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = 1,
                         probability.type = input$probability,
                         sigma = input$sigma,
                         travel.time.type = "linear")
      sim2 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = 1, 
                         probability.type = input$probability,
                         sigma = input$sigma,
                         travel.time.type = input$traveltime, 
                         t0 = rep(input$freeflow, time = 2), 
                         capacity = rep(input$capacity, time = 2),
                         alpha = input$alpha, 
                         beta = input$beta)
      return(list(sim1 = sim1, sim2 = sim2))
    })
    output$simfails2 <- renderText({
      return(paste("Percentage of unacceptable travel times (blue scenario): ", round(100*length(simInputB()$sim2$time[simInputB()$sim2$time>60])/(2*input$T), 2), "%"))
    })
    output$simplot2 <- renderPlot({
      sim1 <- simInputB()$sim1
      sim2 <- simInputB()$sim2
      simulation.plot(sim1$time, sim2$time, T1 = input$T, 
                      main = "Maximum and minimum percieved travel time for two simulations",
                      leg = c(paste(input$probability, "and linear"), paste(input$probability, "and", input$traveltime)))
    })
    simInputC <- reactive({
      w2 <- 1
      sim1 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = 1,
                         probability.type = input$probability,
                         sigma = input$sigma,
                         travel.time.type = input$traveltime,
                         t0 = rep(input$freeflow, time = 2), 
                         capacity = rep(input$capacity, time = 2),
                         alpha = input$alpha, 
                         beta = input$beta)
      if (input$learningprocess == "smooth")
        w2 <- input$weight
      sim2 <- simulation(m = input$T, 
                         min = input$Nmin,
                         max = input$Nmax,
                         w = w2, 
                         probability.type = input$probability,
                         sigma = input$sigma,
                         travel.time.type = input$traveltime, 
                         t0 = rep(input$freeflow, time = 2), 
                         capacity = rep(input$capacity, time = 2),
                         alpha = input$alpha, 
                         beta = input$beta)
      return(list(sim1 = sim1, sim2 = sim2))
    })
    output$simfails3 <- renderText({
      return(paste("Percentage of unacceptable travel times (blue scenario): ", round(100*length(simInputC()$sim2$percieved.time[simInputC()$sim2$percieved.time>60])/(2*input$T), 2), "%"))
    })
    output$simplot3 <- renderPlot({
      sim1 <- simInputC()$sim1
      sim2 <- simInputC()$sim2
      simulation.plot(sim1$percieved.time, sim2$percieved.time, T1 = input$T, 
                      main = "Maximum and minimum percieved travel time for two simulations",
                      leg = c(paste(input$probability, ",", input$traveltime, "and last"), paste(input$probability, ",", input$traveltime, "and", input$learningprocess)))
    })
  }
)