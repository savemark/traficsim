travel.time <- function(x, type = "linear", ...) {
  # x demand
  # y travel times
  linear <- function(x) {
    y <- 10 + (10/60)*x
    return(y)
  }
  power <- function(x, t0, capacity, alpha = NULL, beta = NULL) {
    if (is.null(alpha))
      alpha <- 0
    if (is.null(beta))
      beta <- 1
    y <- t0*(1+alpha*(x/capacity)^beta)
    return(y)
  }
  switch(type,
         linear = linear(x),
         power = power(x, ...)
  )
}

probability <- function(x, type = "logit", ...) {
  # x travel times
  # y probability
  reverse <- function(x) {
    y <- rev(x)/sum(x)
    return(y)
  }
  logit <- function(x, sigma) {
    # sigma >= 0
    y <- exp(-sigma*x)/sum(exp(-sigma*x))
    return(y)
  }
  switch(type, 
         reverse = reverse(x),
         logit = logit(x, ...)
  )
}

simulation <- function(m, 
                       min = 250,
                       max = 750,
                       w = 1, 
                       first.obs = FALSE, 
                       probability.type = "reverse", 
                       travel.time.type = "linear",
                       ...) {
  # n individuals
  # m time steps
  # w weight
  args <- list(...)
  print(args)
  time <- matrix(NA, (m+1), 2)
  percieved.time <- matrix(NA, (m+1), 2)
  demand <- matrix(NA, (m+1), 2)
  demand[1, ] <- c(0, 0)
  time[1, ] <- travel.time(demand[1, ])
  percieved.time[1, ] <- time[1, ]
  for (i in 1:m) {
    n <- runif(1, min = min, max = max)
    choice <- sample.int(2, 
                         size = n, 
                         replace = TRUE, 
                         prob = probability(time[i, ], 
                                            type = probability.type, 
                                            sigma = args[["sigma"]]))
    demand[(i+1), ] <- tabulate(choice, nbins = 2)
    time[(i+1), ] <- travel.time(demand[(i+1), ], 
                                 type = travel.time.type, 
                                 t0 = args[["t0"]], 
                                 capacity = args[["capacity"]], 
                                 alpha = args[["alpha"]], 
                                 beta = args[["beta"]])
    percieved.time[(i+1), ] <- w*time[(i+1), ]+(1-w)*percieved.time[i, ]
  }
  if (!first.obs) {
    demand <- demand[-1 , ]
    time <- time[-1, ]
    percieved.time <- percieved.time[-1, ]
  }
  return(list(
    time = time,
    percieved.time = percieved.time,
    demand = demand
  ))
}

simulation.plot <- function(x, y, T1, main = NULL, leg = NULL) {
  # x first sim time/perc. time
  # y second
  max1 <- apply(x, 1, max)
  min1 <- apply(x, 1, min)
  max2 <- apply(y, 1, max)
  min2 <- apply(y, 1, min)
  r <- range(c(max1, max2, min1, min2))
  plot(max1, type = "n", ylim = r, xlab = "", ylab = "")
  polygon(c(1:T1, T1:1), c(min1, rev(max1)), col = rgb(1, 0, 0, 0.5), border = FALSE)
  polygon(c(1:T1, T1:1), c(min2, rev(max2)), col = rgb(0, 0, 1, 0.5), border = FALSE)
  title(main = main, sub = NULL, xlab = "Time step", ylab = "Time (minutes)",
        line = NA, outer = FALSE)
  legend("topright", 
         leg, # Puts text in the legend
         lty = c(1, 1), # Gives the legend appropriate symbols (lines)
         col = c("red", "blue"))
  points(max2, type = "n")
  lines(max1, col = "red")
  lines(min1, col = "red")
  lines(max2, col = "blue")
  lines(min2, col = "blue")
}