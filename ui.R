library(shiny)

shinyUI(
  fluidPage(
    theme = "bootstrap.css",
    withMathJax(),
    fluidRow(
      column(5,
             titlePanel("Trafic Simulation"),
             h5("Course AH2174, KTH"),
             helpText("Christian Savemark, PhD Student"),
             offset = 2
      )
    ),
    hr(),
    fluidRow(column(5, h5("Motivation"), offset = 2)),
    fluidRow(column(5, p("This is a report for a homework exercise in the trafic simulation course AH2174 at KTH but unlike most reports, it is partially dynamic. 
                         Instead of being just a program with a graphical user interface (GUI) or being just a static report, we combine the two into what perhaps could
                         be described as a general-purpose user interface (GPUI)."), offset = 2)),
    # Perhaps in a not so distant future, several such GPUIs could be linked to create a ''tile'': a 
    # series of papers/GPUIs combined to model something [larger].
    fluidRow(column(5, h5("Problem statement"), offset = 2)),
    fluidRow(column(5, 
                    p("The following simulations deal with a very simplified travel network with two links with common origin and common destination. Each 
                      day, commuters choose one of the links (independently), based on some sort of experience of past travel times. We model choice by using
                      a probability distribution which we repeatedly draw from, average travel times by some function which has demand (i.e. choice) as independent variable
                      and lastly, we model memory of the commuters as a weighting-function of percieved travel time and actual travel time."), 
                    p("Acceptable percieved/actual maximum travel time is 60 minutes. We will report the number of failures (defined as any avg. travel time greater than 60 minutes)
                      as a percentage of the total time steps (times two) and because of the Law of Large Numbers this holds as a approximation to the model's unknown but implied c.d.f. (1-c.d.f)"),
                    offset = 2)
             ),
    fluidRow(column(5, h5("Global parameters"), offset = 2)),
    fluidRow(
      column(5,
             fluidRow(
               column(4,
                      numericInput("T", label = "Time steps \\(T\\)", value = 75)
               ),
               column(4,
                      numericInput("Nmin", label = "#Vehicles (minimum)", value = 500)
               ),
               column(4,
                      numericInput("Nmax", label = "#Vehicles (maximum)", value = 500)
               )
             ),
             p("The number of vehicles \\(N_t \\sim \\text{U}(\\text{minimum}, \\text{maximum})\\) and the number of time steps \\(1, \\dots, T\\) can be set here and will be applied to all simulations."),
             offset = 2
      )
    ),
    fluidRow(column(5, h5("1. Route choice model"), offset = 2)),
    fluidRow(
      column(2,
             radioButtons(
               "probability",
               label = "Probability",
               choices = list(Reverse = "reverse", Logit = "logit"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.probability == 'logit'",
               numericInput("sigma", label = "\\(\\sigma\\)", value = 0.001, min = 0, max = 1, step = 0.00001)
             )
      ),
      column(5,
             p("The first route choice model which is used in the lecture notes and which we will call the reverse choice model here, is defined as
               $$p_1 = \\frac{t_2}{t_1+t_2}, p_2 = \\frac{t_1}{t_1+t_2}.$$
               A more sophisticated route choice model is the multinomial logit choice model, developed by McFadden and Ben-Akiva, among others. Because we are only 
               looking at a very simple travel network with two links, here it is defined as
               $$p_i = \\frac{e^{\\sigma (-t_i)}}{\\sum_{k} e^{\\sigma (-t_k)}} \\quad \\hbox{for \\(i = 1, 2\\)}.$$ The effect of \\(\\sigma > 0\\), which can be set to the left if 
               Logit is chosen, is a parameter of the variance of each (Gumbel distributed) error term in the model. Two standard results are
               $$\\lim_{\\sigma \\rightarrow 0} \\frac{e^{\\sigma (-t_i)}}{\\sum_{k = 1}^{K} e^{\\sigma (-t_k)}} = \\frac{1}{K}$$
               and
               $$\\lim_{\\sigma \\rightarrow \\infty} \\frac{e^{\\sigma (-t_i)}}{\\sum_{k = 1}^{K} e^{\\sigma (-t_k)}} = 1 \\quad \\hbox{for that particular \\(i\\) such that \\(-t_i > -t_j\\) for all \\(j \\neq i\\)}.$$
               These can be illustrated by choosing Logit to the left and setting \\(\\sigma = 1\\) for very high variance: all vehicles will jump to what was the best option (the day before), creating a high maximum travel time on one of the links
               while the other link will have free flow travel time. This will be reversed by setting a lower \\(\\sigma\\)."),
             textOutput("simfails1")
      ),
      column(5, 
             plotOutput("simplot1")
      )
    ),
    fluidRow(column(5, h5("2. Travel times"), offset = 2)),
    fluidRow(
      column(2,
             radioButtons(
               "traveltime",
               label = "Travel times",
               choices = list(Linear = "linear", Power = "power"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.traveltime == 'power'",
               numericInput("freeflow", label = "Free flow \\(t^0_i\\) (minutes)", value = 10),
               numericInput("capacity", label = "Capacity \\(C_i\\)", value = 250),
               numericInput("alpha", label = "\\(\\alpha\\)", value = 4.2, min = 0, step = 0.1),
               numericInput("beta", label = "\\(\\beta\\)", value = 2, min = 1, step = 0.1)
             )
      ),
      column(5,
             p("The volume-delay function in the lecture linear and is defined as
               $$t_i = 10+\\frac{10}{60}q_i.$$
               This function can be generalized into
               $$t_i = t^0_i + \\alpha\\left(\\frac{q_i}{C_i}\\right)^\\beta t^0_i \\quad \\hbox{where \\(t^0_i>0, C_i>0, \\alpha \\geq 0, \\beta \\geq 1\\)}$$
               and we will simply call this a volume-delay power function."),
             p("If \\(\\alpha = 1\\) and \\(\\beta=1\\) then a capacity of 250 will give about twice the free flow travel time. Any higher value of \\(\\alpha\\) or \\(\\beta\\)
               will of course increase travel time, with \\(\\alpha\\) as a shift parameter and \\(\\beta\\) as a scale parameter. Assuming then, that \\(C_i = 250\\) and setting \\(\\alpha = \\frac{25}{6}\\), \\(\\beta = 1\\) would recover the original (linear) function."),
             p("If we set \\(C_i = 250, \\alpha = 4.2, \\beta = 2\\) and assume the probability is of logit type with \\(\\sigma = 0.001\\)
               we get the two simulations on top of each other. It is questionable if this makes any sense, however, since we are calibrating one time series to look like another, simpler one.
               Also, the chosen parameters does not seem to be ''unique'' for a given \\(\\sigma\\). Real data to calibrate against is thus needed."),
             textOutput("simfails2")
      ),
      column(5,
             plotOutput("simplot2")
      )
    ),
    fluidRow(column(5, h5("3. Learning process"), offset = 2)),
    fluidRow(
      column(2,
             radioButtons(
               "learningprocess",
               label = "Learning process",
               choices = list(Last = "last", Smooth = "smooth"),
               inline = TRUE
             ),
             conditionalPanel(
               condition = "input.learningprocess == 'smooth'",
               numericInput("weight", label = "Weight \\(w\\)", value = 0.5, min = 0, max = 1, step = 0.1)
             )
      ),
      column(5,
             p("Now we introduce percieved travel time \\(\\bar{t}^{(d)}_i\\), which might be different from actual travel time \\(t^{(d)}_i\\). The idea is to smooth out previous travel times in
               the memory of our commuters:
               $$\\bar{t}^{(d)}_i = wt^{(d)}_i+(1-w)\\bar{t}^{(d-1)}_i$$
               which is equal to 
               $$wt^{(d)}_i+w(1-w)t^{(d-1)}_i+(1-w)^2\\bar{t}^{(d-2)}_i$$
               and so forth. A larger \\(w\\in(0,1)\\) implies more wight to actual travel time and Last in the options will be the limiting case \\(w=1\\). As can be seen from the graph when \\(w\\) is set to, for example, 0.5, the first day will be an avarage between free flow and actual travel time, 
               hence the steep rise of percieved travel time just after the first time steps and the smaller distances overall between maximums and minimums."),
             textOutput("simfails3")
             ),
      column(5,
             plotOutput("simplot3")
      )
    )
  )
)