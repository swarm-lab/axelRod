shinyUI(fluidPage(

  fluidRow(

    column(width = 3, align = "center", style = "background-color:#ebb397;",
           div(style = "height:450px;",
               selectInput("strat1", label = h5("Player 1's Strategy"),
                           choices = as.list(strats$name),
                           selected = 1))),

    column(width = 6, align = "center", style = "background-color:#f5f5f5;",
           ggvisOutput("display")),

    column(width = 3, align = "center", style = "background-color:#9ecfbf;",
           div(style = "height:450px;",
               selectInput("strat2", label = h5("Player 2's Strategy"),
                           choices = as.list(strats$name),
                           selected = 1)))
  ),

  fluidRow(style = "background-color:#f5f5f5;",

           column(width = 2, align = "center", offset = 2,
                  selectInput("tournament", label = h5("Tournament type"),
                              choices = list("One time" = "onetime", "Repeated" = "repeated"),
                              selected = 1)),

           column(width = 2, align = "center",
                  sliderInput("rounds", label = h5("Number of rounds"), min = 1,
                              max = 100, value = 20)),

           column(width = 2, align = "center",
                  sliderInput("reps", label = h5("Number of replicates"), min = 1,
                              max = 100, value = 20)),

           column(width = 2, align = "center",
                  h5(HTML("&nbsp;")),
                  actionButton("play", "Play", width = "100%"))
  )

))
