shinyServer(function(input, output) {

  react <- reactiveValues(reset_draw = 0, draw = 0)

  observe({
    react$reset_draw
    counter <<- 1
    steps <<- max(dat$rep)
    isolate({ react$draw <- react$draw + 1 })
  })

  react_dat <- reactive({
    react$draw

    if (counter <= steps) {
      counter <<- counter + 1
      invalidateLater(60, NULL)
    }

    dat %>% filter(rep == counter - 1)
  })

  observe({
    if (input$play > 0) {
      isolate({
        idx1 <- strats$name == input$strat1
        idx2 <- strats$name == input$strat2
        tournament <- Tournament$new(type = input$tournament,
                                     players = list("Player 1" = get(strats$fn[idx1]),
                                                    "Player 2" = get(strats$fn[idx2])),
                                     nreps = input$reps, nrounds = input$rounds)
        tournament$play()

        dat <<- group_by(tournament$res, player, rep) %>%
          summarize(score = sum(score)) %>%
          mutate(cum_score = cumsum(score))

        react$reset_draw <- react$reset_draw + 1
      })
    }
  })

  react_dat %>%
    ggvis(x = ~player, y = ~cum_score, fill = ~player, stroke = ~player) %>%
    layer_bars() %>%
    scale_nominal("fill", range = c("#ebb397", "#9ecfbf")) %>%
    scale_nominal("stroke", range = c("#ebb397", "#9ecfbf")) %>%
    hide_legend(c("fill", "stroke")) %>%
    add_tooltip(all_values, "hover") %>%
    add_axis("x", title = "", properties = axis_props(labels = list(fontSize = 18))) %>%
    add_axis("y", title = "y", properties = axis_props(
      labels = list(fontSize = 16),
      title = list(fontSize = 20, stroke = "#f5f5f5", fill = "#f5f5f5"))) %>%
    add_axis("y", title = "y", orient = "right", properties = axis_props(
      labels = list(fontSize = 16),
      title = list(fontSize = 20, stroke = "#f5f5f5", fill = "#f5f5f5"))) %>%
    set_options(width = "auto", height = "450px", resizable = FALSE, duration = 60) %>%
    bind_shiny("display")
})
