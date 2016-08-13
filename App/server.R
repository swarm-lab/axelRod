shinyServer(function(input, output) {

  react <- reactiveValues(reset_draw = 0, draw = 0)

  observe({

    react$reset_draw
    counter <<- 1
    steps <<- max(dat$rep)
    isolate({ react$draw <- react$draw + 1 })

  })

  observe({

    react$draw

    if (counter <= steps) {

      filter(dat, rep == counter) %>%
        ggvis(x = ~player, y = ~cum_score, fill = ~player, stroke = ~player) %>%
        layer_bars() %>%
        scale_nominal("fill", range = c("#ebb397", "#9ecfbf")) %>%
        scale_nominal("stroke", range = c("#ebb397", "#9ecfbf")) %>%
        # scale_ordinal("x", label = c("Player 1", "Player 2")) %>%
        scale_numeric("y", domain = c(0, max(dat$cum_score))) %>%
        hide_legend(c("fill", "stroke")) %>%
        add_tooltip(all_values, "hover") %>%
        add_axis("x", title = "", properties = axis_props(labels = list(fontSize = 18))) %>%
        add_axis("y", title = "", properties = axis_props(labels = list(fontSize = 18))) %>%
        add_axis("y", title = "", properties = axis_props(labels = list(fontSize = 18)), orient = "right") %>%
        set_options(width = "auto", height = "450px", resizable = FALSE, duration = 30) %>%
        bind_shiny("display")

      counter <<- counter + 1

      invalidateLater(60, NULL)

    }

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
          mutate(cum_score = cumsum(score)) %>%
          ungroup

        react$reset_draw <- react$reset_draw + 1

      })
    }

  })

})
