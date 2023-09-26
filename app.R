library(shiny)
library(tidyverse)

NUM_MARBLES = 15
S0 = 14
I0 = 1
R0 = 0
stopifnot(S0+I0+R0==NUM_MARBLES)

initialState = rep(1, NUM_MARBLES)
initialState[runif(I0, 1, NUM_MARBLES+1)] = 2
initialState[runif(R0, 1, NUM_MARBLES+1)] = 3

stateIndicesToNames <- function(states) {
  c('S','I','R')[states] %>% factor(levels = c('S','I','R'))
}

ui = fluidPage(
  titlePanel("Marbles"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("numMarbles", "Number of marbles", min = 1, max = 100, value = NUM_MARBLES),
      sliderInput("numSusceptible", "Number of susceptible", min = 0, max = 100, value = S0),
      sliderInput("numInfected", "Number of infected", min = 0, max = 100, value = I0),
      sliderInput("numRecovered", "Number of recovered", min = 0, max = 100, value = R0),
      actionButton("reset", "Reset"),
      actionButton("shuffle", "Shuffle"),
      actionButton("step", "Step"),
      tableOutput('table')
    ),
    mainPanel(
      plotOutput("marbles"),
      plotOutput('linePlot')
    )
  )
)

server = function(input, output, session) {
  state = reactiveValues(state = initialState)
  states = reactiveVal()
  tbStates = reactiveVal(tibble(step=0,
                                S=sum(initialState==1),
                                I=sum(initialState==2),
                                R=sum(initialState==3)))
  observeEvent(input$reset, {
    state$state = rep(1, input$numMarbles)
    state$state[runif(input$numInfected, 1, input$numMarbles+1)] = 2
    state$state[runif(input$numRecovered, 1, input$numMarbles+1)] = 3
    # Update tbStates
    stateIndicesToNames(state$state) %>%
      table() %>%
      as_tibble() %>%
      pivot_wider(names_from='.', values_from=n) %>%
      transmute(step=0, S=S, I=I, R=R) %>%
      tbStates()
  })
  observeEvent(input$shuffle, {
    old_state = state$state
    new_state = shuffle(old_state)
    state$state = new_state
  })
  observeEvent(input$step, {
    old_state = state$state
    new_state = step(old_state)
    states(list(old_state, new_state))
    state$state = new_state
    tbStates() %>%
      add_row(tibble(step=max(tbStates()$step)+1,
              S=sum(state$state==1),
              I=sum(state$state==2),
              R=sum(state$state==3))) %>%
      tbStates()
  })
  observeEvent(input$run, {
    observe({
      invalidateLater(1000, session)
      new_state = state$state
      new_state = shuffle(new_state)
      new_state = step(new_state)
      state$state = new_state
    })
  })
  observeEvent(input$stop, {
    stopApp()
  })
  output$table = renderTable({
    tbStates()
  }, digits=0)
  output$linePlot = renderPlot({
    tbStates() %>%
      pivot_longer(c(S,I,R)) %>%
      mutate(name=factor(name, levels=c('S','I','R'))) %>%
      ggplot(aes(x=step, y=value, color=name)) +
        geom_line() +
        scale_color_manual(values=c('S'='#22CC33','I'='black','R'='gray')) +
        theme_minimal() +
        theme(legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(margin = margin(0, 1.3, 2.4, -1.3, "cm"),
                                       colour = 'red',
                                       size = 10,
                                       face = 'bold')) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))
  })
  output$marbles = renderPlot({
    current_state = stateIndicesToNames(state$state)
    past_states <- states()
    if (!is.null(past_states)) {
      tibble(past=stateIndicesToNames(past_states[[1]]),
             current=stateIndicesToNames(past_states[[2]])) %>%
        rowid_to_column() %>%
        pivot_longer(c(past, current)) %>%
        mutate(y=if_else(name=='past', 0, 1)) %>%
        ggplot(aes(x=((rowid-1)%%NUM_MARBLES)+1, y=y, fill=value)) +
        geom_point(shape=21, size=20) +
        scale_fill_manual(values=c('S'='#22CC33','I'='black','R'='white')) +
        coord_cartesian(ylim=c(-1,2)) +
        theme_void() +
        theme(legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(margin = margin(0, 1.3, 2.4, -1.3, "cm"),
                                       colour = 'red',
                                       size = 10,
                                       face = 'bold')) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))
    } else {
      tibble(val=current_state) %>%
        rowid_to_column() %>%
        mutate(y=0) %>%
        ggplot(aes(x=rowid, y=y, fill=val)) +
          geom_point(shape=21, size=20) +
          scale_fill_manual(values=c('S'='#22CC33','I'='black','R'='white')) +
        coord_cartesian(ylim=c(-1,2)) +
        theme_void() +
        theme(legend.position="bottom",
              legend.title=element_blank(),
              legend.text=element_text(margin = margin(0, 1.3, 2.4, -1.3, "cm"),
                                       colour = 'red',
                                       size = 10,
                                       face = 'bold')) +
        guides(fill=guide_legend(nrow=1,byrow=TRUE))
    }
  })
}

shuffle = function(state) {
  sample(state, length(state), replace = F)
}

step = function(state) {
  susceptible_indices = which(state == 1)
  infected_indices = which(state == 2)
  recovered = which(state == 3)

  new_infected_indices <- union(infected_indices-1, infected_indices+1) %>%
    setdiff(c(0, length(state)+1)) %>%
    intersect(susceptible_indices)

  state_updated <- state
  state_updated[infected_indices] <- 3
  state_updated[new_infected_indices] <- 2

  state_updated
}

shinyApp(ui = ui, server = server)
