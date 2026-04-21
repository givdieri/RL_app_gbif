mod_species_ui <- function(id, species_choices) {
  ns <- shiny::NS(id)
  shiny::selectInput(ns('species'), 'Species', choices = species_choices)
}

mod_species_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive(input$species)
  })
}
