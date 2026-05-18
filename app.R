# Run the PSI Proxy Severity Shiny app
# setwd to this shiny/ folder, then: shiny::runApp()

library(shiny)


source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
