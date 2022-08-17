if (isTRUE(getOption("shiny.testmode"))) {
  pkgload::load_all(rprojroot::is_r_package$find_file())
}

shiny::shinyApp(
         ui = Radish::radish_ui(),
         server = Radish::radish_server
         )
