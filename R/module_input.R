# Module for input

# All the objects expected in the input
expected_data <- c(
    "dataMerged",
    "all_results",
    "configuration",
    "contrasteList",
    "rld",
    "txi.rsem"
)

# UI ---------------------------------------------------------------------------
InputUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Result input",
    width = 12,
    fileInput(ns("res_data"),
              "Results"
    ),
    htmlOutput(ns("check_data"))
  )
}


# Server -----------------------------------------------------------------------
InputServer <- function(id, contrast_act, input, output, session) {
  stopifnot(is.reactive(contrast_act))
  moduleServer(id, function(input, output, session) {
    
    data_loaded <- eventReactive(input$res_data, {
      req(input$res_data)
      extension <- tools::file_ext(input$res_data$name)
      tmp <- switch(extension,
                    rds = readRDS(input$res_data$datapath),
                    RDS = readRDS(input$res_data$datapath),
                    validate("Invalid file : Need a .rds file")
      )
      validate(need(all(expected_data == names(tmp)), "Missing objects in loaded file"))
      tmp
    })
    
    counts <- eventReactive(data_loaded(),{
      req(data_loaded())
      # Permet de remplacer un nom variable (hgcn_symbol, mgi_symbol) par un nom fixe
      tmp <- data_loaded()[["dataMerged"]] %>%
        dplyr::rename("symbol" = dplyr::contains("symbol"))
      # potentiellement lourd
      tmp %<>% mutate(across(everything(), na_if, "")) 
      # Avoids duplicated symbol names
      tmp$symbol %<>% tidy_symbols()
      tmp
    })
    
    all_results <- eventReactive(data_loaded(),{
      req(data_loaded())
      tmp <- vector(mode = "list", length = length(data_loaded()[["all_results"]]))
      names(tmp) <- names(data_loaded()[["all_results"]]) 
      for (contraste in names(data_loaded()[["all_results"]])) {
        tmp[[contraste]] <- data_loaded()[["all_results"]][[contraste]] %>% 
          dplyr::rename("symbol" = dplyr::contains("symbol"))
        tmp[[contraste]] %<>%
        # Noms de gènes "" -> NA
          mutate(across(everything(), na_if, "")) 
        tmp[[contraste]]$symbol %<>% 
          tidy_symbols()
      }
      tmp
    })
    
    rld <- eventReactive(data_loaded(),{
      req(data_loaded())
      data_loaded()[["rld"]]
    })
    
    config <- eventReactive(data_loaded(), {
      req(data_loaded())
      data_loaded()[["configuration"]]
    })
    
    contrastes <- eventReactive(data_loaded(), {
      req(data_loaded())
      data_loaded()[["contrasteList"]]
    })
    
    txi.rsem <- eventReactive(data_loaded(), {
      req(data_loaded())
      data_loaded()[["txi.rsem"]]
    })
    
    
    res <- eventReactive({
      contrast_act()
      all_results()
    }, {
      all_results()[[contrast_act()]]
    })
    
    output$check_data <- renderUI({
      req(all_results(),
          counts(),
          config()
      )
      HTML(paste("<br> Read <b>",
                 all_results() %>% length(),
                 "</b> contrasts, and <b>",
                 config() %>% nrow(),
                 "</b> samples"))
      
    })
    list(
      res = res,
      counts = counts,
      contrastes = contrastes,
      rld = rld,
      config = config,
      txi.rsem = txi.rsem,
      all_results_names = reactive(names(all_results()))
    )
  })
}


# Test App ---------------------------------------------------------------------
InputApp <- function() {
  ui <- fluidPage(InputUI("test"))
  
  server <- function(input, output, session) {
    InputServer("test", reactive("Cond1_vs_Control"), input, output, session)
  }
  shinyApp(ui, server)
}