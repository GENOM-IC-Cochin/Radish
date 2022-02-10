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
    tmp[[contraste]] <- data_loaded()[["all_results"]][[contraste]] %>% dplyr::rename("symbol" = 9)
    tmp[[contraste]] %<>%
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
    input$contrast_act
    all_results()
}, {
    all_results()[[input$contrast_act]]
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