observeEvent(
    input$res_data
, {
        req(input$res_data)
        extension <- tools::file_ext(input$res_data$name)
        switch(extension,
               RData = load(input$res_data$datapath),
               Rdata = load(input$res_data$datapath),
               validate("Invalid file : Need a .RData file")
        )
        # Permet de remplacer un nom variable (hgcn_symbol, mgi_symbol) par un nom fixe
        my_values$counts <- dataMerged %>% dplyr::rename("symbol" = ncol(dataMerged) - 1)
        # potentiellement lourd
        my_values$counts %<>% mutate(across(everything(), na_if, "")) 
        # Avoids duplicated symbol names
        my_values$counts$symbol %<>% tidy_symbols() 
        my_values$all_results <- vector(mode = "list", length = length(all_results))
        names(my_values$all_results) <- names(all_results) 
        for (contraste in names(all_results)) {
            my_values$all_results[[contraste]] <- all_results[[contraste]] %>% dplyr::rename("symbol" = 9)
            my_values$all_results[[contraste]] %<>%
                mutate(across(everything(), na_if, "")) 
            my_values$all_results[[contraste]]$symbol %<>% 
                tidy_symbols()
        }
        my_values$rld <- rld
        my_values$config <- configuration
        my_values$contrastes <- contrasteList
        my_values$txi.rsem <- txi.rsem
})



res <- eventReactive({
    input$contrast_act
    my_values$all_results
}, {
    my_values$all_results[[input$contrast_act]]
})
