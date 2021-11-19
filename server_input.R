observeEvent(c(
    input$res_data,
    input$auto_inp
), {
        req(input$res_data)
        extension <- tools::file_ext(input$res_data$name)
        switch(extension,
               RData = load(input$res_data$datapath),
               Rdata = load(input$res_data$datapath),
               validate("Invalid file : Need a .RData file")
        )
        # Find the symbol column
        my_values$counts <- find_symb_col(dataMerged)
        # potentiellement lourd
        my_values$counts %<>% mutate(across(everything(), na_if, "")) 
        # Avoids duplicated symbol names
        my_values$counts$symbol %<>% tidy_symbols() 
        my_values$all_results <- vector(mode = "list", length = length(all_results))
        names(my_values$all_results) <- names(all_results) 
        for (contraste in names(all_results)) {
            my_values$all_results[[contraste]] <- find_symb_col(all_results[[contraste]])
            my_values$all_results[[contraste]] %<>%
                mutate(across(everything(), na_if, "")) 
            my_values$all_results[[contraste]]$symbol %<>% 
                tidy_symbols()
        }
        my_values$rld <- rld_df
        my_values$config <- configuration
        my_values$contrastes <- contrasteList
    }
})



res <- eventReactive({
    input$contrast_act
    my_values$all_results
}, {
    my_values$all_results[[input$contrast_act]]
})
