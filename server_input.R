res <- eventReactive(c(
    input$inp_res_table,
    input$auto_inp
), {
    if (input$auto_inp) {
        tmp <- vroom::vroom("./NGS21-023_Resultats/Analyses_supervisees/NGS21-023_ensemblGCRm38r101_deseq2_results_contrast_High_vs_Controle.tsv",
            delim = "\t"
        )
        # Find the symbol column
        nom_col <- colnames(tmp)
        col_symb_test <- str_detect(nom_col, "symbol")
        shinyFeedback::feedbackDanger(
            "inp_res_table",
            !any(col_symb_test),
            "The table does not contain a symbol (gene name) column"
        )
        req(any(col_symb_test))
        col_symbol <- which(col_symb_test)
        colnames(tmp)[col_symbol] <- "symbol"
        return(tmp)
    }
    req(input$inp_res_table)

    extension <- tools::file_ext(input$inp_res_table$name)
    tmp <- switch(extension,
        csv = vroom::vroom(input$inp_res_table$datapath, delim = ","),
        tsv = vroom::vroom(input$inp_res_table$datapath, delim = "\t"),
        validate("Invalid file : Need a .csv or .tsv file")
    )

    # Vérifier l'intégrité des données
    val_cols <- c("baseMean",
                  "log2FoldChange",
                  "lfcSE",
                  "stat",
                  "pvalue",
                  "padj") %in% colnames(tmp)
    shinyFeedback::feedbackDanger(
        "inp_res_table",
        !all(val_cols),
        "The table does not contain minimum column names"
    )
    req(all(val_cols))

    # Find the symbol column
    nom_col <- colnames(tmp)
    col_symb_test <- str_detect(nom_col, "symbol")
    shinyFeedback::feedbackDanger(
        "inp_res_table",
        !any(col_symb_test),
        "The table does not contain a symbol (gene name) column"
    )
    req(any(col_symb_test))
    col_symbol <- which(col_symb_test)
    colnames(tmp)[col_symbol] <- "symbol"

    tmp
})

counts <- eventReactive(c(
    input$inp_compt_table,
    input$auto_inp
), {
    if (input$auto_inp) {
        tmp <- vroom::vroom("./NGS21-023_Resultats/Donnees_Normalisees/NGS21-023_ensemblGCRm38r101_deseq2_NormalizedMatrix.tsv",
            delim = "\t"
        )
    } else {
        req(input$inp_compt_table)

        extension <- tools::file_ext(input$inp_compt_table$name)
        tmp <- switch(extension,
            csv = vroom::vroom(input$inp_compt_table$datapath, delim = ","),
            tsv = vroom::vroom(input$inp_compt_table$datapath, delim = "\t"),
            validate("Invalid file : Need a .csv or .tsv file")
        )

        # Vérifier la vraisemblance des données
        shinyFeedback::feedbackWarning(
            "inp_comp_table",
            ncol(tmp) <= 5,
            "The counts table has less than 5 samples"
        )
        shinyFeedback::feedbackWarning(
            "inp_compt_table",
            nrow(tmp) <= 10000,
            "The counts table has less than 10000 annotations"
        )
    }
    tmp
})


output$size_res <- renderUI({
    req(res())
    HTML(paste0(
        "<p> Number of columns read : ",
        ncol(res()),
        "</p>",
        "<br>",
        "Number of rows read :",
        nrow(res()),
        "</p>"
    ))
})

output$size_count <- renderUI({
    req(counts())
    HTML(paste0(
        "<p> Number of columns read : ",
        ncol(counts()),
        "</p>",
        "<br>",
        "Number of rows read :",
        nrow(counts()),
        "</p>"
    ))
})

