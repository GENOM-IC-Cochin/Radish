res <- eventReactive(c(
    input$inp_res_table,
    input$auto_inp
), {
    if (input$auto_inp) {
        tmp <- vroom::vroom("./NGS21-023_Resultats/Analyses_supervisees/NGS21-023_ensemblGCRm38r101_deseq2_results_contrast_High_vs_Controle.tsv",
            delim = "\t"
        )
        # Find the symbol column
        tmp <- find_symb_col(tmp)
        
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
    tmp <- find_symb_col(tmp)

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
    # Find the symbol column
    tmp <- find_symb_col(tmp) 
    tmp
})

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

