x_max_abs <- eventReactive(res(), {
    min_x <- res() %>%
        as.data.frame() %>%
        pull(log2FoldChange) %>%
        min() %>%
        floor()
    max_x <- res() %>%
        as.data.frame() %>%
        pull(log2FoldChange) %>%
        max() %>%
        ceiling()
    # maximum value of the x axis
    max(c(abs(min_x), abs(max_x)))
})

y_max_abs <- eventReactive(res(), {
    res() %>%
        as.data.frame() %>%
        na.omit() %>%
        transmute(log_padj = -log10(padj)) %>%
        max() %>%
        ceiling()
})

observe({
    updateSliderInput(
        inputId = "x_max",
        max = x_max_abs(),
        value = x_max_abs()
    )
})

observe({
    updateSliderInput(
        inputId = "y_max",
        max = y_max_abs(),
        value = y_max_abs()
    )
})


observeEvent(res(), {
    updateSelectizeInput(
        inputId = "sel_gene",
        choices = as.vector(res()$symbol),
        server = TRUE,
        selected = NULL
    )
})

res_to_plot <- eventReactive(res(), {
    res() %>%
        as.data.frame() %>%
        mutate(sig_expr = factor(case_when(
            log2FoldChange >= 1 & padj <= 0.05 ~ "up",
            log2FoldChange <= -1 & padj <= 0.05 ~ "down",
            TRUE ~ "ns"
        ))) %>%
        mutate(sig_expr = relevel(sig_expr, "up"))
})

volcano_plot <- reactive({
    # Choice of colors/transparency for up/down
    cols <- c("up" = input$up_col, "down" = input$down_col, "ns" = "black")
    alphas <- c("up" = 1, "down" = 1, "ns" = 0.3)

    tmp <- res_to_plot() %>%
        ggplot(aes(
            x = log2FoldChange,
            y = -log10(padj),
            alpha = sig_expr,
            fill = sig_expr
        )) +
        geom_point(
            color = "black",
            na.rm = TRUE,
            shape = 21,
            stroke = 0.1
        ) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
        geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
        scale_fill_manual(
            values = cols,
            labels = c(input$up_leg, input$down_leg, input$ns_leg)
        ) +
        scale_alpha_manual(values = alphas, guide = "none") +
        labs(
            title = input$plot_title,
            x = "Log2 Fold Change",
            y = "-Log10(Adjusted p-value)",
            fill = "Expression\nChange"
        ) +
        scale_x_continuous(
            limits = c(-input$x_max, input$x_max),
            oob = scales::squish
        ) +
        scale_y_continuous(limits = c(NA, input$y_max), oob = scales::squish)
    if (input$theme == "Classic") {
        tmp <- tmp + theme_classic()
    } else if (input$theme == "Gray") {
        tmp <- tmp + theme_gray()
    } else if (input$theme == "Classic with gridlines") {
        tmp <- tmp + theme_bw()
    }
    tmp <- tmp + theme(plot.title = element_text(face = "bold",
                                                 size = 15,
                                                 hjust = 0.5))
    if (input$y_max != y_max_abs()) {
        tmp <- tmp + geom_hline(yintercept = input$y_max, linetype = "dotted")
    }
    if (input$x_max != x_max_abs()) {
        tmp <- tmp + geom_vline(
            xintercept = c(-input$x_max, input$x_max),
            linetype = "dotted"
        )
    }
    if (!is.null(input$sel_gene)) {
        # Choice of genes, do not show labels of non significant genes
        genes_to_highlight <- which(res_to_plot()$symbol %in% input$sel_gene &
            res_to_plot()$sig_expr != "ns")
        tmp <- tmp + geom_label_repel(
            data = res_to_plot()[genes_to_highlight, ],
            aes(label = symbol),
            color = "black",
            fill = "white",
            min.segment.length = 0,
            show.legend = FALSE
        )
    }
    tmp
})

output$volcano_plot <- renderPlot({
    req(res())
    volcano_plot()
})

output$down_volc <- downloadHandler(
    filename = function() {
        paste0("volcano_plot.", req(input$volcano_format))
    },
    content = function(file) {
        ggsave(file, plot = req(volcano_plot()),
               device = req(input$volcano_format))
    }
)
