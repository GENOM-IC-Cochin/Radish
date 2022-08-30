# Module for drawing upset plot

# UI ---------------------------------------------------------------------------
UpsetUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::tabBox(
        width = 12,
        tabPanel(
          title = "Upset Plot",
        plotOutput(ns("upset")),
        bs4Dash::actionButton(
          ns("draw"),
          "Draw Upset plot",
          status = "secondary"
        )
      ),
      tabPanel(
        title = "About",
        HTML(paste(
            "<p> The <strong> upset plot </strong> is a plot displaying information similar as a Venn diagram",
            "but in a more informative way. It allows representation of the sizes of the different intersections.",
            "Here, it is used to display the differences and similarities of differentially expressed genes (DEGs) across",
            "the comparisons.</p>",
            "<p> <strong> Warning : </strong> By default, this plot shows <em> exclusive intersections </em> :",
            "intersections displayed show genes belonging to two (or more) comparison, but <strong> not </strong> to the remaining ones.",
            "For instance, a bar for a single contrast indicates all DEGs in this comparison not found in others.",
            "The <strong> Intersect type </strong> option also has the option <em> inclusive intersection</em>,",
            "that shows genes belonging to the selected contrast(s), but that may also be differentially expressed in other contrasts.",
            "For instance, a bar for a single contrast indicates all DEGs in this comparison. Those DEGs can eventually",
            "be differentially expressed in other comparisons as well. Detailed and visual explanations can be found",
            "<a href='https://upset.app/',",
            "target='_blank'>here</a>."
          ))
      )
      )
    ),
    fluidRow(
        bs4Dash::box(
          title = "Differentially expressed genes",
          status = "danger",
          width = 12,
          fluidRow(
            column(
              width = 6,
              selectInput(
            inputId = ns("deg_type"),
            label = "Type of differentially expressed genes",
            choices = c(
              "Overexpressed only" = "up",
              "Underexpressed only" = "down",
              "All DEGs" = "all"
            )
          ),
          HTML(paste0(
            "<p> <strong>The above setting is heavily influenced by comparison order ",
            " (Treatment vs Control or Control vs Treatment).</p> <p> Moreover, the same gene,",
            " overexpressed in one contrast, under-expressed in another, will be included",
            " in the 'All DEGs' option.</strong> </p>"
          ))
        ),
        column(
          width = 6,
            FilterUI(ns("fil"))
          )
        )
      )
     ),
    fluidRow(
      bs4Dash::column(
        width = 4,
        bs4Dash::box(
          title = "Plot settings",
          status = "info",
          width = 12,
          selectInput(
            inputId = ns("int_type"),
            label = "Intersection type",
            choices = c(
              "exclusive" = "exclusive_intersection",
              "inclusive" = "inclusive_intersection"
            )
          ),
          sliderInput(
            inputId = ns("int_size"),
            label = "Minimum intersection size",
            min = 0,
            max = 100,
            value = 0,
            step = 1
          ),
          sliderInput(
            inputId = ns("min_degree"),
            label = "Minimum degree of the intersections",
            min = 1,
            max = 1,
            value = 1,
            step = 1
          ),
          selectizeInput(
            inputId = ns("contrastes_sel"),
            label = "Select the contrasts to display",
            multiple = TRUE,
            choices = NULL,
            selected = NULL
          ),
          selectInput(
            inputId = ns("ann_plot"),
            label = "Add a plot of counts by set",
            choices = c("no", "boxplot", "violin plot")
          )
        )
      ),
      column(
        width = 4,
        bs4Dash::box(
          title = "Download sets",
          status = "info",
          width = 12,
          selectizeInput(
            inputId = ns("sets_1"),
            label = "First set(s)",
            multiple = TRUE,
            choices = NULL,
            selected = NULL
          ),
          selectInput(
            inputId = ns("operation"),
            label = "Select the operation",
            choices = c(
              "union",
              "exclusive intersect (plot default)",
              "inclusive intersect",
              "except"
            )
          ),
          selectizeInput(
            inputId = ns("sets_2"),
            label = "Second set(s)",
            multiple = TRUE,
            choices = NULL,
            selected = NULL
          ),
          selectInput(
            inputId = ns("format"),
            label = "Select the download format",
            choices = c(
              "IDs" = "1",
              "Gene Names (IDs where names missing)" = "2",
              "Gene Names (missing when names missing)" = "3",
              "IDs + Gene Names + padj" = "4"
            )
          ),
          downloadButton(
            ns("dl_set"),
            label = "Download the created set"
          )
        )
      ),
      column(
        width = 4,
        bs4Dash::box(
          title = "Download",
          status = "info",
          width = 12,
          sliderInput(
            inputId = ns("ratio"),
            label = "Choose the (downloaded) plot aspect ratio",
            value = 1,
            min = 0.5,
            max = 2
          ),
          DownloadUI(ns("dl"))
        )
      )
    )
  )
}


# Server -----------------------------------------------------------------------
UpsetServer <- function(id, all_results, all_results_choice, res) {
  stopifnot(is.reactive(all_results))
  stopifnot(is.reactive(all_results_choice))
  stopifnot(is.reactive(res))

  moduleServer(id, function(input, output, session) {
    observeEvent(all_results_choice(), {
      updateSelectizeInput(
        inputId = "contrastes_sel",
        choices = all_results_choice(),
        selected = all_results_choice(),
        options = list(minItems = 2)
      )
      updateSelectInput(
        inputId = "sets_1",
        choices = names(all_results_choice())
      )
      updateSelectizeInput(
        inputId = "sets_2",
        choices = names(all_results_choice())
      )
    })

    observeEvent(plot_data(), {
      if (input$int_type == "inclusive_intersection") {
        max_int_size <- purrr::map_dbl(plot_data() %>%
          select(all_of(names(all_results_choice())[contrast_sel_numeric()])) %>%
          as.data.frame(), sum) %>%
          max()
      } else {
        max_int_size <- max_exclusive_int_size(plot_data())
      }
      updateSliderInput(
        inputId = "int_size",
        max = max_int_size
      )
    })

    observeEvent(input$contrastes_sel, {
      updateSliderInput(
        inputId = "min_degree",
        max = length(input$contrastes_sel)
      )
    })

    filter_res <- FilterServer(
      "fil",
      res,
      list("pval" = 0.05, "lfc" = 0),
      reactive(0)
    )

    contrast_sel_numeric <- eventReactive(
      input$contrastes_sel,
      input$contrastes_sel %>% as.numeric()
    )

    genes_by_contrast <- eventReactive(
      {
        all_results()
        input$contrastes_sel
        filter_res$lfc()
        filter_res$pval()
        input$deg_type
      },
      {
        req(filter_res$lfc())
        excl_deg <- switch(input$deg_type,
          "up" = c("ns", "down"),
          "down" = c("ns", "up"),
          "all" = c("ns")
        )
        # Create lists of sig genes by contrast
        validate(need(length(contrast_sel_numeric()) > 1, "The upset plot needs at least two contrasts"))
        genes_by_contrast <- vector(mode = "list", length = length(contrast_sel_numeric()))
        names(genes_by_contrast) <- names(all_results_choice())[contrast_sel_numeric()]
        for (i in seq_along(input$contrastes_sel)) {
          genes_by_contrast[[i]] <- all_results()[[contrast_sel_numeric()[i]]] %>%
            res_filter(
              lfc_filter = filter_res$lfc(),
              pval_filter = filter_res$pval()
            ) %>%
            filter(!(sig_expr %in% excl_deg)) %>%
            pull(Row.names)
        }
        genes_by_contrast
      }
    )

    plot_data <- eventReactive(input$draw, {
      unique_genes <- genes_by_contrast() %>%
        unlist() %>%
        unname() %>%
        unique()
      contr_names <- all_results_choice()[contrast_sel_numeric()]

      plot_data <- purrr::map_dfc(genes_by_contrast()[contr_names], ~ unique_genes %in% .x) %>%
        mutate("Row.names" = unique_genes)

      plot_data <- plot_data %>%
        inner_join(res() %>% select(Row.names, baseMean), by = "Row.names") %>%
        tibble::column_to_rownames(var = "Row.names") %>%
        as.data.frame()
      plot_data
    })


    cur_plot <- eventReactive(plot_data(), {
      req(
        plot_data(),
        contrast_sel_numeric(),
        all_results_choice()
      )
      if (input$ann_plot == "boxplot") {
        list_annotation <- list(
          "baseMean" = (
            # note that aes(x=intersection) is supplied by default and can be skipped
            ggplot(mapping = aes(y = baseMean))
            # checkout ggbeeswarm::geom_quasirandom for better results!
            ## + geom_jitter(aes(color = log10(votes)), na.rm = TRUE)
            +
              geom_boxplot(na.rm = TRUE)
              +
              scale_y_log10()
          )
        )
      } else if (input$ann_plot == "violin plot") {
        list_annotation <- list(
          "baseMean" = (
            # note that aes(x=intersection) is supplied by default and can be skipped
            ggplot(mapping = aes(y = baseMean))
            # checkout ggbeeswarm::geom_quasirandom for better results!
            ## + geom_jitter(aes(color = log10(votes)), na.rm = TRUE)
            +
              geom_violin(na.rm = TRUE)
              +
              scale_y_log10()
          )
        )
      } else {
        list_annotation <- list()
      }
      ComplexUpset::upset(
        data = plot_data(),
        intersect = names(all_results_choice())[contrast_sel_numeric()],
        name = "contrast",
        mode = input$int_type,
        width_ratio = 0.2,
        min_size = input$int_size,
        min_degree = input$min_degree,
        annotations = list_annotation,
        set_size = ComplexUpset::upset_set_size() +
          geom_text(aes(label = ..count..), hjust = -0.3, stat = "count", color = "white")
      )
    })

    output$upset <- renderPlot({
      cur_plot()
    })

    DownloadServer(
      id = "dl",
      cur_plot = cur_plot,
      plotname = reactive("upset_plot"),
      ratio = reactive(input$ratio)
    )

    dl_sets <- eventReactive(
      {
        genes_by_contrast()
        input$operation
        c(input$sets_1, input$sets_2)
        input$format
      },
      {
        if (input$operation == "union") {
          res <- Reduce(union, genes_by_contrast()[c(input$sets_1, input$sets_2)])
        } else if (input$operation == "exclusive intersect (plot default)") {
          incl_res <- Reduce(intersect, genes_by_contrast()[c(input$sets_1, input$sets_2)])
          rest_set <- Reduce(union, genes_by_contrast()[setdiff(
            names(all_results_choice()),
            c(input$sets_1, input$sets_2)
          )])
          res <- setdiff(incl_res, rest_set)
        } else if (input$operation == "inclusive intersect") {
          res <- Reduce(intersect, genes_by_contrast()[c(input$sets_1, input$sets_2)])
        } else if (input$operation == "except") {
          except_set <- Reduce(union, genes_by_contrast()[c(input$sets_2)])
          res <- setdiff(genes_by_contrast()[c(input$sets_1)] %>% unlist(), except_set)
        }

        if (input$format == "2") {
          res <- coalesce(all_results()[[1]][res, "symbol"], res)
        } else if (input$format == "3") {
          res <- all_results()[[1]][res, "symbol"]
        } else if (input$format == "4") {
          res <- all_results()[[1]][res, c("ensembl_gene_id", "symbol", "padj")]
        }
        res
      }
    )


    output$dl_set <- downloadHandler(
      filename = function() {
        tmp_name <- paste0(
          gsub(" ", "", paste0(input$sets_1, collapse = "-")),
          "_",
          input$operation,
          "_",
          gsub(" ", "", paste0(input$sets_2, collapse = "-")),
          ".txt"
        )
        gsub(" ", "-", tmp_name)
      },
      content = function(file) {
        if (is.character(dl_sets())) {
          write(dl_sets(), file)
        } else if (is.data.frame(dl_sets())) {
          write.csv(dl_sets(), file, row.names = FALSE)
        }
      }
    )

    exportTestValues(
      plot_data = plot_data()
    )
  })
}
