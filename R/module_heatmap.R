# Heatmap module

# Color brewer palette
brew_vec <- RColorBrewer::brewer.pal.info %>%
  filter(category == "div" & colorblind == TRUE) %>%
  tibble::rownames_to_column() %>%
  pull(rowname)

# Colors for the condition in the heatmap
condition_colors <- RColorBrewer::brewer.pal(8, "Set2")

HeatmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::tabBox(
        width = 12,
        sidebar = bs4Dash::boxSidebar(
          id = ns("sidebar"),
          width = 25,
          selectInput(
            inputId = ns("top_gene"),
            label = "Choose the Heatmap",
            choices = c(
              "Selected genes" = "sel",
              "Top genes from current contrast" = "diff"
            )
          )
        ),
        tabPanel(
          title = "Heatmap",
          plotOutput(ns("heatmap"),
            height = "600px"
          ),
          htmlOutput(ns("gene_number")),
          bs4Dash::actionButton(ns("draw"),
            "Draw heatmap",
            status = "secondary"
          ),
          bs4Dash::actionButton(ns("reset"),
            "Reset defaults",
            status = "secondary"
          )
        ),
        tabPanel(
          title = "Information",
          HTML(paste(
            "<p> <strong> Why only 2000 rows maximum? </strong> </p>",
            "<p> If one goes over 1080, (or 2160 in the case of a 4k screen)",
            "genes, then there are less pixels than rows in the matrix",
            "and the results displayed is a sort of mean of multiple rows.",
            "It is well explained <a href='https://gdevailly.netlify.app/post/plotting-big-matrices-in-r/', target='_blank'>here</a>.",
            "If you are still willing to obtain such a matrix, please contact the developpers </p>"
          ))
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        title = "Settings",
        status = "info",
        width = 3,
        uiOutput(ns("samp_choice")),
        checkboxGroupInput(ns("variables"),
          "Choose the variables to display on the heatmap",
          choices = NULL,
          selected = NULL
        ),
        conditionalPanel(
          condition = "input.top_gene == 'diff'",
          FilterUI(ns("fil")),
          numericInput(
            ns("nb_top_gene"),
            "Select the number of top differentially expressed genes (max 2000)",
            value = 100,
            min = 0,
            max = 2000
          ),
          ns = ns,
        ),
        conditionalPanel(
          condition = "input.top_gene == 'sel'",
          GeneSelectUI(ns("gnsel")),
          ns = ns
        )
      ),
      bs4Dash::box(
        title = "Appearance",
        status = "info",
        width = 3,
        selectInput(
          inputId = ns("palette"),
          label = "Choose the color palette of the heatmap",
          choices = c(brew_vec),
          selected = "RdYlBu"
        ),
        checkboxInput(
          ns("show_names"),
          "Show gene names",
          value = FALSE
        ),
        sliderInput(
          ns("fontsize"),
          "Choose the row names fontsize",
          min = 3,
          max = 12,
          value = 10,
          step = .5
        ),
        sliderInput(
          inputId = ns("ratio"),
          label = "Choose the plot aspect ratio",
          value = 1,
          min = 0.5,
          max = 2
        )
      ),
      bs4Dash::box(
        title = "Advanced Settings",
        status = "info",
        width = 3,
        selectInput(ns("cluster_control"),
          "Cluster by columns",
          choices = c("yes", "no"),
          selected = "yes"
        ),
        conditionalPanel(
          condition = "input.cluster_control == 'no'",
          selectizeInput(
            inputId = ns("col_order"),
            label = "Choose the columns and their order",
            choices = NULL,
            multiple = TRUE
          ),
          ns = ns
        )
      ),
      bs4Dash::box(
        title = "Download",
        status = "info",
        width = 3,
        selectInput(
          inputId = ns("format"),
          label = "Format of the downloaded plot",
          choices = c("png", "pdf", "svg"),
          selected = "pdf"
        ),
        downloadButton(
          outputId = ns("down"),
          label = "Download plot"
        )
      )
    )
  )
}


# Server -----------------------------------------------------------------------

HeatmapServer <- function(id,
                          counts,
                          res,
                          config,
                          contrast_act,
                          contrastes,
                          sel_genes_table) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(sel_genes_table))

  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("nb_top_gene", function(value) {
      if (value > 2000) {
        "Maximum : 2000"
      }
    })

    iv$add_rule("col_order", function(value) {
      # if no cluster, then all samples are necessary
      req(samples_selected())
      if (length(value) != length(samples_selected()) &
        input$cluster_control == "no") {
        "You need to select all samples of available conditions"
      }
    })

    iv$enable()

    # Reordered Matrix
    reord_data <- reactiveVal()
    # Final Matrix
    data <- reactiveVal()

    # Switch to the last modified
    observeEvent(
      {
        base_data()
        input$cluster_control
      },
      {
        if (input$cluster_control == "yes") {
          data(base_data())
        }
      }
    )
    observeEvent(
      {
        reord_data()
        input$cluster_control
      },
      {
        if (input$cluster_control == "no") {
          data(reord_data())
        }
      }
    )


    observeEvent(config(), {
      updateCheckboxGroupInput(
        inputId = "variables",
        label = "Choose the variables to display on the heatmap",
        choices = config() %>%
          select(-File, -Name) %>%
          colnames(),
        selected = config() %>%
          select(-File, -Name) %>%
          colnames()
      )
    })

    observeEvent(
      {
        input$top_gene
        config()
      },
      {
        if (input$top_gene == "sel") {
          freezeReactiveValue(input, "samples")
        }
        output$samp_choice <- renderUI({
          if (input$top_gene == "sel") {
            tagList(
              selectizeInput(
                session$ns("samples"),
                "Select the samples to display",
                choices = config() %>% pull(Name),
                multiple = TRUE
              )
            )
          }
        })
      }
    )

    samples_selected <- reactive({
      if (!is.null(input$samples) & input$top_gene == "sel") {
        input$samples
      } else if (input$top_gene == "diff") {
        req(
          config(),
          contrastes(),
          contrast_act()
        )
        concerned_variable <- contrastes()[strtoi(contrast_act()), 1]
        concerned_levels <- c(
          contrastes()[strtoi(contrast_act()), 2],
          contrastes()[strtoi(contrast_act()), 3]
        )
        config() %>%
          select(all_of(c("Name", concerned_variable))) %>%
          filter(.data[[concerned_variable]] %in% concerned_levels) %>%
          pull(Name)
      }
    })


    observeEvent(input$reset, {
      updateNumericInput(inputId = "nb_top_gene", value = 100)
      updateSelectInput(inputId = "top_gene", selected = "diff")
      updateSelectInput(inputId = "palette", selected = "RdYlBu")
      updateCheckboxInput(inputId = "show_names", value = FALSE)
      updateSliderInput(inputId = "fontsize", value = 10)
      updateSliderInput(inputId = "ratio", value = 1)
      updateSelectizeInput(inputId = "col_order", selected = NULL)
      updateSelectInput(inputId = "cluster_control", selected = "yes")
      updateCheckboxGroupInput(
        inputId = "variables",
        label = "Choose the variables to display on the heatmap",
        choices = config() %>%
          select(-File, -Name) %>%
          colnames(),
        selected = config() %>%
          select(-File, -Name) %>%
          colnames()
      )
    })


    output$gene_number <- renderUI({
      req(data())
      HTML(paste(
        "<p>", nrow(data()),
        "genes are to be displayed on the heatmap </p>"
      ))
    })


    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = counts, # on utilise que counts pour les hm, quand on sélectionne par nom
      sel_genes_table = sel_genes_table
    )


    observeEvent(
      {
        samples_selected()
      },
      {
        freezeReactiveValue(input, "col_order")
        updateSelectizeInput(
          session = session,
          "col_order",
          choices = samples_selected(),
          selected = NULL
        )
      }
    )


    observeEvent(
      {
        input$col_order
        base_data()
      },
      {
        req(input$col_order)
        reord_data(base_data() %>%
          as.data.frame() %>%
          select(all_of(input$col_order)) %>%
          relocate(all_of(input$col_order)) %>%
          as.matrix(rownames = TRUE))
      }
    )


    base_data <- eventReactive(
      {
        res()
        counts()
        samples_selected()
        genes_selected$sel_genes_ids() # sometimes NULL
        genes_selected$sel_genes_names()
        input$top_gene
        input$nb_top_gene
      },
      {
        req(
          samples_selected(),
          config(),
          counts(),
          input$top_gene
        )

        if (input$top_gene == "diff") {
          req(
            input$nb_top_gene,
            res()
          )
          # Si l'on veut que les plus différentiellement exprimés
          res() %>%
            res_filter(lfc_filter = 0, pval_filter = 0.05) %>%
            filter(sig_expr != "ns") %>%
            slice_min(order_by = padj, n = input$nb_top_gene) %>%
            mutate(name = coalesce(symbol, Row.names)) %>%
            tibble::remove_rownames() %>%
            tibble::column_to_rownames(var = "name") %>%
            select(all_of(samples_selected())) %>%
            as.matrix(., rownames = TRUE)
        } else {
          # Si l'on veut sélectionner à la main
          counts() %>%
            filter(symbol %in% genes_selected$sel_genes_names() |
              Row.names %in% genes_selected$sel_genes_ids()) %>%
            mutate(name = coalesce(symbol, Row.names)) %>%
            tibble::column_to_rownames(var = "name") %>%
            select(all_of(samples_selected())) %>%
            as.matrix(., rownames = TRUE)
        }
      },
      ignoreNULL = FALSE # if the selected genes are NULL, no problem
    )


    # Met en correspondance les conditions choisies et les échantillons
    # (Pour les couleurs de la heatmap)
    # retour : un df avec la correspondance désirée
    annotation_col <- eventReactive(
      {
        config()
        data()
        samples_selected()
        input$variables
      },
      {
        ## if(!is.null(input$variables) & input$top_gene == "sel") {
        if (!is.null(input$variables)) {
          data.frame("Name" = samples_selected()) %>%
            inner_join(config(), by = "Name") %>%
            select(all_of(c("Name", input$variables))) %>%
            tibble::column_to_rownames(var = "Name")
        } else {
          return(NULL)
        }
      },
      ignoreNULL = FALSE
    )


    # A partir des annotations, leur affecte une couleur
    # retour : une liste avec la correspondance selon le format de pheatmap
    # pour le paramètre annotation_colors
    annotation_colors <- eventReactive(
      {
        config()
        annotation_col()
      },
      {
        if (is.null(annotation_col())) {
          return(NULL)
        }
        ret <- vector(mode = "list", length = ncol(annotation_col()))
        # one variable = one list element
        names(ret) <- colnames(annotation_col())
        # The number of levels across all variables
        diff_levels <- annotation_col() %>%
          unlist() %>%
          n_distinct()
        condition_colors <- scales::hue_pal()(diff_levels)
        # current color number
        cur_nb_col <- 1
        for (i in seq_along(ret)) {
          # quelle condition correspond à ces ech
          quels_cond <- unique(annotation_col()[, i]) %>% as.character()
          # condition_colors : variable globale
          ret[[i]] <- setNames(condition_colors[cur_nb_col:(cur_nb_col + length(quels_cond) - 1)], quels_cond)
          cur_nb_col <- cur_nb_col + length(quels_cond)
        }
        ret
      },
      ignoreNULL = FALSE
    )


    cur_plot <- eventReactive(
      {
        input$draw
      },
      {
        req(
          input$draw,
          data()
        )
        req(iv$is_valid())
        if (input$top_gene == "sel") {
          # I need at least some samples to plot
          validate(need(length(input$samples) > 1, "Need at least two samples"))
        }
        ComplexHeatmap::pheatmap(
          name = "z-score",
          mat = data(),
          color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 9, name = input$palette)))(256),
          cluster_rows = TRUE,
          cluster_cols = ifelse(input$cluster_control == "yes", TRUE, FALSE),
          # No row names if top genes
          show_rownames = input$show_names,
          annotation_col = if (is.null(annotation_col())) {
            NA
          } else {
            annotation_col()
          },
          annotation_colors = if (is.null(annotation_colors())) {
            NA
          } else {
            annotation_colors()
          },
          border_color = NA,
          fontsize = 10,
          fontsize_row = input$fontsize,
          scale = "row",
          heatmap_height = unit(5 * (input$ratio^0.5), "in"),
          heatmap_width = unit(5 / (input$ratio^0.5), "in")
        )
      }
    )

    output$heatmap <- renderPlot({
      cur_plot()
    })


    output$down <- downloadHandler(
      filename = function() {
        paste0("heatmap.", input$format)
      },
      content = function(file) {
        if (input$format == "png") {
          ragg::agg_png(file,
            width = 8 / (input$ratio)^0.5,
            height = 8 * (input$ratio)^0.5,
            units = "in",
            res = 600
          )
        } else if (input$format == "pdf") {
          pdf(file,
            width = 7 / (input$ratio)^0.5,
            height = 7 * (input$ratio)^0.5
          )
        } else if (input$format == "svg") {
          svglite::svglite(file,
            width = 8 / (input$ratio)^0.5,
            height = 8 * (input$ratio)^0.5
          )
        }
        ComplexHeatmap::draw(cur_plot())
        dev.off()
      }
    )

    exportTestValues(
      hmdata = head(data(), n = 50)
    )
  })
}


# Test App ---------------------------------------------------------------------

HeatmapApp <- function() {
  ui <- fluidPage(
    bs4Dash::tabsetPanel(
      type = "tabs",
      tabPanel("Input", InputUI("inp")),
      tabPanel("Heatmap", HeatmapUI("hm"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("1"))
    HeatmapServer(
      id = "hm",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive("1"),
      contrastes = list_loaded$contrastes,
      sel_genes_table = reactive(head(list_loaded$res()))
    )
  }
  shinyApp(ui, server)
}
