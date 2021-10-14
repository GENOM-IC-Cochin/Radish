heatmap_panels <- tabsetPanel(
    id = "settings",
    type = "hidden",
    tabPanel("diff",
             numericInput(
                 "nb_top_gene",
                 "Select the number of top differentially expressed genes",
                 value = 100,
                 min = 0,
                 max = 20000),
    ),
    tabPanel("all",
             selectizeInput(
                 "sel_gene_hm_nm",
                 "Select genes (by name) present on the heatmap",
                 choices = NULL,
                 multiple = TRUE,
                 options = list(maxItems = 200)
             ),
             selectizeInput(
                 "sel_gene_hm_id",
                 "Select genes (by id) present on the heatmap",
                 choices = NULL,
                 multiple = TRUE,
                 options = list(maxItems = 200)
             ),
             checkboxInput(
                 "show_names",
                 "Show gene names",
                 value = FALSE
             )
    )
)

global_theme <- create_theme(#one day in its own css
    theme = "flatly",
    adminlte_color(
        light_blue = "#006499"
    ),
    adminlte_global(
        content_bg = "#FFF",
        info_box_bg = "#EEF0F6"
    )
)


ui <- dashboardPage(
    header = dashboardHeader(
        title = "SHARE",
        leftUi = tagList(
            dropdownBlock(
                id = "contr",
                selectInput(
                    inputId = "contrast_act",
                    label = "Select the contrast you want to study",
                    choices = NULL,
                    selected = NULL
                ),
                title = "Current contrast",
                badgeStatus = NULL
            )
        )
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home"),
            menuItem("Data", tabName = "inp"),
            menuItem("PCA", tabName = "pca"),
            menuItem("Interactive table", tabName = "tabl_gene"),
            menuItem("Volcano plot", tabName = "volcano"),
            menuItem("Heatmap", tabName = "heatmap"),
            img(src = "LOGO_GENOM'IC_WHITE.svg",
                style="position:fixed;bottom:0;margin:0 0 15px 25px;")
        )
    ),
    body = dashboardBody(
        use_theme(global_theme),
        shinyFeedback::useShinyFeedback(),
        tabItems(
            tabItem(
                tabName = "home",
                tabBox(
                    title = "SHARE : A Shiny app for RNA-Seq Exploration",
                    width = 12,
                    tabPanel(
                        "About",
                        includeMarkdown("www/intro.md")
                    ),
                    tabPanel(
                        "Authors",
                        includeMarkdown("www/authors.md")
                    )
                )
            ),
            tabItem(
                tabName = "inp",
                checkboxInput(
                    inputId = "auto_inp",
                    label = "Auto-detection of input files (debug feature)",
                    value = FALSE 
                ),
                conditionalPanel(
                    condition = "input.auto_inp == false",
                    fileInput("res_data",
                              NULL
                    )
                )
            ),
            tabItem(
                tabName = "pca",
                fluidRow(
                    tabBox(
                        width = 12,
                        tabPanel(
                            title = "PCA plot",
                            plotOutput("pca")
                        ),
                        tabPanel(
                            title = "Screeplot",
                            plotOutput("scree")
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = "Settings",
                        status = "orange",
                        width = 4,
                        selectizeInput(
                            inputId = "excl_samp",
                            label = "Select samples to exclude",
                            multiple = TRUE,
                            choices = NULL,
                            selected = NULL,
                            options = NULL
                        ),
                        selectInput(
                            inputId = "theme_pca",
                            label = "Choose the theme for the plot",
                            choices = themes_gg,
                            selected = "Classic"
                        )
                    ),
                    box(title = "Download",
                        status = "orange",
                        width = 4,
                        selectInput(
                            inputId = "pca_format",
                            label = "Format of the dowloaded plot",
                            choices = c("svg", "png", "pdf"),
                            selected = "pdf"
                        ),
                        downloadButton(
                            outputId = "down_pca",
                            label = "Download plot"
                        )
                    )
                )
            ),
            tabItem(
                tabName = "volcano",
                fluidRow(
                    box(title = "Volcano Plot",
                        status = "primary",
                        width = 12,
                        plotOutput(outputId = "volcano_plot")
                    )
                ),
                fluidRow(
                    box(title = "Aesthetics",
                        status = "orange",
                        width = 4,
                        sliderInput(
                            inputId = "x_max",
                            label = "Maximum value of the x axis",
                            min = 0,
                            max = 100,
                            value = 10
                        ),
                        sliderInput(
                            inputId = "y_max",
                            label = "Maximum value of the y axis",
                            min = 0,
                            max = 100,
                            value = 10
                        ),     
                        sliderInput(
                            inputId = "lfc_cut",
                            label = "LogFoldChange limit for significance",
                            min = 0,
                            max = 5,
                            value = 1,
                            step = .5
                        ),
                        sliderTextInput(
                            inputId = "pval_cut",
                            label = "pvalue limit for significance",
                            choices = c(0.0001, 0.001, 0.01, 0.05, 0.1),
                            selected = 0.05
                        ),
                        colourInput(
                            inputId = "up_col",
                            label = "Choose the color of the upregulated genes",
                            value = "#fe7f00"
                        ),
                        colourInput(
                            inputId = "down_col",
                            label = "Choose the color of the downregulated genes",
                            value = "#007ffe"
                        ),
                        selectInput(
                            inputId = "theme",
                            label = "Choose the theme for the plot",
                            choices = themes_gg,
                            selected = "Classic"
                        ),
                        sliderInput(
                            inputId = "volc_ratio",
                            label = "Choose the plot aspect ratio",
                            value = 1,
                            min = 0.5,
                            max = 2
                        )
                    ),
                    box(title = "Text",
                        status = "orange",
                        width = 4,
                        textInput(
                            inputId = "plot_title",
                            label = "Title of the plot",
                            value = "Gene expression change"
                        ),
                        textInput(
                            inputId = "up_leg",
                            label = "Choose the upregulated legend name",
                            value = "up"
                        ),
                        textInput(
                            inputId = "down_leg",
                            label = "Choose the downregulated legend name",
                            value = "down"
                        ),
                        textInput(
                            inputId = "ns_leg",
                            label = "Choose the nonsignificant legend name",
                            value = "ns"
                        ),
                        selectizeInput(
                            inputId = "sel_gene_vp_nm",
                            label = "Select which significant genes (by name) to highlight :",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectizeInput(
                            inputId = "sel_gene_vp_id",
                            label = "Select which significant genes (by id) to highlight :",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        sliderInput(
                            inputId = "vp_lab_size",
                            label = "Choose the size of the labels",
                            value = 3,
                            min = 1,
                            max = 4,
                            step = .25
                        )
                    ),
                    box(title = "Download",
                        status = "orange",
                        width = 4,
                        selectInput(
                            inputId = "volcano_format",
                            label = "Format of the dowloaded plot",
                            choices = c("svg", "png", "pdf"),
                            selected = "pdf"
                        ),
                        downloadButton(
                            outputId = "down_volc",
                            label = "Download plot"
                        )
                    )
                )
            ),
            tabItem(tabName = "tabl_gene",
                    fluidRow(
                        box(title = "Settings",
                            status = "orange",
                            width = 3,
                            numericInput(
                                inputId = "pval_cutoff",
                                label = "Enter the maximum p-value :",
                                value = 0.05,
                                min = 0,
                                max = 1,
                                step = .05
                            ),
                            numericInput(
                                inputId = "lfc_cutoff",
                                label = "Enter the minimum (absolute) logFold2Change :",
                                value = 1,
                                min = 0
                            ),
                            checkboxGroupInput(
                                inputId = "genes_columns",
                                label = "Choose the columns to display :"
                            )
                        ),
                        box(title = "Gene Table",
                            status = "primary",
                            width = 9,
                            DT::dataTableOutput(outputId = "genes")
                        )
                    )
            ),
            tabItem(
                tabName = "heatmap",
                fluidRow(
                         box(title = "Settings",
                             status = "orange",
                             width = 3,
                             checkboxInput(
                                 inputId = "top_gene",
                                 label = "",
                                 value = TRUE
                             ),
                             checkboxGroupInput(
                                 inputId = "sel_cond",
                                 label = "Choose the conditions",
                             ),
                             selectInput(
                                 inputId = "palette_hm",
                                 label = "Choose the color palette of the heatmap",
                                 choices = brewer.pal.info %>%
                                     filter(category == "div" & colorblind == TRUE) %>%
                                     rownames_to_column() %>%
                                     pull(rowname),
                                 selected = "YlOrRd"
                             ),
                             heatmap_panels,
                             actionButton("draw_hm",
                                          "Draw heatmap",
                                          class = "btn-warning")
                         ),
                         box(title = "Heatmap",
                             status = "primary",
                             width = 9,
                             plotOutput("heatmap"),
                             selectInput(inputId = "heatmap_format",
                                         label = "Format of the downloaded plot :",
                                         choices = c("png", "pdf", "svg"),
                                         selected = "pdf"),
                             downloadButton(
                                 outputId = "down_hm",
                                 label = "Download plot"
                             )
                    )
                )
            )
        )
    )
)


