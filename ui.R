ui <- dashboardPage(

   dashboardHeader(title = "Plotting tool"),

   dashboardSidebar(
      sidebarMenu(
         menuItem("Inputs", tabName = "inp"),
         menuItem("Volcano plot", tabName = "volcano"),
         menuItem("Interactive table", tabName = "tabl_gene")
      )
   ),
   dashboardBody(

      shinyFeedback::useShinyFeedback(),

      tabItems(
         tabItem(tabName = "inp",
                 checkboxInput(inputId = "auto_inp",
                               label = "Auto-detection of input files (debug feature)",
                               value = TRUE),
                 conditionalPanel(condition = "input.auto_inp == false",
                                  fluidRow(
                                     column(
                                        width = 6,
                                        p("Please enter the result table (csv or tsv format: NO .xls)"),
                                        br(),
                                        fileInput("inp_res_table", NULL, accept = c(".csv", ".tsv")),
                                        hr(),
                                        htmlOutput("size_res")
                                     ),
                                     column(
                                        width = 6,
                                        p("Please enter the counts table (csv or tsv format: NO .xls)"),
                                        br(),
                                        fileInput("inp_compt_table", NULL, accept = c(".csv", ".tsv")),
                                        hr(),
                                        htmlOutput("size_count")
                                     )
                                  )
                 )
         ),

         tabItem(tabName = "volcano",
                 sidebarLayout(
                    sidebarPanel(
                       h3("Settings"),
                       selectInput(inputId = "volcano_format",
                                   label = "Format of the dowloaded plot",
                                   choices = c("svg", "jpg", "png"),
                                   selected = "png")
                    ),
                    mainPanel(
                       plotOutput(outputId = "volcano_plot"),

                       downloadButton(outputId = "down_volc",
                                      label = "Download plot")
                    )
                 )
         ),

         tabItem(tabName = "tabl_gene")
      )
   )
)