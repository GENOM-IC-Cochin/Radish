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
                 fluidRow(
                    column(
                       width = 6,
                       p("Please enter the result table (csv or tsv format: NO .xls)"),
                       br(),
                       fileInput("inp_res_table", NULL, accept = c(".csv", ".tsv"))
                    ),
                    column(
                       width = 6,
                       p("Please enter the counts table (csv or tsv format: NO .xls"),
                       br(),
                       fileInput("inp_compt_table", NULL, accept = c(".csv", ".tsv"))
                    )
                 )
         ),

         tabItem(tabName = "volcano",
                 plotOutput(outputId = "volcano_plot"),

                 downloadButton(outputId = "down_volc",
                                label = "Download plot")
         ),

         tabItem(tabName = "tabl_gene")
      )
   )
)