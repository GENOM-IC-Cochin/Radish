library(shinytest2)


test_that("DT interaction", {
  # No need to test the gene select module in every module it is used in
  # Using countsplot to test it is enough
  app <- AppDriver$new(name = "app", height = 973, width = 1619, seed = 42)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "tabl_gene")
  app$set_inputs(
        `gntab-genes_rows_selected` = c(1, 3, 7),
        `gntab-genes_row_last_clicked` = 7,
        allow_no_input_binding_ = TRUE
      )
  app$set_inputs(sidebar_tab = "countsplot")
  app$click("cnts-draw")
  app$wait_for_idle()
  app$expect_values(export = "cnts-countsdata")
  app$stop()
})
