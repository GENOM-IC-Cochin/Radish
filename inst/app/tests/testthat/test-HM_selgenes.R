test_that("HM selected genes test", {
  app <- AppDriver$new(name = "app", height = 973, width = 1619)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "tabl_gene")
  app$set_inputs(
    `gntab-genes_rows_selected` = c(1, 3, 7, 8, 9),
    `gntab-genes_row_last_clicked` = 9,
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(sidebar_tab = "heatmap")
  app$set_inputs(`hm-top_gene` = "sel")
  app$wait_for_idle()
  app$set_inputs(`hm-samples` = c("Control_1", "Control_2", "Cond1_1", "Cond1_2", "Cond2_1", "Cond2_2"))
  app$click("hm-draw")
  app$wait_for_idle()
  app$expect_values(export = "hm-hmdata")
  app$stop()
})