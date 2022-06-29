
test_that("DT interaction", {
  # No need to test the gene select module in every module it is used in
  # Using countsplot to test it is enough
  app <- AppDriver$new(name = "app", height = 973, width = 1619, seed = 42)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "tabl_gene")
  app$wait_for_idle()
  app$set_inputs(`gntab-input_type` = "IDs")
  app$upload_file(`gntab-identifiers` = "selected_genes_ids.txt")
  app$wait_for_idle()
  app$click("gntab-select_genes")
  app$wait_for_idle()
  app$expect_values(export = "gntab-sel_genes_table")
  app$stop()
})