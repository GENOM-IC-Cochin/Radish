test_that("upset plot", {
  # No need to test the gene select module in every module it is used in
  # Using countsplot to test it is enough
  app <- AppDriver$new(name = "app", height = 973, width = 1619, seed = 42)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "upset")
  app$click("up-draw")
  app$wait_for_idle()
  app$set_inputs(`up-sets_1` = "PBC vs Control",
                 `up-sets_2` = "PSC vs Control",
                 `up-operation` = "except")
  app$expect_download("up-dl_set")
  app$set_inputs(`up-sets_1` = "PBC vs Control",
                 `up-sets_2` = "PSC vs Control",
                 `up-operation` = "exclusive intersect (plot default)")
  app$wait_for_idle()
  app$expect_download("up-dl_set")
  app$expect_values(export = "up-plot_data")
  app$stop()
})
