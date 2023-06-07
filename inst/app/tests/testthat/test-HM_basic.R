# HM test
test_that("HM  basic test", {
  app <- AppDriver$new(name = "app", height = 973, width = 1619)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "heatmap")
  app$set_inputs(`hm-top_gene` = "diff")
  app$set_inputs(`hm-nb_top_gene` = 80)
  app$click("hm-draw")
  app$wait_for_idle()
  app$expect_values(export = "hm-hmdata")
  app$stop()
})
