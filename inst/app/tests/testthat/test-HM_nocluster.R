test_that("HM no cluster test", {
  app <- AppDriver$new(name = "app", height = 973, width = 1619)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$set_inputs(sidebar_tab = "heatmap") 
  app$set_inputs(`hm-cluster_control` = "no")
  app$set_inputs(`hm-col_order` = c("Control_1", "Cond1_1", "Control_2", "Cond1_2", "Control_3", "Cond1_3", "Control_4", "Cond1_4")) 
  app$click("hm-draw")
  app$wait_for_idle()
  app$expect_values(export = "hm-hmdata")
  app$stop()
})


