

test_that("PCA data test", {
  app <- AppDriver$new(name = "app", height = 973, width = 1619)
  app$set_inputs(sidebar_tab = "inp")
  app$click("inp-demo")
  app$wait_for_idle()
  app$click("pca-draw")
  app$set_inputs(sidebar_tab = "pca")
  app$set_inputs(`pca-excl_samp` = "Control_1")
  app$click("pca-recomp_pca")
  app$click("pca-draw")
  app$expect_values(export = c("pca-pcadata", "pca-levels"))
  app$stop()
})