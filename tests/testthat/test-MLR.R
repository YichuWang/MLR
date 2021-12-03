test_that("MLR works", {
  expect_equal(as.vector(MLR(c(3,5,7,9),c(1,2,3,4),y_hat = F,
                             f_test = F)$beta_hat),
               c(1,2))
})


test_that("MLR works", {
  expect_equal(as.vector(MLR(c(2,4,6,8),c(1,2,3,4),
                             intercept = F,y_hat = F, f_test = F)$beta_hat),
               c(2))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$coef[,1]),

               as.numeric(MLR(swiss_feritility,
                              swiss_predictors)$beta_hat))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$coef[,3]),

               as.numeric(MLR(swiss_feritility,
                              swiss_predictors, t_test = T)$t_test$t_statistics))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$coef[,4]),

               as.numeric(MLR(swiss_feritility,
                              swiss_predictors,t_test = T)$t_test$p_values))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$fstatistic[1]),

               as.numeric(MLR(swiss_feritility,
                              swiss_predictors,t_test = F,f_test = T)$f_test$f_statistics))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$fstatistic[2:3]),

               as.numeric(MLR(swiss_feritility,
                              swiss_predictors,t_test = F,f_test = T)$f_test$df))
})


test_that("MLR works", {
  expect_equal(as.numeric(pf(summary(lm(swiss_feritility ~
                                          swiss_predictors))$fstatistic[1],
                             summary(lm(swiss_feritility ~
                                          swiss_predictors))$fstatistic[2],
                             summary(lm(swiss_feritility ~
                                          swiss_predictors))$fstatistic[3],lower.tail=FALSE)),

               as.numeric(MLR(swiss_feritility, swiss_predictors,t_test = F,f_test = T)$f_test$p_value))
  })


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$R_squared[,1]),

               as.numeric(MLR(swiss_feritility, swiss_predictors)$r_square))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$R_squared[,1]),

               as.numeric(MLR(swiss_feritility, swiss_predictors)$adjusted_r_square))
})


test_that("MLR works", {
  expect_equal(as.numeric(summary(lm(swiss_feritility ~
                                       swiss_predictors))$R_squared[,1]),

               as.numeric(MLR(swiss_feritility, swiss_predictors)$adjusted_r_square))
})
