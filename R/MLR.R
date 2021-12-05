#'MLR
#'
#'MLR is used to fit linear regression models.
#'It could be used to find the estimated y, the value of SSE and/or SSY,
#'the value of r square and/or adjusted r square,the t and/or f statistics,
#'p values and the hat matrix.
#'
#'@importFrom stats pf pt
#'
#'@param y a vector, outcomes.
#'
#'@param x a matrix, rows represent each observation, columns represent predictor.
#'
#'@param intercept a logical value, "TRUE" by default, the model has intercept.
#'if it's "FALSE", the model doesn't have intercept.
#'
#'@param y_hat a logical value, "TRUE" by default, output the value of fitted y.
#'if it's "FALSE", fitted value y will not be output.
#'
#'@param SSE a logical value, "FALSE" by default, SSE will not be output
#'if it's "TRUE", output the value of SSE.
#'
#'@param SSY a logical value, "FALSE" by default, SSY will not be output
#'if it's "TRUE", output the value of SSY.
#'
#'@param r_square a logical value, "TRUE" by default, output the value of r square.
#'if it's "FALSE", r square will not be output.
#'
#'@param adjusted_r_square a logical value, "FALSE" by default, adjusted r square will not be output.
#'if it's "TRUE", output the value of adjusted r square.
#'
#'@param t_test a logical value, "FALSE" by default, the results of t-test and p-values will not be output.
#'if it's "TURE", output the results of t-test and p-values.
#'
#'@param f_test a logical value, "TRUE" by default,output the results of f-test and p-values.
#'if it's "FALSE", the results of t-test and f-values will not be output
#'
#'@param hat_matrix a logical value, "FALSE" by default, the results of hat matrix will not be output.
#'if it's "TRUE", output the results hat matrix.
#'
#'@return the result list
#'
#'@examples
#'MLR(swiss_feritility, swiss_predictors)
#'MLR(swiss_feritility, swiss_predictors,intercept = FALSE)
#'MLR(c(3,5,7,9),c(1,2,3,4),y_hat = FALSE,f_test = FALSE)
#'
#'@export
#'



MLR = function(y,
               x,
               intercept = TRUE,
               y_hat = TRUE,
               SSE = FALSE,
               SSY = FALSE,
               r_square = FALSE,
               adjusted_r_square = FALSE,
               t_test = FALSE,
               f_test = TRUE,
               hat_matrix = FALSE){

  # build the covariates matrix
  x <- as.matrix(x)
  # build the outcomes matrix
  y <- as.matrix(y)
  # find the number of predictors
  p <- ncol(x)
  # find the number of sample size
  n <- nrow(x)

  ## Name the columns without predictor name
    if (length(colnames(x)) == 0){
    predictor_name <- character(p)
    for (i in 1:p) {
      predictor_name[i] <- paste0("predictor_", i, collapse = "")
      colnames(x) <- predictor_name
    }
    }
  if (length(colnames(y)) == 0){
    colnames(y) <- "outcomes"
  }


  #if the model have intercept, add the intercept column
  if (intercept == TRUE) {
    x <- cbind(intercept = rep(1, n), x)
    p <- p + 1
  }

  #create result list
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  result <- list(beta_hat = beta_hat)
  fitted_value <- x %*% beta_hat
  residuals <- y-fitted_value

  #find the y_hat, the fitted value of y
  if (y_hat == TRUE){
    result$y_hat <- fitted_value
  }

  #find SSE
  if(SSE ==TRUE){
    SSE <- sum(residuals ^ 2)
    result$SSE <- SSE
  }

  #find SSY
  if(SSY == TRUE){
    if (intercept == TRUE) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y ^ 2)
    }
    result$SSY <- SSY
  }

  #find r_square
  if (r_square == TRUE) {
    if (intercept == TRUE) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY
    result$r_square <- r_square
  }

  #find adjusted_r_square
  if (adjusted_r_square == TRUE) {
    if (intercept == TRUE) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY
    adjusted_r_square <- 1 - (SSE/(n - p))/(SSY/(n - intercept))
    result$adjusted_r_square  <- adjusted_r_square
  }


  #find t test statistics, df, and p_values
  if (t_test == TRUE) {
    MSE = sum(residuals^2)/(n - p)
    beta_hat_var = solve(t(x) %*% x) * MSE
    t_statistics = (beta_hat) / sqrt(beta_hat_var[(p + 1) * (1:p) - p])
    p_values = 2 * pt(-abs(t_statistics), n - p)
    t_test = list(t_statistics = t_statistics,
                  p_values = p_values,
                  df = n - p)
    result$t_test = t_test
  }

  #find f_statistics, df, and p_value
  if (f_test == TRUE) {
    if (intercept == TRUE) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY

    f_statistics = (r_square) / (1 - r_square) * ((n-p)/(p-intercept))
    p_value = 1 - pf(f_statistics, p - intercept, n - p)
    f_test = list(
      f_statistics = f_statistics,
      p_value = p_value,
      df = c(p - intercept, n - p)
    )
    result$f_test = f_test
  }

  #find the hat matrix
  if (hat_matrix == TRUE) {
    hat_matrix = x %*% solve(t(x) %*% x) %*% t(x)
    result$hat_matrix = hat_matrix
  }

  #the return step
  return(result)
}
