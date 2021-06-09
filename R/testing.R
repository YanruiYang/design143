#' Testing the design models from scratch
#' @param function_name name of the testing function
#' @param k number of levels of the testing function
#' @param design_type the type of design: randomLHS, ortho_arrayLHS, maxiMin
#' @param n number of training/testing samples
#' @param cov the covariance type: gauss, matern5_2, matern3_2, exp, powexp
#' @return the mean square error
rmse <- function(function_name,k, design_type, n, cov = "matern5_2"){
  if (design_type == "randomLHS") {
    train_design <- randomLHS(n,k)
    test_design <- randomLHS(n,k)
  }
  if (design_type == "ortho_arrayLHS") {
    train_design <- ortho_arrayLHS(n,k)
    test_design <- ortho_arrayLHS(n,k)
  }
  if (design_type == "maxiMin") {
    train_design <- maxiMin(n,k)
    test_design <- maxiMin(n,k)
  }
  response <- apply(train_design,1,function_name)
  model <- DiceKriging::km(formula = ~1,train_design,response,covtype = cov, control = list(trace = FALSE))
  test_data <- apply(test_design,1,function_name)
  predicted <- predict(model, data.frame(test_design), "UK")$mean
  rmse <- sqrt(mean((predicted-test_data)^2))/sqrt(mean((mean(response)-test_data)^2))
#  emax <- max(abs(predicted-test_data))/max(abs(mean(response)-test_data))
  rmse
}

#' Create rmse boxplots to compare covairance models
#' @param function_name name of the testing function
#' @param k number of levels of the testing function
#' @param design_type the type of design: randomLHS, ortho_arrayLHS, maxiMin
#' @param n number of training and testing samples
#' @param cov covariance: all (all five possible cov types), gauss, matern5_2, matern3_2, exp, powexp
#' @param m number of repetitions
#' @return the mean square error
boxplot_cov <- function(function_name,k, design_type, n,cov = "all", m = 20){
  if (cov %in% c("gauss", "matern5_2", "matern3_2", "exp", "powexp")){
    output <- rep(NA,m)
    for (i in 1:m){
      output[i] <- rmse(function_name,k, design_type, n, cov)
    }
    boxplot(output, main = paste(function_name, m, "run", design_type, cov, sep = " "))
  } else{
  output <- matrix(rep(NA,m*5),ncol = 5)
  for (i in 1:m){
    output[i,1] <- rmse(function_name,k, design_type, n, cov = "gauss")
    output[i,2] <- rmse(function_name,k, design_type, n, cov = "matern5_2")
    output[i,3] <- rmse(function_name,k, design_type, n, cov = "matern3_2")
    output[i,4] <- rmse(function_name,k, design_type, n, cov = "exp")
    output[i,5] <- rmse(function_name,k, design_type, n, cov = "powexp")
  }
  boxplot(output[,1],output[,2],output[,3],output[,4],output[,5],
          main = paste(function_name, m, "run", design_type, sep = " "),
          names = c("Gauss", "Matern5-2","Matern3-2","Exp", "PowerExp"),cex.names=0.8)
  }
}

#' Create rmse boxplots to compare design types
#' @param function_name name of the testing function
#' @param k number of levels of the testing function
#' @param design_type the type of design: all (all three design types), randomLHS, ortho_arrayLHS, maxiMin
#' @param n number of training and testing samples
#' @param cov covariance: gauss, matern5_2, matern3_2, exp, powexp
#' @param m number of repetitions
#' @return the mean square error
boxplot_design <- function(function_name,k, design_type = "all", n,cov = "matern5_2", m = 20){
  if (design_type %in% c("randomLHS", "ortho_arrayLHS", "maxiMin")){
    output <- rep(NA,m)
    for (i in 1:m){
      output[i] <- rmse(function_name,k, design_type, n, cov)
    }
    boxplot(output, main = paste(function_name, m, "run", design_type, cov, sep = " "))
  } else{
    output <- matrix(rep(NA,m*3),ncol = 3)
    for (i in 1:m){
      output[i,1] <- rmse(function_name,k, design_type = "randomLHS", n, cov)
      output[i,2] <- rmse(function_name,k, design_type = "ortho_arrayLHS", n, cov)
      output[i,3] <- rmse(function_name,k, design_type = "maxiMin", n, cov)
    }
    boxplot(output[,1],output[,2],output[,3],
            main = paste(function_name, m, "run", cov, sep = " "),
            names = c("randomLHS", "ortho_arrayLHS", "maxiMin"),cex.names = 0.8)
  }
}

