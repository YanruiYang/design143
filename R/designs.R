#' Create a random Latin hypercube design
#'
#' Source: library(lhs)
#' randomLHS(4,3) returns a 4x3 matrix with each column constructed as follows:
#' A random permutation of (1,2,3,4) is generated, say (3,1,2,4) for each of K columns.
#' Then a uniform random number is picked from each indicated quartile one between 0 and .25,
#' then one between .25 and .5, finally one between .75 and 1.
#'
#' @param n the number of rows or samples
#' @param k the number of columns or parameters/variables
#' @return A design matrix
randomLHS <- function(n,k){
  D <- lhs::randomLHS(n, k)
  D
}


#' Construct an orthogonal array Latin hypercube
#'
#' Source: library(lhs)
#' Create an orthogonal array Latin hypercube
#'
#' @param n the number of rows or samples
#' @param k the number of columns or parameters/variables
#' @return A design matrix
ortho_arrayLHS <- function(n,k){
  D <- lhs::create_oalhs(n,k,TRUE,FALSE)
  D
}

#' Construct a maxiMin design
#'
#' Source: library(lhs)
#' Create a maxiMin design
#'
#' @param n the number of rows or samples
#' @param k the number of columns or parameters/variables
#' @return A design matrix
maxiMin <- function(n,k){
  D <- lhs::maximinLHS(n, k)
  D
}
