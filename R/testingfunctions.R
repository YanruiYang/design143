#' Create Borehole testing function
#' Source: Virtual Library of Simulation Experiments
#'
#' @param xx 8-dim inputs
#' @return water flow rates
borehole <- function(xx)
{
  ##########################################################################
  #
  # BOREHOLE FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  if (is.matrix(xx)){
  rw <- qnorm(xx[,1], 0.10,0.0161812)
  r  <- qlnorm(xx[,2],7.71,1.0056)
  Tu <- qunif(xx[,3], 63070,115600)
  Hu <- qunif(xx[,4],990,1110)
  Tl <- qunif(xx[,5],63.1,116)
  Hl <- qunif(xx[,6],700,820)
  L  <- qunif(xx[,7],1120,1680)
  Kw <- qunif(xx[,8],9855,12045)
  } else {
    rw <- qnorm(xx[1], 0.10,0.0161812)
    r  <- qlnorm(xx[2],7.71,1.0056)
    Tu <- qunif(xx[3], 63070,115600)
    Hu <- qunif(xx[4],990,1110)
    Tl <- qunif(xx[5],63.1,116)
    Hl <- qunif(xx[6],700,820)
    L  <- qunif(xx[7],1120,1680)
    Kw <- qunif(xx[8],9855,12045)
  }

  frac1 <- 2 * pi * Tu * (Hu-Hl)

  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)

  y <- frac1 / frac2
  return(y)
}

#' Create OTL Circuit testing function
#' Source: https://www.sfu.ca/~ssurjano/otlcircuit.html
#'
#' @param xx 6-dim inputs
#' @return midpoint voltage
#'
otlcircuit <- function(xx){
  ##########################################################################
  #
  # OTL CIRCUIT FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # Vm = midpoint voltage
  # xx = c(Rb1, Rb2, Rf, Rc1, Rc2, beta)
  #
  ##########################################################################
  if(is.matrix(xx)){
    Rb1  <- qunif(xx[,1],50,150)
    Rb2  <- qunif(xx[,2],25,70)
    Rf   <- qunif(xx[,3],0.5,3)
    Rc1  <- qunif(xx[,4],1.2,2.5)
    Rc2  <- qunif(xx[,5],0.25,1.2)
    beta <- qunif(xx[,6],50,300)
  }else{
    Rb1  <- qunif(xx[1],50,150)
    Rb2  <- qunif(xx[2],25,70)
    Rf   <- qunif(xx[3],0.5,3)
    Rc1  <- qunif(xx[4],1.2,2.5)
    Rc2  <- qunif(xx[5],0.25,1.2)
    beta <- qunif(xx[6],50,300)
  }


  Vb1 <- 12*Rb2 / (Rb1+Rb2)
  term1a <- (Vb1+0.74) * beta * (Rc2+9)
  term1b <- beta*(Rc2+9) + Rf
  term1 <- term1a / term1b

  term2a <- 11.35 * Rf
  term2b <- beta*(Rc2+9) + Rf
  term2 <- term2a / term2b

  term3a <- 0.74 * Rf * beta * (Rc2+9)
  term3b <- (beta*(Rc2+9)+Rf) * Rc1
  term3 <- term3a / term3b

  Vm <- term1 + term2 + term3
  return(Vm)
}

#' Create Piston testing function
#' Source: https://www.sfu.ca/~ssurjano/piston.html
#'
#' @param xx 7-dim inputs
#' @return cycle time
#'
piston <- function(xx)
{
  ##########################################################################
  #
  # PISTON FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # OUTPUT AND INPUT:
  #
  # C = cycle time
  # xx = c(M, S, V0, k, P0, Ta, T0)
  #
  ##########################################################################

  if(is.matrix(xx)){
    M  <- qunif(xx[,1],30,60)
    S  <- qunif(xx[,2],0.005,0.020)
    V0 <- qunif(xx[,3],0.002,0.010)
    k  <- qunif(xx[,4],1000,5000)
    P0 <- qunif(xx[,5],90000,110000)
    Ta <- qunif(xx[,6],290,296)
    T0 <- qunif(xx[,7],340,360)
  } else{
    M  <- qunif(xx[1],30,60)
    S  <- qunif(xx[2],0.005,0.020)
    V0 <- qunif(xx[3],0.002,0.010)
    k  <- qunif(xx[4],1000,5000)
    P0 <- qunif(xx[5],90000,110000)
    Ta <- qunif(xx[6],290,296)
    T0 <- qunif(xx[7],340,360)
  }

  Aterm1 <- P0 * S
  Aterm2 <- 19.62 * M
  Aterm3 <- -k*V0 / S
  A <- Aterm1 + Aterm2 + Aterm3

  Vfact1 <- S / (2*k)
  Vfact2 <- sqrt(A^2 + 4*k*(P0*V0/T0)*Ta)
  V <- Vfact1 * (Vfact2 - A)

  fact1 <- M
  fact2 <- k + (S^2)*(P0*V0/T0)*(Ta/(V^2))

  C <- 2 * pi * sqrt(fact1/fact2)
  return(C)
}

#' Create FRIEDMAN FUNCTION
#' Source: https://www.sfu.ca/~ssurjano/fried.html
#'
#' @param xx 5-dim inputs
#' @return the Friedman function modeling result
#'
friedman <- function(xx){
  ##########################################################################
  #
  # FRIEDMAN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, x3, x4, x5)
  #
  ##########################################################################
  if(is.matrix(xx)){
    x1  <- xx[,1]
    x2  <- xx[,2]
    x3  <- xx[,3]
    x4  <- xx[,4]
    x5  <- xx[,5]
  } else{
    x1  <- xx[1]
    x2  <- xx[2]
    x3  <- xx[3]
    x4  <- xx[4]
    x5  <- xx[5]
  }

  term1 <- 10*sin(pi*x1*x2)
  term2 <- 20*(x3-0.5)^2
  term3 <- 10*x4
  term4 <- 5*x5

  f <- term1 + term2 + term3 + term4
  return(f)
}
