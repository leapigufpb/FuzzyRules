########################################
## Create Intervalar Type-2 Fuzzy Set ##
########################################

## Check two vectors - vmax >= vmin - Auxiliar Function
#' @export
Check <- function(vmax, vmin) {
  cont = 0
  for (i in 1: length(vmax)) {
    x <- ifelse(vmax[i]< 1e-5, 0, vmax[i])
    y <- ifelse(vmin[i]< 1e-5, 0, vmin[i])
    if (x>=y) {
      cont = cont + 1
    }
  }
  if ( cont == length(vmax) ) {
    return(1)
  }
  else {
    return(0)
  }
}


## Create an object IT2FS
#' @export
Create.IT2FS <- function(U, FSMax, FSMin) {
  ## Check if the length of membership function 1 is the same of the length of membership function 2
  FS1 <- FSMax[[3]][,2]
  FS2 <- FSMin[[3]][,2]
  flag = 1
  if ( length(U) != length(FSMax[[3]][,2]) || length(U) != length(FSMin[[3]][,2]) || length(FSMax[[3]][,2]) != length(FSMin[[3]][,2]) ) {
    print("Support of the both Fuzzy Sets MAX and MIN of IT2FS must be the same and equal to the Universe")
  }
  ## Check if FSMax >= FSMin
  if (Check (FS1, FS2) == 0){
    stop(" It is not possible to create IT2FS. MF superior must be greather or equal than MF inferior")
  }

  ## name =c("IT2FS", FSMax[[1]], FSMin[[1]])
  ## parameters <- list(FSMax[[2]], FSMin[[2]])

  ## FSMax[[3]][,1] ## Universe U values
  ## FSMax[[3]][,2] ## FSMax values
  ## FSMin[[3]][,1] ## Universe U values
  ## FSMin[[3]][,2] ## FSMin values
  MF_IT2FS <- cbind(FSMax[[3]][,1], FSMax[[3]][,2], FSMin[[3]][,2])

  ## FSMax[[4]][,2] ## alpha values
  ## FSMax[[4]][,1] ## FSMax values
  ## FSMin[[4]][,2] ## alpha values
  ## FSMin[[4]][,1] ## FSMin values
  alpha_cuts <- cbind(FSMax[[4]][,1], FSMin[[4]][,1], FSMin[[4]][,2])

  Lst <- list(name =c("IT2FS", FSMax[[1]], FSMin[[1]]), parameters <- list(FSMax[[2]], FSMin[[2]]), Membership.Function=MF_IT2FS, alphacuts=alpha_cuts)

}

##################################################################
## Plot a Interval Type-2 Fuzzy Set w.r.t the Universe (plotFS) ##
##################################################################
#' @export
plotIT2FS <- function(Universe,IT2FS) {
  if (IT2FS[[1]][1] == "IT2FS") {
    T2FuzzySet <- IT2FS[[3]]
    plot (Universe, T2FuzzySet[,2], pch=".", lty=1, ylim=c(0,1))
    graphics::lines (Universe, T2FuzzySet[,2], col='blue', lty=1, ylim=c(0,1))
    graphics::points (Universe, T2FuzzySet[,3], pch=".", lty=1, ylim=c(0,1))
    graphics::lines (Universe, T2FuzzySet[,3], col='red', lty=1, ylim=c(0,1))
  }
  else {
    print("This is not a IT2FS")
  }
}


## POR QUE TEM Create.IT2FS E Create.IT2FS2???
## O PRIMEIRO NÃO VAI FUNCIONAR, POIS x PODE TER VALOR DIFERENTE DOS
## VALORES ARMAZENADOS
#' @export
Create.IT2FS2 <- function(U, FSMax, FSMin) {
  ## Check if the length of membership function 1 is the same of the length of membership function 2
  FS1 <- FSMax[[3]][,2]
  FS2 <- FSMin[[3]][,2]
  flag = 1
  if ( length(U) != length(FSMax[[3]][,2]) || length(U) != length(FSMin[[3]][,2]) || length(FSMax[[3]][,2]) != length(FSMin[[3]][,2]) ) {
    print("Support of the both Fuzzy Sets MAX and MIN of IT2FS must be the same and equal to the Universe")

	## REVER ESSA MSG - OS SUPORTES PODEM SER DIFERENTES!
  }
  ## Check if FSMax >= FSMin
  if (Check (FS1, FS2) == 0){
    stop(" It is not possible to create IT2FS. MF superior must be greather or equal than MF inferior")
  }

  ## name =c("IT2FS", FSMax[[1]], FSMin[[1]])
  ## parameters <- list(FSMax[[2]], FSMin[[2]])

  ## FSMax[[3]][,1] ## Universe U values
  ## FSMax[[3]][,2] ## FSMax values
  ## FSMin[[3]][,1] ## Universe U values
  ## FSMin[[3]][,2] ## FSMin values
  MF_IT2FS <- cbind(FSMax[[3]][,1], FSMax[[3]][,2], FSMin[[3]][,2])

  ## FSMax[[4]][,2] ## alpha values
  ## FSMax[[4]][,1] ## FSMax values
  ## FSMin[[4]][,2] ## alpha values
  ## FSMin[[4]][,1] ## FSMin values
  alpha_cuts <- cbind(FSMax[[4]][,1], FSMin[[4]][,1], FSMin[[4]][,2])

  Lst <- list(name =c("IT2FS", FSMax[[1]], FSMin[[1]]), parameters <- listfuzzy(FSMax[[2]], FSMin[[2]]), Membership.Function=MF_IT2FS, alphacuts=alpha_cuts)
}

## REVER list(FSMax[[2]], FSMin[[2]]) - parametros nao estao acessiveis



#################################################################
## Degree of membership of an element x to a fuzzy set (IT2FS) ##
## Deveretornar um par de valores [mu_{FSMin}, mu_{FSMax}]
#################################################################
#' @export
dmIT2FS <- function(x,U,FS1) {

## Quando FSMax eh triangular
## Um IF para todos os FSMax e outro para todos os FSMin ???
## No final cria o par de valores de saída y_min e Y_max
  parameters <- FS1[[2]][[1]]
  y_max = switch(FS1[[1]][2],
         "Triangular" = fpTrian(x, parameters,FS1[[3]][,2]),
         "Gaussian" = fpGau(x, parameters),
         "Trapezoidal" = fpTrap(x, parameters,FS1[[3]][,2]))

  parameters <- FS1[[2]][[2]]
  y_min = switch(FS1[[1]][3],
                 "Triangular" = fpTrian(x, parameters,FS1[[3]][,3]),
                 "Gaussian" = fpGau(x, parameters),
                 "Trapezoidal" = fpTrap(x, parameters, FS1[[3]][,3]))

    y <- c(y_min, y_max)
    return(y)
}


fpTrian <- function(x,parameters, mbvalues){
  aa <- 0.0
  bb <- max(mbvalues)#1.0
  T1 <- c(parameters[1], parameters[2])
  T2 <- c(aa, bb)
  T3 <- c(parameters[2], parameters[3])
  T4 <- c(bb, aa)
  ##linear regresssions to estimate Triangular fuzzy set
  res=stats::lm(formula = T2 ~ T1)
  intercept1 <- res$coefficients[1]
  coeff1 <- res$coefficients[2]
  res=stats::lm(formula = T4 ~ T3)
  intercept2 <- res$coefficients[1]
  coeff2 <- res$coefficients[2]
  yL <- x * coeff1 + intercept1
  if (yL < 0 || yL > max(mbvalues) || is.na(yL)) { yL = 0}
  yR <- x * coeff2 + intercept2
  if (yR < 0 || yR > max(mbvalues) || is.na(yR)) { yR = 0}
  y = max(yL, yR)

  return(y)

}

#' @export
fpGau <- function(x,parameters){
  y = exp(-0.5*((x-parameters[1])/parameters[2])^2)
  if ( y <= 10^(-4) ) {
    y = 0                ## Valor menor do que 10^-4 => 0
  }
  return(y)
}

#' @export
fpTrap <- function(x, parameters, mbvalues){

    if ( x >= parameters[2] & x <= parameters[3] ) {
      y = max(mbvalues)
      return(y)
    }
    else {
      maxmbvalues <- max(mbvalues)
      aa <- 0.0
      bb <- maxmbvalues#1.0
      T1 <- c(parameters[1], parameters[2])
      T2 <- c(aa, bb)
      T3 <- c(parameters[3], parameters[4])
      T4 <- c(bb, aa)
      ##linear regresssions to estimate Triangular fuzzy set
      res=stats::lm(formula = T2 ~ T1)
      intercept1 <- res$coefficients[1]
      coeff1 <- res$coefficients[2]
      res=stats::lm(formula = T4 ~ T3)
      intercept2 <- res$coefficients[1]
      coeff2 <- res$coefficients[2]
      yL <- x * coeff1 + intercept1
      if (yL < 0 || yL > maxmbvalues || is.na(yL)) { yL = 0}
      yR <- x * coeff2 + intercept2
      if (yR < 0 || yR > maxmbvalues || is.na(yR)) { yR = 0}
      y = max(yL, yR)
      return(y)
    }
  return(y)
}

##########################################
## Method for computing OR on two IT2FS ##
## Mamdani type using MAX               ##
##########################################
#' @export
OR.IT2FS <- function(FRBS,x1,x2) {
  if (FRBS == "Mandani") {
    w1 <- max(x1[1],x2[1])
    w2 <- max(x1[2],x2[2])
  }
  out <- c(w1, w2)
  return(out)
}



###########################################
## Method for computing AND on two IT2FS ##
## Mamdani type using MIN                ##
###########################################
#' @export
AND.IT2FS <- function(FRBS,x1,x2) {
  if (FRBS == "Mandani") {
    w1 <- min(x1[1],x2[1])
    w2 <- min(x1[2],x2[2])
  }
  out <- c(w1, w2)
  return(out)
}

#########################################################
## Intersection of two fuzzy sets on the same Universe ##
## Intersection is given by the Min (interFS)          ##
#########################################################
#' @export
interFS.IT2FS  <- function(U,FS1, FS2) {
  fs1 <- FS1[[3]][,2]
  fs2 <- FS2[[3]][,2]
  InterFS <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    InterFS[i] <- min(fs1[i],fs2[i])
  }
  return(InterFS)
}


################################################
## Method for computing Implication on two FS ##
## Mamdani type using MIN                     ##
################################################
#' @export
Implication.IT2FS <- function(FRBS,x,U,FS) {

  xx= x
  FS2 <- FS
  MF_saida <- c()

  for(i in 1:length(x)){

    jj <- ifelse(i==1, 3, 2)

    FS[[3]] <- FS2[[3]][,c(1,jj)]
    x <- xx[i]

    if (FRBS == "Mandani") {
      FS1 <- ConstFS(U,x)
      MF <- interFS(U,FS1, FS)
    }

    MF_saida <- cbind(MF_saida,MF)
  }
  return(MF_saida)

}



#############################################
## Method for computing Aggregation on FSs ##
## Mamdani type using MAX                  ##
##############################################
#' @export
Aggregation.IT2FS <- function(FRBS,U,Lst) {
  if (FRBS == "Mandani") {
    MF <- Lst[[1]]
    for (i in 2:length(Lst)) {
      for ( j in 1:length(U) ) {
        MF[j] <- max(MF[j], Lst[[i]][j])
      }
    }
  }
  return(MF)
}




#########################################################
## Intersection of two fuzzy sets on the same Universe ##
## Intersection is given by the Min (interFS)          ##
#########################################################
#' @export
interFS.IT2FS  <- function(U,FS1, FS2) {
  fs1 <- FS1[[3]][,2]
  fs2 <- FS2[[3]][,2]
  InterFS <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    InterFS[i] <- min(fs1[i],fs2[i])
  }
  return(InterFS)
}





