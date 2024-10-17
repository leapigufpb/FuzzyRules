#######################################################################################
## Fuzzy Rule-based System for AI and Classification package for 'R'                 ##
##                                                                                   ##
## Ronei Marcos de Moraes, Liliane dos Santos Machado, Jodavid Araujo Ferreira       ##
## Pamplona/Joao Pessoa, October 2023 - August 2024                                  ##
##                                                                                   ##
## Fuzzy Sets:                                                                       ##
##                                                                                   ##
## Membership functions implemented:                                                 ##
## - Triangular                                                                      ##
## - Trapezoidal                                                                     ##
## - Constant                                                                        ##
## - Singleton                                                                       ##
## - Gaussian                                                                        ##
##                                                                                   ##
## Operations implemented:                                                           ##
## - Intersection of two fuzzy sets                                                  ##
## - Union of two fuzzy sets                                                         ##
## - Complement of a fuzzy set                                                       ##
##                                                                                   ##
## t-norms and dual t-conorms implemented:                                           ##
## - Zadeh                                                                           ##
## - Probabilistic                                                                   ##
## - Lukasiewicz                                                                     ##
##                                                                                   ##
## Fuzzy Logic:                                                                      ##
##                                                                                   ##
## Mamdani type                                                                      ##
## - Defuzzification Methods:                                                        ##
##   -- Centroid or Center of Gravity Method - CoG                                   ##
##   -- Center of Sums Method - CoS                                                  ##
##   -- Center of Largest Area Method - CoLA                                         ##
##   -- First of Maxima Method - FoM                                                 ##
##   -- Last of Maxima Method - LoM                                                  ##
##   -- Mean of Maxima Method - MoM                                                  ##
##                                                                                   ##
##                                                                                   ##
#######################################################################################


################################################
##             START OF FUNCTIONS             ##
################################################

###################################################################
## Define the Universe X (a limited set) of Work (Set.Universe): ##
## from min(X) to max(X) with fixed resolution = 0.05            ##
###################################################################
## Bug: trash in the last decimals of Universe was fixed with [X = round(X,6)]

#' @export
Set.Universe <- function(minX, maxX) {
## Check if the membership function can be created for those parameters and the Universe U
  flag = 1
  if (maxX < minX) stop("Error: The Universe can not be created: min < max")

  if (flag == 1) {
    resolution = 0.05
    h <- round((maxX - minX)/resolution) + 1
    X <- rep(0, h)
    X[1] <- minX
    aux <- minX
    i = 2
    while ( i <= h ) {
      X[i] <- X[i-1] + resolution
      i=i+1
    }

    # Clean the vector w.r.t. some trashes in the last decimals
    X = round(X,6)
    # return Universe by resolution
    return(X)
  }
}


######################################################
## The sets XXXFS must be suppourt in the Universe, ##
## it means: min(X) <= support(xxxFS) <= max(X)     ##
######################################################

##################################
## Triangular fuzzy set (TriFS) ##
## Parameters:                  ##
## a - the lower boundary       ##
## b - max value of MF - center ##
## c - the upper boundary       ##
##################################
##  Bug shift MF was fixed. Changed code for copying MF_temp to the MF vector

#' @export
TriFS <- function(U,a,b,c, alphamax = 1) {
## Check if the membership function can be created for those parameters and the Universe U
  flag = 1

  if (a < min(U) || a > max(U) || c < min(U) || c > max(U)){
    stop("Support of the Fuzzy Sets must be include in the Universe")
  }
  if (a > b || b > c || c < a) stop("a <= b <= c for a triangular membership function")
  # -- Jodavid --
  if (alphamax > 1) stop("alphamax value must be less than 1")

  if (flag == 1) {
    ## Auxiliar variables
    aa <- 0.0
    bb <- alphamax # Jodavid - 1.0
    T1 <- c(a, b)
    T2 <- c(aa, bb)
    T3 <- c(b, c)
    T4 <- c(bb, aa)

    supportL <- seq(a, b, 0.05)
    supportR <- seq(b, c, 0.05)

    ##linear regresssions to estimate fuzzy set
    res = stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res = stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a triangular fuzzy set (a,b)
    if(length(supportL) == 1) {
      left_interval = 1
    } else {
      left_interval <- supportL*coeff1 + intercept1
    }

    ## Right-side of a triangular fuzzy set (c,d)
    if (length(supportR) == 1) {
      right_interval = 1
    } else {
      right_interval <-  supportR*coeff2 + intercept2
    }

    ## Unifying Left and Right
    si <- supportR
    si <- si[2:length(si)]
    support = c(supportL, si)
    ri <- right_interval
    ri<- ri[2:length(ri)]
    MF_temp=c(left_interval, ri)

    pos_min_support <- max(which(U<=a))
    pos_max_support <- min(which(U>=c))

    MF <- rep(0, length(U))

    ## Copia MF_temp para o vetor MF substituindo os zeros nas posicoes adequadas

    j=1
    for (i in pos_min_support:pos_max_support) {
      MF[i] <- MF_temp[j]
      j = j + 1
    }

   ## Build the matrix MF_Tri with Universe and its Triangular Membership Function
   MF_Tri <- cbind(U, MF)


   ## Compute the alpha-cuts for the Triangular Fuzzy Set
   #alpha <- seq(0, 1.0, 0.05)
   # -- Jodavid --
   # O valor do TraFS de 81, foi referência para esse
   alpha <- seq(0, alphamax, length.out = 41) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
   # -- Jodavid --

   ##linear regresssions to estimate fuzzy set
   res = stats::lm(formula = T1 ~ T2)
   intercept1 <- res$coefficients[1]
   coeff1 <- res$coefficients[2]
   res = stats::lm(formula = T3 ~ T4)
   intercept2 <- res$coefficients[1]
   coeff2 <- res$coefficients[2]

   ## Left-side of a triangular fuzzy set (a,b)
   left_interval <- alpha*coeff1 + intercept1
   ## Right-side of a triangular fuzzy set (c,d)
   right_interval <-  alpha*coeff2 + intercept2

   ri <- sort(right_interval)
   ri<- ri[2:length(ri)]
   support=c(left_interval, ri)
   compl <- (alphamax-alpha) #Jodavid = (1-alpha)
   compl <- compl[2:length(compl)]
   alpha_cut=c(alpha, compl)
   alpha_cuts <- cbind(support, alpha_cut)

   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts

   Lst <- list(name="Triangular", parameters=c(a,b,c), Membership.Function=MF_Tri, alphacuts=alpha_cuts)

  return(Lst)
  }
}




###################################################
## Trapezoidal fuzzy set (TraFS)                 ##
## Parameters:                                   ##
## a - the lower boundary                        ##
## b and c - flat or interval of max value of MF ##
## d - the upper boundary                        ##
###################################################

#' @export
TraFS <- function(U,a,b,c,d, alphamax = 1) {

## Check if the membership function can be created for those parameters and the Universe U
  flag = 1

  if (a < min(U) || a > max(U) || d < min(U) || d > max(U)) {
    stop("Support of the Fuzzy Sets must be include in the Universe")
  }
  if (a > b || a > c || a > d || b > c || b > d || c > d) {
    stop("a <= b <= c <= d for a trapezoidal membership function")
  }
  # -- Jodavid --
  if (alphamax > 1) stop("alphamax value must be less than 1")

  if (flag == 1) {
    ## Auxiliar variables
    aa <- 0.0
    bb <- alphamax # Jodavid 1.0
    T1 <- c(a, b)
    T2 <- c(aa, bb)
    T3 <- c(c, d)
    T4 <- c(bb, aa)
    supportL <- seq(a, b, 0.05)
    supportC <- seq(b, c, 0.05)
    supportR <- seq(c, d, 0.05)

    ##linear regresssion to estimate fuzzy set
    res = stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res = stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]

    ## Left-side of a trapezoidal fuzzy set (a,b)
    if(length(supportL) == 1) {
      left_interval = 1
    } else {
      left_interval <- supportL*coeff1 + intercept1
    }

    ## Flat of a trapezoidal fuzzy set (b,c)
    #center_interval <- rep(1, length(supportC))
    center_interval <- rep(alphamax, length(supportC))

    ## Right-side of a trapezoidal fuzzy set (c,d)
    if (length(supportR) == 1) {
      right_interval = 1
    } else {
      right_interval <-  supportR*coeff2 + intercept2
    }

    ## Unifying Left, Center and Right
    ui <- supportC
    ui <- ui[2:length(ui)]
    si <- supportR
    si <- si[2:length(si)]
    support = c(supportL, ui, si)
    ti <- center_interval
    ti<- ti[2:length(ti)]
    ri <- right_interval
    ri<- ri[2:length(ri)]
    MF_temp=c(left_interval, ti, ri)

    pos_min_support <- max(which(U<=a))
    pos_max_support <- min(which(U>=d))

    MF <- rep(0, length(U))

    ## Copia MF_temp para o vetor MF substituindo os zeros nas posicoes adequadas

    j=1
    for (i in pos_min_support: pos_max_support) {
      MF[i] <- MF_temp[j]
      j = j + 1
    }

   MF <- round(MF, 7)
   MF_Tra <- cbind(U, MF)

   ## Compute the alpha-cuts for the Triangular Fuzzy Set
   #alpha <- seq(0, 1.0, 0.05)
   # -- Jodavid --
   # Imagem referência para as demais, ou seja, o valor out das outras foram baseadas nessa
   alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
   # -- Jodavid --


   ##linear regresssions to estimate fuzzy set
   res = stats::lm(formula = T1 ~ T2)
   intercept1 <- res$coefficients[1]
   coeff1 <- res$coefficients[2]
   res = stats::lm(formula = T3 ~ T4)
   intercept2 <- res$coefficients[1]
   coeff2 <- res$coefficients[2]

   ## Left-side of a trapezoidal fuzzy set (a,b)
   left_interval <- alpha*coeff1 + intercept1
   ## Flat of a trapezoidal fuzzy set (b,c)
   center_interval <- seq(b, c, length.out = length(alpha)) # Correspondente ao tamanho default do trap #seq(b, c, 0.05)
   ## Right-side of a trapezoidal fuzzy set (c,d)
   right_interval <-  alpha*coeff2 + intercept2

   ri <- sort(right_interval)
   ri<- ri[2:length(ri)]
   ti <- center_interval
   ti<- ti[2:length(ti)]
   support=c(left_interval, ti, ri)
   compl <- (alphamax - alpha)
   compl <- compl[2:length(compl)]
   compl2 <- rep(1, length(ti))
   ## compl2 <- compl2[2:length(compl2)]
   alpha_cut=c(alpha, compl2, compl)
   alpha_cuts <- cbind(support, alpha_cut)

   lengthvetorout <- (21*4)-3 #intervalos de alpha - 2 valores repetidos de interseção


  if(length(alpha_cuts[,1]) < lengthvetorout){
    quantidade <- lengthvetorout - length(alpha_cuts[,1])
    # Verificando se é par
    if((quantidade %% 2) == 0){
      quant_metade <- quantidade/2
      ini_1 <- rep(alpha_cuts[1,1],quant_metade)
      ini_2 <- rep(alpha_cuts[1,2],quant_metade)
      fim_1 <- rep(utils::tail(alpha_cuts,1)[1],quant_metade)
      fim_2 <- rep(utils::tail(alpha_cuts,1)[2],quant_metade)
      temp1 <- c(ini_1,alpha_cuts[,1],fim_1)
      temp2 <- c(ini_2,alpha_cuts[,2],fim_2)
      alpha_cuts <- cbind(support = temp1,alpha_cut = temp2)
      alpha_cuts <- matrix(alpha_cuts, ncol=2);
      colnames(alpha_cuts) <- c("support", "alpha_cut")
    }else{
      quant_metade <- floor(quantidade/2)
      ini_1 <- rep(alpha_cuts[1,1],quant_metade)
      ini_2 <- rep(alpha_cuts[1,2],quant_metade)
      fim_1 <- rep(utils::tail(alpha_cuts,1)[1],quant_metade+1)
      fim_2 <- rep(utils::tail(alpha_cuts,1)[2],quant_metade+1)
      temp1 <- c(ini_1,alpha_cuts[,1],fim_1)
      temp2 <- c(ini_2,alpha_cuts[,2],fim_2)
      alpha_cuts <- cbind(support = temp1,alpha_cut = temp2)
      alpha_cuts <- matrix(alpha_cuts, ncol=2);
      colnames(alpha_cuts) <- c("support", "alpha_cut")
    }

  }else{
    alpha_cuts <- alpha_cuts
  }


   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts

   Lst <- list(name="Trapezoidal", parameters=c(a,b,c,d), Membership.Function=MF_Tra, alphacuts=alpha_cuts)

  return(Lst)
  }
}



##################################
## Constant fuzzy set (ConstFS) ##
## Parameters:                  ##
## x is the MF constant value   ##
## for all elements in U        ##
##################################
#' @export
ConstFS <- function(U,x, alphamax = 1) {
## Check if the membership function can be created for those parameters and the Universe U
  flag = 1

  if (x < 0 || x > 1) {
    stop("Value x for Constant Fuzzy Set must be include in the [0,1]")
  }
  # -- Jodavid --
  if (alphamax > 1) stop("alphamax value must be less than 1")

  if (flag == 1) {
    MF <- rep(x, length(U))
  }
  MF_Const <- cbind(U, MF)

   ## Compute the alpha-cuts for the Constant Fuzzy Set - REVER - [min(U),max(U)] VALE AtEH 0.7
  #alpha <- seq(0, 1.0, 0.05)
  # -- Jodavid --
  alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
  # -- Jodavid --
   left_interval <- rep(0, length(alpha))
   right_interval <- rep(0, length(alpha))
   interval <- seq(0, x, 0.05)

   ## Left-side of a constant fuzzy set
   for (i in 1 : length(interval)) {
     left_interval[i] <- min(U)
   }
   ## Right-side of a constant fuzzy set
   for (i in 1 : length(interval)) {
     right_interval[i] <- max(U)
   }

   ri <- sort(right_interval)
   ri<- ri[2:length(ri)]
   support=c(left_interval, ri)
   compl <- (alphamax-alpha)
   compl <- compl[2:length(compl)]
   alpha_cut=c(alpha, compl)
   alpha_cuts <- cbind(support, alpha_cut)

   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts

   Lst <- list(name="Constant", parameters=c(x), Membership.Function=MF_Const, alphacuts=alpha_cuts)

  return(Lst)

}



###################################
## Singleton fuzzy set (SingFS): ##
## Parameters:                   ##
## x only one value for          ##
## MF = 1, if U = x; 0, U <> x.  ##
###################################


## Singleton fuzzy set (SingFS): MF = 1, if U = x; 0, U <> x.
#' @export
SingFS <- function(U,x, alphamax = 1) {
## Check if the membership function can be created for those parameters and the Universe U
  flag = 1

  if (x < min(U) || x > max(U)) {
    stop("Value x for Constant Fuzzy Set must be include in the Universe")
  }
  # -- Jodavid --
  if (alphamax > 1) stop("alphamax value must be less than 1")

  if (flag == 1) {
    MF <- rep(0, length(U))
    resolution <- U[2] - U[1]
    if ( (x / resolution - round(x/resolution)) == 0 ) {
      position <- which(U == x)
      MF[position] = 1
    } else {
      position <- round(x/resolution)+1
      MF[position] = 1    ## approximate value of x position in U
    }
  }
  MF_Sing <- cbind(U, MF)

   ## Compute the alpha-cuts for the Constant Fuzzy Set - Only position U=x has all alpha-cuts
  #alpha <- seq(0, 1.0, 0.05)
  # -- Jodavid --
  alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
  # -- Jodavid --

   ## Left-side of a singleton fuzzy set
   left_interval <- rep(U[position], length(alpha))
   ## Right-side of a singleton fuzzy set
   right_interval <-  rep(U[position], length(alpha))

   ri <- sort(right_interval)
   ri<- ri[2:length(ri)]
   support=c(left_interval, ri)
   compl <- (alphamax-alpha)
   compl <- compl[2:length(compl)]
   alpha_cut=c(alpha, compl)
   alpha_cuts <- cbind(support, alpha_cut)

   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts

   Lst <- list(name="Singleton", parameters=c(x), Membership.Function=MF_Sing, alphacuts=alpha_cuts)

  return(Lst)

}



##################################
## Gaussian fuzzy set (GauFS)   ##
## Parameters:                  ##
## m – Mean of FS               ##
## s – Standasd Deviation of FS ##
##################################
## Non-zero values in the MF - fixed
## Outliers in the alpha-cuts - fixed
#' @export
GauFS <- function(U,m,s, alphamax = 1) {
## Check if the membership function can be created for those parameters and the Universe U
  flag = 1

  if (m < min(U) || m > max(U)) {
    stop("Mean of the Gaussian Fuzzy Set must be include in the Universe")
  }
  # -- Jodavid --
  if (alphamax > 1) stop("alphamax value must be less than 1")

  if (flag == 1) {
    ## Auxiliar variables
    ## Left-side of a Gaussian fuzzy set (m,s)
    left_interval <- rep(0, length(U))
    right_interval <- rep(0, length(U))
    MF <- rep(0, length(U))
    i = 1
    left_interval[1] <- exp(-0.5*((U[1]-m)/s)^2)
    while (left_interval[i] < 1  & i < length(U)) {
      i = i + 1
      left_interval[i] <- exp(-0.5*((U[i]-m)/s)^2)
    }
    for (j in (i+1):length(U)) {
      right_interval[j] <- exp(-0.5*((U[j]-m)/s)^2)
    }
    ## Unifying Left and Right
    for ( j in 1:length(U) ) {
      MF[j] <- max(left_interval[j], right_interval[j])
  }

  MF <- round(MF, 7)
  MF_Gau <- cbind(U, MF)

   ## Compute the alpha-cuts for the Gaussian Fuzzy Set
  #alpha <- seq(0, 1.0, 0.05)
  # -- Jodavid --
  alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
  # -- Jodavid --


   left_interval <- rep(0, length(alpha))
   right_interval <- rep(0, length(alpha))

   for(j in 1 : length(alpha)) {
     left_interval[j] <- m + s*sqrt(-2*log(alpha[j]))
   }
   for(j in 1 : length(alpha)) {
     right_interval[j] <- m - s*sqrt(-2*log(alpha[j]))
   }

  #########################################################
  ## Check if the extreme values are +/- Infinity
  ##
  if (left_interval[1] == Inf) {
    left_interval[1] <- U[max(which(MF>0))]
    ##   Procura os valores > U_max de left_interval e substitui pelo valor de U_max
    check <- left_interval > max(U)
    for (i in 1:length(left_interval)) {
      if (check[i] == TRUE) {
        left_interval[i] <- max(U)
      }
    }
  }

  if (right_interval[1] == -Inf) {
    right_interval[1] <- U[1]
    ##   Procura os valores < U_min de right_interval e substitui pelo valor de U_min
    check <- right_interval < min(U)
    for (i in 1:length(right_interval)) {
      if (check[i] == TRUE) {
        right_interval[i] <- min(U)
      }
    }
  }
  #########################################################

    ## Unifying Left and Right
    ri <- sort(left_interval)
    ri<- ri[2:length(ri)]
    support=c(right_interval, ri)
    compl <- (alphamax-alpha)
    compl <- compl[2:length(compl)]
    alpha_cut=c(alpha, compl)
    alpha_cuts <- cbind(support, alpha_cut)

   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts

   Lst <- list(name="Gaussian", parameters=c(m,s), Membership.Function=MF_Gau, alphacuts=alpha_cuts)

  return(Lst)
  }
}



#########################################################
## Intersection of two fuzzy sets on the same Universe ##
## Intersection is given by a t-norm                   ##
#########################################################
#' @export
interFS  <- function(U,FS1, FS2, method="Zadeh") {

  fs1 <- FS1[[3]][,2]
  fs2 <- FS2[[3]][,2]
  InterFS <- rep(0, length(U))

  if (method == "Zadeh") {
    for ( i in 1:length(U) ) {
      InterFS[i] <- min(fs1[i],fs2[i])
    }
  }

  if (method == "Probabilistic") {
    for ( i in 1:length(U) ) {
      InterFS[i] <- (fs1[i]*fs2[i])
    }
  }

  if (method == "Lukasiewicz") {
    for ( i in 1:length(U) ) {
      InterFS[i] <- min(fs1[i]+fs2[i]-1.0)
    }
  }

  return(InterFS)
}



##################################################
## Union of two fuzzy sets on the same Universe ##
## Union is given by a t-conorm                 ##
##################################################
#' @export
uniFS  <- function(U,FS1, FS2, method="Zadeh") {

  UniFS <- rep(0, length(U))
  fs1 <- FS1[[3]][,2]
  fs2 <- FS2[[3]][,2]

  if (method == "Zadeh") {
    for ( i in 1:length(U) ) {
      UniFS[i] <- max(fs1[i],fs2[i])
    }
  }

  if (method == "Probabilistic") {
    for ( i in 1:length(U) ) {
      UniFS[i] <- (fs1[i]+fs2[i]-fs1[i]*fs2[i])
    }
  }

  if (method == "Lukasiewicz") {
    for ( i in 1:length(U) ) {
      UniFS[i] <- min(fs1[i]+fs2[i],1.0)
    }
  }

  return(UniFS)
}


###########################################################
## Complement of a fuzzy set w.r.t the Universe (compFS) ##
###########################################################
#' @export
compFS  <- function(U,FS) {
  fs <- FS[[3]][,2]
  CompFS <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    CompFS[i] <- (1-fs[i])
  }
  return(CompFS)
}



#################################################################
## New Plot a fuzzy set w.r.t the Universe (plotFS) 07-11-2024 ##
#################################################################
#' @export
plotFS <- function(Universe,FS, col = "blue", lty=1, pch = ".", ylim = c(0,1), main="Fuzzy Set") {

  if(max(ylim) > 1 || min(ylim) < 0) stop("ylim must be between 0 and 1")

  if (FS[[1]] == "Triangular" || FS[[1]] == "Trapezoidal" || FS[[1]] == "Gaussian" || FS[[1]] == "Singleton" || is.na(FS[[1]])) {
    FuzzySet <- FS[[3]][,2]
    plot(Universe, FuzzySet, pch=pch, lty=lty, ylim=ylim, main=main)
    graphics::lines (Universe, FuzzySet, col=col, lty=lty, ylim=ylim, main=main)
  }
  else {
    FuzzySet <- FS
    plot(Universe, FuzzySet, pch=pch, lty=lty, ylim=ylim, main=main)
    graphics::lines (Universe, FuzzySet, col=col, lty=lty, ylim=ylim, main=main)
  }
}



################################################################
## Degree of membership of an element x to a fuzzy set (dmFS) ##
################################################################
#' @export
dmFS <- function(x,U,FS) {
  if (FS[[1]] == "Triangular") {
    parameters <- FS[[2]]
    aa <- 0.0
    bb <- 1.0
    T1 <- c(parameters[1], parameters[2])
    T2 <- c(aa, bb)
    T3 <- c(parameters[2], parameters[3])
    T4 <- c(bb, aa)
    ##linear regresssions to estimate Triangular fuzzy set
    res = stats::lm(formula = T2 ~ T1)
    intercept1 <- res$coefficients[1]
    coeff1 <- res$coefficients[2]
    res = stats::lm(formula = T4 ~ T3)
    intercept2 <- res$coefficients[1]
    coeff2 <- res$coefficients[2]
    yL <- x * coeff1 + intercept1
    if (yL < 0 || yL > 1 || is.na(yL)) { yL = 0}
    yR <- x * coeff2 + intercept2
    if (yR < 0 || yR > 1 || is.na(yR)) { yR = 0}
    y = max(yL, yR)
    return(y)
  }
  if (FS[[1]] == "Trapezoidal") {
    parameters <- FS[[2]]
    if ( x >= parameters[2] & x <= parameters[3] ) {
      y = 1
      return(y)
    }
    else {
      aa <- 0.0
      bb <- 1.0
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
      if (yL < 0 || yL > 1 || is.na(yL)) { yL = 0}
      yR <- x * coeff2 + intercept2
      if (yR < 0 || yR > 1 || is.na(yR)) { yR = 0}
      y = max(yL, yR)
      return(y)
    }
  }
  if (FS[[1]] == "Gaussian") {
    parameters <- FS[[2]]
    y = exp(-0.5*((x-parameters[1])/parameters[2])^2)
    if ( y <= 10^(-4) ) {
      y = 0                ## Valor menor do que 10^-4 => 0
    }
    return(y)
  }
  if (FS[[1]] == "Singleton") {
    parameters <- FS[[2]]
    if ( x == parameters[1] ) {
      y = 1
    } else {
      y = 0
    }
    return(y)
  }
}


#######################################
## Method for computing OR on two FS ##
## Mamdani type using MAX            ##
#######################################
#' @export
OR <- function(FRBS,x,y) {
  if (FRBS == "Mamdani") {
    out <- max(x,y)
  }
  return(out)
}



########################################
## Method for computing AND on two FS ##
## Mamdani type using MIN             ##
########################################
#' @export
AND <- function(FRBS,x,y) {
  if (FRBS == "Mamdani") {
    out <- min(x,y)
  }
  return(out)
}




######################################
## Method for computing NOT on a FS ##
## Mamdani type using (1 - MF(FS))  ##
######################################
#' @export
NOT <- function(FRBS,x) {
  if (FRBS == "Mamdani") {
    out <- (1 - x)
  }
  return(out)
}



################################################
## Method for computing Implication on two FS ##
## Mamdani type using MIN                     ##
################################################
#' @export
Implication <- function(FRBS,x,U,FS) {
  if (FRBS == "Mamdani") {
    FS1 <- ConstFS(U,x)
    MF <- interFS(U,FS1, FS)
  }
  return(MF)
}



#############################################
## Method for computing Aggregation on FSs ##
## Mamdani type using MAX                  ##
##############################################
#' @export
Aggregation <- function(FRBS,U,Lst) {
  if (FRBS == "Mamdani") {
    MF <- Lst[[1]]
    for (i in 2:length(Lst)) {
      for ( j in 1:length(U) ) {
        MF[j] <- max(MF[j], Lst[[i]][j])
      }
    }
  }
  return(MF)
}


#####################################
## Criar lista iniciando com Vetor ##
#####################################
#' @export
listfuzzy <- function(vetor,vetor2){
  if(is.vector(vetor)){
    returnlist <- list(vetor)
  }
  if(is.list(vetor) && is.list(vetor2)){
    returnlist <- c(vetor, vetor2)
  }
  if(is.list(vetor) && is.vector(vetor2, mode = "double")){
    vetor2 <- list(vetor2)
    returnlist <- c(vetor, vetor2)
  }
  return(returnlist)
}



#######################################
## defuzzification methods available ##
## CoG, CoS, CoLA, FoM, LoM and MoM  ##
#######################################
#' @export
defuzzFS <- function(U,FS_Agreg, FS_Outs, method="CoG") {

## Centroid or Center of Gravity Method - CoG
## x-coordinate of Center of Gravity for a Fuzzy Set
## using FS_Agreg

  if (method == "CoG") {
    defuzz = 0
    sum_memberhip = 0
    for (i in 1: length(U)) {
      defuzz = defuzz + (U[i] * FS_Agreg[i])
      sum_memberhip = sum_memberhip + FS_Agreg[i]
    }
    defuzz = defuzz/sum_memberhip
    return(defuzz)
  }

## Center of Sums Method - CoS
## A Fuzzy Set C is a Union of is fuzzy sets $A_{c_i}$, $i=1,...,n$
## x-coordinate is a weighted mean of geometric centers of the areas $A_{c_i}$
## using FS_Outs

  if (method == "CoS") {
    defuzz = 0
    areaFS <- rep(0, length(FS_Outs))
    xx <- rep(0, length(FS_Outs))
    # Using an approximation by Monte Carlo for  computing the Area
    for (i in 1: length(FS_Outs)) {
      areaFS[i] <- sum(FS_Outs[[i]])/(length(which(FS_Outs[[i]]>0))+2)*(U[max(which(FS_Outs[[i]]>0))]-U[min(which(FS_Outs[[i]]>0))])
      xx[i] <- (U[min(which(FS_Outs[[i]]>0))]+U[max(which(FS_Outs[[i]]>0))])/2
      defuzz <- defuzz + (areaFS[i]*xx[i])
    }
   defuzz <- defuzz/sum(areaFS)
   return(defuzz)
  }

## Center of Largest Area Method - CoLA
## A Fuzzy Set C is a Union of fuzzy sets $A_{c_i}$, $i=1,...,n$
## x-coordinate of center of gravity of the fuzzy sets $A_{c_i}$ with the largest area
## using FS_Outs

 if (method == "CoLA") {
    defuzz = 0
    areaFS <- rep(0, length(FS_Outs))
    max_area = 0
    sum_memberhip = 0
    xx <- rep(0, length(FS_Outs))

    # Using an approximation by Monte Carlo for computing the Area
    for (i in 1: length(FS_Outs)) {
      areaFS[i] <- sum(FS_Outs[[i]])/(length(which(FS_Outs[[i]]>0))+2)*(U[max(which(FS_Outs[[i]]>0))]-U[min(which(FS_Outs[[i]]>0))])
      # Choose the FS with largest Area
      if (max_area < areaFS[i]) {
         max_area = areaFS[i]
         area = i
      }
    }

    FS_Agreg <- FS_Outs[[area]]

    for (i in 1: length(U)) {
      defuzz = defuzz + (U[i] * FS_Agreg[i])
      sum_memberhip = sum_memberhip + FS_Agreg[i]
    }
    defuzz = defuzz/sum_memberhip
    return(defuzz)
  }

## First of Maxima Method - FoM
## x-coordinate of the min of x for the maximum membership values of a Fuzzy Set
## using FS_Agreg

 if (method == "FoM") {
    defuzz = 0
    i = min(which(FS_Agreg>=max(FS_Agreg)))
    defuzz = U[i]
    return(defuzz)
  }

## Last of Maxima Method - LoM
## x-coordinate of the max of x for the maximum membership values of a Fuzzy Set
## using FS_Agreg

 if (method == "LoM") {
    defuzz = 0
    i = max(which(FS_Agreg>=max(FS_Agreg)))
    defuzz = U[i]
    return(defuzz)
  }

## Mean of Maxima Method - MoM
## x-coordinate of the mean of set of x for which the membership values of a Fuzzy Set is maximum
## using FS_Agreg

 if (method == "MoM") {
    defuzz = 0
    i = min(which(FS_Agreg>=max(FS_Agreg)))
    j = max(which(FS_Agreg>=max(FS_Agreg)))
    defuzz = (U[i]+U[j])/2
    return(defuzz)
  }

}


##############################################
##             END OF FUNCTIONS             ##
##############################################

