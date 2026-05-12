#######################################################################################
## Fuzzy Rule-based System for AI and Classification package for 'R'                 ##
##                                                                                   ##
## Ronei Marcos de Moraes, Liliane dos Santos Machado, Jodavid Araujo Ferreira,      ##
## Sergio Cauã dos Santos                                                            ##
## Pamplona/Joao Pessoa, October 2023 - April 2026                                   ##
##                                                                                   ##
## Type-1 Fuzzy Sets:                                                                ##
##                                                                                   ##
## Type-1 Membership functions implemented:                                          ##
## - Triangular                                                                      ##
## - Trapezoidal                                                                     ##
## - Rectangular                                                                     ##
## - Constant                                                                        ##
## - Singleton                                                                       ##
## - Gaussian                                                                        ##
## - Cauchy                                                                          ##
## - Generalized Bell                                                                ##
## - Pi-Shaped                                                                       ##
## - Sigmoidal                                                                       ##
## - S-Shaped                                                                        ##
## - Z-Shaped                                                                        ##
##                                                                                   ##
## Operations on Type-1 Fuzzy Sets implemented:                                      ##
## - Intersection of two fuzzy sets                                                  ##
## - Union of two fuzzy sets                                                         ##
## - Complement of a fuzzy set                                                       ##
## - Support of a fuzzy set                                                          ##
## - Kernel of a fuzzy set                                                           ##
## - Core of a fuzzy set                                                             ##
## - Height of a fuzzy set                                                           ##
## - Degree of membership of an element x to a fuzzy set                             ##
## - Overlap function for two fuzzy sets                                             ##
## - Grouping function for two fuzzy sets                                            ##
## - Overlap index for two fuzzy sets                                                ##
## - Grouping index for two fuzzy sets                                               ##
## - Jaccard fuzzy index between two Type-1 Fuzzy Sets                               ##
##                                                                                   ##
## Plots of Type-1 Fuzzy Sets implemented:                                           ##
## - Plot a fuzzy set                                                                ##
## - Plot alpha-cuts of a fuzzy set                                                  ##
##                                                                                   ##
## t-norms and dual t-conorms implemented:                                           ##
## - Zadeh                                                                           ##
## - Probabilistic                                                                   ##
## - Lukasiewicz                                                                     ##
## - Weber                                                                           ##
##                                                                                   ##
## Type-1 Fuzzy Logic:                                                               ##
##                                                                                   ##
## Type-1 Fuzzy Logical Operations implemented:                                      ##
## - AND for multiple fuzzy sets                                                     ##
## - OR for multiple fuzzy sets                                                      ##
## - NOT for a fuzzy set                                                             ##
##                                                                                   ##
## Type-1 Mamdani type                                                               ##
## - Implication for a rule                                                          ##
## - Implication of rules                                                            ##
## - Defuzzification Methods:                                                        ##
##   -- Centroid or Center of Gravity Method - CoG                                   ##
##   -- Center of Sums Method - CoS                                                  ##
##   -- Center of Largest Area Method - CoLA                                         ##
##   -- First of Maxima Method - FoM                                                 ##
##   -- Last of Maxima Method - LoM                                                  ##
##   -- Middle of Maxima Method - MoM                                                ##
##   -- First of Support Method - FoS                                                ##
##   -- Last of Support Method - LoS                                                 ##
##   -- Mean of Support Method - MoS                                                 ##
##   -- BAsic Defuzzification Distribution Method - BADD                             ##
##                                                                                   ##
## Interval Type-2 Fuzzy Sets (IT2FS)                                                ##
##                                                                                   ##
## Type-2 Membership functions implemented:                                          ##
## - Create an IT2FS from any combination of two membership functions described      ##
##   below:                                                                          ##
##   -- Triangular                                                                   ##
##   -- Trapezoidal                                                                  ##
##   -- Rectangular                                                                  ##
##   -- Gaussian                                                                     ##
##   -- Cauchy                                                                       ##
##   -- Generalized Bell                                                             ##
##   -- Pi-shaped                                                                    ##
##                                                                                   ##
## Operations on Type-2 Fuzzy Sets implemented:                                      ##
## - Intersection of two IT2FS                                                       ##
## - Union of two IT2FS                                                              ##
## - Complement of an IT2FS                                                          ##
## - Degree of membership of an element x to a IT2FS                                 ##
## - Jaccard fuzzy index between two IT2FS                                           ##
##                                                                                   ##
## Plot of an Interval Type-2 Fuzzy Set                                              ##
##                                                                                   ##
## Type-2 Fuzzy Logic:                                                               ##
##                                                                                   ##
## Type-2 Fuzzy Logical Operations implemented:                                      ##
## - AND for two IT2FS                                                               ##
## - OR for two IT2FS                                                                ##
## - NOT for a two IT2FS                                                             ##
##                                                                                   ##
## Type-2 Mamdani type                                                               ##
## - Implication for a rule                                                          ##
## - Implication of rules                                                            ##
## - Type Reduction Methods:                                                         ##
##   -- Karnik-Mendel (KM)                                                           ##
##   -- Enhanced Karnik-Mendel (EKM)                                                 ##
##   -- Iterative Algorithm with Stop Condition (IASC)                               ##
##   -- Enhanced Iterative Algorithm with Stop Condition (EIASC)                     ##
##   -- Nie-Tan (NT)                                                                 ##
##                                                                                   ##
##                                                                                   ##
#######################################################################################


################################################
##             START OF FUNCTIONS             ##
################################################

###################################################################
## Define the Universe X (a limited set) of Work (Set.Universe): ##
## from min(X) to max(X) with fixed resolutionFS = 0.05          ##
###################################################################
## Bug: trash in the last decimals of Universe was fixed with [X = round(X,6)]

resolutionFS = 0.05

#' @export
Set.Universe <- function(minX, maxX) {
    ## Check if the membership function can be created for those parameters and the Universe U
    flag = 1
    if (maxX < minX) stop("Error: The Universe can not be created: min < max")
    
    if (flag == 1){
        h <- round((maxX - minX)/resolutionFS) + 1
        X <- rep(0, h)
        X[1] <- minX
        aux <- minX
        i = 2
        while ( i <= h ) {
            X[i] <- X[i-1] + resolutionFS
            i=i+1
        }
        
        # Clean the vector w.r.t. some trashes in the last decimals
        X = round(X,6)
        # return Universe by resolutionFS
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
    
    check_resolution(c(a, b, c), c("a", "b", "c"), "Triangular Fuzzy Set")
    
    if (flag == 1) {
        ## Auxiliar variables
        aa <- 0.0
        bb <- alphamax # Jodavid - 1.0
        T1 <- c(a, b)
        T2 <- c(aa, bb)
        T3 <- c(b, c)
        T4 <- c(bb, aa)
        
        supportL <- seq(a, b, resolutionFS)
        supportR <- seq(b, c, resolutionFS)
        
        #Checking the intersections
        ## numerical inaccuracy correction
        if(abs(supportL[length(supportL)] - b) > 1e-10){ supportL <- c(supportL, b)}
        if(abs(supportR[length(supportR)] - c) > 1e-10){ supportR <- c(supportR,c)}
        
        ##linear regresssions to estimate fuzzy set
        res=lm(formula = T2 ~ T1)
        intercept1 <- res$coefficients[1]
        coeff1 <- res$coefficients[2]
        res=lm(formula = T4 ~ T3)
        intercept2 <- res$coefficients[1]
        coeff2 <- res$coefficients[2]
        
        ## Left-side of a triangular fuzzy set (a,b)
        if(length(supportL) == 1) {
            left_interval = alphamax
        } else {
            left_interval <- supportL*coeff1 + intercept1
        }
        
        ## Right-side of a triangular fuzzy set (c,d)
        if (length(supportR) == 1) {
            right_interval = alphamax
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
        # -- Jodavid --
        alpha <- seq(0, alphamax, length.out = 21) # Size equivalent to going from 0 to 1 with step 0.05
        # -- Jodavid --
        
        ##linear regresssions to estimate fuzzy set
        res=lm(formula = T1 ~ T2)
        intercept1 <- res$coefficients[1]
        coeff1 <- res$coefficients[2]
        res=lm(formula = T3 ~ T4)
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
        
        Lst <- list(name="Triangular", parameters=c(a,b,c, alphamax), Membership.Function=MF_Tri, alphacuts=alpha_cuts)
        
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
    
    check_resolution(c(a, b, c, d), c("a", "b", "c", "d"), "Trapezoidal Fuzzy Set")
    
    if (flag == 1) {
        ## Auxiliar variables
        aa <- 0.0
        bb <- alphamax # Jodavid 1.0
        T1 <- c(a, b)
        T2 <- c(aa, bb)
        T3 <- c(c, d)
        T4 <- c(bb, aa)
        supportL <- seq(a, b, resolutionFS)
        supportC <- seq(b, c, resolutionFS)
        supportR <- seq(c, d, resolutionFS)
        
        #Checking the intersections
        ## numerical inaccuracy correction
        if(abs(supportL[length(supportL)] - b) > 1e-10){ supportL <- c(supportL, b)}
        if(abs(supportC[length(supportC)] - c) > 1e-10){ supportC <- c(supportC,c)}
        if(abs(supportR[length(supportR)] - d) > 1e-10){ supportR <- c(supportR, d)}
        
        ##linear regresssion to estimate fuzzy set
        res=lm(formula = T2 ~ T1)
        intercept1 <- res$coefficients[1]
        coeff1 <- res$coefficients[2]
        res=lm(formula = T4 ~ T3)
        intercept2 <- res$coefficients[1]
        coeff2 <- res$coefficients[2]
        
        ## Left-side of a trapezoidal fuzzy set (a,b)
        if(length(supportL) == 1) {
            left_interval = alphamax
        } else {
            left_interval <- supportL*coeff1 + intercept1
        }
        
        ## Flat of a trapezoidal fuzzy set (b,c)
        center_interval <- rep(alphamax, length(supportC))
        
        ## Right-side of a trapezoidal fuzzy set (c,d)
        if (length(supportR) == 1) {
            right_interval = alphamax
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
        
        ## Copy MF_temp to the MF vector replacing the zeros in the appropriate positions
        j=1
        for (i in pos_min_support: pos_max_support) {
            MF[i] <- MF_temp[j]
            j = j + 1
            
        }
        
        MF <- round(MF, 7)
        MF_Tra <- cbind(U, MF)
        
        ## Compute the alpha-cuts for the Triangular Fuzzy Set
        # -- Jodavid --
        # Imagem referência para as demais, ou seja, o valor out das outras foram baseadas nessa
        alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
        #-- Jodavid --
        
        
        ##linear regresssions to estimate fuzzy set
        res=lm(formula = T1 ~ T2)
        intercept1 <- res$coefficients[1]
        coeff1 <- res$coefficients[2]
        res=lm(formula = T3 ~ T4)
        intercept2 <- res$coefficients[1]
        coeff2 <- res$coefficients[2]
        
        ## Left-side of a trapezoidal fuzzy set (a,b)
        left_interval <- alpha*coeff1 + intercept1
        ## Flat of a trapezoidal fuzzy set (b,c)
        center_interval <- c(b,c)
        ## Right-side of a trapezoidal fuzzy set (c,d)
        right_interval <-  alpha*coeff2 + intercept2
        
        ri <- sort(right_interval)
        ri<- ri[2:length(ri)]
        support=c(left_interval, center_interval, ri)
        compl <- (alphamax - alpha)
        compl <- compl[2:length(compl)]
        compl2 <- c(alphamax, alphamax)
        alpha_cut=c(alpha, compl2, compl)
        alpha_cuts <- cbind(support, alpha_cut)
        
        lengthvetorout <- (length(alpha) * 3) - 2 #alpha ranges - 2 intersecting repeating values
        
        
        if(length(alpha_cuts[,1]) < lengthvetorout){
            quantidade <- lengthvetorout - length(alpha_cuts[,1])
            # Checking if it is even
            if((quantidade %% 2) == 0){
                quant_metade <- quantidade/2
                ini_1 <- rep(alpha_cuts[1,1],quant_metade)
                ini_2 <- rep(alpha_cuts[1,2],quant_metade)
                fim_1 <- rep(tail(alpha_cuts,1)[1],quant_metade)
                fim_2 <- rep(tail(alpha_cuts,1)[2],quant_metade)
                temp1 <- c(ini_1,alpha_cuts[,1],fim_1)
                temp2 <- c(ini_2,alpha_cuts[,2],fim_2)
                alpha_cuts <- cbind(support = temp1,alpha_cut = temp2)
                alpha_cuts <- matrix(alpha_cuts, ncol=2);
                colnames(alpha_cuts) <- c("support", "alpha_cut")
            }else{
                quant_metade <- floor(quantidade/2)
                ini_1 <- rep(alpha_cuts[1,1],quant_metade)
                ini_2 <- rep(alpha_cuts[1,2],quant_metade)
                fim_1 <- rep(tail(alpha_cuts,1)[1],quant_metade+1)
                fim_2 <- rep(tail(alpha_cuts,1)[2],quant_metade+1)
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
        
        Lst <- list(name="Trapezoidal", parameters=c(a,b,c,d, alphamax), Membership.Function=MF_Tra, alphacuts=alpha_cuts)
        
        return(Lst)
    }
}

######################################
## Rectangular fuzzy set (RectFS)   ##
## Parameters:                      ##
## a - the lower boundary           ##
## b - the upper boundary           ##
######################################

#' @export
RectFS <- function(U, a, b, alphamax = 1) {
    
    ## Check if the membership function can be created for those parameters and the Universe U
    flag = 1
    
    if (a < min(U) || a > max(U) || b < min(U) || b > max(U)) {
        stop("Support of the Fuzzy Sets must be include in the Universe")
    }
    if (a > b) {
        stop("a < b for a rectangular membership function")
    }
    
    if (alphamax > 1) stop("alphamax value must be in (0,1]")
    
    check_resolution(c(a, b), c("a", "b"), "Rectangular Fuzzy Set")
    
    ## a and b are inside the support
    if (a >= min(U) & a <= max(U) & b >= min(U) & b<= max(U)) {
        MF <- rep(0, length(U))
        supportRect <- seq(a, b, resolutionFS)
        pos_min_support <- max(which(U<=a))
        pos_max_support <- min(which(U>=b))
        Rect_interval <- rep(alphamax, length(supportRect))
        j=1
        for (i in pos_min_support: pos_max_support) {
            MF[i] <- Rect_interval[j]
            j = j + 1
        }
        MF_Rect <- cbind(U, MF)
    }
    
    ## Compute the alpha-cuts for the Rectangular Fuzzy Set
    
    alpha <- seq(0, alphamax, resolutionFS)
    left_interval <- rep(0, length(alpha))
    right_interval <- rep(0, length(alpha))
    interval <- seq(0, alphamax, resolutionFS)
    
    ## Left-side of a Rectangular fuzzy set
    for (i in 1 : length(interval)) {
        left_interval[i] <- a
    }
    ## Right-side of a Rectangular fuzzy set
    for (i in 1 : length(interval)) {
        right_interval[i] <- b
    }
    
    center_interval <- c(a,b)
    
    ri <- sort(right_interval)
    ri<- ri[2:length(ri)]
    ti <- center_interval
    
    support=c(left_interval, ti, ri)
    compl <- (alphamax - alpha)
    compl <- compl[2:length(compl)]
    compl2 <- c(alphamax, alphamax)
    ## compl2 <- compl2[2:length(compl2)]
    alpha_cut=c(alpha, compl2, compl)
    alpha_cuts <- cbind(support, alpha_cut)
    
    
    ## Output is a list with the name of type of FS, its parameters,
    ## the Membership Function created and its alpha cuts
    
    Lst <- list(name="Rectangular", parameters=c(a,b, alphamax), Membership.Function=MF_Rect, alphacuts=alpha_cuts)
    
    return(Lst)
    
}


##################################
## Constant fuzzy set (ConstFS) ##
## Parameters:                  ##
## x is the MF constant value   ##
## for all elements in U        ##
##################################

#' @export
ConstFS <- function(U,x, alphamax = x) {
    ## Check if the membership function can be created for those parameters and the Universe U
    flag = 1
    
    if (x < 0 || x > 1) {
        stop("Value x for Constant Fuzzy Set must be include in the [0,1]")
    }
    # -- Jodavid --
    if (alphamax > 1) stop("alphamax value must be in (0,1]")
    
    if (flag == 1) {
        MF <- rep(x, length(U))
    }
    MF_Const <- cbind(U, MF)
    
    ## Compute the alpha-cuts for the Constant Fuzzy Set
    
    alpha <- seq(0, x, resolutionFS)
    # -- Jodavid --
    # alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
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
    # ri<- ri[2:length(ri)]
    support=c(left_interval, ri)
    compl <- (x-alpha)
    #compl <- compl[2:length(compl)]
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
    if (alphamax > 1) stop("alphamax value must be in (0,1]")
    
    check_resolution(x, "x", "Singleton Fuzzy Set")
    
    if (flag == 1) {
        MF <- rep(0, length(U))
        resolutionFS <- U[2] - U[1]
        if ( (x / resolutionFS - round(x/resolutionFS)) < 1^(-10) ) {
            position <- which(U == x)
            MF[position] = 1
        } else {
            position <- round(x/resolutionFS)+1
            MF[position] = 1    ## approximate value of x position in U
        }
    }
    MF_Sing <- cbind(U, MF)
    
    ## Compute the alpha-cuts for the Constant Fuzzy Set - Only position U=x has all alpha-cuts
    alpha <- seq(0, 1.0, resolutionFS)
    # -- Jodavid --
    # alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
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
    
    check_resolution(m, "m", "Gaussian Fuzzy Set")
    
    if (flag == 1) {
        ## Auxiliar variables
        ## Left-side of a Gaussian fuzzy set (m,s)
        left_interval <- rep(0, length(U))
        right_interval <- rep(0, length(U))
        MF <- rep(0, length(U))
        i = 1
        left_interval[1] <- alphamax * (exp(-0.5*((U[1]-m)/s)^2))
        while (left_interval[i] < 1  & i < length(U)) {
            i = i + 1
            left_interval[i] <- alphamax * (exp(-0.5*((U[i]-m)/s)^2))
        }
        for (j in (i+1):length(U)) {
            right_interval[j] <- alphamax * (exp(-0.5*((U[j]-m)/s)^2))
        }
        ## Unifying Left and Right
        for ( j in 1:length(U) ) {
            MF[j] <- max(left_interval[j], right_interval[j])
        }
        
        MF <- round(MF, 7)
        MF_Gau <- cbind(U, MF)
        
        ## Compute the alpha-cuts for the Gaussian Fuzzy Set
        alpha <- seq(0, alphamax, resolutionFS)
        # -- Jodavid --
        # alpha <- seq(0, alphamax, length.out = 21) # Tamanho equivalente a ir de 0 até 1 com passo 0.05
        # -- Jodavid --
        
        
        left_interval <- rep(0, length(alpha))
        right_interval <- rep(0, length(alpha))
        
        for(j in 1 : length(alpha)) {
            left_interval[j] <- m + s*sqrt(-2*log(alpha[j]/alphamax))
        }
        for(j in 1 : length(alpha)) {
            right_interval[j] <- m - s*sqrt(-2*log(alpha[j]/alphamax))
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
        
        Lst <- list(name="Gaussian", parameters=c(m,s, alphamax), Membership.Function=MF_Gau, alphacuts=alpha_cuts)
        
        return(Lst)
    }
}

###################################
## Cauchy fuzzy set (CauFS)      ##
## \mu_A(x) = 1/[1+((x-a)/b)^2]  ##
## Parameters:                   ##
## a - center of FS \in Universe ##
## b - width of FS <> 0          ##
###################################

#' @export
CauFS <- function(U,a,b, alphamax = 1) {
    ## Check if the membership function can be created for those parameters and the Universe U
    flag = 1
    
    if (a < min(U) || a > max(U)) {
        stop("Center of the Cauchy Fuzzy Set must be include in the Universe")
    }
    if (b == 0) {
        stop("Parameter of width of the Cauchy Fuzzy Set can not be zero")
    }
    
    # -- Jodavid --
    if (alphamax > 1) stop("alphamax value must be in (0,1]")
    
    check_resolution(a, "a", "Cauchy Fuzzy Set")
    
    if (flag == 1) {
        ## Auxiliar variables
        ## Left-side of a Cauchy fuzzy set (a,b)
        left_interval <- rep(0, length(U))
        right_interval <- rep(0, length(U))
        MF <- rep(0, length(U))
        i = 1
        left_interval[1] <- alphamax * (1/(1+((U[1]-a)/b)^2))
        while (left_interval[i] < alphamax  & i < length(U)) {
            i = i + 1
            left_interval[i] <- alphamax * (1/(1+((U[i]-a)/b)^2))
        }
        for (j in (i+1):length(U)) {
            right_interval[j] <- alphamax * (1/(1+((U[j]-a)/b)^2))
        }
        ## Unifying Left and Right
        for ( j in 1:length(U) ) {
            MF[j] <- max(left_interval[j], right_interval[j])
        }
        
        MF <- round(MF, 7)
        MF_Cau <- cbind(U, MF)
        
        ## Compute the alpha-cuts for the Cauchy Fuzzy Set
        ## Dutta (2017) Bell-shaped Fuzzy Soft Sets and Their Applications in Medical Diagnosis
        alpha <- seq(0, alphamax, resolutionFS)
        # -- Jodavid --
        # alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
        # -- Jodavid --
        
        
        left_interval <- rep(0, length(alpha))
        right_interval <- rep(0, length(alpha))
        
        for(j in 1 : length(alpha)) {
            left_interval[j] <- a + b *sqrt((alphamax-alpha[j])/alpha[j])
        }
        for(j in 1 : length(alpha)) {
            right_interval[j] <- a - b *sqrt((alphamax-alpha[j])/alpha[j])
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
        
        Lst <- list(name="Cauchy", parameters=c(a,b, alphamax), Membership.Function=MF_Cau, alphacuts=alpha_cuts)
        
        return(Lst)
    }
}

########################################
## Generalized Bell fuzzy set (GBeFS) ##
## \mu_A(x) = 1/(1+abs((x-c)/a)^{2b}))##
## Parameters:                        ##
## a - width of FS <> 0               ##
## b - slopes of FS > 0               ##
## c - center of FS \in Universe      ##
########################################

#' @export
GBeFS <- function(U,a,b,c, alphamax = 1) {
    ## Check if the membership function can be created for those parameters and the Universe U
    flag = 1
    
    if (c < min(U) || c > max(U)) {
        stop("Center of the Generalized Bell Fuzzy Set must be include in the Universe")
    }
    if (a == 0) {
        stop("Parameter of width of the Generalized Bell Fuzzy Set can not be zero")
    }
    if (b <= 0) {
        stop("Parameter of slope of the Generalized Bell Fuzzy Set must be greather than zero")
    }
    
    # -- Jodavid --
    if (alphamax > 1) stop("alphamax value must be in (0,1]")
    
    check_resolution(c, "c", "Generalized Bell Fuzzy Set")
    
    if (flag == 1) {
        ## Auxiliar variables
        ## Left-side of a Generalized Bell fuzzy set (a,b,c)
        left_interval <- rep(0, length(U))
        right_interval <- rep(0, length(U))
        MF <- rep(0, length(U))
        i = 1
        left_interval[1] <- alphamax * (1 / (1 + abs(((U[1]-c)/a)^(2*b))))
        while (left_interval[i] < alphamax  & i < length(U)) {
            i = i + 1
            left_interval[i] <-alphamax * (1 / (1 + abs(((U[i]-c)/a)^(2*b))))
        }
        for (j in (i+1):length(U)) {
            right_interval[j] <- alphamax * (1 / (1 + abs(((U[j]-c)/a)^(2*b))))
        }
        ## Unifying Left and Right
        for ( j in 1:length(U) ) {
            MF[j] <- max(left_interval[j], right_interval[j])
        }
        
        MF <- round(MF, 7)
        MF_GBe <- cbind(U, MF)
        
        ## Compute the alpha-cuts for the Cauchy Fuzzy Set
        alpha <- seq(0, alphamax, resolutionFS)
        # -- Jodavid --
        # alpha <- seq(0, alphamax, length.out = 21) # Tamannho equivalente a ir de 0 até 1 com passo 0.05
        # -- Jodavid --
        
        
        left_interval <- rep(0, length(alpha))
        right_interval <- rep(0, length(alpha))
        
        for(j in 1 : length(alpha)) {
            left_interval[j] <- c + a *(((alphamax-alpha[j])/alpha[j])^(1/(2*b)))
        }
        for(j in 1 : length(alpha)) {
            right_interval[j] <- c - a *(((alphamax-alpha[j])/alpha[j])^(1/(2*b)))
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
        
        Lst <- list(name="Generalized Bell", parameters=c(a,b,c,alphamax), Membership.Function=MF_GBe, alphacuts=alpha_cuts)
        
        return(Lst)
    }
}



##################################
## Pi-shaped fuzzy set (PiFS)   ##
## Parameters:                  ##
## a - left base point of FS    ##
## b - left shoulder of FS      ##
## c - right shoulder of FS     ##
## d - right base point of FS   ##
##################################

#' @export
PiFS <- function(U,a,b,c,d, alphamax = 1){
    flag = 1
    
    ## Check if the membership function can be created for those parameters and the Universe U
    if(a < min(U) || a > max(U) || d < min(U) || d > max(U)){
        stop("Support of the Fuzzy Sets must be include in the Universe")
    }
    if (a >= b || a >= c || a >= d || b >= c || b >= d || c >= d) {
        stop("a < b < c < d for a pi membership function")
    }
    if(alphamax > 1) stop("alphamax value must be less than 1")
    
    check_resolution(c(a, b, c, d), c("a", "b", "c", "d"), "Pi Fuzzy Set")
    
    if(flag == 1){
        
        #MEMBERSHIP FUNCTION
        region_0 <- seq(min(U),a, resolutionFS)
        region_1 <- seq(a, (a+b)/2, resolutionFS)
        region_2 <- seq((a+b)/2, b, resolutionFS)
        region_3 <- seq(b, c, resolutionFS)
        region_4 <- seq(c, (c+d)/2, resolutionFS)
        region_5 <- seq((c+d)/2, d, resolutionFS)
        region_6 <- seq(d,max(U), resolutionFS)
        
        #image of each region
        y_0 <- rep(0, length(region_0))
        y_1 <- alphamax * (2 * ((region_1 - a)/(b-a))**2)
        y_2 <- alphamax *(1 - (2 * ((region_2 - b)/(b-a))**2))
        y_3 <- rep(alphamax,length(region_3))
        y_4 <- alphamax *(1 - (2 * ((region_4 - c)/(d-c))**2))
        y_5 <- alphamax *(2 * ((region_5 - d)/(d-c))**2)
        y_6 <- rep(0, length(region_6))
        
        #joining the intervals
        #checking the intersection
        if (y_0[length(y_0)] == y_1[1]){
            y_1 <- y_1[2:length(y_1)]
        }
        if (y_1[length(y_1)] == y_2[1]){
            y_2 <- y_2[2:length(y_2)]
        }
        if (y_2[length(y_2)] == y_3[1]){
            y_3 <- y_3[2:length(y_3)]
        }
        if (y_3[length(y_3)] == y_4[1]){
            y_4 <- y_4[2:length(y_4)]
        }
        if (y_4[length(y_4)] == y_5[1]){
            y_5 <- y_5[2:length(y_5)]
        }
        if (y_5[length(y_5)] == y_6[1]){
            y_6 <- y_6[2:length(y_6)]
        }
        
        #joining the intervals
        MF_temp <- c(y_0,y_1,y_2,y_3,y_4,y_5,y_6)
        
        
        pos_min_support <- min(which(U<=a))
        pos_max_support <- max(which(U>=d))
        
        MF <- rep(0, length(U))
        
        
        ##Copies MF_temp to the MF vector replacing the zeros in the appropriate positions
        j=1
        for (i in pos_min_support:pos_max_support) {
            MF[i] <- MF_temp[j]
            j = j + 1
            
        }
        
        MF <- round(MF, 7)
        MF_Pi <- cbind(U, MF)
        
        
        #ALPHA-CUTS
        alpha_crescente <- seq(0, alphamax, length.out = 21)
        alpha_decrescente <- rev(alpha_crescente)
        
        
        #creating the supports
        support_crescente <- rep(0,length(alpha_crescente))
        support_centro <- U[which(MF_Pi[,2] == alphamax)]
        support_decrescente <- rep(0,length(alpha_decrescente))
        
        #Region 1 - Reverse
        idx = which((alpha_crescente >= 0) & (alpha_crescente < alphamax/2))
        support_crescente[idx] <- (a + (b - a) * (alpha_crescente[idx]/(2 * alphamax))**(1/2))
        
        #Region 2 - Reverse
        idx = which((alpha_crescente >= alphamax/2) & (alpha_crescente <= alphamax))
        support_crescente[idx] <- (b - (b - a) * ((1 - (alpha_crescente[idx]/alphamax))/2)**(1/2))
        
        #Region 3 - Center
        support_centro <- c(support_centro[1], support_centro[length(support_centro)])
        alpha_centro <- c(alphamax, alphamax)
        
        #Region 4 - Reverse
        idx = which((alpha_decrescente >= alphamax/2) & (alpha_decrescente <= alphamax))
        support_decrescente[idx] <- (c + (d - c) * ((1 - (alpha_decrescente[idx]/alphamax))/2)**(1/2))
        
        #Region 5 - Reverse
        idx = which((alpha_decrescente >= 0) & (alpha_decrescente < alphamax/2))
        support_decrescente[idx] <- (d - (d - c) * (alpha_decrescente[idx]/(2 * alphamax))**(1/2))
        
        #joining
        support = c(support_crescente, support_centro, support_decrescente)
        alpha_cut = c(alpha_crescente, alpha_centro, alpha_decrescente)
        
        
        alpha_cuts =  cbind(support, alpha_cut)
        alpha_cuts
        
        Lst <- list(name="Pi", parameters=c(a,b,c,d,alphamax), Membership.Function=MF_Pi, alphacuts=alpha_cuts)
        
        return(Lst)
    }
}



##################################
## Sigmoidal fuzzy set (SigFS)  ##
## Parameters:                  ##
## a - slope of FS              ##
## b - Crossover point of FS    ##
##################################

#' @export
SigFS <- function(U,a,b, alphamax = 1) {
   ## Check if the membership function can be created for those parameters and the Universe U
   flag = 1
   if (a < min(U) || a > max(U)) {
        stop("Center of the Sigmoidal Fuzzy Set must be include in the Universe")
   }
   if (b < min(U) || b > max(U))
        stop("Parameter of width of the Sigmoidal Fuzzy Set must be include in the Universe")

   MF <- rep(0, length(U))
   
   ## Sigmoidal function for any alphamax
   for ( j in 1:length(U) ) {
            MF[j] <- alphamax*(1 / (1 + exp(-a * (U[j] - b))))
   }

   MF <- round(MF, 7)
   MF_Sig <- cbind(U, MF)
   
   ## plot(U,MF_Sig[,2])

   ## Compute the alpha-cuts for the Sigmoidal Fuzzy Set  for any alphamax
   alpha <- seq(0, alphamax, resolutionFS)
   left_interval <- alpha
   right_interval <- alpha
   
   for(j in 1 : length(alpha)) {
      left_interval[j] <- (b + (1/a) * log(alpha[j]/ (alphamax - alpha[j])))
   }
   for(j in 1 : length(alpha)) {
            right_interval[j] <- U[length(U)]
   }
           
        #########################################################
        ## Check if the extreme values are +/- Infinity
        ##
        if (left_interval[1] == -Inf) {
            left_interval[1] <- U[min(which(MF>0))]
        }
        
        if (left_interval[length(left_interval)] == Inf) {
            left_interval[length(left_interval)] <- U[length(U)]
        }
        #########################################################

   compl <- (alphamax-alpha)
   support <- c(left_interval, right_interval)
   alpha_cut=c(alpha, compl)
   alpha_cuts <- cbind(support, alpha_cut)
   
   ## plot(support, alpha_cut)
   ## graphics::lines (support, alpha_cut)
   
   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts
        
   Lst <- list(name="Sigmoidal", parameters=c(a,b,alphamax), Membership.Function=MF_Sig, alphacuts=alpha_cuts)

}



##################################
## S-Shaped fuzzy set (SShaFS)  ##
## Parameters:                  ##
## a - lower bound              ##
## c - upper bound              ##
##################################

#' @export
SShaFS <- function(U, a, c, alphamax = 1) {
    # b is the crossover point of FS, for which membership degree is 0.5
    b <- (a + c) / 2    
    MF <- rep(0, length(U))
    
    # adding alphamax to the membership function
    for (i in 1:length(U)) {
        if (U[i] <= a) {
            MF[i] <- 0
        } else if (U[i] > a && U[i] <= b) {
            MF[i] <- alphamax * (2 * ((U[i] - a) / (c - a))^2)
        } else if (U[i] > b && U[i] < c) {
            MF[i] <- alphamax * (1 - 2 * ((U[i] - c) / (c - a))^2)
        } else {
            MF[i] <- alphamax
        }
    }
    
    MF <- round(MF, 7)
    MF_SSha <- cbind(U, MF)
    
    ## plot(U,MF_Sig[,2])
    
    ## Compute the alpha-cuts for the S-Shaped Fuzzy Set
    alpha <- seq(0, alphamax, resolutionFS)
    left_interval <- alpha
    right_interval <- alpha
    
    for(j in 1 : length(alpha)) {
        if (alpha[j] <= 0.5) {
            left_interval[j] <- a + (c - a) * sqrt(alpha[j] / 2)
        } else {
            left_interval[j] <- c - (c - a) * sqrt((1 - alpha[j]) / 2)
        }
    } 
    for(j in 1 : length(alpha)) {
        right_interval[j] <- U[length(U)]
    }
    
    compl <- (alphamax-alpha)
    support <- c(left_interval, right_interval)
    alpha_cut=c(alpha, compl)
    alpha_cuts <- cbind(support, alpha_cut)
    
    ## plot(support, alpha_cut)
    ## graphics::lines (support, alpha_cut)
    
    ## Output is a list with the name of type of FS, its parameters,
    ## the Membership Function created and its alpha cuts
    
    Lst <- list(name="S-Shaped", parameters=c(a,b,alphamax), Membership.Function=MF_SSha, alphacuts=alpha_cuts)
    
}




##################################
## Z-shaped fuzzy set (ZShaFS)  ##
## Parameters:                  ##
## a - left shoulder            ##
## c - right base               ##
##################################

#' @export
ZShaFS <- function(U,a,c, alphamax = 1) {

  b <- (a + c) / 2  # Crossover point of FS, for which membership degree is 0.5
  MF <- rep(0, length(U))
  
  ## Z-shaped function for any alphamax
  for (i in 1:length(U)) {
    if (U[i] <= a) {
      MF[i] <- alphamax
    } else if (U[i] > a && U[i] <= b) {
      MF[i] <- alphamax*(1 - 2 * ((U[i] - a) / (c - a))^2)
    } else if (U[i] > b && U[i] < c) {
      MF[i] <- alphamax*(2 * ((U[i] - c) / (c - a))^2)
    } else {
      MF[i] <- 0
    }
    ## print(MF[i])
  }
  
  
  MF <- round(MF, 7)
  MF_ZSha <- cbind(U, MF)
   
  ## plot(U,MF_ZSha[,2])

  ## Compute the alpha-cuts for the Z-Shaped Fuzzy Set for any alphamax
  alpha <- seq(0, alphamax, resolutionFS)
  left_interval <- alpha
  right_interval <- alpha

   for(j in 1 : length(alpha)) {
            left_interval[j] <- U[1]
   }
   
   for(j in 1 : length(alpha)) {
            left_interval[j] <- U[1]
   }
   for(j in 1 : length(alpha)) {
     if (alpha[j] >= alphamax/2) {
       right_interval[j] <- a + (c - a) * sqrt((1 - (alpha[j] / alphamax)) / 2)
     } else {
       right_interval[j] <- c - (c - a) * sqrt(alpha[j] / (2*alphamax))
     }
   }
  
   right_interval <- sort(right_interval)
   compl <- (alphamax-alpha)
   support <- c(left_interval, right_interval)
   alpha_cut=c(alpha, compl)
   alpha_cuts <- cbind(support, alpha_cut)
   
   ## plot(support, alpha_cut)
   ## graphics::lines (support, alpha_cut)
    
   ## Output is a list with the name of type of FS, its parameters,
   ## the Membership Function created and its alpha cuts
        
   Lst <- list(name="Z-Shaped", parameters=c(a,b,alphamax), Membership.Function=MF_ZSha, alphacuts=alpha_cuts)  
   
}



#########################################################
## Intersection of two fuzzy sets on the same Universe ##
## Intersection is given by a t-norm                   ##
#########################################################

#' @export
interFS  <- function(U,FS1, FS2, method="Zadeh", lambda=0) {
    
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
            InterFS[i] <- max(0, fs1[i]+fs2[i]-1.0)
        }
    }
    
    ## lambda in (-1,+oo) for Weber's t-norm 
    
    if (method == "Weber" && lambda > -1) {
        for ( i in 1:length(U) ) {
            InterFS[i] <- max(0, (fs1[i]+fs2[i]-1.0+lambda*fs1[i]*fs2[i]) / (1+lambda) )
        }
    }
    
    if (method == "Weber" && lambda <= -1) {
        stop("lambda parameter for Weber's t-norm must be grater than -1")
    }
    
    return(InterFS)
}



##################################################
## Union of two fuzzy sets on the same Universe ##
## Union is given by a t-conorm                 ##
##################################################

#' @export
uniFS  <- function(U,FS1, FS2, method="Zadeh", lambda=0) {
    
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
            UniFS[i] <- min(1.0, fs1[i]+fs2[i])
        }
    }
    
    ## lambda in (-1,+oo) for Weber's t-conorm 
    
    if (method == "Weber") {
        for ( i in 1:length(U) ) {
            UniFS[i] <- min(1.0, (fs1[i]+fs2[i]-(lambda*fs1[i]*fs2[i]/(1+lambda))))
        }
    }
    
    if (method == "Weber" && lambda <= -1) {
        stop("lambda parameter for Weber's t-conorm must be grater than -1")
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


######################################################################
## The set of all elements that have a nonzero degree of membership ##
## in the fuzzy set A is the support of A                           ##
## supp(A) = {x \in X | \mu_A (x) > 0}                              ##
## Support of a fuzzy set w.r.t the Universe (supportFS) 13/08/2025 ##
######################################################################

#' @export
supportFS <- function(U,FS) {
    if (class(FS[1])=="numeric") {
        # Opera sobre funcoes de pertinencia resultantes de operacoes no "FuzzyRules". Exemplo: fr1
        tempor <- which(FS>0)
        support_position_min <-  max(1, min(tempor) - 1)
        support_position_max <-  min(length(U), max(tempor) + 1)
        supportFS <- c(U[support_position_min], U[support_position_max]) 
    } else {
        # Opera sobre funcoes de pertinencia criadas no "FuzzyRules". Exemplo: SingFS
        supportFS <- c(min(FS$alphacuts[,1]), max(FS$alphacuts[,1]))
    }
    return(supportFS)
}


######################################################################
## The set of all elements which completely belong to A is the      ##
## kernel of the fuzzy set A, i. e.:                                ##
## ker(A) = {x | x ∈ X and A(x) = 1}                                ##
## Kernel of a fuzzy set w.r.t the Universe (kernelFS)   08/25/2025 ##
######################################################################

#' @export
kernelFS <- function(U,FS) {
    kernelFS <- NULL
    position <- 0
    position <- which(FS[[3]][,2] == 1.0)
    if (length(position) == 0) {
        cat("Kernel is empty. The maximum membership grade is not 1.0 \n")
        return(kernelFS)
        stop()      
    }
    vectorUniverse <- FS[[3]][,1]
    vectorUniverse[position]
    kernelFS <- c(min(vectorUniverse[position]), max(vectorUniverse[position]))
    return(kernelFS)
}


######################################################################
## The set of elements having the largest degree of membership in   ##
## the fuzzy set A is the core of A                                 ##
## core(A) = {x \in X | \mu_A (x) = sup_{x \in X} \mu_A (x)}        ##
## Core of a fuzzy set w.r.t the Universe (coreFS)       08/25/2025 ##
######################################################################

#' @export
coreFS <- function(U,FS) {
    max_membership <- max(FS[[3]][,2])
    position <- which(FS[[3]][,2]==max_membership)
    vectorUniverse <- FS[[3]][,1]
    vectorUniverse[position]
    coreFS <- c(min(vectorUniverse[position]), max(vectorUniverse[position]))
    return(coreFS)
}

######################################################################
## The maximum value of the membership function in a fuzzy set A    ##
## on X is named the height of a A, i. e.:                          ##
## height(A) = sup_{x \in X} \mu A(x)                               ##
## A fuzzy set A is said to be normalized iff height(A) = 1         ##
## Height of a fuzzy set (heightFS)                      08/25/2025 ##
######################################################################

#' @export
heightFS <- function(U,FS) {
    max_membership <- max(FS[[3]][,2])
    if (max_membership == 1) {
        cat("This fuzzy set is normalized: height = 1 \n")
    }
    heightFS <- max_membership
    return(heightFS)
}


################################################################
## Degree of membership of an element x to a fuzzy set (dmFS) ##
################################################################

#' @export
dmFS <- function(x,U,FS) {
    if (FS[[1]] == "Triangular") {
        parameters <- FS[[2]]
        alphamax <- parameters[4]
        aa <- 0.0
        bb <- alphamax
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
        if (yL < 0 || yL > alphamax || is.na(yL)) { yL = 0 }
        yR <- x * coeff2 + intercept2
        if (yR < 0 || yR > alphamax || is.na(yR)) { yR = 0 }
        y = max(yL, yR)
        return(y)
    }
    if (FS[[1]] == "Trapezoidal") {
        parameters <- FS[[2]]
        alphamax <- parameters[5]
        if (x >= parameters[2] & x <= parameters[3]) {
            y = alphamax
            return(y)
        } else {
            aa <- 0.0
            bb <- alphamax
            T1 <- c(parameters[1], parameters[2])
            T2 <- c(aa, bb)
            T3 <- c(parameters[3], parameters[4])
            T4 <- c(bb, aa)
            res=stats::lm(formula = T2 ~ T1)
            intercept1 <- res$coefficients[1]
            coeff1 <- res$coefficients[2]
            res=stats::lm(formula = T4 ~ T3)
            intercept2 <- res$coefficients[1]
            coeff2 <- res$coefficients[2]
            yL <- x * coeff1 + intercept1
            if (yL < 0 || yL > alphamax || is.na(yL)) { yL = 0 }
            yR <- x * coeff2 + intercept2
            if (yR < 0 || yR > alphamax || is.na(yR)) { yR = 0 }
            y = max(yL, yR)
            return(y)
        }
    }
    if (FS[[1]] == "Rectangular") {
        parameters <- FS[[2]]
        alphamax <- parameters[3]
        if (x >= parameters[1] & x <= parameters[2]) {
            y = alphamax
        } else {
            y = 0
        }
        return(y)
    }
    if (FS[[1]] == "Constant") {
        parameters <- FS[[2]]
        y = parameters[1]  # pertinência constante para qualquer x
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
    if (FS[[1]] == "Gaussian") {
        parameters <- FS[[2]]
        alphamax <- parameters[3]
        y = alphamax * exp(-0.5 * ((x - parameters[1]) / parameters[2])^2)
        if (y <= 10^(-4)) { y = 0 }
        return(y)
    }
    if (FS[[1]] == "Cauchy") {
        parameters <- FS[[2]]
        alphamax <- parameters[3]
        a <- parameters[1]
        b <- parameters[2]
        y <- alphamax * (1 / (1 + ((x - a) / b)^2))
        if (y <= 10^(-4)) { y = 0 }
        return(y)
    }
    if (FS[[1]] == "Generalized Bell") {
        parameters <- FS[[2]]
        a <- parameters[1]
        b <- parameters[2]
        c <- parameters[3]
        alphamax <- parameters[4]
        y <- alphamax * (1 / (1 + abs(((x - c) / a)^(2 * b))))
        if (y <= 10^(-4)) { y = 0 }
        return(y)
    }
    if (FS[[1]] == "Pi") {
        parameters <- FS[[2]]
        a <- parameters[1]
        b <- parameters[2]
        c <- parameters[3]
        d <- parameters[4]
        alphamax <- parameters[5]
        if (x <= a || x >= d) {
            y = 0
        } else if (x >= b & x <= c) {
            y = alphamax
        } else if (x > a & x < b) {
            y = alphamax * (2 * ((x - a) / (d - a))^2)
        } else {
            y = alphamax * (1 - 2 * ((x - d) / (d - a))^2)
        }
        if (y <= 10^(-4)) { y = 0 }
        return(y)
    }
}




##########################################################################################
## The Jaccard index of similarity between two fuzzy sets A, B on the same Universe U   ##
##    is the real number:                                                               ##
## $J_0 (A,B) =  \frac{\sum_{u \in U} min \{\mu_A(u), \mu_B(u)\} }                      ##  
##                    {\sum_{u \in U} max \{\mu_A(u), \mu_B(u)\} }$                     ##
## Jaccard fuzzy index of two fuzzy sets (jaccardFS)                         04/26/2026 ##
## lambda in (-1,+oo) for Weber's t-norm and t-conorm                                   ##
##########################################################################################

#' @export
jaccardFS <- function (U, FS1, FS2, method="Zadeh", type=0, lambda=0) {
    if (type == 0) {
    jaccardFS <- sum(interFS(U, FS1, FS2, method, lambda))/sum(uniFS(U, FS1, FS2, method, lambda))
    return(jaccardFS)
    } else {
    stop("Method or type must be specified for  computing Jaccard")
    }
    
}



#######################################################################
## Overlap function for two fuzzy sets A, B on the same Universe U   ##
##    is the real number:                                            ##
## Three options for Overlap index:                                  ##
## -- Mean                                                           ##
## -- Product                                                        ##
## -- Weighted Minimum                                               ##
##                                                        04/26/2026 ##
#######################################################################

#' @export
OverlapFunctionFS  <- function(U, FS1, FS2, method="Mean", p=1) {
    fs1 <- FS1[[3]][,2]
    fs2 <- FS2[[3]][,2]
    if (method == "Mean") {  
           OverlapFun <- (pmin(fs1, fs2))/length(U)   ## Hierro2025 
        }
    if (method == "Prod") {
        if (p<1) {
           stop("parameter for Overlap with Product must be grater than or equal 1")
        }
    }    
    if (method == "Prod") {
        if (p>=1) {
           OverlapFun <- (fs1^p * fs2^p)   ## Bedregal2013
        }
    }
    if (method == "Weight_Min1") {
        if (p<=0) {
           stop("parameter for Overlap with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Min1") {
        if (p>0) {
           OverlapFun <- (pmin(fs1, fs2))^p   ## Miguel2019
                                              ## Similar to (fs1^p * fs2^p)  (Bedregal2013)
        }
    }
    if (method == "Weight_Min2") {
        if (p<=0) {
           stop("parameter for Overlap with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Min2") {
        if (p>0) {
           OverlapFun <- 1 - (1 - pmin(fs1, fs2))^p   ## calculated from Hierro2025
        }
    }
    
    return(OverlapFun)
} 


#######################################################################
## Overlap index for two fuzzy sets A, B on the same Universe U      ##
##    is the real number:                                            ##
## Three options for Overlap index:                                  ##
## -- Mean                                                           ##
## -- Product                                                        ##
## -- Weighted Minimum                                               ##
##                                                        04/26/2026 ##
#######################################################################

#' @export
OverlapIndexFS  <- function(U, FS1, FS2, method="Mean", p=1) {
    fs1 <- FS1[[3]][,2]
    fs2 <- FS2[[3]][,2]
    OverlapInd <- 0
    if (method == "Mean") {  
           OverlapInd <- sum(pmin(fs1, fs2))/length(U)   ## Hierro2025
        }
    if (method == "Prod") {
        if (p<=1) {
           stop("parameter for Overlap with Product must be grater than 1")
        }
    }    
    if (method == "Prod") {
        if (p>1) {
           OverlapInd <- min(fs1^p * fs2^p)   ## calculated from Bedregal2013, p>1   
        }
    }     
    if (method == "Weight_Min1") {
        if (p<=0) {
           stop("parameter for Overlap with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Min1") {
        if (p>0) {
           OverlapInd <- max(pmin(fs1, fs2))^p    ## Miguel2019, p>0
        }
    }
    if (method == "Weight_Min2") {
        if (p<=0) {
           stop("parameter for Overlap with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Min2") {
        if (p>0) {
           OverlapInd <- max(1 - (1 - min(fs1, fs2))^p)   ## calculated from Hierro2025
        }
    }

    return(OverlapInd)
}  



#######################################################################
## Grouping function for two fuzzy sets A, B on the same Universe U  ##
##    is the real number:                                            ##
## Three options for Grouping index:                                 ##
## -- Mean                                                           ##
## -- Product                                                        ##
## -- Weighted Maximum                                               ##
##                                                        04/26/2026 ##
#######################################################################

#' @export
GroupingFunctionFS  <- function(U, FS1, FS2, method="Mean", p=0) {
    fs1 <- FS1[[3]][,2]
    fs2 <- FS2[[3]][,2]
    if (method == "Mean") {
           GroupingFun <- (pmax(fs1, fs2))/length(U)   ## Hierro2025 
        }
    if (method == "Prod") {
        if (p<=1) {
           stop("parameter for Grouping with Product must be grater than 1")
        }
    }    
    if (method == "Prod") {
        if (p>1) {
           GroupingFun <- (1-(1-fs1)^p * (1-fs2)^p)   ## Bedregal2013, p>1
        }
    }
    if (method == "Weight_Max1") {
        if (p<=0) {
           stop("parameter for Grouping with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Max1") {
        if (p>0) {
           GroupingFun <- 1 - (1 - pmax(fs1, fs2))^p   ## calculated from  Miguel2019, p>0
        }
    }
    if (method == "Weight_Max2") {
        if (p<=0) {
           stop("parameter for Grouping with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Max2") {
        if (p>0) {
           GroupingFun <- (pmax(fs1, fs2))^p   ## Hierro2025 - function 𝑔(𝑡, 𝑠) = (max{𝑡, 𝑠})^𝑝, 𝑝 > 0;
        }
    }
    
    return(GroupingFun)
} 


#######################################################################
## Grouping index for two fuzzy sets A, B on the same Universe U     ##
##    is the real number:                                            ##
## Three options for Grouping index:                                 ##
## -- Mean                                                           ##
## -- Product                                                        ##
## -- Weighted Maximum                                               ##
##                                                        04/26/2026 ##
#######################################################################

#' @export
GroupingIndexFS  <- function(U, FS1, FS2, method="Mean", p=0) {
    fs1 <- FS1[[3]][,2]
    fs2 <- FS2[[3]][,2]
    Grouping <- 0
    if (method == "Mean") {
           GroupingInd <- sum(pmax(fs1, fs2))/length(U)   ## Hierro2025
        }
    if (method == "Prod") {
        if (p<=1) {
           stop("parameter for Grouping with Product must be grater than 1")
        }
    }    
    if (method == "Prod") {
        if (p>1) {
           GroupingInd <- max(1-(1-fs1)^p * (1-fs2)^p)   ## calculated from Bedregal2013, p>1
        }
    }    
    if (method == "Weight_Max1") {
        if (p<=0) {
           stop("parameter for Grouping with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Max1") {
        if (p>0) {
           GroupingInd <- min(1 - (1 - max(fs1, fs2))^p)   ## calculated from Miguel2019, p>0
        }
    }
    if (method == "Weight_Max2") {
        if (p<=0) {
           stop("parameter for Grouping with Weighted Maximum must be grater than 0")
        }
    }    
    if (method == "Weight_Max2") {
        if (p>0) {
           GroupingInd <- min(pmax(fs1, fs2))^p   ## calculated from Hierro2025
        }
    }
    
    return(GroupingInd)
}  




#######################################################
## New Plot a fuzzy set w.r.t the Universe (plotFS)  ##
## Last updat: 02-25-2026                            ##
#######################################################

#' @export
plotFS <- function(Universe,FS, col = "blue", lty=1, pch = ".", ylim = c(0,1), main="Fuzzy Set") {
    
    if(max(ylim) > 1 || min(ylim) < 0) stop("ylim must be between 0 and 1")
    
    if (FS[[1]] == "Triangular" || FS[[1]] == "Trapezoidal" || FS[[1]] == "Gaussian" ||
        FS[[1]] == "Rectangular" || FS[[1]] == "Constant" || FS[[1]] == "Singleton" ||
        FS[[1]] == "Cauchy" || FS[[1]] == "Generalized Bell" || FS[[1]] == "Sigmoidal"|| 
        FS[[1]] == "S-Shaped" || FS[[1]] == "Z-Shaped" || FS[[1]] == "Pi" || is.na(FS[[1]])) {
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


###########################################################
## New Plot alpha-cuts of a fuzzy set (plotFSAlphaCuts)  ##
## Last updat: 02-25-2026                                ##
###########################################################

#' @export
plotFSAlphaCuts <- function(Universe,FS, main="Alpha Cuts of a Fuzzy Set") {
    
    ##  if(max(ylim) > 1 || min(ylim) < 0) stop("ylim must be between 0 and 1")
    ## plot(teste[[4]][,1], teste[[4]][,2])
    
    if (FS[[1]] == "Triangular" || FS[[1]] == "Trapezoidal" || FS[[1]] == "Gaussian" ||
        FS[[1]] == "Rectangular"  || FS[[1]] == "Constant" || FS[[1]] == "Singleton" ||
        FS[[1]] == "Cauchy" || FS[[1]] == "Generalized Bell" || FS[[1]] == "Sigmoidal"|| 
        FS[[1]] == "S-Shaped" || FS[[1]] == "Z-Shaped" || FS[[1]] == "Pi"|| is.na(FS[[1]])){
        domain <- FS[[4]][,1]
        Alpha_Cut <- FS[[4]][,2]
        plot(domain, Alpha_Cut, main=main)
        graphics::lines (domain, Alpha_Cut, main=main)
    }
    else {
        FuzzySet <- FS
        plot(domain, Alpha_Cut, main=main)
        graphics::lines (domain, Alpha_Cut, main=main)
    }
}



##################################################################
## Method for computing OR for multiple membership values of FS ##
## Mamdani type using MAX                                       ##
## Limited to 12 values                                         ##
## Default values for more than two membership values           ##
##################################################################

#' @export
OR <- function(FRBS, a1, a2, a3=0, a4=0, a5=0, a6=0, a7=0, a8=0, a9=0, a10=0, a11=0, a12=0) {
    if (FRBS == "Mamdani") {
        out <- max(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    }
    return(out)
}



###################################################################
## Method for computing AND for multiple membership values of FS ##
## Mamdani type using MIN                                        ##
## Limited to 12 values                                          ##
## Default values for more than two membership values            ##
###################################################################

#' @export
AND <- function(FRBS, a1, a2, a3=1, a4=1, a5=1, a6=1, a7=1, a8=1, a9=1, a10=1, a11=1, a12=1) {
    if (FRBS == "Mamdani") {
        out <- min(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
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


#################################################
## Create a fuzzy lista starting with a Vector ##
#################################################

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

###############################################################
## Check if check if the value is within the resolution step ##
###############################################################

#' @export
check_resolution <- function(vals, names, func_name) {
    for (i in seq_along(vals)) {
        if (round(vals[i] / resolutionFS, 6) != round(vals[i] / resolutionFS)) {
            warning(paste0("Value ", names[i], " for ", func_name, " must be included with the same resolution in the Universe"))
        }
    }
}



#############################################################
## Defuzzification methods available:                      ##
## CoG, CoS, CoLA, FoM, LoM, MoM, FoS, LoS, MoS, and BADD  ##
## v is a parameter for BADD defuzzification method        ##
## v default for BADD is 1, so BADD = CoG                  ##
## Last updat: 08-14-2025                                  ##
#############################################################

#' @export
defuzzFS <- function(U,FS_Agreg, FS_Outs, method="CoG", v=1) {
    
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
        if (sum_memberhip > 0) {
            defuzz = defuzz/sum_memberhip
        }  else {
            defuzz <- (U[1] +U[length(FS_Agreg)])/2
        }
        return(defuzz)
    }
    
    ## Center of Sums Method - CoS
    ## A Fuzzy Set C is a Union of is fuzzy sets $A_{c_i}$, $i=1,...,n$
    ## x-coordinate is a weighted mean of geometric centers of the areas $A_{c_i}$ 
    ## using FS_Outs
    ## VER Jain2022ComparativeAnalysis_DefuzzificationTechniques_FuzzyOutput, pg 878
    
    if (method == "CoS") {
        defuzz = 0
        areaFS <- rep(0, length(FS_Outs))
        xx <- rep(0, length(FS_Outs))
        # Using an approximation by Monte Carlo for  computing the Area
        for (i in 1: length(FS_Outs)) {
            if (sum(FS_Outs[[i]]) > 0) {
                areaFS[i] <- sum(FS_Outs[[i]])/(length(which(FS_Outs[[i]]>0))+2)*(U[max(which(FS_Outs[[i]]>0))]-U[min(which(FS_Outs[[i]]>0))])
                xx[i] <- (U[min(which(FS_Outs[[i]]>0))]+U[max(which(FS_Outs[[i]]>0))])/2
            } else {
                areaFS[i] <- 0
                xx[i] <- 0
            }         
            defuzz <- defuzz + (areaFS[i]*xx[i])
        }
        if (sum(areaFS) > 0) {
            defuzz <- defuzz/sum(areaFS)
        } else {
            defuzz <- (U[1] +U[length(FS_Agreg)])/2
        }
        return(defuzz)
    }
    
    ## Center of Largest Area Method - CoLA
    ## A Fuzzy Set C is a Union of fuzzy sets $A_{c_i}$, $i=1,...,n$
    ## x-coordinate of center of gravity of the fuzzy sets $A_{c_i}$ with the largest area
    ## using FS_Outs
    ## See Jain2022ComparativeAnalysis_DefuzzificationTechniques_FuzzyOutput, pg 879
    
    if (method == "CoLA") {
        defuzz = 0
        areaFS <- rep(0, length(FS_Outs))
        max_area = 0
        sum_memberhip = 0
        xx <- rep(0, length(FS_Outs))
        
        # Using an approximation by Monte Carlo for computing the Area
        for (i in 1: length(FS_Outs)) {
            if (sum(FS_Outs[[i]]) > 0) {
                areaFS[i] <- sum(FS_Outs[[i]])/(length(which(FS_Outs[[i]]>0))+2)*(U[max(which(FS_Outs[[i]]>0))]-U[min(which(FS_Outs[[i]]>0))])
            } else {
                areaFS[i] <- 0
            }      
            # Choose the FS with largest Area
            if (sum(areaFS) > 0) {
                if (max_area < areaFS[i]) {
                    max_area = areaFS[i]
                    area = i
                }
            } else {
                max_area = 0
                area = 1
            }
        }
        
        FS_Agreg <- FS_Outs[[area]]
        
        for (i in 1: length(U)) {
            defuzz = defuzz + (U[i] * FS_Agreg[i])
            sum_memberhip = sum_memberhip + FS_Agreg[i]
        }
        if (sum_memberhip > 0) {
            defuzz = defuzz/sum_memberhip
        }  else {
            defuzz <- (U[1] +U[length(FS_Agreg)])/2
        }
        return(defuzz)
    }
    
    ## First of Maxima Method - FoM
    ## x-coordinate of the min of x for the maximum membership values of a Fuzzy Set
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 179
    
    if (method == "FoM") {
        defuzz = 0
        i = min(which(FS_Agreg>=max(FS_Agreg)))
        defuzz = U[i]
        return(defuzz)
    }
    
    ## Last of Maxima Method - LoM
    ## x-coordinate of the max of x for the maximum membership values of a Fuzzy Set
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 179
    
    if (method == "LoM") {
        defuzz = 0
        i = max(which(FS_Agreg>=max(FS_Agreg)))
        defuzz = U[i]
        return(defuzz)
    }
    
    ## Middle of maxima Method - MoM
    # The MOM is the average of FOM and LOM, i.e. MoM(A) = (FoM(A) + LoM(A))/2
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 179
    
    
    if (method == "MoM") {
        defuzz = 0
        i = min(which(FS_Agreg>=max(FS_Agreg)))
        j = max(which(FS_Agreg>=max(FS_Agreg)))
        defuzz = (U[i]+U[j])/2
        return(defuzz)
    }
    
    
    
    ## First of Support Method - FoS
    ## x-coordinate of the min of for the support of a Fuzzy Set
    ## supp(A) = {x ∈ X |μ A (x) > 0}.
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 183
    
    
    if (method == "FoS") {
        defuzz = min(supportFS(U,FS_Agreg))
        return(defuzz)
    }
    
    
    ## Last of Support Method - LoS
    ## x-coordinate of the max of for the support of a Fuzzy Set
    ## supp(A) = {x ∈ X |μ A (x) > 0}.
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 183
    
    
    if (method == "LoS") {
        defuzz = max(supportFS(U,FS_Agreg))
        return(defuzz)
    }
    
    
    ## Mean of Support Method - MoS
    ## x-coordinate of the mean of for the support of a Fuzzy Set
    ## supp(A) = {x ∈ X |μ A (x) > 0}.
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 179
    
    
    if (method == "MoS") {
        defuzz = mean(supportFS(U,FS_Agreg))
        return(defuzz)
    }
    
    
    
    ## BAsic Defuzzification Distribution Method - BADD
    ## It is an extension of GoG, MoS and MoM method. BADD depends on v, which a free parameter in [0, +oo).
    ## When v=0, BADD is MoS, when v=1, BADD=CoG, when v=00, BADD=MoM
    ## using FS_Agreg
    ## See Liu2007ParameterizedDefuzzification_MaximumEntropyWeightingFunction, pg 179
    ## See Roychowdhury2001_Survey_DefuzzificationStrategies, pg 686
    ## See Yager1993_Defuzzification_SelectionBased_FuzzySet, pg 260
    ## BADD(A) = (\int_a^b x \times \mu_A^v (x) dx)/(\int_a^b \mu_A^v (x) dx)
    
    
    if (method == "BADD") {
        if (v < 0) stop("alphamax value must be in [0,+oo)")
        defuzz = 0
        sum_memberhip = 0
        for (i in 1: length(U)) {
            defuzz = defuzz + (U[i] * FS_Agreg[i]^v)
            sum_memberhip = sum_memberhip + FS_Agreg[i]^v
        }
        if (sum_memberhip > 0) {
            defuzz = defuzz/sum_memberhip
        }  else {
            defuzz <- (U[1] +U[length(FS_Agreg)])/2
        }
        return(defuzz)
    }
} 




########################################
## Create Intervalar Type-2 Fuzzy Set ##
## New function 03-20-2026            ##
########################################

## Check two vectors - vmax >= vmin - Auxiliar Function

#' @export
Check <- function(vmax, vmin) {
   aa <- vmax[[3]][,2] - vmin[[3]][,2] 
   aa <- round(aa, 7)
   if (sum(aa<0) <= 0) {
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
  if (Check (FSMax, FSMin) == 0){
    stop(" It is not possible to create IT2FS. MF superior must be greather or equal than MF inferior")
  }

  MF_IT2FS <- cbind(FSMax[[3]][,1], FSMax[[3]][,2], FSMin[[3]][,2])

  alpha_cuts <- cbind(FSMax[[4]][,1], FSMin[[4]][,1], FSMin[[4]][,2])

  Lst <- list(name =c("IT2FS", FSMax[[1]], FSMin[[1]]), parameters <- list(FSMax[[2]], FSMin[[2]]), Membership.Function=MF_IT2FS, alphacuts=alpha_cuts)

}


##################################################################
## Plot a Interval Type-2 Fuzzy Set w.r.t the Universe (plotFS) ##
## New refactored function - 03-24-2026                         ## 
##################################################################

#' @export
plotIT2FS <- function(Universe,IT2FS, main="Type-2 Fuzzy Set") {
  if (IT2FS[[1]][1] == "IT2FS") {
    T2FuzzySet <- IT2FS[[3]]
    plot (Universe, T2FuzzySet[,2], pch=".", lty=1, ylim=c(0,1), main=main)
    lines (Universe, T2FuzzySet[,2], col='blue', lty=1, ylim=c(0,1))
    points (Universe, T2FuzzySet[,3], pch=".", lty=1, ylim=c(0,1))
    lines (Universe, T2FuzzySet[,3], col='red', lty=1, ylim=c(0,1))
  }
  else {
    T2FuzzySet <- IT2FS
    plot (Universe, T2FuzzySet[,1], pch=".", lty=1, ylim=c(0,1), main=main)
    lines (Universe, T2FuzzySet[,1], col='blue', lty=1, ylim=c(0,1))
    points (Universe, T2FuzzySet[,2], pch=".", lty=1, ylim=c(0,1))
    lines (Universe, T2FuzzySet[,2], col='red', lty=1, ylim=c(0,1))
  }
}



#################################################################
## Degree of membership of an element x to a fuzzy set (IT2FS) ##
## Return a pair of output values [mu_{FSMin}, mu_{FSMax}]
#################################################################

#' @export
dmIT2FS <- function(x,U,FS1) {

  ## In the end creates the pair of output values y_min and Y_max
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

  ## Return a pair of output values y_min and y_max
    y <- c(y_min, y_max)
    return(y)
}


#' @export
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


#########################################################
## Intersection of two IT2FS on the same Universe      ##
## Intersection is given by the Min                    ##
#########################################################

#' @export
interFS.IT2FS  <- function(U,FS1, FS2) {
  fs1_up <- FS1[[3]][,2]
  fs2_up <- FS2[[3]][,2]
  fs1_lo <- FS1[[3]][,3]
  fs2_lo <- FS2[[3]][,3]
  InterFS_up <- rep(0, length(U))
  InterFS_lo <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    InterFS_up[i] <- min(fs1_up[i],fs2_up[i])
    InterFS_lo[i] <- min(fs1_lo[i],fs2_lo[i])
  }
  out <- cbind(InterFS_up, InterFS_lo)
  return(out)
}


#########################################################
## Union of two IT2FS on the same Universe             ##
## Union is given by the Max                           ##
#########################################################

#' @export
uniFS.IT2FS  <- function(U,FS1, FS2) {
  fs1_up <- FS1[[3]][,2]
  fs2_up <- FS2[[3]][,2]
  fs1_lo <- FS1[[3]][,3]
  fs2_lo <- FS2[[3]][,3]
  UniFS_up <- rep(0, length(U))
  UniFS_lo <- rep(0, length(U))
  for ( i in 1:length(U) ) {
    UniFS_up[i] <- max(fs1_up[i],fs2_up[i])
    UniFS_lo[i] <- max(fs1_lo[i],fs2_lo[i])
  }
  out <- cbind(UniFS_up, UniFS_lo)
  return(out)
}



###########################################################
## Complement of a IT2FS w.r.t the Universe              ##
###########################################################

#' @export
compFS.IT2FS  <- function(U,FS1) {
  fs1_up <- FS1[[3]][,2]
  fs1_lo <- FS1[[3]][,3]
    CompFS_up <- rep(0, length(U))
    CompFS_lo <- rep(0, length(U))
    for ( i in 1:length(U) ) {
        CompFS_lo[i] <- (1-fs1_up[i])
        CompFS_up[i] <- (1-fs1_lo[i])
    }
  out <- cbind(CompFS_up, CompFS_lo)
  return(out)
}



##########################################################################################
## The Jaccard index of similarity between two IT2FS on the same Universe U             ##
##    is the real number:                                                               ##
## $J_0 (A,B) =  \frac{\sum_{u \in U} interFS.IT2FS \{\mu_A(u), \mu_B(u)\} }            ##  
##                    {\sum_{u \in U} uniFS.IT2FS \{\mu_A(u), \mu_B(u)\} }$             ##
## Jaccard fuzzy index of two fuzzy sets (jaccardFS)                         09-04-2026 ##
##########################################################################################

#' @export
jaccardFS.IT2FS <- function (U, FS1, FS2) {
    jaccardFS <- sum(interFS.IT2FS(U, FS1, FS2))/sum(uniFS.IT2FS(U, FS1, FS2))
    return(jaccardFS)
}




######################################################
## Method for computing OR on values from two IT2FS ##
## Mamdani type using MAX                           ##
######################################################

#' @export
OR.IT2FS <- function(FRBS,x1,x2) {
  if (FRBS == "Mamdani") {
    w1 <- max(x1[1],x2[1])
    w2 <- max(x1[2],x2[2])
  }
  out <- c(w1, w2)
  return(out)
}



#######################################################
## Method for computing AND on values from two IT2FS ##
## Mamdani type using MIN                            ##
#######################################################

#' @export
AND.IT2FS <- function(FRBS,x1,x2) {
  if (FRBS == "Mamdani") {
    w1 <- min(x1[1],x2[1])
    w2 <- min(x1[2],x2[2])
  }
  out <- c(w1, w2)
  return(out)
}



#########################################
## Method for computing NOT on a IT2FS ##
## Mamdani type using (1 - IT2FS(x))   ##
#########################################

#' @export
NOT.IT2FS <- function(FRBS,x) {
    if (FRBS == "Mamdani") {
      w1 <- (1 - x[2])
      w2 <- (1 - x[1])
    }
  out <- c(w1, w2)
  return(out)
}



#####################################################################
## Method for computing Implication for Type-2 Mamdani Fuzzy Logic ##
## Mamdani type using MIN                                          ##
## New refactored function - 03-24-2026                            ## 
#####################################################################

#' @export
Implication.IT2FS <- function(FRBS,x,U,FS) {

if (FRBS == "Mamdani") {
  MF_saidaMax <- pmin(x[2],FS[[3]][,2])
  MF_saidaMin <- pmin(x[1],FS[[3]][,3])
}

MF_saida <- cbind(MF_saidaMax,MF_saidaMin)
return(MF_saida)

}


#####################################################################
## Method for computing Aggregation for Type-2 Mamdani Fuzzy Logic ##
## Mamdani type using MAX                                          ##
## New refactored function - 03-24-2026                            ##
#####################################################################

#' @export
Aggregation.IT2FS <- function(FRBS,U,Lst) {
  if (FRBS == "Mamdani") {
    MF_Max <- Lst[[1]][,1]
    MF_Min <- Lst[[1]][,2]
    for (i in 2:length(Lst)) {
      for ( j in 1:length(U) ) {
        MF_Max[j] <- max(MF_Max[j], Lst[[i]][j,1])
        MF_Min[j] <- max(MF_Min[j], Lst[[i]][j,2])
      }
    }
  }
  MF_sai <- cbind(MF_Max,MF_Min)
  return(MF_sai)
}



#############################################
## Create a list starting with a Type-2 FS ##
#############################################

#' @export
listfuzzy2 <- function(FS21, FS22){
    
    returnlist <- list()
    
    if(is.matrix(FS21)){
        returnlist <- list(FS21)
    }
    
    if(is.list(FS21) && is.list(FS22)){
        returnlist <- c(FS21, FS22)
    }
    
    if(is.list(FS21) && is.matrix(FS22)){
        FS22 <- list(FS22)
        returnlist <- c(FS21, FS22)
    }
    
    return(returnlist)
}


#############################################################################
## Type Reduction - Refactored from the Matlab code of the function t2f_TR_KM 
## from Taskin and Kumbasar (2015)
## Type Reduction function KM - Karnik-Mendel Algorithm
## It was done to run with the FuzzyRules structure
## Input: U - Universe of discourse
##        Agg - aggregation of the rules:
##            - upper membership function
##            - lower membership function
#############################################################################

#' @export
t2f_TR_KM_U <- function(U, Agg){

  # Construir matrizes equivalentes ao algoritmo original
  Y <- cbind(U, U)
 # F <- cbind(agg_L, agg_U)
  F <- Agg

  ## KM Algorithm for Computing Y Left
  
  lowerY <- Y[,1]
  ind <- order(lowerY)
  lowerY <- lowerY[ind]
  sortedF <- F[ind,]
  
  FOrt <- rowSums(F) / 2
  isZero <- (sum(FOrt) == 0)
  nRules <- nrow(F)
  
  if(isZero){
    yn <- 0
  } else {
    yn <- sum(lowerY * FOrt) / sum(FOrt)
  }
  
  repeat{
    
    sPointLeft <- 0
    
    for(i in 1:(nRules-1)){
      if(yn >= lowerY[i] && yn <= lowerY[i+1]){
        sPointLeft <- i
        break
      }
    }
    
    fn <- numeric(nRules)
    
    for(i in 1:nRules){
      if(i <= sPointLeft){
        fn[i] <- sortedF[i,2]
      } else{
        fn[i] <- sortedF[i,1]
      }
    }
    
    if(sum(fn) == 0){
      ynPrime <- 0
    } else{
      ynPrime <- sum(lowerY * fn) / sum(fn)
    }
    
    if(abs(yn - ynPrime) < 10^-3){
      yLeft <- yn
      L <- sPointLeft
      break
    } else{
      yn <- ynPrime
    }
  }

  ## KM Algorithm for Computing Y Right
  
  UpperY <- Y[,2]
  ind <- order(UpperY)
  UpperY <- UpperY[ind]
  sortedF <- F[ind,]
  
  FOrt <- rowSums(F) / 2
  isZero <- (sum(FOrt) == 0)
  nRules <- nrow(F)
  
  if(isZero){
    yn <- 0
  } else{
    yn <- sum(UpperY * FOrt) / sum(FOrt)
  }
  
  repeat{
    
    sPointRight <- 0
    
    for(i in 1:(nRules-1)){
      if(yn >= UpperY[i] && yn <= UpperY[i+1]){
        sPointRight <- i
        break
      }
    }
    
    fn <- numeric(nRules)
    
    for(i in 1:nRules){
      if(i <= sPointRight){
        fn[i] <- sortedF[i,1]
      } else{
        fn[i] <- sortedF[i,2]
      }
    }
    
    if(sum(fn) == 0){
      ynPrime <- 0
    } else{
      ynPrime <- sum(UpperY * fn) / sum(fn)
    }
    
    if(abs(yn - ynPrime) < 10^-3){
      yRight <- yn
      R <- sPointRight
      break
    } else{
      yn <- ynPrime
    }
  }

  return(list(yLeft = yLeft, yRight = yRight, L = L, R = R))
}

##########################################################################
## End of the refactored code from the Matlab code of the function t2f_TR_KM 
## from Taskin and Kumbasar (2015)
##########################################################################



#############################################################################
## Type Reduction - Refactored from the Matlab code of the function t2f_TR_EKM 
## from Taskin and Kumbasar (2015)
## Type Reduction function EKM - Extended Karnik-Mendel Algorithm 
## It was done to run with the FuzzyRules structure
## Input: U - Universe of discourse
##        Agg - aggregation of the rules:
##            - upper membership function
##            - lower membership function
#############################################################################

#' @export
t2f_TR_EKM <- function(U, Agg) {

  # Construir matrizes equivalentes ao algoritmo original
  Y <- cbind(U, U)
 # F <- cbind(agg_L, agg_U)
  F <- Agg
  
  ## EKM Algorithm for Computing Y Left
  
  # a) Sort Y matrix
  lowerY <- Y[,1]
  ind <- order(lowerY)
  lowerY <- lowerY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  N <- nrow(F)
  l <- round(N/2.4)
  
  a <- sum(lowerY[1:l] * upperF[1:l]) + 
       sum(lowerY[(l+1):N] * lowerF[(l+1):N])
  
  b <- sum(upperF[1:l]) + 
       sum(lowerF[(l+1):N])
  
  y <- a / b
  
  repeat {
    
    # Step 2
    lussu <- 0
    for (i in 1:(N-1)) {
      if (y >= lowerY[i] && y <= lowerY[i+1]) {
        lussu <- i
        break
      }
    }
    
    # Step 3
    if (lussu == l) {
      yLeft <- y
      L <- l
      break
    } else {
      
      # Step 4
      s <- sign(lussu - l)
      idx1 <- min(l, lussu) + 1
      idx2 <- max(l, lussu)
      
      if (idx1 <= idx2) {
        deltaA <- sum(lowerY[idx1:idx2] * (upperF[idx1:idx2] - lowerF[idx1:idx2]))
        deltaB <- sum(upperF[idx1:idx2] - lowerF[idx1:idx2])
      } else {
        deltaA <- 0
        deltaB <- 0
      }
      
      aussu <- a + s * deltaA
      bussu <- b + s * deltaB
      yussu <- aussu / bussu
      
      # Step 5
      y <- yussu
      a <- aussu
      b <- bussu
      l <- lussu
    }
  }
  
  ## EKM Algorithm for Computing Y Right
  
  # Sort Y matrix
  upperY <- Y[,2]
  ind <- order(upperY)
  upperY <- upperY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  r <- round(N/1.7)
  
  a <- sum(upperY[1:r] * lowerF[1:r]) + 
       sum(upperY[(r+1):N] * upperF[(r+1):N])
  
  b <- sum(lowerF[1:r]) + 
       sum(upperF[(r+1):N])
  
  y <- a / b
  
  repeat {
    
    # Step 2
    russu <- 0
    for (i in 1:(N-1)) {
      if (y >= upperY[i] && y <= upperY[i+1]) {
        russu <- i
        break
      }
    }
    
    # Step 3
    if (russu == r) {
      yRight <- y
      R <- r
      break
    } else {
      
      # Step 4
      s <- sign(russu - r)
      idx1 <- min(r, russu) + 1
      idx2 <- max(r, russu)
      
      if (idx1 <= idx2) {
        deltaA <- sum(upperY[idx1:idx2] * (upperF[idx1:idx2] - lowerF[idx1:idx2]))
        deltaB <- sum(upperF[idx1:idx2] - lowerF[idx1:idx2])
      } else {
        deltaA <- 0
        deltaB <- 0
      }
      
      aussu <- a - s * deltaA
      bussu <- b - s * deltaB
      yussu <- aussu / bussu
      
      # Step 5
      y <- yussu
      a <- aussu
      b <- bussu
      r <- russu
    }
  }
  
  return(list(yLeft = yLeft, yRight = yRight, L = L, R = R))
}

##########################################################################
## End of the refactored code from the Matlab code of the function t2f_TR_EKM 
## from Taskin and Kumbasar (2015)
##########################################################################


#############################################################################
## Type Reduction - Refactored from the Matlab code of the function t2f_TR_IASC 
## from Taskin and Kumbasar (2015)
## Type Reduction function IASC - Iterative Algorithm with Stop Condition
## It was done to run with the FuzzyRules structure
## Input: U - Universe of discourse
##        Agg - aggregation of the rules:
##            - upper membership function
##            - lower membership function
#############################################################################

#' @export
t2f_TR_IASC <- function(U, Agg) {

  # Construir matrizes equivalentes ao algoritmo original
  Y <- cbind(U, U)
 # F <- cbind(agg_L, agg_U)
  F <- Agg
  
  ## IASC Algorithm for Computing Y Left
  
  # Sort Y matrix (coluna 1)
  lowerY <- Y[,1]
  ind <- order(lowerY)
  lowerY <- lowerY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  N <- nrow(F)
  a <- sum(lowerY * lowerF)
  b <- sum(lowerF)
  yl <- lowerY[N]
  l <- 0
  
  repeat {
    # Step 2
    l <- l + 1
    a <- a + lowerY[l] * (upperF[l] - lowerF[l])
    b <- b + (upperF[l] - lowerF[l])
    c <- a / b
    
    # Step 3
    if (c >= yl) {
      L <- l - 1
      yLeft <- yl
      break
    } else {
      yl <- c
    }
  }
  
  ## IASC Algorithm for Computing Y Right
  
  # Sort Y matrix (coluna 2)
  upperY <- Y[,2]
  ind <- order(upperY)
  upperY <- upperY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  a <- sum(upperY * upperF)
  b <- sum(upperF)
  yr <- upperY[1]
  r <- 0
  
  repeat {
    # Step 2
    r <- r + 1
    a <- a - upperY[r] * (upperF[r] - lowerF[r])
    b <- b - (upperF[r] - lowerF[r])
    c <- a / b
    
    # Step 3
    if (c <= yr) {
      R <- r - 1
      yRight <- yr
      break
    } else {
      yr <- c
    }
  }
  
  return(list(yLeft = yLeft, yRight = yRight, L = L, R = R))
}

##########################################################################
## End of the refactored code from the Matlab code of the function t2f_TR_IASC 
## from Taskin and Kumbasar (2015)
##########################################################################



#############################################################################
## Type Reduction - Refactored from the Matlab code of the function t2f_TR_EIASC 
## from Taskin and Kumbasar (2015)
## Type Reduction function EIASC - Enhanced Iterative Algorithm with Stop
##                                 Condition
## It was done to run with the FuzzyRules structure
## Input: U - Universe of discourse
##        Agg - aggregation of the rules:
##            - upper membership function
##            - lower membership function
#############################################################################

#' @export
t2f_TR_EIASC <- function(U, Agg) {

  # Construir matrizes equivalentes ao algoritmo original
  Y <- cbind(U, U)
 # F <- cbind(agg_L, agg_U)
  F <- Agg
  
  ## EIASC Algorithm for Computing Y Left
  
  # Sort Y matrix (coluna 1)
  lowerY <- Y[,1]
  ind <- order(lowerY)
  lowerY <- lowerY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  N <- nrow(F)
  a <- sum(lowerY * lowerF)
  b <- sum(lowerF)
  L <- 0
  
  repeat {
    # Step 2
    L <- L + 1
    a <- a + lowerY[L] * (upperF[L] - lowerF[L])
    b <- b + (upperF[L] - lowerF[L])
    yl <- a / b
    
    # Step 3
    if (yl <= lowerY[L + 1]) {
      yLeft <- yl
      break
    }
  }
  
  ## EIASC Algorithm for Computing Y Right
  
  # Sort Y matrix (coluna 2)
  upperY <- Y[,2]
  ind <- order(upperY)
  upperY <- upperY[ind]
  sortedF <- F[ind,]
  lowerF <- sortedF[,1]
  upperF <- sortedF[,2]
  
  # Step 1
  a <- sum(upperY * lowerF)
  b <- sum(lowerF)
  R <- N
  
  repeat {
    # Step 2
    a <- a + upperY[R] * (upperF[R] - lowerF[R])
    b <- b + (upperF[R] - lowerF[R])
    yr <- a / b
    R <- R - 1
    
    # Step 3
    if (yr >= upperY[R]) {
      yRight <- yr
      break
    }
  }
  
  return(list(yLeft = yLeft, yRight = yRight, L = L, R = R))
}

##########################################################################
## End of the refactored code from the Matlab code of the function t2f_TR_EIASC 
## from Taskin and Kumbasar (2015)
##########################################################################



#############################################################################
## Type Reduction - Refactored from the Matlab code of the function t2f_TR_NT 
## from Taskin and Kumbasar (2015)
## Type Reduction function NT - Nie-Tan
## It was done to run with the FuzzyRules structure
## Input: U - Universe of discourse
##        Agg - aggregation of the rules:
##            - upper membership function
##            - lower membership function
#############################################################################

#' @export
t2f_TR_NT <- function(U, Agg) {

# Construir matrizes equivalentes ao algoritmo original
  Y <- cbind(U, U)
  F <- Agg
  
  # Sort Y matrix
  lowerY <- Y[, 1]
  upperY <- Y[, 2]
  
  ind <- order(lowerY)
  lowerY <- lowerY[ind]
  upperY <- upperY[ind]
  sortedF <- F[ind, ]
  
  lowerF <- sortedF[, 1]
  upperF <- sortedF[, 2]
  
  # Convert to crisp Y matrix
  if (isTRUE(all.equal(lowerY, upperY))) {
    y <- lowerY
  } else {
    y <- (lowerY + upperY) / 2
  }
  
  # Compute outputs
  yLeft <- sum(y * (upperF + lowerF)) / sum(upperF + lowerF)
  yRight <- yLeft
  
  L <- "no"
  R <- "no"
  
  return(list(yLeft = yLeft, yRight = yRight, L = L, R = R))
}


##########################################################################
## End of the refactored code from the Matlab code of the function t2f_TR_NT 
## from Taskin and Kumbasar (2015)
##########################################################################


##############################################
##             END OF FUNCTIONS             ##
##############################################


