########################################
## Create Intervalar Type-2 Fuzzy Set ##
########################################

## Check two vectors - vmax >= vmin - Auxiliar Function

Check <- function(vmax, vmin) {
  cont = 0
  for (i in 1: length(vmax)) {
    if (vmax[i] >= vmin[i]) {
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

plotIT2FS <- function(Universe,IT2FS) {
  if (IT2FS[[1]][1] == "IT2FS") {
    T2FuzzySet <- IT2FS[[3]]
    plot (Universe, T2FuzzySet[,2], pch=".", lty=1, ylim=c(0,1))
    lines (Universe, T2FuzzySet[,2], col='blue', lty=1, ylim=c(0,1))
    points (Universe, T2FuzzySet[,3], pch=".", lty=1, ylim=c(0,1))
    lines (Universe, T2FuzzySet[,3], col='red', lty=1, ylim=c(0,1))
  }
  else {
    print("This is not a IT2FS")
  }
}




Create.IT2FS2 <- function(U, FSMax, FSMin) {
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
