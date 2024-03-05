################################################################
# Lendo o codigo das funcoes
################################################################

library(devtools)
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySets.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySetsTypeII.R")

###################################
## Development code for IT2FS #####
###################################

#Exemplo com Dois Triângulos

minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"

XMax <- TriFS(X, 6, 12.5, 19)
XMin <- TriFS(X, 7, 12, 15)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

















#XMin <- TriFS(X, 7.5, 12.5, 17.5)
U=X
a=7
b=10
c=12
d=13
XMin <- TraFS(X, a,b,c,d, alphamax = .5)
plotFS(X,XMin)
XMin[[4]]


plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)



U <- X
FSMax <- XMax
FSMin <- XMin
T2 <- Create.IT2FS2(U, XMax, XMin)
## Vai precisar de um codigo para criar/plotar um IT2FS com max < 1.




#########################################
##             END OF CODE             ##
#########################################


