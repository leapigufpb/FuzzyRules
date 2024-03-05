################################################################
# Lendo o codigo das funcoes
################################################################

library(devtools)
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySets.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySetsTypeII.R")

###################################
## Development code for IT2FS #####
###################################

# -------------------------------------------------------------------
#Exemplo com Dois Triângulos - Correto
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

XMax <- TriFS(X, 6, 12.5, 19)
XMin <- TriFS(X, 7, 12.5, 15)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)


#Exemplo com Dois Triângulos - Com Erro
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

XMax <- TriFS(X, 6, 12.5, 19)
XMin <- TriFS(X, 7, 12, 15)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

T2 <- Create.IT2FS(U, XMax, XMin)


# -------------------------------------------------------------------
#Exemplo com Dois Trapezios - Correto
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=7;b=10;c=12;d=13
XMax <- TraFS(X, a,b,c,d)
a=8;b=11;c=12;d=12
XMin <- TraFS(X, a,b,c,d)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)


#Exemplo com Dois Triângulos - Com Erro
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=7;b=10;c=12;d=13
XMax <- TraFS(X, a,b,c,d)
a=8;b=11;c=12;d=15
XMin <- TraFS(X, a,b,c,d)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

T2 <- Create.IT2FS(U, XMax, XMin)




# -------------------------------------------------------------------
#Exemplo com Um triangulo e um trapezio - Correto

minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

XMax <- TriFS(X, 6, 12.5, 19)
a=8;b=11;c=15;d=16
XMin <- TraFS(X, a,b,c,d, alphamax = .5)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

XMax[[4]]
XMin[[4]]


T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

#Exemplo com um triangulo e um trapezio - Errado

minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

XMax <- TriFS(X, 6, 12.5, 19)
a=8;b=11;c=15;d=22
XMin <- TraFS(X, a,b,c,d,alphamax = .5)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

# -------------------------------------------------------------------
#Exemplo com Um trapezio e um triangulo - Correto

minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=5;b=11;c=15;d=19
XMax <- TraFS(X, a,b,c,d)

XMin <- TriFS(X, 6, 12.5, 17,alphamax = .5)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")
abline(h = 0.5, col="gray", lty=2)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

#Exemplo com um triangulo e um trapezio - Errado

minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=5;b=11;c=15;d=19
XMax <- TraFS(X, a,b,c,d)

XMin <- TriFS(X, 6, 12.5, 22,alphamax = .5)

plotFS(U,XMax)
par(new=TRUE)
plotFS(U,XMin, col="red")

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)


#########################################
##             END OF CODE             ##
#########################################


