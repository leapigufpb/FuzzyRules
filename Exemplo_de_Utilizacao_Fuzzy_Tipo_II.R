################################################################
# Lendo o codigo das funcoes
################################################################

library(devtools)
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySets.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySetsTypeII.R")
#source("/home/jodavid/Documents/LEAPIG/Pos DOC/github/FuzzyRules/FuzzySetsTypeII.R")
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
XMin <- TriFS(X, 7, 12.5, 17, alphamax = .5)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

x1 <- dmIT2FS(6,U,T2)
x2 <- dmIT2FS(8,U,T2)

abline(v = 12, col="gray", lty=2)
abline(h = 0.9, col="gray", lty=2)
abline(h = 0.92, col="gray", lty=2)

temp <- AND.IT2FS(FRBS, x1, x2)
temp <- OR.IT2FS(FRBS, x1, x2)


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
XMin <- TraFS(X, a,b,c,d, alphamax = .5)

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

dmIT2FS(12,U,T2)

abline(v = 12, col="gray", lty=2)
abline(h = 0.9, col="gray", lty=2)

abline(h = 0.5, col="gray", lty=2)

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

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

abline(v = 12, col="gray", lty=2)
abline(h = 0.9, col="gray", lty=2)
abline(h = 0.92, col="gray", lty=2)


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

T2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2)

dmIT2FS(12,U,T2)

abline(v = 12, col="gray", lty=2)
abline(h = 0.9, col="gray", lty=2)
abline(h = 0.92, col="gray", lty=2)

#########################################
##             END OF CODE             ##
#########################################


# -------------------------------------------------------------------

x1 <- dmIT2FS(6,U,T2)
x2 <- dmIT2FS(8,U,T2)

abline(v = 12, col="gray", lty=2)
abline(h = 0.9, col="gray", lty=2)
abline(h = 0.92, col="gray", lty=2)

temp <- AND.IT2FS(FRBS, x1, x2)
temp <- OR.IT2FS(FRBS, x1, x2)

