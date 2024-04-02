################################################################
# Lendo o codigo das funcoes
################################################################

library(devtools)
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySets.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySetsTypeII.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/km.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/ekm.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/eiasc.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/da.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/dastar.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/dand.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/costrwsr.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/ecostrwsr.R")
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/src_R/sc.R")


###################################
## Development code for IT2FS #####
###################################

# -------------------------------------------------------------------
#Exemplo com Dois Trapezios - Correto

# ------------ BEGIN - primeiro Trapezio ------------------------- #
# Trapezio do X1
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=7;b=10;c=12;d=13
XMax <- TraFS(X, a,b,c,d)
a=8;b=11;c=12;d=12
XMin <- TraFS(X, a,b,c,d, alphamax = .5)

T2_1 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2_1)

# ------------ END - primeiro Trapezio ------------------------- #

# ------------ BEGIN - segundo Trapezio ------------------------- #
# Trapezio do X2
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

a=6;b=11;c=13;d=15
XMax <- TraFS(X, a,b,c,d)
a=7;b=10;c=11;d=13
XMin <- TraFS(X, a,b,c,d, alphamax = .5)

T2_2 <- Create.IT2FS(U, XMax, XMin)
plotIT2FS(U,T2_2)

# ------------ END - segundo Trapezio ------------------------- #

abline(v=9, col="yellow")
abline(v=12, col="yellow")

# ------------ BEGIN - Terceiro Trapezio ------------------------- #
# Trapezio do Y1
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
FRBS = "Mandani"
U <- X

tip.cheap <- TriFS(X, 0, 7, 12, alphamax = 1)
tip.cheap2 <- TriFS(X, 1, 5, 9, alphamax = 0.5)

# Olhar PLOT
plotFS(U, tip.cheap)
par(new=TRUE)
plotFS(U, tip.cheap2, col="red", ylim=c(0,1))


T2_Y <- Create.IT2FS(U, tip.cheap, tip.cheap2)
plotIT2FS(U,T2_Y)
# ------------ END - Terceiro Trapezio ------------------------- #


# -------------------------------------------------------------------
# Estamos fazenndo para 1 (um)caso com 1 (uma) regra

### Definindo pontos em X1 e X2
x1 = 11 # Para trapezio 1
x2 = 12 # para Trapezio 2

# Definindo a Implicação
#temp <- OR.IT2FS(FRBS, dmIT2FS(x1,U,T2_1),dmIT2FS(x2,U,T2_2))
temp <- AND.IT2FS(FRBS, dmIT2FS(x1,U,T2_1),dmIT2FS(x2,U,T2_2))

#Definindo a Função de Pertinência
FS <- T2_Y
fr1 <- Implication.IT2FS(FRBS,temp,U,FS)

# Visualizando o PLOT
plotFS(U, fr1[,2])
par(new=TRUE)
plotFS(U, fr1[,1], col="red", ylim=c(0,1))

# -------------------------------------------------------------------
sorted <- T

wl <- fr1[,1]
wr <- fr1[,2]

x <- U

# --------------------------------------------------------------------- #
# <> -----------------------------------------<>
# KM - BEGIN
# <> -----------------------------------------<>

# Usando o KM
ret1 <- km(wl, wr, x, maximum=T, w.which=F, sorted=sorted, k.which=T)
ret2 <- km(wl, wr, x, maximum=F, w.which=F, sorted=sorted, k.which=T)
# Salvando os resultados
yr <- ret1[1]
kr <- ret1[2]
yl <- ret2[1]
kl <- ret2[2]
# Defuzzificação - Encontrando Y Médio
yc <- (yl+yr)/2
# Valor e y médio
yc

# Criando plot
plotFS(U, fr1[,2])
par(new=TRUE)
plotFS(U, fr1[,1], col="red", ylim=c(0,1))
abline(v=yr, col="green")
abline(v=yl, col="green")
# Aplicando Linha do y médio
abline(v=yc, col="green")

# <> -----------------------------------------<>
# KM - END
# <> -----------------------------------------<>

# --------------------------------------------------------------------- #


# --------------------------------------------------------------------- #

# <> -----------------------------------------<>
# EKM - BEGIN
# <> -----------------------------------------<>

# Usando o EKM
ret1 <- ekm(wl, wr, x, maximum=T, w.which=F, sorted=sorted, k.which=T)
ret2 <- ekm(wl, wr, x, maximum=F, w.which=F, sorted=sorted, k.which=T)
yr <- ret1[1]
kr <- ret1[2]
yl <- ret2[1]
kl <- ret2[2]
# Defuzzificação - Encontrando Y Médio
yc <- (yl+yr)/2

# Valor de y central
yc

# Criando plot
plotFS(U, fr1[,2])
par(new=TRUE)
plotFS(U, fr1[,1], col="red", ylim=c(0,1))
abline(v=yr, col="green")
abline(v=yl, col="green")
# Aplicando Linha do y médio
abline(v=yc, col="green")

# <> -----------------------------------------<>
# EKM - END
# <> -----------------------------------------<>

# --------------------------------------------------------------------- #

# --------------------------------------------------------------------- #
# <> -----------------------------------------<>
# DAND - BEGIN
# <> -----------------------------------------<>

# Usando o DAND
ret1 <- km.dand(wl, wr, x, maximum=T, w.which=F, sorted=sorted, k.which=T)
ret2 <- km.dand(wl, wr, x, maximum=F, w.which=F, sorted=sorted, k.which=T)
yr <- ret1[1]
kr <- ret1[2]
yl <- ret2[1]
kl <- ret2[2]
# Defuzzificação - Encontrando Y Médio
yc <- (yl+yr)/2

# Valor de y central
yc

# Criando plot
plotFS(U, fr1[,2])
par(new=TRUE)
plotFS(U, fr1[,1], col="red", ylim=c(0,1))
abline(v=yr, col="green")
abline(v=yl, col="green")
# Aplicando Linha do y médio
abline(v=yc, col="green")

# <> -----------------------------------------<>
# DAND - END
# <> -----------------------------------------<>
# --------------------------------------------------------------------- #
