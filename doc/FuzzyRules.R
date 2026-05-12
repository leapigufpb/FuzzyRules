## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  library(devtools)
#  devtools::install_github("leapigufpb/FuzzyRules")

## -----------------------------------------------------------------------------
library(FuzzyRules)

## -----------------------------------------------------------------------------
minX = 0
maxX = 25
X <- Set.Universe(minX, maxX)
FRBS <- "Mamdani"

## -----------------------------------------------------------------------------
service.poor <- GauFS(X, 0.0, 1.5)
service.good <- GauFS(X, 5.0, 1.5)
service.excelent <- GauFS(X, 10, 1.5, alphamax = .3)

plotFS(X, service.poor, col = "blue")
par(new = TRUE)
plotFS(X, service.good, col = "red")
par(new = TRUE)
plotFS(X, service.excelent, col = "green")

## -----------------------------------------------------------------------------
food.rancid <- TraFS(X, 0, 0, 2, 4)
food.delicious <- TraFS(X, 7, 9, 10, 10)

plotFS(X, food.rancid, col = "blue")
par(new = TRUE)
plotFS(X, food.delicious, col = "red")

## -----------------------------------------------------------------------------
tip.cheap <- TriFS(X, 0, 5, 10)
tip.average <- TriFS(X, 7.5, 12.5, 17.5)
tip.generous <- TriFS(X, 15, 20, 25)

plotFS(X, tip.cheap, col = "blue")
par(new = TRUE)
plotFS(X, tip.average, col = "red")
par(new = TRUE)
plotFS(X, tip.generous, col = "green")

## -----------------------------------------------------------------------------
plotFS(X, uniFS(X, service.poor, service.good, method = "Zadeh"))
plotFS(X, interFS(X, service.poor, service.good, method = "Zadeh"))
plotFS(X, compFS(X, service.good))

## -----------------------------------------------------------------------------
service <- 3
food <- 8

## -----------------------------------------------------------------------------
# Regra 1
temp <- OR(FRBS, dmFS(service, X, service.poor), dmFS(food, X, food.rancid))
fr1 <- Implication(FRBS, temp, X, tip.cheap)
plotFS(X, fr1)
Out_Rules_Lst <- listfuzzy(fr1)

# Regra 2
temp <- dmFS(service, X, service.good)
fr2 <- Implication(FRBS, temp, X, tip.average)
Out_Rules_Lst <- listfuzzy(Out_Rules_Lst, fr2)
plotFS(X, fr2)

# Regra 3
temp <- OR(FRBS, dmFS(service, X, service.excelent), dmFS(food, X, food.delicious))
fr3 <- Implication(FRBS, temp, X, tip.generous)
Out_Rules_Lst <- listfuzzy(Out_Rules_Lst, fr3)
plotFS(X, fr3)

## -----------------------------------------------------------------------------
OutFS <- Aggregation(FRBS, X, Out_Rules_Lst)
plotFS(X, OutFS)

FinalDecision <- defuzzFS(X, OutFS, Out_Rules_Lst, method = "CoG")
cat("Resultado usando CoG: ", FinalDecision, "\n")

FinalDecision2 <- defuzzFS(X, OutFS, Out_Rules_Lst, method = "CoS")
cat("Resultado usando CoS: ", FinalDecision2, "\n")

FinalDecision3 <- defuzzFS(X, OutFS, Out_Rules_Lst, method = "CoLA")
cat("Resultado usando CoLA: ", FinalDecision3, "\n")

