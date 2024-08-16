################################################################
# Lendo o codigo das funcoes
################################################################

library(devtools)
source_url("https://raw.githubusercontent.com/leapigufpb/FuzzyRules/main/FuzzySets.R")


################################################################
## MAIN PROGRAM - TIP EXAMPLE 1                               ##
## SIMILAR TO 'Matlab' AND 'Sets' Package FOR 'R'             ##
## Universe = [0,0; 25,0]                                     ##
## Three Fuzzy Linguistic Variables:                          ##
## Service: Gaussian type (Poor, Good, Excelent)              ##
## Food: Trapezoidal type (rancid, delicious)                 ##
## Tip (decision): Triangular type (cheap, average, generous) ##
################################################################

## Creating Fuzzy Variables and Fuzzy Partitions
## Example 1 - It is the same of the R-Package 'Sets'

## Creating Fuzzy Variables and Fuzzy Partitions
## Example is the same of the R-Package 'Sets'
## One Domain was used for Evaluation and Tip

## Universe X - evaluation by the consumer in [0,10]
minX = 0
maxX = 25

X <- Set.Universe(minX, maxX)
#FRBS = "Mandani"
FRBS= "Mamdani"

## Service

service.poor <- GauFS(X, 0.0, 1.5)
plotFS(X,service.poor)

service.good <- GauFS(X, 5.0, 1.5)
plotFS(X,service.good)

service.excelent <- GauFS(X, 10, 1.5, alphamax = .3)
plotFS(X,service.excelent)

## Plot of Linguistic Variable "Service"
plotFS(X,service.poor, col = "blue")
par(new = T)
plotFS(X,service.good, col = "red")
par(new = T)
plotFS(X,service.excelent, col = "green")
# BUG do Positron
par(new = T)
plotFS(X,service.excelent, col = "green")

## Food

food.rancid <- TraFS(X, 0, 0, 2, 4)
plotFS(X,food.rancid)

food.delicious <- TraFS(X, 7, 9, 10, 10)
plotFS(X,food.delicious)

# food.rancid[[1]]

## Plot of Linguistic Variable "Food"
plotFS(X,food.rancid, col = "blue")
par(new = T)
plotFS(X,food.delicious, col = "red")
# BUG do Positron
par(new = T)
plotFS(X,food.delicious, col = "red")

## Tip

tip.cheap <- TriFS(X, 0, 5, 10)
plotFS(X,tip.cheap)

tip.average <- TriFS(X, 7.5, 12.5, 17.5)
plotFS(X,tip.average)

tip.generous <- TriFS(X, 15, 20, 25)
plotFS(X,tip.generous)

## Plot of Linguistic Variable "Tip"
plotFS(X,tip.cheap, col = "blue")
par(new = T)
plotFS(X,tip.average, col = "red")
par(new = T)
plotFS(X,tip.generous, col = "green")
# BUG do Positron
par(new = T)
plotFS(X,tip.generous, col = "green")

## Union, Intersection and Complement
## Default for Union and Intersection: method = "Zadeh"

plotFS(X,uniFS(X, service.poor, service.good))
plotFS(X,interFS(X,service.poor, service.good))
plotFS(X,uniFS(X, service.poor, service.excelent))

# Union and Intersection: method="Probabilistic"
plotFS(X,uniFS(X, service.poor, service.good, method="Probabilistic"))
plotFS(X,interFS(X,service.poor, service.good, method="Probabilistic"))
plotFS(X,uniFS(X, service.poor, service.excelent, method="Probabilistic"))
plotFS(X,interFS(X,service.poor, service.excelent, method="Probabilistic"))

# Union and Intersection: method="Lukasiewicz"
plotFS(X,uniFS(X, service.poor, service.good, method="Lukasiewicz"))
plotFS(X,interFS(X,service.poor, service.good, method="Lukasiewicz"))
plotFS(X,uniFS(X, service.poor, service.excelent, method="Lukasiewicz"))
plotFS(X,interFS(X,service.poor, service.excelent, method="Lukasiewicz"))

plotFS(X,uniFS(X, food.rancid, food.delicious))
plotFS(X,uniFS(X, food.rancid, food.delicious, method = "Probabilistic"))
plotFS(X,uniFS(X, food.rancid, food.delicious, method = "Lukasiewicz"))

plotFS(X,interFS(X, food.rancid, food.delicious))
plotFS(X,interFS(X, food.rancid, food.delicious, method = "Probabilistic"))
plotFS(X,interFS(X, food.rancid, food.delicious, method = "Lukasiewicz"))

# Complement of a Fuzzy Set
plotFS(X,compFS(X,service.good))
plotFS(X,compFS(X,service.poor))
plotFS(X,compFS(X,service.excelent))

plotFS(X,compFS(X, food.rancid))
plotFS(X,compFS(X, food.delicious))



## Set the input
## Evaluation by the consumer (Facts)

service = 3
food = 8


## Rules -- Specify rules and add outputs in a list

## A list is created in order to store the output of each rule

## Out_Rules_Lst <- list()

## 1) IF service is poor OR food is rancid THEN tip is cheap

## antecedent of the rule
## temp <- NOT(FRBS, dmFS(service,X,service.poor))
temp <- OR(FRBS, dmFS(service,X,service.poor), dmFS(food,X,food.rancid))

# consequent of the rule
fr1 <- Implication(FRBS, temp, X, tip.cheap)
plotFS(X,fr1)

# Create a list with the output
Out_Rules_Lst <- listfuzzy(fr1)

# plot the rule fr1
#par(mfrow=c(1,3))
#plotFS(X,service.poor, col = "blue")
#plotFS(X,food.rancid, col = "blue")
#plotFS(X,fr1, col = "blue")


## 2) IF service is good THEN tip is average

## temp <- NOT(FRBS, dmFS(service,X,service.good))
temp <- dmFS(service,X,service.good)
fr2 <- Implication(FRBS, temp, X, tip.average)
plotFS(X,fr2)

## Cria lista com as saidas. Precisa de duas para dar certo
#Out_Rules_Lst <- list(fr1,fr2)
Out_Rules_Lst <- listfuzzy(Out_Rules_Lst,fr2)

# plot the rule fr2
#par(mfrow=c(1,2))
#plotFS(X,service.good, col = "blue")
#plotFS(X,fr2, col = "blue")

## 3) IF service is excelent OR food is delicious THEN tip is generous

## temp <- NOT(OR(FRBS, dmFS(service,X,service.excelent), dmFS(food,X,food.delicious)))
temp <- OR(FRBS, dmFS(service,X,service.excelent), dmFS(food,X,food.delicious))
fr3 <- Implication(FRBS, temp, X, tip.generous)
plotFS(X,fr3)

## Compute the agregation of all previous Rules
## Concatenando as listas
Out_Rules_Lst <- listfuzzy(Out_Rules_Lst,fr3)

# plot the rule fr3
#par(mfrow=c(1,3))
#plotFS(X,service.excelent, col = "blue")
#plotFS(X,food.delicious, col = "blue")
#plotFS(X,fr3, col = "blue")

## Numero de regras eh o comprimento da lista
length(Out_Rules_Lst)

OutFS <-Aggregation(FRBS, X, Out_Rules_Lst)
plotFS(X,OutFS)

# plot all the rules and Aggregation
X11() #jodavid
par(mfrow=c(4,3))
plotFS(X,service.poor, col = "blue", main = "service.poor")
abline(v=service)
plotFS(X,food.rancid, col = "blue", main = "food.rancid")
abline(v=food)
plotFS(X,fr1, col = "blue", main = "Implication")
plotFS(X,service.good, col = "red", main = "service.good")
abline(v=service)
plotFS(X,service.good, col = "red", main = "service.good")
plotFS(X,fr2, col = "red", main = "Implication")
plotFS(X,service.excelent, col = "green", main = "service.excelent")
abline(v=service)
plotFS(X,food.delicious, col = "green", main = "food.delicious")
abline(v=food)
plotFS(X,fr3, col = "green", main = "Implication")
plotFS(X,OutFS, main = "Aggregation")


## Compute Defuzzification in order provide final decision

FinalDecision <- defuzzFS(X,OutFS,Out_Rules_Lst,method="CoG")
## OR FinalDecision <- defuzzFS(X,OutFS,Out_Rules_Lst)

cat("Tip result using CoG is approx. ", FinalDecision, "\n")

## Output is Tip is approx. 14.8859

FinalDecision2 <- defuzzFS(X,OutFS,Out_Rules_Lst,method="CoS")
cat("Tip result using CoS = ", FinalDecision2, "\n")

## Output is Tip is approx. 14.75458 

FinalDecision3 <- defuzzFS(X,OutFS,Out_Rules_Lst,method="CoLA")
cat("Tip result using CoLA is approx. ", FinalDecision3, "\n")

## Output is Tip is approx. 20

FinalDecision4 <- defuzzFS(X,OutFS,Out_Rules_Lst,method="FoM")
cat("Tip result using FoM is approx. ", FinalDecision4, "\n")

## Output is Tip is approx. 17.55

FinalDecision5 <- defuzzFS(X,OutFS,Out_Rules_Lst,method="LoM")
cat("Tip result using LoM is approx. ", FinalDecision5, "\n")

## Output is Tip is approx. 22.45

FinalDecision6 <- defuzzFS(X,OutFS,Out_Rules_Lst,method="MoM")
cat("Tip result using MoM is approx. ", FinalDecision6, "\n")

## Output is Tip is approx. 20


####################################################
##             END OF CODE - EXAMPLE 1            ##
####################################################
