
# Loading the Data Groceries from STAT Assign 5 folder
myGroc <- read.csv("Groceries.csv", header = TRUE)
#myGroc1 <- read.csv("groceries1.csv", header = TRUE)


install.packages("arules")
install.packages("arulesViz")
library("arulesViz")
library("arules")  

#### Top 5 items with high frequency ###########
itemcount <- table(myGroc$Product)
sort(itemcount, decreasing = TRUE)
par(mar = c(3,2,2,2))
barplot(sort(itemcount, decreasing = TRUE)[1:5])

#OR
#itemFrequencyPlot(myGroc,topN=5,type="absolute")

####Part B ###############
#install.packages("apricom")
#library(apricom)

str(myGroc)

myGroc <- split(myGroc[,"Product"], f = myGroc$ID) 
myGroc <- lapply(myGroc, unique)
myGroc <- as(myGroc, "transactions")


?apriori
?sort

rules <- apriori(myGroc, parameter = list(support = 0.005, confidence = 0.2,target = "rules"))

sortrules <- sort(rules, decreasing = TRUE, by = "lift")

inspect(head(sortrules, 5)) #top 5 rules with highest lift values

#customer who buy (citrus fruit, veggie, whole milk) are 4.08 times likely 
#to buy root veggie

##########CUSTOMIZE HOW MANY IN LHS #########

#options(digits = 1)
#inspect(rules)

#######PART C################################


sodarule <- apriori(myGroc, parameter = list(support = 0.005, confidence = 0.2, maxlen = 2),appearance = list(default = "lhs", rhs =" soda"))

sodasortrules <- sort(sodarule, decreasing = TRUE, by = "lift")

inspect(head(sodasortrules, 5))

############PART D###########################

juicesetup <- apriori(myGroc, parameter = list(support = 0.005, confidence = 0.2, maxlen = 2),appearance = list(default = "lhs", rhs = " soda"))
options(digits = 1)
inspect(x = juicesetup)


    #lhs                            rhs     support confidence lift
#[24] { fruit/vegetable juice}    => { soda} 0.018   0.3        1 

#############PART E##########################

butterrule <- apriori(myGroc, parameter = list(support = 0.005, confidence = 0.2),
                      appearance = list(default = "lhs", rhs =" butter"))

buttersortrules <- sort(butterrule, by = "lift")

inspect(butterrule)



