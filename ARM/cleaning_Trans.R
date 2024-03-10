library(arules)
#install.packages("arulesViz")
library(arulesViz)
library(tokenizers)
#install.packages('dplyr')
library(dplyr)
library(tidytext)
library(RColorBrewer)

#Read in transaction data for Abstracts

tr <- read.transactions('Abstracts_Trans.csv', format = "basket", sep = ',', rm.duplicates = FALSE, cols= NULL)
inspect(tr[2,])
itemLabels(tr[2,])

#see words that occur the most
sample_trans <- sample(tr)
summary(sample_trans)
#health, care, study, diverse, people

#Read transaction data into a dataframe for cleaning
abstract_df <- read.csv("Abstracts_Trans.csv", header = FALSE, sep = ',')
head(abstract_df)

#make sure all columns are char
sapply(abstract_df, class)

(str(abstract_df))

#remove words
abstract_df[abstract_df == 'health'] <- ''
abstract_df[abstract_df == 'care'] <- ''
abstract_df[abstract_df == 'study'] <- ''
abstract_df[abstract_df == 'diverse'] <- ''
abstract_df[abstract_df == 'people'] <- ''
abstract_df[abstract_df == 'evidence'] <- ''
abstract_df[abstract_df == 'patients'] <- ''
abstract_df[abstract_df == 'years'] <- ''
abstract_df[abstract_df == 'year'] <- ''
abstract_df[abstract_df == 'literature'] <- ''
abstract_df[abstract_df == 'aimed'] <- ''
abstract_df[abstract_df == 'review'] <- ''
abstract_df[abstract_df == 'systematic'] <- ''
abstract_df[abstract_df == 'studies'] <- ''
abstract_df[abstract_df == 'process'] <- ''
abstract_df[abstract_df == 'outcomes'] <- ''
abstract_df[abstract_df == 'results'] <- ''
abstract_df[abstract_df == 'methods'] <- ''
abstract_df[abstract_df == 'gaht'] <- ''
abstract_df[abstract_df == 'data'] <- ''
abstract_df[abstract_df == 'person'] <- ''

#assign to a new df and remove first row
df_clean <- abstract_df[-1,]

#select by word size
mydf <-NULL
mydf2 <- NULL
for (i in 1:ncol(df_clean)){
  mylist = c()
  mylist = c(mylist,grepl("[A-z]{4,}", df_clean[[i]]))
  
  mylist2 = c()
  mylist2 = c(mylist2,grepl("[A-z]{13,}", df_clean[[i]]))
  
  mydf <- cbind(mydf,mylist)
  mydf2 <- cbind(mydf2,mylist2)
}
#mydf shows FALSE for empty cells and those with words <4 characters TRUE for all 4+
#mydf2 shows FALSE for empty cells and those with words <13 characters TRUE for all 12+


#for all true, replace with blank
df_clean[!mydf] <- ''
df_clean[mydf2] <- ''
df_clean[1]

#did not work to remove whitespace
#remove_empty <- df_clean %>% mutate_all(trimws)

#Save to csv so we can use read.transactions easier
write.csv(df_clean, "Abstract_TransData_Clean.csv", row.names=FALSE)

tr <- read.transactions('Abstract_TransData_Clean.csv', format = 'basket', sep = ',', rm.duplicates=FALSE, cols= NULL)
inspect(tr[2,])
#itemLabels(tr[1,])

#ARM
#Top 15 rules for support
rules <- apriori(tr, parameter = list(supp = 0.05, conf = 0.9, minlen = 2))

top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support") 
inspect(head(top.support, 15))

rules <- apriori(tr, parameter = list(supp = 0.05, conf = 0.9, minlen = 4))

top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support") 
inspect(head(top.support, 15))


#Top 15 rules for confidence
rules <- apriori(tr, parameter = list(supp = 0.03, conf = 1, minlen = 2))

top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence") 
inspect(head(top.confidence, 15))
# or inspect(sort(top.support)[1:10])

#Top 15 rules for lift
rules <- apriori(tr, parameter = list(supp = 0.04, conf = 0.6, minlen = 2))

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift") 
inspect(head(top.lift, 15))
# or inspect(sort(top.support)[1:10])

#Top 15 rules for lift
rules <- apriori(tr, parameter = list(supp = 0.06, conf = 0.05, minlen = 2))

top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift") 
inspect(head(top.lift, 15))

#Networks viz

plot(top.support[1:20], method = "graph", engine = 'interactive', shading = 'confidence')
######

plot(top.confidence[1:20], method = "graph", engine = 'interactive', shading = 'confidence')
