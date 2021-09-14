# Step 0: Install and load R libraries.
# 1. Required libraries: arules, arulesViz, and RColorBrewer. Install them if they are not already installed

library(arules)
library(arulesViz)
library(RColorBrewer)

# 2. The arulesViz and RColorBrewer libraries will assist in visualizing the data.
# 3. For more information on those libraries, type ?LibraryName in you preferred R IDE.

# Step 1: Load the dataset and Explore the data.
data("Groceries")

# a. Type Groceries in the console
# b. How many transactions are there in the dataset?
9835 transactions

# c. How many items are there in the Groceries dataset?
169 items

# 2. Most popular items: 
arules::itemFrequencyPlot(Groceries, topN = 20,col = brewer.pal(8, 'Dark2'), main =
'Relative Item Frequency Plot', type = "relative", ylab = "Item Frequency (Relative)")

# a. What is the most popular item?
whole milk

# b. What is the least popular item?
domestic eggs

# c. How many items occur more than 15% by count? What are they?
4 items

d. Save the plot.


# Step 2: Use the apriori library to extract the rules.
# 1. Assign the association output: 
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.2))

# a. Capture the output. Save it.


# b. What is the minimum itemset length?
1

# c. What is the maximum itemset length?
10 

# d. What is the confidence?
0.2

# e. What is the support?
0.01

# f. How many rules were generated?
232 rules

# g. Use summary(rules) to find out how many rules have one items, two items, three items?
summary(rules)
1 = 1
2 = 151
3 = 80

# i. Save the summary(rules) output.


# 3. Let us inspect the top 20 rules: inspect(rules([1:20])
inspect(rules[1:20])
inspect(sort(rules[1:20], by="lift"))

# a. What top three association rules have the highest Lift? Name each three association rule numbers, lefthand side (lhs) and right-hand side (rhs).
rule  left hand side (lhs)  right hand side (rhs)
8     onions                other vegetables
10    berries               yogurt
13    hamburger meat        other vegetables

# b. Comment on what you believe rule #1 means.
Whole milk is the most popular item and it can be paired with anything

# c. Is your answer supported by what you found to be the most popular item in the question 1.2.a in Step 1?
yes 

# d. What is the support, confidence, and lift for a lhs of berries leading to a rhs of yogurt according to the output?
lhs       rhs      support     confidence   lift
berries   yogurt   0.01057448  0.3180428    2.279848 

# Step 3: Finding redundancy and pruning the association rules:
# 1. To find redundant association rules. We will focus on confidence, but feel free to try support and left as well.

redundant <- is.redundant(rules, measure="confidence")
which(redundant)

# c. Save the output.


# d. How many rules are redundant based on this criterion? Which ones? List them out.
based on 5 rules. They are 69, 117, 141, 181, 217

# 2. Let us do some pruning based on what was found above:
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

# d. It is a long list. Printout the top 20 with inspect(rules.pruned[1:20])
# i. Save the output.
inspect(rules.pruned[1:20])

# e. What are the top three rules after pruning based on lift? List them.
lhs                                 rhs
citrus fruit, other vegetables      root vegetables
other vegetables, yogurt            whipped/sour cream
tropical fruit, other vegetables    root vegetables

# Step 4: Visualizing the association rules:
# 1. Make sure you have loaded arulesViz.
plot(rules)
# a. Save the plot.


plot(rules, method="grouped")
# a. Save the plot.


plot(rules, method="graph")
# a. Save the plot.


# 5. What items have the most associations based on the plots? Does that confirm your understanding of the
# association rules in the Groceries dataset? Any surprises?
