

rm(list = ls())
gc()


# load libraries --------------------------------------------------------------

library(data.table)
library(ggplot2)

# read data -------------------------------------------------------------------

mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')


mutant_moneyball <- mutant_moneyball[, c("Member", "TotalValue_ebay", 
                                     "TotalValue60s_ebay", "TotalValue70s_ebay",
                                     "TotalValue80s_ebay", "TotalValue90s_ebay",
                                     "60s_Appearance_Percent", "70s_Appearance_Percent",
                                     "80s_Appearance_Percent", "90s_Appearance_Percent") ]

#order based on TotalValue_ebay
mutant_moneyball = mutant_moneyball[order(mutant_moneyball$TotalValue_ebay, decreasing = TRUE), ] 

#keep top 10 members based on TotalValue_ebay
mutant_moneyball = mutant_moneyball[c(1,2,3,4,5, 6, 7, 8, 9, 10), ]

ggplot(mutant_moneyball, aes(`90s_Appearance_Percent`, TotalValue90s_ebay)) + 
    
    geom_point(aes(color=Member))
