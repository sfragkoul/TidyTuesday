

rm(list = ls())
gc()


# load libraries --------------------------------------------------------------

library(data.table)
library(ggplot2)
library(stringr)
library(patchwork)

# read data -------------------------------------------------------------------

mutant_moneyball <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-19/mutant_moneyball.csv')


mutant_moneyball <- mutant_moneyball[, c("Member", "TotalValue_ebay", 
                                     "TotalValue60s_ebay", "TotalValue70s_ebay",
                                     "TotalValue80s_ebay", "TotalValue90s_ebay",
                                     "60s_Appearance_Percent", "70s_Appearance_Percent",
                                     "80s_Appearance_Percent", "90s_Appearance_Percent") ]

percents = c("60s_Appearance_Percent", "70s_Appearance_Percent",
             "80s_Appearance_Percent", "90s_Appearance_Percent")

for (i in percents){
    data <- str_replace(mutant_moneyball[[i]], "%", "") |> as.numeric()
    mutant_moneyball[[i]] <- data
}



#order based on TotalValue_ebay
mutant_moneyball = mutant_moneyball[order(mutant_moneyball$TotalValue_ebay, 
                                          decreasing = TRUE), ] 

#keep top 10 members based on TotalValue_ebay
mutant_moneyball = mutant_moneyball[c(1,2,3,4,5), ]


mutant_moneyball[, c("Member", "TotalValue60s_ebay",
                     "TotalValue70s_ebay", "TotalValue80s_ebay",
                     "TotalValue90s_ebay")] |>
    
    tidyr::pivot_longer(cols = c("TotalValue60s_ebay",
                                 "TotalValue70s_ebay", "TotalValue80s_ebay",
                                 "TotalValue90s_ebay"),
                        names_to = "TotalValue", values_to = "value")


mutant_moneyball[, c("Member", "60s_Appearance_Percent",
                     "70s_Appearance_Percent", "80s_Appearance_Percent",
                     "90s_Appearance_Percent")] |>
    
    tidyr::pivot_longer(cols = c("60s_Appearance_Percent",
                                 "70s_Appearance_Percent", 
                                 "80s_Appearance_Percent",
                                 "90s_Appearance_Percent"),
                        names_to = "Appearance", values_to = "perc")




sixties = ggplot(mutant_moneyball, 
       
    aes(`60s_Appearance_Percent`, TotalValue60s_ebay)) + 
    
    geom_point(aes(color=Member)) +
    
    scale_x_continuous(breaks = c(25, 50, 75, 100), 
                       limits = c(0, 100)) +

    scale_y_continuous(limits = c(70, 24000)) +
    
    theme_minimal() +
    
    theme(axis.line = element_line(colour = "#595959"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none", 
          axis.title.x=element_blank())
        

seventies = ggplot(mutant_moneyball, 
                 
                 aes(`70s_Appearance_Percent`, TotalValue70s_ebay)) + 
    
    geom_point(aes(color=Member)) +
    
    scale_x_continuous(breaks = c(25, 50, 75, 100), 
                       limits = c(0, 100)) +
    
    scale_y_continuous(limits = c(70, 24000)) +
    
    theme_minimal() +
    
    theme(axis.line = element_line(colour = "#595959"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    
    ylab("Total value of issues on eBay" ) +
    
    xlab("Appearance percentage in the issues" ) 



eighties = ggplot(mutant_moneyball, 
                            
                            aes(`80s_Appearance_Percent`, TotalValue80s_ebay)) + 
    
    geom_point(aes(color=Member)) +
    
    scale_x_continuous(breaks = c(25, 50, 75, 100), 
                       limits = c(0, 100)) +
    
    scale_y_continuous(limits = c(70, 24000)) +
    
    theme_minimal() +
    
    theme(axis.line = element_line(colour = "#595959"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    
    ylab("Total value of issues on eBay" ) +
    
    xlab("Appearance percentage in the issues" ) 



ninties  = ggplot(mutant_moneyball, 
                           
                           aes(`90s_Appearance_Percent`, TotalValue90s_ebay)) + 
    
    geom_point(aes(color=Member)) +
    
    scale_x_continuous(breaks = c(25, 50, 75, 100), 
                       limits = c(0, 100)) +
    
    scale_y_continuous(limits = c(70, 24000)) +
    
    theme_minimal() +
    
    theme(axis.line = element_line(colour = "#595959"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom") +
    
    ylab("Total value of issues on eBay" ) +
    
    xlab("Appearance percentage in the issues" ) 



multi = sixties / seventies / eighties / ninties


ggsave(
    plot = multi, filename = "multi.png",
    width = 8, height = 10, units = "in", dpi = 600
)
