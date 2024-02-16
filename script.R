# Script written by Jean-Luc Jucker on 16 February 2024.

# Set and print working directory ####
setwd('C:/Users/jeanl/projects/bar-plots')
getwd()

# Load data; we define all strings as factors ####
df <- read.csv('data.csv', stringsAsFactors = TRUE)

# Function to generate bar plots ####
library(tidyverse)
library(ggplot2)

# We create a counter. This needs to be an external variable.
counter <- 0

# Function to automatically generate bar plots ####
make_bar_plots <- function(df){ 
  
  # We loop over the columns
  for (col in colnames(df)){ 
    
    # (1) Preparation #########################################################
    # Store N
    N <- sum(!is.na(df[col]))
    # Update counter at each iteration
    counter <- counter + 1
    
    # (2) Basic bar plot ######################################################
    # We only plot factors
    if (is.factor(df[[col]])) {
      p <- ggplot(data = df[col] %>% filter(!is.na(df[col])),
                  aes(x=fct_rev(fct_infreq(!!sym(col)))))
      # Compute percents for bars
      p <- p + geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.55,
                        fill = 'black') 
      
    # (3) Annotations #########################################################
      
      # Add N as annotation
      p <- p + geom_text(aes(label= scales::comma(..count..),
                        y = ((..count..)/sum(..count..)), family = 'Roboto'),
                         stat="count",
                         hjust = 1.2, size=4.2, color='white') # Roboto is nicer
                                                              # than Arial!
      # Add percent as annotation
      p <- p + geom_text(aes(label = scales::percent((..count..)/sum(..count..),
                                                     accuracy = 0.1L),
                        y = ((..count..)/sum(..count..)), family = 'Roboto'),
                        stat="count", hjust = -0.1, size=4.2)
      # Add a title
      p <- p + labs(title = paste(col),
                    subtitle = paste0('N = ', format(N, big.mark = ',')
                    ))
      
    # (4) Aesthetics ##########################################################
      p <- p + theme_minimal()
      p <- p + theme(axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.text.y = element_text(colour='black', size = 12.5),
                     plot.margin = margin(r=25, l=1, b=0, t=1),
                     plot.title = element_text(size = 16.5),
                     plot.subtitle = element_text(size = 12.5),
                     panel.grid = element_blank(),
                     text = element_text(family = 'Roboto'))
      # Flip coordinates
      p <- p + coord_flip(clip = 'off')
        
     # (5) Print and export   #################################################
      print(p)
      # Export
      ggsave(paste(counter, '_', col, '.png'), path = 'plots/', 
             width = 16.5,
             height = 5, units = 'cm') # this size is ideal for MS Word
    } 
  }
}

# Implement function on data
make_bar_plots(df)