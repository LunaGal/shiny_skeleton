library(shiny)
library(ggplot2)
library(dplyr)
library(vegan)
library(plyr)
library(reshape2)
library(data.table)
library(RColorBrewer)
library(ggpubr)
library(car)
library(funrar)
library(emmeans)
library(lsmeans)

# Auxiliary Functions

test_filterer <- function(table_input) {
  zotuBar <- table_input
  zotuBar$Rep<-rep(1,nrow(zotuBar))
  zotuBar2<-ddply(zotuBar,.(species, Marker), summarize, Sum = sum(c(Rep)) )

  zotuBarTotal<-aggregate(Sum ~ Marker, data = zotuBar2, FUN = sum)
  colnames(zotuBarTotal)[2]<-"Total"
  zotuBar3<-merge(zotuBar2, zotuBarTotal, by = "Marker")
  zotuBar3$RelAbun<-zotuBar3$Sum/zotuBar3$Total

  ##
  zotuBar3$Quantity<-factor(zotuBar3$Marker)

  # Filter out rows with blank in the "species" column
  zotuBar3_filtered <- zotuBar3[!is.na(zotuBar3$species) & zotuBar3$species != "", ]
  return(zotuBar3_filtered)
}

# Global Variables

phyl_levs <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
group_options <- c("Site")
raw_table <- read.csv("Van_QIIME/Fishes.df4.csv")
test_table <- test_filterer(raw_table)
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
colourCount <- length(unique(zotuBar3$species))

# The app
ui <- fluidPage(
  selectInput("tax_level", "Phylogenetic Level", choices = phyl_levs),
  selectInput("grouping", "Group By", choices = group_options),
  plotOutput("chart")
)
server <- function(input, output, session) {
  output$chart <- renderPlot({ggplot(test_table, aes(x = Quantity, y = Sum, fill = species)) +
                               geom_bar(stat = 'identity', color = "black", size = 0.25) +
                               scale_fill_manual(values = getPalette(colourCount)) +
                               ylab("zOTU") +
                               scale_x_discrete(drop = FALSE) +
                               xlab("") +
                               theme_bw() +
                               theme(
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(),
                                 axis.text = element_text(size = 12),
                                 panel.border = element_blank(),
                                 axis.title.y = element_text(size = 14),
                                 axis.line.x = element_line(color = "black", size = 0.5),
                                 axis.line.y = element_line(color = "black", size = 0.5),
                                 legend.key.size = unit(1.2, "lines"),  # Adjust the legend key size here
                                 legend.text = element_text(size = 14)   # Adjust the legend text size here
                               )})
}
shinyApp(ui, server)
