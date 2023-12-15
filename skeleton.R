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

# Takes a taxonomy table
# Outputs a taxonomy table that groups by `group` and counts the number of
# observations of each species/genus/family/etc
relabun_by_group <- function(table_input, group="Marker", taxlevel="species") {
  zotuBar <- table_input
  zotuBar$Rep<-rep(1,nrow(zotuBar))

  # Group and count
  zotuBar2<-ddply(zotuBar, c(taxlevel, group), summarize, Sum = sum(c(Rep)) )
  # Count total number of each marker
  zotuBarTotal<-aggregate(formula(paste0("Sum~",group)), data = zotuBar2, FUN = sum)
  colnames(zotuBarTotal)[2]<-"Total"
  zotuBar3<-merge(zotuBar2, zotuBarTotal, by = group)
  zotuBar3$RelAbun<-zotuBar3$Sum/zotuBar3$Total

  ##
  zotuBar3$Quantity<-factor(zotuBar3[[group]])

  # Filter out rows with blank in the "species" column
  zotuBar3_filtered <- zotuBar3[!is.na(zotuBar3[[taxlevel]]) & zotuBar3[[taxlevel]] != "", ]
  return(zotuBar3_filtered)
}

# Takes a table
# Returns a ggplot code block
make_plot <- function(in_table, taxlevel="species") {
  taxlevel <- sym(taxlevel)
  return({
    ggplot(in_table, aes(x = Quantity, y = Sum, fill = !!taxlevel)) +
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

full_pipeline <- function(tax_table, group="Marker", taxlevel="species") {
  print(taxlevel)
  tab <- relabun_by_group(tax_table, group = group, taxlevel = taxlevel)
  print(tab)
  return(make_plot(tab, taxlevel = taxlevel))
}

# Global Variables

phyl_levs <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
group_options <- c("Marker", "Site")
raw_table <- read.csv("Van_QIIME/Fishes.df4.csv")
test_table <- relabun_by_group(raw_table)
getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
colourCount <- length(unique(raw_table$species))

# The app
ui <- fluidPage(
  selectInput("tax_level", "Phylogenetic Level", choices = phyl_levs),
  selectInput("grouping", "Group By", choices = group_options),
  plotOutput("chart")
)
server <- function(input, output, session) {
  output$chart <- renderPlot(full_pipeline(raw_table, taxlevel=input$tax_level))
}
shinyApp(ui, server)
