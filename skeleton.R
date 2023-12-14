library(shiny)

# Global Variables

phyl_levs = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
group_options = c("Site")

# The app
ui <- fluidPage(
  selectInput("tax_level", "Phylogenetic Level", choices = phyl_levs),
  selectInput("grouping", "Group By", choices = group_options)
)
server <- function(input, output, session) {
}
shinyApp(ui, server)
