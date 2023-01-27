#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Harmonic Syntax"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("What is this?"),
            p("This is a shiny app designed to walk you through a derivation. It is a derivation of syntax in harmonic grammar."),
            h3("Defined operations"),
            p("Merge, Agree, Label"),
            # Copy the line below to make a select box
            actionButton("load", label = "Load Eval"),
            numericInput("winner", label = h3("Optimal output?"), value = 1),
            actionButton("proceed", label = "Next Cycle"),
            img(src = "uni_leipzig_logo_v2.svg", height = 142, width = 338),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h6("The evaluation table and a representational tree for the input will appear here."),
            plotOutput("tree"),
            textOutput("save"),
            tableOutput("eval")
        )
    )
))
