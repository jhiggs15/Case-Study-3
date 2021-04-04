#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##REALLLL

library(shiny)
library(ggplot2)
library(gridExtra)
source("helper.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(title = "PCA Analysis",
               tabPanel("Analysis",
                        sidebarLayout(
                            sidebarPanel( width = 4,
                                          selectInput( inputId = "df",label = "Select a Data Set...", 
                                                       choices = data(iris, trees, mtcars, OrchardSprays)
                                          ),
                                          uiOutput("vars"),
                                          uiOutput("pcs"),
                                          uiOutput("dt")
                            ),
                            fluidRow(width = 7,
                                     column(width = 7,
                                            plotOutput("plot"),
                                            fluidRow(
                                                actionButton("perc", "Percent of Variance Explained"),
                                                actionButton("PCA", "Score Plot"),
                                                actionButton("pat", "Pattern Plot")
                                            )
                                     )
                            )
                            
                        )
               ),
               navbarMenu("Explanation", 
                          tabPanel("PCA Step by Step", 
                                   tags$h2("Pattern Plot"),
                                   tags$p("The goal of Principle Component Analysis (PCA) is to reduce the dimensionality of large data by condensing the variables into a smaller set called principle components which are simply linear combinations of the originals. The cost of this technique is that a portion of the data is not included in these principal components. However, many times a large portion of the data can be retained within a few principle components so that all features may be visualized in a smaller dimension."),
                                   tags$p("To preform PCA we must first standardize our data in some way so that each feature will contributes equally to the analysis, preventing features with large or small data from inappropriately impacting the results. Many standardization can be performed but mean centering will be used here. Where mean centering is the subtraction of the features mean from all its observations."),
                                   tags$p("X - mean(X) : where X is all the observations in a feature"),
                                   tags$p("Now that the data has been standardized a covariance matrix must be calculated. A covariance matrix for two variables x and y would be calculated as follows. Note that the covariance is a symetric matrix."),
                                   tags$p("|cv(x,x)|cv(x,y)|"),
                                   tags$p("|cv(y,x)|cv(y,y)|"),
                                   tags$p("The understanding of the covariance matrix in two dimensions can also be extended to any dimesion of features where each row and column signify a feature. For example the value at row 4 and column 3, would be the covariance of the 4th feature and the 3rd feature."),
                                   tags$p("With the covariance matrix cv calculated we will now derive the characteristic vectors or eigen vectors of the covariance matrix cv"),
                                   tags$p("This is done by first finding the eigen values using the following equation "),
                                   tags$p("|cv - lambda * I |=0 "),
                                   tags$p("where lambda * I is the identity matrix with lambda along the diagnol. Here lambda represents a eigen value. Also | surrounding the equation represents the determinant. while above it represented a matrix"),
                                   tags$p("Once the determinant is found, the resulting polynomial is solved and the soutions are the eigen values"),
                                   tags$p("Each eigen value found is then plugged into"),
                                   tags$p("(cv - lambda * I) * v  = 0"),
                                   tags$p("where lambda is an eigen value and v is a n x 1 vector that is the eigen vector"),
                                   tags$p("We then preform row reduction on the matrix. We then plug in a value of our choice (say for example 1) for each component that does not have a pivot and solve the equation"),
                                   tags$p("An example matrix would be as follows where the third column does not have a pivot"),
                                   tags$p("|a|b|c|"),
                                   tags$p("|d|e|f|"),
                                   tags$p("|0|0|0|"),
                                   tags$p("We repeat this for all eigen values generating v eigen vectors where v is the number of eigen values."),
                                   tags$p("The eigen vectors we have just found should be ordered in descending order (greatest to smallest) by eigen value, where the eigen vector with the largest eigen value is Principle Component 1 and the next largest is Principle Component 2 etc etc."),
                                   tags$p("Our final step is to reorient our data set from their original axis to an axis defined by each Principle Component. This is done by the equation "),
                                   tags$p("Final Data Set = transpose(Feature Vector) * transpose(Standardized Orginal Data Set) "),
                                   tags$p("")
                                   ),
                          tabPanel("Write Up",
                                   tags$h2("What data did you analyze?"),
                                   tags$p("The data sets iris, trees, mtcars, and OrchardSprays were chosen from the R datasets package and analyzed."),
                                   tags$h2("Why this topic is interesting or important to you? (Motivation)"),
                                   tags$p("This topic was interesting to me because it gave me an opportunity to explore the unsupervised machine learning method PCA and understand how it reduces the dimensionality of data. This was especially interesting because I was also able to apply and deepen my understanding of known linear algebra and statistic concepts such as eigen vectors, eigen values, and co-variance."),
                                   tags$h2("How did you analyze the data?"),
                                   tags$p("Before any analysis could be done, the data was transformed using PCA and important values such as eigen values and eigen vectors were returned. With this information each data set was able to be described in a scree plot, score plot and pattern plot (see the Plot Explanation tab) which visually represents correlations and groupings within the data which would otherwise not be apparent."),
                                   tags$h4("Iris"),
                                   tags$p("petal length and petal width appear to have a strong linear relationship"),
                                   tags$p("The versicolor and virginica species of iris appear to be most similar"),
                                   tags$h4("Trees"),
                                   tags$p("Girth and volume appear to have a strong linear relationship"),
                                   tags$h4("Mtcars"),
                                   tags$p("Based off the features it appeared to be difficult to fit a lot of the data into the first two components, which account for <75% of the data. With this the accuarcy of these conclusions will not be as complete as those prior."),
                                   tags$p("The following features appear to be related"),
                                   tags$p("am and gear"),
                                   tags$p("disp and carb"),
                                   tags$p("vs and mpg"),
                                   tags$h4("Orchard Sprays"),
                                   tags$p("Again because of the data  <75% of the data was able to be fit into the first few components. Because of this the accuarcy of the analysis may not be as strong as past analysis."),
                                   tags$p("The noticed graphs helped group together two main distinctions in treatments"),
                                   tags$p("ABCD and EFGH")
                                   
                                   )
                          ),
               navbarMenu("Plot Explanation",
                          tabPanel("Scree Plot",
                                   tags$h2("Scree Plot"),
                                   tags$p("In this plot the percent variance and cumulative percent variance of each principal component is shown. This is calculated for each principal component by the following equation"),
                                   tags$p("EigenVector[x] / sum(EigenVectors)"),
                                   tags$p("This works because each eigen vector of the covariance matrix represents a direction of the greatest possible variance, where each eigen vector accounts for a single dimension. Each of these vectors has a certain amount of importance or significance in terms of how much of the data (or in our case variance) it captures. This is explained by each eigen vectors corresponding eigen value. Therefore, the above equation calculates the percentage of variance each component accounts for. ")
                                   ),
                          tabPanel("Score Plot",
                                   tags$h2("Score Plot"),
                                   tags$p("The score plot is the graphical representation of the reorientation of our original data based on the eigen vectors of the covariance matrix. Mathematically these values displayed are the projection of the data onto the span of each principal component. ")
                                   ),
                          tabPanel("Pattern Plot",
                                   tags$h2("Pattern Plot"),
                                   tags$p("The pattern plot represents the correlation between the original variables and the selected principal components. This is useful in finding relationships between variables as variables that are strongly correlated appear closer together.")
                                   )
               )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        get(input$df)
    })
    
    PCA_ret <- reactive({
        PCA(isolate(data())[ ,input$features])
    })
    
    PCA_eigs <- reactive({
        PCA_ret()$eigen_vals
    })
    
    PCA_perc <- reactive({
        (PCA_eigs()/sum(PCA_eigs())) * 100
    })
    
    PCA_cor <- reactive({
        PCA_ret()$cor
    })
    
    PCA_cump <- reactive({
        cum_perc = c()
        for(i in (1 : length(PCA_perc()))){
            cum_perc = c(cum_perc, sum(PCA_perc()[1:i]))
        }
        cum_perc
    })
    
    PCA_poin <- reactive({
        PCA_ret()$new_vals
    })
    
    PCA_vals <- reactive({
        vals = colnames(PCA_ret()$new_vals)[1:length(PCA_ret()$eigen_vals)]
        factor(vals, levels = unique(vals))
    })
    
    v <- reactiveValues(value = NULL)
    
    df <- reactiveValues(var_exp = NULL, pattr = NULL)
    
    observeEvent(input$perc, {
        v$value <- ggplot(df$var_exp, aes(x = PCA_vals())) +
            geom_line(aes(y = PCA_perc(), color = "Percent Variance of PC"), group = 1) + geom_point(aes(y = PCA_perc()), size = 3) +
            labs(x = "Principal Component", y = "Percent of Variance") +
            geom_line(aes(y = PCA_cump(), color = "Cumulative Percent of Variance"), group = 1) + geom_point(aes(y = PCA_cump()), size = 3) + ylim(0, 100)
        
    })
    
    observeEvent(input$PCA, {
        v$value <- ggplot() + geom_point(aes(x = PCA_poin()[[input$PCS1]], y = PCA_poin()[[input$PCS2]], color = data()[[input$category]]), size = 3)+
            labs(x = "First PC Selected", y = "Second PC Selected" ) 
    })  
    
    observeEvent(input$pat, {
        v$value <- ggplot(df$pattr, aes(x = PCA_cor()[[input$PCS1]], y = PCA_cor()[[input$PCS2]], color = rownames((PCA_cor())))) +
            geom_point(size = 3) +
            geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) +
            labs(x = "First PC Selected", y = "Second PC Selected" ) 
        
        
    })
    
    observeEvent(input$vars,{
        cum_perc = c()
        for(i in (1 : PCA_perc())){
            cum_perc = c(cum_perc, sum(PCA_perc()[1:i]))
        }
        
        df$var_exp = data.frame(pc = PCA_vals(), perc = PCA_perc(), cum_p = cum_perc)
        
    })
    
    observeEvent(input$pcs,{
        df$pattr = PCA_cor()
    })
    
    
    output$vars <- renderUI({
        checkboxGroupInput( inputId = "features",
                            choices = colnames(data()),
                            selected = colnames(data()), label = "Select variables...")
    })
    
    output$dt <- renderUI({
        selectInput( inputId = "category", 
                      choices = c("None Selected", colnames(data())[PCA_ret()$nn]),
                      selected = "None Selected", label = "Select Category...")
    })
    
    output$pcs <- renderUI({
        
        fluidRow(
            column(width = 6, selectInput( inputId = "PCS1",
                                            choices = PCA_vals(),
                                            selected = PCA_vals()[1], label = "First PC")),
            column(width = 6, selectInput( inputId = "PCS2",
                                            choices = PCA_vals(),
                                            selected = PCA_vals()[1], label = "Second PC"))
        )
        
    })
    
    output$plot <- renderPlot({v$value})
    
    output$eigen_form <- renderUI({
        withMathJax("\\[\\frac{EigenVector_{x}}{\\sum EigenVector}\\]")
    })

    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
