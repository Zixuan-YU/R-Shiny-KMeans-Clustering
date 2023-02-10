# Load the required libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(magrittr)

# Load the Iris dataset
data("iris")

# Extract the attributes to perform K-means clustering
df <- iris[,-5] # ecluding the 
species <- iris$Species

# Run PCA to reduce dimensionality
pca <- prcomp(df)

# K-means clustering
kmeans_clustering <- function(df, K) {
        # Fit the k-means model
        clustering_fit <- kmeans(df, centers = K)
        
        # Cluster assignments
        cluster_assignments <- clustering_fit$cluster
        
        return(list(cluster_assignments, clustering_fit))
}

# UI
ui <- fluidPage(
        titlePanel("K-Means Clustering of the Iris dataset"),
        
        fluidRow(
                #selecting number of clusters
                column(2,numericInput("K", "Number of Clusters", 
                                      value = 4, min = 1, max = 9,
                                      step = 1)),
                #clustering assignment displayed in plot
                column(9,plotOutput("plot"))
        ),
        fluidRow(column(3,tableOutput("table1")),
                 column(7,tableOutput("table2"))
        ),
        
        hr(),
        print("by Zixuan Yu   Feb 8th,2023") # add the footer
        
)


# Server
server <- function(input, output) {
        # Cluster the data
        cluster_results <- reactive({
                kmeans_clustering(pca$x[, 1:2], input$K)
        })
        
        # Plot the results
        output$plot <- renderPlot({
                # use PCA result to plot (for the purpose of dimensional reduction)
                ggplot(data.frame(pca$x[, 1:2]), aes(x = PC1, y = PC2, 
                                                     color = species, 
                                                        # color by species
                                                     shape = factor(cluster_results()[[1]]))) + 
                                                        # shape representing cluster assignment
                        geom_point(size = 3, show.legend=TRUE) + 
                        scale_color_brewer(type = "qual", palette = "Set2") + 
                                # change color palette
                        scale_shape_manual("cluster",values = c(1:input$K))+
                        ggtitle( paste("K-Means Clustering with K =", input$K))+
                        theme_bw()
        })
        
        df_res <- iris # make a copy of the original data used 
        
        # Table1 showing how many species in each cluster
        output$table1 <- renderTable({
                df_res$cluster = cluster_results()[[1]] # add cluster assignment to df_res
                df_res %>%
                        group_by(cluster,Species) %>% 
                        tally() # count how many Species in each cluster
        })
        
        # Table2 showing the summary statistics of each cluster
        output$table2 <- renderTable({
                df_res$cluster = cluster_results()[[1]] # add cluster assignment to df_res
                df_res %>% group_by(cluster) %>%
                        summarise(n = n(),
                                avg.Sepal.Length = mean(Sepal.Length),
                                avg.Sepal.Width = mean(Sepal.Width),
                                avg.Petal.Length = mean(Petal.Length),
                                avg.Petal.Width = mean(Petal.Width))

                })

}

# Run the app with UI and server components
shinyApp(ui, server)
