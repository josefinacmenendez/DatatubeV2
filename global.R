#global.R
library(shiny)
library(shinydashboard)
library(functional)
library(ggplot2)
library(plyr)
library(dplyr)

#read data files
dge_dataframe <- read.csv('nvsdays_ranked_2.csv')
contigs <- sort(unique(as.vector(dge_dataframe$id)))
ranked_contigs <- unique(as.vector(dge_dataframe$id))

#Function that returns a subset of the dataframe given a set of contigs
getDgeSubset<- function(dge, conts){
  return(subset(dge, dge$id %in% conts))
}

#Function that returns a subset of a dataframe given a maximum value for rank
getRankedDgeSubset <- function(dge_dataframe, lower_limit, upper_limit){
  return(nvsdays_ranked_2[which(nvsdays_ranked_2$rank >= lower_limit & nvsdays_ranked_2$rank <= upper_limit),])
}

#function to plot a subset of contigs
plot_heatmap_subset <- function(conts){
  diff_exp_subset <- getDgeSubset(dge_dataframe, conts)
  plot <- plot_heatmap(diff_exp_subset)
  return(plot)
}

#generic function to plot heatmaps
plot_heatmap <- function(diff_exp_data){
  plot <- ggplot(data = diff_exp_data, aes(x = day, y = id))+
  ggtitle("Differential expression in H. glaberrima contigs in days 2, 12 and 20 post-injury of the radial nerve") +
  geom_tile(aes(fill=log2FoldChange)) +
  #geom_text(aes(label = log2FoldChange)) +
  geom_text(aes(label = log2FoldChange), colour = "#999999") +
  #scale_fill_gradient(low="white",high="steelblue",name="Differential expression") +
  scale_fill_gradient2(low='red',mid = 'black',high = 'green', name= 'Differential expression') + 
  theme(plot.title = element_text(size = 18),
        
        legend.position = 'bottom', 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  guides(fill = guide_colorbar(barwidth = 30 , 
                               barheight = 2,
                               title.position = "top", 
                               title.hjust = 0.5))
  return(plot)
}

plot_heatmaps <- function(diff_exp_data , adder, lower_limit, upper_limit){

  max_limit <- max(diff_exp_data$rank) - 37
  while(max_limit > upper_limit){
    dif_exp_subset <- getRankedDgeSubset(diff_exp_data , lower_limit , upper_limit)
    cur_plot <- plotHeatmap(dif_exp_subset)
    print(cur_plot)
    print(max_limit > upper_limit)
    lower_limit <- upper_limit
    upper_limit <- upper_limit + adder
  #plot_heatmaps_rec(diff_exp_data , adder , lower_limit , upper_limit)
  }
}

