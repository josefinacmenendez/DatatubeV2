#process csv files with differential gene expression data
#to rank the genes in descending order of change in differential expression
#change in differential expression is defined as abs(log2foldchange_1 - log2foldchange_2)
#output: dataframe with ranked differential expression for the set of contigs

#read data files
nvs2 <- read.csv("N_vs_day2_whole_list.csv")
nvs12<- read.csv("N_vs_day12_whole_list.csv")
nvs20<- read.csv("N_vs_day20_whole_list.csv")

#subset data to include contigs and log2FoldChange
nvs2 <- filter(nvs2) %>%select(id, log2FoldChange)
nvs12<- filter(nvs12)%>%select(id, log2FoldChange)
nvs20<- filter(nvs20)%>%select(id, log2FoldChange)

#change colnames
colnames(nvs2) <- c('id' , 'log2FoldChangeDay2' )
colnames(nvs12)<- c('id' , 'log2FoldChangeDay12')
colnames(nvs20)<- c('id' , 'log2FoldChangeDay20')
#join the data frames
nvsdays <- join_all(list(nvs2,nvs12,nvs20), by = 'id', match = 'all')
#omit missing data
nvsdays <- na.omit(nvsdays)
#omit infinit values
nvsdays <- nvsdays[!is.infinite(nvsdays$log2FoldChangeDay2),]
nvsdays <- nvsdays[!is.infinite(nvsdays$log2FoldChangeDay12),]
nvsdays <- nvsdays[!is.infinite(nvsdays$log2FoldChangeDay20),]

#Function that returns a data frame with the change in differential expression with respect to each day
getDeltaDifExp <- function(nvsdays){
  delta_diff_exp <- data.frame(matrix(ncol = 3, nrow = 0))
  c_ids <- data.frame(matrix(ncol = 1, nrow = 0))
  
  for(i in seq_len(nrow(nvsdays))){
    print(as.character(nvsdays[i,1]))
    dif1 <- abs(nvsdays[i,4] - nvsdays[i,3])
    dif2 <- abs(nvsdays[i,4] - nvsdays[i,2])
    dif3 <- abs(nvsdays[i,3] - nvsdays[i,2])
    delta_diff_exp <- rbind(delta_diff_exp, c(dif1, dif2, dif3))
  }
  c_ids <- as.character(nvsdays[,1])
  c_rank <- rep(dim(delta_diff_exp)[1], dim(delta_diff_exp)[1])
  delta_diff_exp <- cbind(c_ids , delta_diff_exp, c_rank)
  colnames(delta_diff_exp) <- c('ID' , 'Day20_Day12' , 'Day20_Day2' , 'Day12_Day2', 'rank')
  return(delta_diff_exp)
}

#write ddf into a .csv
write.csv(ddf, 'ddf.csv', row.names = F)
#Process ddf dataframe to sort by decreasing change in differential expression
rnk = 1
while(rnk < max(ddf$rank)){
  print(rnk)
  tmp_mxs <- c( max(ddf[rnk:max(dim(ddf)) , ]$Day20_Day12) , 
                max(ddf[rnk:max(dim(ddf)) , ]$Day20_Day2) , 
                max(ddf[rnk:max(dim(ddf)) , ]$Day12_Day2) )
  tmp_max <- max(tmp_mxs)
  ddf[which(ddf$Day20_Day12 == tmp_max | ddf$Day20_Day2 == tmp_max | ddf$Day12_Day2 == tmp_max) , ]$rank <- rnk
  ddf <- ddf[order(ddf$rank),]
  rnk = rnk + 1
}
nvsdays_ranked <- merge(nvsdays, ddf_ranked[, c(1,5)], "id")
nvsdays_ranked <- nvsdays_ranked[order(nvsdays_ranked$rank), ]
write.csv(nvsdays_ranked, 'nvsdays_ranked.csv' , row.names=F)

#create new data frame with columns: id, log2FoldChange, day, rank
day2 <- nvsdays_ranked[, c(1,2,5)]
day12<- nvsdays_ranked[, c(1,3,5)]
day20<- nvsdays_ranked[, c(1,4,5)]

day2_col <- rep("Day2", max(dim(day2)))
day12_col<- rep("Day12",max(dim(day12)))
day20_col<- rep("Day20",max(dim(day20)))

day2 <- cbind(day2, day2_col)
day12<- cbind(day12,day12_col)
day20<- cbind(day20,day20_col)

#change column names
common_col_names <- c("id", "log2FoldChange" , "rank" , "day")
colnames(day2) <- common_col_names
colnames(day12)<- common_col_names
colnames(day20)<- common_col_names

nvsdays_ranked_2 <- rbind(day2,day12,day20)
write.csv(nvsdays_ranked_2, 'nvsdays_ranked_2.csv',row.names=F)