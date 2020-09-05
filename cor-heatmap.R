library("ggplot2")
library("dplyr")
library("reshape")
library("psych")
library("ggtree")
library("aplot")
table1 <- mtcars[,1:5] %>% as.matrix()
table2 <- mtcars[,6:11] %>% as.matrix()
#psych包的corr.test()计算相关性系数与P值,adjust = "fdr"进行多重矫正
pp <- corr.test(table1,table2,method="pearson",adjust = "fdr")
#抽提出相关性系数与P值
cor <- pp$r 
pvalue <- pp$p
#根据P值用循环将P值做成*
display <- pvalue
l1 <- nrow(display);l2 <- ncol(display)
for(i in 1:l1){
  for(k in 1:l2){
    a <- as.numeric(display[i,k])
    if(a <= 0.001){
      a <- "***"
    }
    if( 0.001 < a && a <= 0.01){
      a <- "**"
    }
    if(0.01 < a && a < 0.05){
      a <- "*"
    }
    if(a >= 0.05){
      a <- ""
    }
    display[i,k] <- a
  }
}
#对数据进行格式转换适用于ggplot2绘图
heatmap <- melt(cor)
heatmap[,4] <- melt(pvalue)[,3]
heatmap[,5] <- melt(display)[,3]
names(heatmap) <- c("sample","gene","cor","pvalue","display")
#导出数据
write.table (heatmap,file ="heatmap.xls", sep ="\t", row.names = F)  
#根据相关性系数矩阵，用ggtree绘制行聚类与列聚类树
phr <- hclust(dist(cor)) %>% ggtree(layout="rectangular", branch.length="none")
phc <- hclust(dist(t(cor))) %>% ggtree() + layout_dendrogram()
#ggplot2绘制热图
pp <- ggplot(heatmap,aes(gene,sample,fill=cor)) + geom_tile()+
  theme_minimal()+scale_fill_viridis_c()+geom_text(aes(label=display),size=5,color="white")+
  scale_y_discrete(position="right")+xlab(NULL) + ylab(NULL)
#通过aplot包将聚类树与热图拼接
pp %>% insert_left(phr, width=.1) %>%
  insert_top(phc, height=.1)

