library(ggplot2)
library(ggtree)
library(aplot)
p <- read.table("heatmap.txt",header = T,sep="\t",row.names = 1) %>%
    scale() %>% t() %>% data.frame()
phr <- hclust(dist(p)) %>% ggtree(layout="rectangular", branch.length="none") #根据数据绘制行聚类树
phc <- hclust(dist(t(p))) %>% ggtree() + layout_dendrogram() #列聚类树
p$gene <- rownames(p)
p1 <- gather(p, 1:30, key="condition", value='expr') %>% #宽表转长表
ggplot(aes(condition,gene,fill=expr)) + geom_tile()+ theme_minimal()+ 
theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
scale_fill_viridis_c() +scale_y_discrete(position="right")+xlab(NULL) + ylab(NULL) 

p1 %>% insert_left(phr, width=.1) %>%
                  insert_top(phc, height=.2)
