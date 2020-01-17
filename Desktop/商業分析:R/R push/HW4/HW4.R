finance <- read.csv("financialdata.csv")
data <- finance[,-1] #去掉第一行公司id

#把Factor型態轉成numeric型態
data$op_profit_growth_rate <- as.numeric(data$op_profit_growth_rate)
data$current_ratio <- as.numeric(data$current_ratio)
data$quick_rartio <- as.numeric(data$quick_rartio)
str(data)

#1.
M <- cor(data) #求出數據及內各個變數的Corr

#用melt函數Reshape資料
library(reshape2)
melted_Corrmat <- melt(M)
head(melted_Corrmat)
write.csv(melted_Corrmat,'melted_Corrmat.csv')

#畫Heatmap
library(ggplot2)
ggplot(data = melted_Corrmat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "grey") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#2.
library(nsprcomp)
spca <- nscumcomp(data, k=80, nneg=T, scale=T) #每組4個非零係數*16個變數
summary(spca)
screeplot(spca)

pve=(spca$sdev)^2 / (sum(spca$sdev^2))
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
abline(h=0.8)

#3.
ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#4.
biplot(spca,scale=T)



