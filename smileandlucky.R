setwd("C:/Users/abc/Desktop/social")#设置工作目录
smile1=bruceR::import("smile1.xlsx")#导入数据
lucky1=bruceR::import("lucky1.xlsx")
smile2=bruceR::import("smile2.xlsx")
lucky2=bruceR::import("lucky2.xlsx")
smile1$time=gsub("秒", "", smile1$time)#消除中文
smile2$time=gsub("秒", "", smile2$time)
lucky1$time=gsub("秒", "", lucky1$time)
lucky2$time=gsub("秒", "", lucky2$time)
smile1=dplyr::mutate(smile1,time=as.numeric(time))#转换数据类型
smile2=dplyr::mutate(smile2,time=as.numeric(time))
lucky1=dplyr::mutate(lucky1,time=as.numeric(time))
lucky2=dplyr::mutate(lucky2,time=as.numeric(time))

library(dplyr)#清洗数据，选择变量
smile1.1=select(smile1,time,zongfen,xingbie,qq, waizai1:waizai7,qingjing1:qingjing9,shejiao1:shejiao3)#第一次数据
smile1.1=smile1.1%>%mutate(fake=rowSums(select(smile1, qingjing6,qingjing7,waizai3)))
smile1.1=smile1.1%>%mutate(real=rowSums(select(smile1, shejiao1:shejiao3,waizai1:waizai2,waizai4:waizai7,qingjing1:qingjing5,qingjing8,qingjing9)))####去除假笑因子
lucky1.1=select(lucky1,time,zongfen,xingbie,qq,hy1:hy18)

smile2.1=select(smile2,time,zongfen,xingbie,qq,waizai1:waizai7,qingjing1:qingjing9,shejiao1:shejiao3)#第二次数据
smile2.1=smile2.1%>%
         mutate(fake=rowSums(
               select(smile2, qingjing6,qingjing7,waizai3)))
smile2.1=smile2.1%>%mutate(real=rowSums(select(smile2, shejiao1:shejiao3,waizai1:waizai2,waizai4:waizai7,qingjing1:qingjing5,qingjing8,qingjing9)))####去除假笑因子
lucky2.1=select(lucky2,time,zongfen,xingbie,qq,hy1:hy18)


########合并第一次数据和

s1andl1=merge(smile1.1,lucky1.1,by="qq")#匹配qq一致的数据
s2andl2=merge(smile2.1,lucky2.1,by="qq")

s1andl1.1=s1andl1%>%filter(time.x>=20&time.y>=17)#剔除作答时间
s2andl2.1=s2andl2%>%filter(time.x>=20&time.y>=17)


View(s1andl1.1)
View(s2andl2.1)

##############作图

hist(s1andl1.1$fake,xlab = "Fake Smile",main ="Histogram of Fake Smile" )#假笑直方图
hist(s1andl1.1$lucky,xlab = "Lucky",main ="Histogram of Lucky" )#幸运直方图
cor_fakeandlucky = cor(s1andl1.1$fake, s1andl1.1$lucky, method = "pearson")#假笑相关
print(cor_fakeandlucky)
hist(s1andl1$zongfen.x,xlab = "Smile",main = ("Histogram of Smile"))#总分直方图第一次
hist(s1andl1$zongfen.y,xlab = "Lucky",main = ("Histogram of Lucky"))
library(ggplot2)#散点图
ggplot(s1andl1, aes(x =zongfen.x , y = zongfen.y)) +
  geom_point(color = "black", shape = 15) +
  geom_smooth(formula = y~x, method = "lm", colour = "brown") +
  geom_hline(yintercept = mean(s1andl1$zongfen.y), size =0.5, color = "orange") +
  theme_minimal() +
  labs(x = "Smile", y = "Lucky")
library(ggplot2)#假笑因子
ggplot(s1andl1, aes(x =fake , y = zongfen.y)) +
  geom_point(color = "black", shape = 15) +
  geom_smooth(formula = y~x, method = "lm", colour = "brown") +
  geom_hline(yintercept = mean(s1andl1$zongfen.y), size =0.5, color = "orange") +
  theme_minimal() +
  labs(x = "FakeSmile", y = "Lucky")
library(ggplot2)#真笑因子
ggplot(s1andl1, aes(x =real , y = zongfen.y)) +
  geom_point(color = "black", shape = 15) +
  geom_smooth(formula = y~x, method = "lm", colour = "brown") +
  geom_hline(yintercept = mean(s1andl1$zongfen.y), size =0.5, color = "orange") +
  theme_minimal() +
  labs(x = "RealSmile", y = "Lucky")




cor_total_first = cor(s1andl1$zongfen.x, s1andl1$zongfen.y, method = "pearson")#相关第一次
print(cor_total_first)
cor_total_second = cor(s2andl2$zongfen.x, s2andl2$zongfen.y, method = "pearson")#相关第二次
print(cor_total_second)

#####################################交叉滞后检验###########
library(dplyr)
smile1.1.1=select(smile1.1,qq,zongfen,fake,real)
lucky1.1.1=select(lucky1.1,qq,zongfen,)
smile2.1.1=select(smile2.1,qq,zongfen,fake,real)
lucky2.1.1=select(lucky2.1,qq,zongfen,)
temp1 = inner_join(smile1.1.1, lucky1.1.1, by = "qq")  # 合并S1和l1
temp2 = inner_join(temp1,smile2.1.1, by = "qq")  # 将结果与S2合并
result = inner_join(temp2, lucky2.1.1, by = "qq")  # 最后与l2合并
View(result)

library(psych)
cor_first_fakeSandL=cor(s1andl1.1$fake,s1andl1.1$zongfen.y,method = "pearson")#MR3和幸运

cor_first_realSandL=cor(s1andl1.1$real,s1andl1.1$zongfen.y,method = "pearson")#MR1$MR2和幸运
test_corfake_real=r.test(n=213,r12=0.478221121020359,r34=0.582345966883292,n2=213)#相关系数差异检验
print(test_corfake_real)

cor_S1andL1=cor(result$zongfen.x,result$zongfen.y,method   ="pearson")
cor_S1andS2=cor(result$zongfen.x,result$zongfen.x.x,method ="pearson" )
cor_S1andL2=cor(result$zongfen.x,result$zongfen.y.y,method = "pearson")
cor_L1andL2=cor(result$zongfen.y,result$zongfen.y.y,method = "pearson")
cor_L1andS2=cor(result$zongfen.y,result$zongfen.x.x,method = "pearson")
cor_S2andL2=cor(result$zongfen.x.x,result$zongfen.y.y,method = "pearson")

test_cor=r.test(n=77,r12=0.6408246,r34=0.5300229,n2=77)#相关系数差异检验
print(test_cor)

library(lavaan)###做交叉滞后模型图
library(semPlot)

cor_total_result=data.frame(Smile_one=result$zongfen.x,Lucky_one=result$zongfen.y,
                            Smile_two=result$zongfen.x.x,Lucky_two=result$zongfen.y.y)
model='# 自回归路径
Smile_two ~ Smile_one
Lucky_two ~ Lucky_one

# 交叉滞后路径
Smile_two ~ Lucky_one
Lucky_two ~ Smile_one

# 同时相关
Smile_one ~~ Lucky_one
Smile_two ~~ Lucky_two'
fit = sem(model, data=cor_total_result)#拟合模型
summary(fit, standardized=TRUE, fit.measures=TRUE)
semPaths(fit, 
         whatLabels="est", 
         layout="tree", 
         title=TRUE, 
         edge.label.cex=1, 
         sizeMan=8, 
         sizeLat=10, 
         nCharNodes=5, 
         asize=2, 
         edge.color="black", 
         node.label.cex=1.5,
         mar=c(5, 5, 5, 5))

#####################因素分析############
library(psych)
library(corrplot)
library(dplyr)
View(smile1)
str(smile1)
smile1=select(smile1,waizai1,waizai2,waizai3,waizai4,waizai5,waizai6,waizai7,qingjing1,qingjing2,qingjing3,qingjing4,qingjing5,
              qingjing6,qingjing7,qingjing8,qingjing9,shejiao1,shejiao2,shejiao3)
correlation_matrix <- cor(smile1)
corrplot(correlation_matrix, method = "color")
KMO(smile1)###评估因子分析模型适用性
scree(correlation_matrix)
fa.parallel(correlation_matrix,n.obs = 213,fa="both",fm="minres")
fa3=fa(correlation_matrix,n.obs=213,nfactors = 3,fm="minres",rotate = "none")
print(fa3)
fa3$loadings
fa.diagram(fa3)####画图
fa3_var=fa(correlation_matrix,n.obs=213,nfactors = 3,fm="minres",rotate = "varimax")#旋转因子
fa.diagram(fa3_var)####画图


######################各个因子的分析#####################
# 假设 S1, S2, L1, L2 已经存在，并且有一个共同的列用于合并，例如 "id"
# 如果没有 "id" 列，请确保每个数据框有一个唯一标识列

# 为每个数据框创建唯一标识列（如果没有）
smile1.1$id <- 1:nrow(smile1.1)
smile2.1$id <- 1:nrow(smile2.1)
lucky1.1$id <- 1:nrow(lucky1.1)
lucky2.1$id <- 1:nrow(lucky2.1)

# 合并数据框，处理列名冲突
S_combined <- merge(smile1.1, smile2.1, by = "id", suffixes = c(".S1", ".S2"))
L_combined <- merge(lucky1.1, lucky2.1, by = "id", suffixes = c(".L1", ".L2"))

# 最后合并 S_combined 和 L_combined
df <- merge(S_combined, L_combined, by = "id")

# 检查合并后的数据框列名
print(colnames(df))

# 计算 S_combined 的因子得分
df$MR1_S <- rowMeans(df[, c("shejiao1.S1", "shejiao2.S1", "waizai4.S1", "waizai6.S1", "waizai1.S1", "waizai5.S1", "shejiao3.S1", "waizai2.S1", "waizai7.S1")], na.rm = TRUE)
df$MR2_S <- rowMeans(df[, c("qingjing2.S1", "qingjing3.S1", "qingjing1.S1", "qingjing9.S1", "qingjing8.S1", "qingjing4.S1", "qingjing5.S1")], na.rm = TRUE)
df$MR3_S <- rowMeans(df[, c("qingjing7.S1", "qingjing6.S1", "waizai3.S1")], na.rm = TRUE)

# 计算 L_combined 的因子得分
df$MR1_L <- rowMeans(df[, c("hy1.L1", "hy2.L1", "hy3.L1", "hy4.L1", "hy5.L1")], na.rm = TRUE)
df$MR2_L <- rowMeans(df[, c("hy6.L1", "hy7.L1", "hy8.L1")], na.rm = TRUE)
df$MR3_L <- rowMeans(df[, c("hy9.L1", "hy10.L1", "hy11.L1")], na.rm = TRUE)
df$MR4_L <- rowMeans(df[, c("hy12.L1", "hy13.L1", "hy14.L1")], na.rm = TRUE)
df$MR5_L <- rowMeans(df[, c("hy15.L1", "hy16.L1", "hy17.L1", "hy18.L1")], na.rm = TRUE)
# 相关分析
cor_results <- list()

# 定义要分析的因子对
factors_S <- c("MR1_S", "MR2_S", "MR3_S")
factors_L <- c("MR1_L", "MR2_L", "MR3_L", "MR4_L", "MR5_L")

# 计算相关系数和 p 值
for (fs in factors_S) {
  for (fl in factors_L) {
    if (fs %in% colnames(df) && fl %in% colnames(df)) {
      correlation <- cor(df[[fs]], df[[fl]], use = "complete.obs")
      p_value <- cor.test(df[[fs]], df[[fl]])$p.value
      cor_results[[paste(fs, fl, sep = "_")]] <- c(correlation, p_value)
    }
  }
}

# 打印结果
for (pair in names(cor_results)) {
  cat(pair, "相关系数:", cor_results[[pair]][1], "p-value:", cor_results[[pair]][2], "\n")
}

##############各因子的贡献
Spca_result =  prcomp(df[, c("MR1_S", "MR2_S", "MR3_S")], scale. = TRUE)#smile的方差贡献
summary(Spca_result)
Lpca_result <- prcomp(df[, c("MR1_L", "MR2_L", "MR3_L", "MR4_L", "MR5_L")], scale. = TRUE)#lucky的方差贡献
summary(Lpca_result)