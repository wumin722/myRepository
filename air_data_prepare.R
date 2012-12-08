###以行的形式读入数据
envir <- readLines("/media/lenovo/R/Rworkspace/Rstudio_space/multivariate/envir.txt", encoding="utf-8")
envir <- iconv(envir, "gbk", "utf-8")
###将数据分开

DataSplit <- function(vec) {
  vect <- unlist(strsplit(vec, split=" "))
  if(nchar((vect)[2]) == 1) {
    vect[3] <- paste(vect[2], vect[3], sep=" ")
    vect <- vect[3:length(vect)]
  }
  else vect <- vect[2:length(vect)]
  vect
}
###转化为data.frame
envir.data <- lapply(envir, function(p) DataSplit(p))
envir.matrix <- c()
for(i in 1:length(envir.data)) {
  envir.matrix <- rbind(envir.matrix, envir.data[[i]])
}

envir.df <- as.data.frame(envir.matrix, stringsAsFactors=F)
head(envir.df)
long <- read.csv("/media/lenovo/R/Rworkspace/Rstudio_space/multivariate/longitude.csv", 
                 header = FALSE)
head(long)
###匹配经纬度
index <- match(envir.df[, 1], long[, 1])
which(is.na(long[index, 2:3]))
envir.df <- cbind(envir.df, long[index, 2:3])
which(envir.df[, 1]=="广 州")
envir.df[75, 9:10] <- c("113°14′", "23°10′")
envir.df
graph.df <- envir.df[, c(1:7, 9, 10, 8)]
head(graph.df)
names(graph.df) <- c("City", "SO2", "SO2.change", "NO2", "NO2.change",
                     "PM10", "PM10.change", "LONG", "LAT", "G")
graph.df$LONG <- as.character(graph.df$LONG)
graph.df$LAT <- as.character(graph.df$LAT)

graph.df <- with(graph.df, graph.df[which(!is.na(LONG)), ])


###将经纬度变为数值型
LongToDigit <- function(char) {
  vect <- unlist(strsplit(char, split="°"))
  decimal <- unlist(strsplit(vect[2], split="′"))
  as.numeric(paste(vect[1], decimal[1], sep="."))
}

###将变动的百分数变为数值型
PercentToDigit <- function(char) {
  vect <- unlist(strsplit(char, split="%"))
  as.numeric(vect[1])
}
graph <- transform(graph.df, SO2=as.numeric(SO2), NO2=as.numeric(NO2), 
                   PM10=as.numeric(PM10),  
                   SO2.change=unlist(lapply(SO2.change, PercentToDigit)),
                   NO2.change=unlist(lapply(NO2.change, PercentToDigit)),
                   PM10.change=unlist(lapply(PM10.change, PercentToDigit)),
                   LONG=unlist(lapply(LONG, LongToDigit)),   
                   LAT=unlist(lapply(graph.df$LAT, LongToDigit)))   


df <- graph[ , c(2,4,6)]
row.names(df) <- graph[, 1]
#加载聚类包
library(cluster)
agn1 <- agnes(df,  stand = TRUE)
op <- par(mar=c(1,1,1,3))
dagn <- as.dendrogram(as.hclust(agn1))

plot(dagn, col=3, main="全国空气质量重点监测城市——系统聚类", 
     col.main=2, horiz=TRUE, center=TRUE,
     nodePar=list(lab.cex=0.6, lab.col="forest green", pch=NA))
par(op)
glabel <- cutree(agn1, k=3)
cbind(df[which(glabel == 2), ], graph[which(glabel == 2), 10])


#在地图上标出
library(ggmap)
china <- get_map("china", zoom=4, maptype="terrain")
layer <- ggmap(china) + geom_point(aes(x = LONG, y = LAT, colour = G, group = G),
                                   size = I(3), data = graph)

layer + scale_colour_manual(values = c("red", "white", "green", "blue"))
#用经纬度对空气质量等级进行聚类，由于劣三级与一级都只有一个样本，所以不用
df <- graph[, 8:10]

df <- df[with(df, which(G != "劣三级" & G != "一级"), ]
df <- df[with(df, order(G)), ]

length(with(df, which(G == "二级")))
tail(df)

###用logistic回归分类
model <- glm(factor(G) ~ LONG + LAT, data=df, family="binomial")
###用最小近邻聚类
library(class)
xnew <- newPoints()
model <- knn(train=df[, 1:2], xnew, cl=df[, 3], k=3, prob=TRUE)

area.points <- as.data.frame(cbind(xnew, as.factor(model)), stringsAsFactors=F)
names(area.points) <- c("LONG", "LAT", "G")
area.points$G <- with(area.points, ifelse( G == 2, "三级", "二级"))
area.points$LONG <- as.numeric(area.points$LONG)
area.points$LAT <- as.numeric(area.points$LAT)
#选出空气质量为三级的点
area.three <- area.points[with(area.points, which(G=="三级")), ]


picture <- ggmap(china) + geom_point(aes(x = LONG, y = LAT, colour = G),
                                     size = I(3), data = df, alpha=I(0.7)) + 
                                       scale_colour_manual(values = c("red", "green"))
picture + geom_point(aes(x = LONG, y = LAT, colour = G), size = I(0.1), data = area.three)
subset(graph, subset=(LAT > 34 & LAT < 37 & LONG > 110 & LONG < 115))
