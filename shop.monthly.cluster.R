source("../auxiliary/mylib.R")
source("../auxiliary/dbhandle.R")
mylib(c("data.table"))

db <- dbhandle(db="DS")

sql <- "select * from DS.dbo.ShopMonthlyOverView where YearMonth = 201805"
dt1 <- sqlQuery(db, sql) %>% data.table

sql <- "select * from DsWorkSpace.dbo.ShopMonthlyForMillion where YearMonth = 201805"
dt2 <- sqlQuery(db, sql) %>% data.table

dt <- dt1[dt2, on = c("ShopId", "YearMonth")]
dt[, `:=` (web.ga.perc = round(WebGaSessions / GaSessions, 2),
           web.tg.perc = round(WebOrderTgCounts / OrderTgCounts, 2),
           return.perc = round(ReturnPurchaseMemberCounts / PurchasedMemberCounts, 2),
           arpu = round(OrderSales / PurchasedMemberCounts, 2))]
mydata.all <- dt[OrderSales>=100000, .(ShopId, OrderSales, x, R3, WebConversion, AppConversion, BeenMillionLastSixMonths,
       web.ga.perc, web.tg.perc, return.perc, arpu, D1Plus, D2Plus)]
mydata.all[, `:=` (arpu=cut(arpu, c(0, 1000, 2000, 3000, max(arpu)), labels = c("0", "1", "2", "3")),
                   web.ga.perc=cut(web.ga.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1")),
                   return.perc=cut(return.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1")),
                   WebConversion=round(WebConversion*100, 2),
                   AppConversion=round(AppConversion*100, 2))]
mydata.all[, `:=` (arpu=as.integer(as.character(arpu)),
                   web.ga.perc=as.integer(as.character(web.ga.perc)),
                   return.perc=as.integer(as.character(return.perc)))]
mydata.all <- na.omit(mydata.all)
# mydata <- mydata.all[, .(WebConversion, AppConversion, BeenMillionLastSixMonths,
#                          web.ga.perc, web.tg.perc, return.perc, arpu, D1Plus, D2Plus)]
mydata <- mydata.all[, .(WebConversion, AppConversion, BeenMillionLastSixMonths,
                         web.ga.perc, return.perc, arpu)]
# mydata <- scale(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var), na.rm = T)
n.group <- 30
for (i in 2:n.group) wss[i] <- sum(kmeans(mydata, 
                                          centers=i)$withinss)
plot(1:n.group, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

n.group <- 8
# K-Means Cluster Analysis
fit <- kmeans(mydata, n.group) # 5 cluster solution
# get cluster means 
View(aggregate(mydata,by=list(fit$cluster),FUN=mean))
# append cluster assignment
# mydata.all <- data.frame(mydata.all, fit$cluster)
mydata.all[, fit.cluster:=fit$cluster]
table(mydata.all$fit.cluster)
# mydata.all[fit.cluster==5, .(ShopId)]


# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=n.group) # cut tree into 5 clusters
mydata.all[, fit.hcluster:=groups]
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=n.group, border="red")
# Ward Hierarchical Clustering with Bootstrapped p values
mylib("pvclust")
fit <- pvclust(mydata, method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

# Model Based Clustering
mylib("mclust")
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model


# K-Means Clustering with 5 clusters
fit <- kmeans(mydata,n.group)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
mylib("cluster") 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

clusplot(mydata, groups, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


# Centroid Plot against 1st 2 discriminant functions
mylib("fpc")
fpc::plotcluster(mydata, fit$cluster)
fpc::plotcluster(mydata, groups)

tmp <- mydata.all[, .(mean(OrderSales), .N), by = .(fit.cluster, D1Plus, web.ga.perc)]
setorder(tmp, fit.cluster, D1Plus, web.ga.perc)
mydata.all[, .(mean(OrderSales), mean(D1Plus), mean(D2Plus), .N), by = .(fit.cluster)]
mydata.all[, .(mean(OrderSales), .N), by = .(fit.hcluster)]
