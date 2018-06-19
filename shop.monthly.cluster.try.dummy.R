source("../auxiliary/mylib.R")
source("../auxiliary/dbhandle.R")
mylib(c("data.table", "fastDummies"))

db <- dbhandle(db="DS")

sql <- "select * from DS.dbo.ShopMonthlyOverView where YearMonth = 201805"
dt1 <- sqlQuery(db, sql) %>% data.table

sql <- "select * from DsWorkSpace.dbo.ShopMonthlyForMillion where YearMonth = 201805"
dt2 <- sqlQuery(db, sql) %>% data.table
close(db)
dt <- dt1[dt2, on = c("ShopId", "YearMonth")]
dt[, `:=` (web.ga.perc = round(WebGaSessions / GaSessions, 2),
           web.tg.perc = round(WebOrderTgCounts / OrderTgCounts, 2),
           return.perc = round(ReturnPurchaseMemberCounts / PurchasedMemberCounts, 2),
           arpu = round(OrderSales / PurchasedMemberCounts, 2))]
# mydata.all <- dt[OrderSales>=100000, .(ShopId, AvgNetSalesSixMonths, x, R3, 
#                                        WebConversion, AppConversion, 
#                                        BeenMillionLastSixMonths, 
#                                        web.ga.perc, web.tg.perc, return.perc, 
#                                        arpu, D1Plus, D2Plus)]
mydata.all <- dt[AvgNetSalesSixMonths>1000000&ShopStatusDef=="Open", 
                 .(ShopId, ShopName, AvgNetSalesSixMonths, 
                   WebConversion, AppConversion, 
                   # BeenMillionLastSixMonths, 
                   web.ga.perc, web.tg.perc, return.perc, 
                   arpu)]
mydata.all[, `:=` (arpu=cut(arpu, c(0, 1000, 2000, 3000, max(na.omit(arpu))), labels = c("0", "1", "2", "3"), include.lowest = TRUE),
                   web.ga.perc=cut(web.ga.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1"), include.lowest = TRUE),
                   return.perc=cut(return.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1"), include.lowest = TRUE),
                   WebConversion=round(WebConversion*100, 2),
                   AppConversion=round(AppConversion*100, 2))]
# mydata.all[, `:=` (arpu=as.integer(as.character(arpu)),
#                    web.ga.perc=as.integer(as.character(web.ga.perc)),
#                    return.perc=as.integer(as.character(return.perc)))]
mydata.all <- na.omit(mydata.all)
mydata <- mydata.all[, .(WebConversion, AppConversion, 
                         # BeenMillionLastSixMonths,
                         web.ga.perc, return.perc, arpu)]
mydata <- fastDummies::dummy_cols(mydata)
# mydata <- scale(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var), na.rm = T)
n.group <- 30
for (i in 2:n.group) wss[i] <- sum(kmeans(mydata, 
                                          centers=i)$withinss)
plot(1:n.group, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

n.group <- 5
# K-Means Cluster Analysis
fit.km <- kmeans(mydata, n.group, iter.max = 1000) # 5 cluster solution
# get cluster means 
# View(aggregate(mydata,by=list(fit$cluster),FUN=mean))
# append cluster assignment
# mydata.all <- data.frame(mydata.all, fit$cluster)
mydata.all[, fit.cluster:=fit.km$cluster]
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
summary(fit) # display the best model
mydata.all[, fit.mclust:=fit$classification]
plot(fit) # plot results 


# K-Means Clustering with 5 clusters
# fit <- kmeans(mydata,n.group)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
mylib("cluster") 
clusplot(mydata, mydata.all$fit.cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusplot(mydata, mydata.all$fit.hcluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
clusplot(mydata, mydata.all$fit.mclust, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


# Centroid Plot against 1st 2 discriminant functions
mylib("fpc")
fpc::plotcluster(mydata, mydata.all$fit.cluster)
fpc::plotcluster(mydata, mydata.all$fit.hcluster)
fpc::plotcluster(mydata, mydata.all$fit.mclust)
mydata.all[ShopId %in% c(711, 14)]
# mydata.all[ShopId %in% c(711, 1074), fit.cluster:=4]
mydata.all[, `:=` (web.ga.perc=as.integer(as.character(web.ga.perc)),
                   return.perc=as.integer(as.character(return.perc)),
                   arpu=as.integer(as.character(arpu)))]
tmp <- mydata.all[, .(AvgNetSalesSixMonths=mean(AvgNetSalesSixMonths), 
                      mAvgNetSalesSixMonths=median(AvgNetSalesSixMonths), 
                      AvgNetSalesSixMonths.sd=sd(AvgNetSalesSixMonths),
                      WebConversion=mean(WebConversion), 
                      mWebConversion=median(WebConversion), 
                      AppConversion=mean(AppConversion),
                      mAppConversion=median(AppConversion),
                      web.ga.perc=mean(web.ga.perc),
                      mweb.ga.perc=median(web.ga.perc),
                      return.perc=mean(return.perc),
                      mreturn.perc=median(return.perc),
                      arpu=mean(arpu),
                      marpu=median(arpu),
                      # been.milli=mean(BeenMillionLastSixMonths),
                      # D1Plus=mean(D1Plus),
                      # D2Plus=mean(D2Plus), 
                      .N), by = .(fit.cluster)]
setorder(tmp, fit.cluster)
tmp
View(tmp)
mydata.all[, `:=` (web.ga.perc=as.integer(as.character(web.ga.perc)),
                   return.perc=as.integer(as.character(return.perc)),
                   arpu=as.integer(as.character(arpu)))]
tmp.h <- mydata.all[, .(AvgNetSalesSixMonths=mean(AvgNetSalesSixMonths), 
                      mAvgNetSalesSixMonths=median(AvgNetSalesSixMonths), 
                      AvgNetSalesSixMonths.sd=sd(AvgNetSalesSixMonths),
                      WebConversion=mean(WebConversion), 
                      mWebConversion=median(WebConversion), 
                      AppConversion=mean(AppConversion),
                      mAppConversion=median(AppConversion),
                      web.ga.perc=mean(web.ga.perc),
                      mweb.ga.perc=median(web.ga.perc),
                      return.perc=mean(return.perc),
                      mreturn.perc=median(return.perc),
                      arpu=mean(arpu),
                      marpu=median(arpu),
                      # been.milli=mean(BeenMillionLastSixMonths),
                      # D1Plus=mean(D1Plus),
                      # D2Plus=mean(D2Plus), 
                      .N), by = .(fit.hcluster)]
setorder(tmp.h, fit.hcluster)
tmp.h
View(tmp.h)

tmp1 <- dt[, .(ShopId, x, SalePageCounts, D1Plus, D2Plus, R3)]
mydata.all <- tmp1[mydata.all, on ="ShopId"]
View(mydata.all)
save(mydata.all, file="data/output/shop_cluster-dummy.RData")
write.csv(mydata.all, file="data/output/shop_cluster-dummy.csv", row.names = F)

# prediction
mydata.predict <- dt[AvgNetSalesSixMonths <= 1000000 & OrderSales > 0 &ShopStatusDef == "Open", 
                     .(ShopId, ShopName, AvgNetSalesSixMonths, 
                       WebConversion, AppConversion, 
                       BeenMillionLastSixMonths, 
                       web.ga.perc, web.tg.perc, return.perc, 
                       arpu)]
mydata.predict[, `:=` (arpu=cut(arpu, c(0, 1000, 2000, 3000, max(na.omit(arpu))), labels = c("0", "1", "2", "3"), include.lowest = TRUE),
                       web.ga.perc=cut(web.ga.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1"), include.lowest = TRUE),
                       return.perc=cut(return.perc, c(0, 0.4, 0.6, 1), labels = c("-1", "0", "1"), include.lowest = TRUE),
                       WebConversion=round(WebConversion*100, 2),
                       AppConversion=round(AppConversion*100, 2))]
mydata.predict[, `:=` (arpu=as.integer(as.character(arpu)),
                       web.ga.perc=as.integer(as.character(web.ga.perc)),
                       return.perc=as.integer(as.character(return.perc)))]
View(mydata.predict)
dim(mydata.predict)
mydata.predict <- na.omit(mydata.predict)
mydata.p <- mydata.predict[, .(WebConversion, AppConversion, 
                               # BeenMillionLastSixMonths,
                               web.ga.perc, return.perc, arpu)]
# fitted(fit.km)
mylib("flexclust")
fit.kcc <- as.kcca(fit.km, data=mydata)
# fit.kcc <- kcca(mydata, k=n.group, kccaFamily("kmeans"))
summary(fit.kcc)
barplot(fit.kcc)
image(fit.kcc)
# mydata.all[, fit.kccluster:=fit.km@cluster]
# table(mydata.all$fit.kccluster)
# tmp <- mydata.all[, .(AvgNetSalesSixMonths=mean(AvgNetSalesSixMonths), 
#                       AvgNetSalesSixMonths.sd=sd(AvgNetSalesSixMonths),
#                       WebConversion=mean(WebConversion), 
#                       AppConversion=mean(AppConversion),
#                       web.ga.perc=mean(web.ga.perc),
#                       return.perc=mean(return.perc),
#                       arpu=mean(arpu),
#                       mAvgNetSalesSixMonths=median(AvgNetSalesSixMonths), 
#                       mWebConversion=median(WebConversion), 
#                       mAppConversion=median(AppConversion),
#                       mweb.ga.perc=median(web.ga.perc),
#                       mreturn.perc=median(return.perc),
#                       marpu=median(arpu),
#                       # been.milli=mean(BeenMillionLastSixMonths),
#                       # D1Plus=mean(D1Plus),
#                       # D2Plus=mean(D2Plus), 
#                       .N), by = .(fit.kccluster)]
# setorder(tmp, fit.kccluster)
# tmp
# tmp1 <- dt[, .(ShopId, x, SalePageCounts, D1Plus, D2Plus, R3)]
# mydata.all <- tmp1[mydata.all, on ="ShopId"]
# View(mydata.all)
# save(mydata.all, file="data/output/shop_cluster-2.RData")
# write.csv(mydata.all, file="data/output/shop_cluster-2.csv", row.names = F)
pred.km <- predict(fit.kcc, newdata=mydata.p, k=n.group, kccaFamily("kmeans"))
table(pred.km)
mydata.predict[, fit.kccluster:=pred.km]
tmp1 <- dt[, .(ShopId, x, SalePageCounts, D1Plus, D2Plus, R3)]
mydata.predict <- tmp1[mydata.predict, on ="ShopId"]
save(mydata.predict, file="data/output/shop_cluster_potential-dummy.RData")
write.csv(mydata.predict, file="data/output/shop_cluster_potential-dummy.csv", row.names = F)

tmp <- mydata.predict[, .(AvgNetSalesSixMonths=mean(AvgNetSalesSixMonths), 
                          mAvgNetSalesSixMonths=median(AvgNetSalesSixMonths), 
                          AvgNetSalesSixMonths.sd=sd(AvgNetSalesSixMonths),
                          WebConversion=mean(WebConversion), 
                          mWebConversion=median(WebConversion), 
                          AppConversion=mean(AppConversion),
                          mAppConversion=median(AppConversion),
                          web.ga.perc=mean(web.ga.perc),
                          mweb.ga.perc=median(web.ga.perc),
                          return.perc=mean(return.perc),
                          mreturn.perc=median(return.perc),
                          arpu=mean(arpu),
                          marpu=median(arpu),
                          # been.milli=mean(BeenMillionLastSixMonths),
                          # D1Plus=mean(D1Plus),
                          # D2Plus=mean(D2Plus), 
                          .N), by = .(fit.kccluster)]
setorder(tmp, fit.kccluster)
tmp
