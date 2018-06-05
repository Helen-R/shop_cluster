if (!"mylib" %in% ls()) source("../auxiliary/mylib.R")
mylib("data.table")
dt <- fread("data/raw_input/ShopKPI.csv")
setnames(dt, colnames(dt), c("Id","SupplierId","ShopId",
                             "TotalTradesOrderGroupCounts",
                             "TotalWebTradesOrderGroupCounts",
                             "TotalAppTradesOrderGroupCounts",
                             "TotalTradesOrderGroupAmount",
                             "TotalWebTradesOrderGroupAmount",
                             "TotalAppTradesOrderGroupAmount",
                             "GaSessionFromWeb","GaSessionFromApp",
                             "AppNewDownloadCounts","TotalMemberCounts",
                             "FirstPurchasedCounts","ReturnPurchasedCounts",
                             "ReadyToBuyCounts","WebNewRegisterMemberCounts",
                             "AppNewRegisterMemberCounts",
                             "LocationWizardNewRegisterMemberCounts",
                             "OtherNewRegisterMemberCounts","PurchasingMemberCounts",
                             "WaitingToShippingTradesOrderSlaveCounts",
                             "WaitingToShippingTradesOrderSlaveAmount",
                             "WaitingToShippingTradesOrderSlaveRate",
                             "AverageShippingDays","CancelRate","ReturnGoodsRate",
                             "QuestionSolvedRate","AverageQuestionResponseTime","DateTime"))
mydata.all <- dt
mydata.all <- na.omit(mydata.all) # listwise deletion of missing
mydata <- mydata.all[, -c(1:3,20,ncol(mydata.all)), with=F]
mydata <- scale(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var), na.rm = T)
n.group <- 30
for (i in 2:n.group) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:n.group, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

n.group <- 9
# K-Means Cluster Analysis
fit <- kmeans(mydata, n.group) # 5 cluster solution
# get cluster means 
View(aggregate(mydata,by=list(fit$cluster),FUN=mean))
# append cluster assignment
# mydata.all <- data.frame(mydata.all, fit$cluster)
mydata.all[, fit.cluster:=fit$cluster]
table(mydata.all$fit.cluster)
mydata.all[fit.cluster==5, .(ShopId)]


# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=n.group) # cut tree into 5 clusters
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

# Centroid Plot against 1st 2 discriminant functions
mylib("fpc")
plotcluster(mydata, fit$cluster)
