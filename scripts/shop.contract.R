source("../auxiliary/mylib.R")
mylib("data.table")
dt.raw <- fread("data/raw_input/DimShop+ (DataServiceDW).csv", encoding = "UTF-8")
# system("sed 's/\\0//g' data/raw_input/ShopContract.csv > ShopContract.csv")
# dt <- read.csv("data/raw_input/ShopContract.csv", encoding = "UTF-8", header = F) %>% data.table()
# setnames(dt, c("ShopContractId","SupplierId","ShopId","ContractPackageDocNumber",
#                "ContractPackageName","ShopContractStartDate","ShopContractEndDate",
#                "ContractRenewRequestId","ShopContractCreatedUser",
#                "ShopContractCreatedDateTime","PaymentWayDef","UpdatedDateTime"))

# dt1 <- fread("data/raw_input/20180523_續約管理.csv", encoding = "UTF-8")
# View(dt3[ShopTypeDef!="Lite"&ShopTypeDef!="Free"&!(ShopId %in% dt1$商店序號)])
# 
# dt1[!商店序號 %in% dt3[ShopStatusDef=="Open"&ShopTypeDef!="Lite"&ShopTypeDef!="Free"]$ShopId&商店狀態=="營業中"]
# dt3[`ShopId (FactShopContract)` %in% c(38797, 39099)]
# dt1[商店序號 %in% c(38797, 39099)]
# 
# dt3[!ShopId%in%dt$ShopId]
# dt[!ShopId%in%dt3$ShopId, uniqueN(ShopId)]

dt <- dt.raw[, .(ShopId, ShopName, ShopCategoryName, SupplierId, SupplierName,
                 ShopContractId, ContractPackageDocNumber,
                 ShopContractPackageName, ShopSdOwner, ShopStatusDef, ShopTypeDef,
                 `ShopContractStartDate (FactShopContract)`, 
                 `ShopContractEndDate (FactShopContract)`)]
setnames(dt, c("ShopContractStartDate (FactShopContract)",
               "ShopContractEndDate (FactShopContract)"), c("st.dt", "ed.dt"))
setorder(dt, ShopId, st.dt, ed.dt)
dt[, `:=` (nxt.st.dt=shift(st.dt, n=1L, type = "lead"),
           last.ed.dt=shift(ed.dt, n=1L, type = "lag")), by = ShopId]
dt[, `:=` (st.dt=as.Date(st.dt, format="%Y/%m/%d"),
           ed.dt=as.Date(ed.dt, format="%Y/%m/%d"),
           nxt.st.dt=as.Date(nxt.st.dt, format="%Y/%m/%d"),
           last.ed.dt=as.Date(last.ed.dt, format="%Y/%m/%d"))]
dt[st.dt]
dt[ShopId==88]
dt[ShopId == 14]
dt[, `:=` (contract.duration = as.integer(ed.dt - st.dt),
           contract.gap = as.integer(nxt.st.dt - ed.dt))]
View(dt[, .(ShopId, ShopName, ShopContractPackageName, ShopSdOwner, st.dt, ed.dt, nxt.st.dt, contract.duration, contract.gap)])
dt[]