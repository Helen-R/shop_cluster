--calculate historical data
CREATE TABLE DsWorkSpace.dbo.DataForRFM_forhelen (
    [DateId] [int] NOT NULL
    , [ShopId] [bigint] NOT NULL
    , [MemberId] [int] NOT NULL
    , [SalePageId] [int] NOT NULL
    , [TotalPayment] [decimal](38, 2) NULL
    , [PromotionDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
    , [ECouponDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
    , [TotalDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
    , [TgCounts] [varchar](20) NULL
    , [Quantity] [int] NOT NULL DEFAULT 0
    , SalesOrderSlaveStatusDef [varchar](20) NULL
    , [CreatedDateTime] [datetime] NOT NULL DEFAULT GETDATE()
);
CREATE TABLE #tmp (
[DateId] [int] NOT NULL
, [ShopId] [bigint] NOT NULL
, [MemberId] [int] NOT NULL
, [SalePageId] [int] NOT NULL
, [TotalPayment] [decimal](38, 2) NULL
, [PromotionDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [ECouponDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [TotalDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [Quantity] [int] NOT NULL DEFAULT 0
--, [SalesOrderSlaveId] [bigint] NULL
, [TradesOrderGroupCode] [varchar](20) NULL
, SalesOrderSlaveStatusDef [varchar](20) NULL
);

CREATE TABLE #tmp2 (
[DateId] [int] NOT NULL
,[ShopId] [bigint] NOT NULL
,[MemberId] [int] NOT NULL
, [SalePageId] [int] NOT NULL
, [TotalPayment] [decimal](38, 2) NULL
, [PromotionDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [ECouponDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [TotalDiscount] [decimal](19, 2) NOT NULL DEFAULT 0
, [Quantity] [int] NOT NULL DEFAULT 0
, [TradesOrderGroupCode] [varchar](20) NULL
, SalesOrderSlaveStatusDef [varchar](20) NULL
);

DECLARE @nmonth INT
DECLARE @nday INT
SET @nmonth = 1
SET @nday = 1

INSERT INTO #tmp ([DateId], [ShopId], [MemberId], [SalePageId], [TotalPayment], [PromotionDiscount], [ECouponDiscount], [TotalDiscount], [Quantity], [TradesOrderGroupCode], SalesOrderSlaveStatusDef)
SELECT [DateId]
, [ShopId]
, [MemberId]
, [SalePageId]
, [SalesOrderSlaveTotalPayment] AS [TotalPayment]
, [PromotionDiscount]
, [ECouponDiscount]
, ([PromotionDiscount] + [ECouponDiscount]) AS [TotalDiscount]
, [Quantity]
--, [SalesOrderSlaveId]
, [TradesOrderGroupCode]
, 'Return' as SalesOrderSlaveStatusDef
FROM DataServiceDW.dbo.FactSalesOrder t --WITH (nolock, INDEX(IX_ShopId_MemberId_SalesOrderSlaveDateTime))
--WHERE (SalesOrderSlaveStatusDef <> 'Cancel' or SalesOrderSlaveStatusDef <> 'Fail')
WHERE exists (SELECT '' FROM DataServiceDW.dbo.FactReturnGoodsOrder b with(nolock) WHERE t.SalesOrderSlaveId = b.SalesOrderSlaveId and b.StatusDef in ('Finish'))

INSERT INTO #tmp2 ([DateId], [ShopId], [MemberId], [SalePageId], [TotalPayment], [PromotionDiscount], [ECouponDiscount], [TotalDiscount], [Quantity], [TradesOrderGroupCode], SalesOrderSlaveStatusDef)
SELECT [DateId]
, [ShopId]
, [MemberId]
, [SalePageId]
, [SalesOrderSlaveTotalPayment] AS [TotalPayment]
, [PromotionDiscount]
, [ECouponDiscount]
, ([PromotionDiscount] + [ECouponDiscount]) AS [TotalDiscount]
, [Quantity]
, [TradesOrderGroupCode]
, SalesOrderSlaveStatusDef
FROM DataServiceDW.dbo.FactSalesOrder t 
WHERE not exists (SELECT '' FROM DataServiceDW.dbo.FactReturnGoodsOrder b with(nolock) WHERE t.SalesOrderSlaveId = b.SalesOrderSlaveId and b.StatusDef in ('Finish'))
--and not exists (SELECT '' FROM DataServiceDW.dbo.FactCancelOrder c with(nolock) WHERE t.SalesOrderSlaveId = c.SalesOrderSlaveId and c.StatusDef = 'Finish');

INSERT INTO DsWorkSpace.dbo.DataForRFM_forhelen ([DateId],[ShopId],[MemberId], [SalePageId], [SalesOrderSlaveStatusDef],[TotalPayment],[PromotionDiscount],[ECouponDiscount],[TotalDiscount],[TgCounts],[Quantity])
SELECT [DateId]
, t.[ShopId]
, t.[MemberId]
, [SalePageId]
, SalesOrderSlaveStatusDef
, SUM([TotalPayment]) AS [TotalPayment]
, SUM([PromotionDiscount]) AS [PromotionDiscount]
, SUM([ECouponDiscount]) AS [ECouponDiscount]
, SUM([TotalDiscount]) AS [TotalDiscount]
, COUNT(DISTINCT [TradesOrderGroupCode]) AS [TgCounts]
, SUM([Quantity]) AS [Quantity]
FROM #tmp2 t
JOIN DataServiceDW.dbo.DimMember m with(nolock) on t.[ShopId] = m.ShopId and t.[MemberId] = m.MemberId
GROUP BY [DateId], t.[ShopId], t.[MemberId], [SalePageId], SalesOrderSlaveStatusDef;

DROP TABLE #tmp, #tmp2
