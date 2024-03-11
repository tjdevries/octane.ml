SELECT a.*
FROM (
    SELECT ProductID, SUM(SalesAmount) AS TotalSales
    FROM Sales
    GROUP BY ProductID
) AS a
WHERE a.TotalSales > (
    SELECT AVG(TotalSales)
    FROM (
        SELECT SUM(SalesAmount) AS TotalSales
        FROM Sales
        GROUP BY ProductID
    ) AS AvgSales
)
