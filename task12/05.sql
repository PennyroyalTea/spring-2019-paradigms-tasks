-- Выведите форму правления с максимальной суммарной площадью стран, которые её
-- придерживаются (вывод: форма правления и суммарная площадь). (0,25 баллов)
SELECT GovernmentForm, SUM(SurfaceArea) AS S
FROM Country
GROUP BY GovernmentForm
ORDER BY S DESC
LIMIT 1;