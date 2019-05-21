-- Выведите названия пяти самых больших по площади стран в убывающем по
-- площади порядке (от большей к меньшей), для стран с одинаковой
-- площадью порядок лексикографический. (0,25 баллов)
SELECT Name 
FROM Country 
ORDER BY SurfaceArea DESC, Name 
LIMIT 5;