-- Выведите название страны с максимальным уровнем грамотности по последним
-- данным, которые доступны для страны. В выводе: название страны и уровень
-- грамотности, именно в таком порядке и без лишних полей. (0,75 баллов)
SELECT Country.Name, LiteracyRate.Rate
FROM Country
JOIN LiteracyRate ON Country.Code = LiteracyRate.CountryCode
ORDER BY LiteracyRate.Rate DESC
LIMIT 1;