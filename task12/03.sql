-- Выведите столицу Малайзии (Malaysia) (в выводе: только название города).
-- (0,5 баллов)
SELECT City.Name
FROM Country
JOIN Capital ON Country.Code = Capital.CountryCode
JOIN City ON Capital.CityId = City.Id
WHERE Country.Name = 'Malaysia'
LIMIT 1;
