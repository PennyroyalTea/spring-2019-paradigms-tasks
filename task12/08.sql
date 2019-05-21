-- Выведите названия стран, у которых столица - не самый многочисленный город.
-- Если для страны не задан ни один город, то выводить ее не нужно. Вывод
-- должен быть отсортирован в порядке уменьшения плотности населения страны,
-- при равной плотности в лексикографическом порядке (вывод: название страны,
-- население страны, площадь страны) (1,25 балла)
SELECT Country.Name, City.Population, Country.SurfaceArea
FROM Country
JOIN Capital ON Country.Code = Capital.CountryCode
JOIN City ON Capital.CityId = City.Id
WHERE City.Population < (
	SELECT MAX(City.Population)
	FROM City
	WHERE City.CountryCode = Country.Code
	);
