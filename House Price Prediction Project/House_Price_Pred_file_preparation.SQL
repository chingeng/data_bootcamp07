SELECT A.number_of_bedrooms,
    A.number_of_bathrooms,
    A.living_area,
    A.lot_area,
    A.number_of_floors,
    A.waterfront_present,
    A.condition_of_the_house,
    A.grade_of_the_house,
    A.area_of_the_house_excluding_basement,
    A.area_of_the_basement,
    2023 - CAST(A.built_year AS INT) AS age_of_the_house,
    A.price
FROM (
    SELECT * FROM house_2016 
    UNION ALL 
    SELECT * FROM house_2017) as A;
