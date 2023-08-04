-- Finding which staff make the highest sales.
with order_price AS (
  select mo.order_id,
	SUM(m.menu_price) AS total_price
  from menu_orders AS mo
  JOIN menus AS m
  ON mo.menu_id = m.menu_id
  group by mo.order_id),
staff_sale AS(
  SELECT orders.order_id, staffs.staff_name
  FROM orders
  join staffs
  on orders.staff_id = staffs.staff_id)

SELECT ss.staff_name, SUM(op.total_price) AS sales
from staff_sale AS ss
JOIN order_price AS op
on ss.order_id = op.order_id
group by ss.staff_name
order by sales DESC;

/* output: Lisa = 790, John = 695, Mary = 615 
So, Lisa make the highest sales*/

-- Finding which menu is the most popular
select m.menu_name,
	COUNT(mo.menu_id) as menu_freq
  from menu_orders AS mo
  JOIN menus AS m
  ON mo.menu_id = m.menu_id
  group by m.menu_name
  order by menu_freq DESC;
-- French Fries are the most popular.

-- Finding which customers make the highest purchases.
with order_price AS (
  select mo.order_id,
	SUM(m.menu_price) AS total_price
  from menu_orders AS mo
  JOIN menus AS m
  ON mo.menu_id = m.menu_id
  group by mo.order_id),
customer_order AS (
  SELECT o.order_id ,c.c_name as customer
  from orders as o
  JOIN customers as c
  on o.c_id = c.c_id)
  
SELECT co.customer, SUM(op.total_price) as total
from order_price AS op
JOIN customer_order as co
on op.order_id = co.order_id
group by co.customer
order by total DESC;

/* Ava = 930, Oliver = 610, Mia = 505, Noah = 55
So Ava make the highest purchases*/
