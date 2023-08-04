CREATE TABLE staffs(
  staff_id INT PRIMARY KEY,
  staff_name TEXT,
  staff_email TEXT,
  staff_mobile VARCHAR(10)
);  
CREATE TABLE categories(
  cat_id INT PRIMARY KEY,
  cat_name TEXT
);
CREATE TABLE menus(
  menu_id INT PRIMARY KEY,
  menu_name TEXT,
  menu_price INT,
  cat_id INT,
  FOREIGN KEY(cat_id) REFERENCES categories(car_id)
);
CREATE TABLE customers(
  c_id INT PRIMARY KEY,
  c_name TEXT,
  c_mobile VARCHAR(10),
  c_email TEXT
);
CREATE TABLE orders(
  order_id INT PRIMARY KEY,
  staff_id INT,
  c_id INT,
  FOREIGN KEY(staff_id) REFERENCES staffs(staff_id),
  FOREIGN KEY(c_id) REFERENCES customers(c_id)
);
CREATE TABLE menu_orders(
  order_id INT,
  menu_id INT,
  FOREIGN KEY(order_id) REFERENCES orders(order_id),
  FOREIGN KEY(menu_id) REFERENCES menus(menu_id)
);
