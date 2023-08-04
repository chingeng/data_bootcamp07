INSERT into categories VALUES
  (1,'Appetizers'),
  (2,'Soups'),
  (3,'Pasta'),
  (4,'Pizza'),
  (5,'Dessert');
INSERT INTO menus VALUES
   (1,'French Fries',25,1),
   (2,'Chicken Wings',40,1),
   (3,'Guacamole',30,1),
   (4,'Edamame',15,1),
   (5,'Tomato Soup',50,2),
   (6,'Sweet Corn Soup',50,2),
   (7,'Mushroom Soup',50,2),
   (8,'Chicken Soup',50,2),
   (9,'Lasagne',80,3),
   (10,'Shrimp and pesto sauce',100,3),
   (11,'Tomato sauce pasta',90,3),
   (12,'Cabonara',100,3),
   (13,'Hawaiian',150,4),
   (14,'Magherita',150,4),
   (15,'Prawn Cocktail',200,4),
   (16,'Veggie',250,4),
   (17,'Ice cream',30,5),
   (18,'Cheese tart',45,5),
   (19,'Brownie',35,5),
   (20,'Cake',40,5);
INSERT INTO staffs VALUES
  (1,'John','john@res.com','0893728463'),
  (2,'Mary','mary@res.com','0902843957'),
  (3,'Lisa','lisa@res.com','0825738472');
INSERT INTO customers VALUES
  (1,'Mia','0928562547','mia_mm@gg.com'),
  (2,'Ava','0893827451','a_va@gg.com'),
  (3,'Noah','0917384726','noooah@gg.com'),
  (4,'Oliver','0993746211','oliverr@gg.com');
INSERT INTO orders VALUES
  (1,2,2),
  (2,2,1),
  (3,1,1),
  (4,3,3),
  (5,3,4),
  (6,1,2),
  (7,2,4),
  (8,3,2),
  (9,1,2),
  (10,3,4);
INSERT INTO menu_orders VALUES
  (1,2),(1,4),(1,15),(2,1),(2,10),
  (3,2),(3,8),(3,16),(3,20),(4,1),
  (4,17),(5,3),(5,9),(6,1),(6,6),
  (6,19),(7,5),(7,7),(7,11),(7,18),
  (8,9),(8,12),(8,13),(8,17),(9,1),
  (9,14),(9,17),(10,4),(10,16);
