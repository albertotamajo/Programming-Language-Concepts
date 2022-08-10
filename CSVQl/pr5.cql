IMPORT 'A.csv' :: (String) AS A;
GET A('C1',"0" AS 'Zero','C1') WHERE ORDERBY(ASC,'C1','Zero')
