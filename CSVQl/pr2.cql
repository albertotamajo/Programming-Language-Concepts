IMPORT 'A.csv' :: (String,String,String) AS A;
GET A('C3','C1') WHERE 'C1' = 'C2', ORDERBY(ASC,'C3','C1')