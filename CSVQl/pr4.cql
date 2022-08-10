IMPORT 'A.csv' :: (String,String) AS A;
GET A(*) WHERE NOTEMPTY('C2'), ORDERBY(ASC,'C1','C2')