IMPORT 'A.csv' :: (String,String) AS A;
IMPORT 'B.csv' :: (String,String) AS B;
x := GET A(*) PRODUCT GET B(*) GIVEN True = True;
GET x(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4')