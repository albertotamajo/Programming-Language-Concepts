IMPORT 'F.csv' :: (String,String,String) AS F;
IMPORT 'G.csv' :: (String,String,String) AS G;
f := GET F(*) WHERE NOTEMPTY('C2') && NOTEMPTY('C3'); 
x := GET f(*) PRODUCT GET G(*) GIVEN 'C2' = 'C4' && 'C3' = 'C5';
GET x('C1','C6') WHERE ORDERBY(ASC,'C1','C6')