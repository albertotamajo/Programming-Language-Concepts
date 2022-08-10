IMPORT 'R.csv' :: (String,String) AS R;
x := GET (GET R(*) PRODUCT GET R(*) GIVEN 'C2' = 'C3')('C1','C2','C4');
GET (GET x(*) PRODUCT GET R(*) GIVEN 'C3' = 'C4') ('C1','C5') WHERE ORDERBY(ASC,'C1','C5')