IMPORT 'P.csv' :: (String,String,String) AS P;
IMPORT 'Q.csv' :: (String,String,String) AS Q;
IMPORT 'R.csv' :: (String) AS R;
IMPORT 'S.csv' :: (String) AS S;
IMPORT 'T.csv' :: (String,String,String,String) AS T;
x := GET P(*) PRODUCT GET Q(*) GIVEN True = True;
y := GET x(*) PRODUCT GET R(*) GIVEN True = True;
z := GET y(*) PRODUCT GET S(*) GIVEN True = True;
w := GET z(*) PRODUCT GET T(*) GIVEN True = True;
GET w(*) WHERE ORDERBY(ASC,'C1','C2','C3','C4','C5','C6','C7','C8','C9','C10','C11','C12')