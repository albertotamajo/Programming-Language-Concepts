IMPORT 'S.csv' :: (String,String,String) AS S;
IMPORT 'T.csv' :: (String,String,String) AS T;
s := GET S('C3') WHERE 'C1' = 'C2';
t := GET T('C1') WHERE 'C2' = 'C3'; 
x := GET s(*) PRODUCT GET t(*) GIVEN True = True;
GET x(*) WHERE ORDERBY(ASC,'C1','C2')