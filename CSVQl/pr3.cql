IMPORT 'P.csv' :: (String,String,String,String) ('S1','S2','S3','S4') AS P;
IMPORT 'Q.csv' :: (String,String,String,String) ('S11','S22','S33','S44') AS Q;
x := GET P(*) PRODUCT GET Q(*) GIVEN 'S1' = 'S11';
GET x('C1', 
      IF EMPTY('S2') THEN 'S22' ELSE 'S2' AS 'A',
      IF EMPTY('S3') THEN 'S33' ELSE 'S3' AS 'B',
      IF EMPTY('S4') THEN 'S44' ELSE 'S4' AS 'C'
      ) 
    WHERE ORDERBY(ASC,'C1','A','B','C')