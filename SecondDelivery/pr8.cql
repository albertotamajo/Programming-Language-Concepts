IMPORT 'P.csv' :: (String,String,String,String) ('S1','S2','S3','S4') AS P;
IMPORT 'Q.csv' :: (String,String,String,String) ('S11','S22','S33','S44') AS Q;
x := GET P(*) PRODUCT GET Q(*) GIVEN 'S4' = 'S44';
GET x(
      IF EMPTY('S11') THEN 'S1' ELSE 'S11' AS 'A',
      IF EMPTY('S22') THEN 'S2' ELSE 'S22' AS 'B',
      IF EMPTY('S33') THEN 'S3' ELSE 'S33' AS 'C',
      'S44'
      ) 
    WHERE ORDERBY(ASC,'A','B','C','S44')