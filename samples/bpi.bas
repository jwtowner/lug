5 PRINT "MONTE CARLO ESTIMATION OF PI"
10 PRINT "ENTER NUMBER OF POINTS:";
15 INPUT N
20 LET C = 0
30 FOR I = 1 TO N
40 LET X = RND(1)
50 LET Y = RND(1)
60 IF (X^2 + Y^2) <= 1 THEN LET C = C + 1
70 NEXT I
80 LET P = 4 * C / N
90 PRINT "ESTIMATED VALUE OF PI = "; P
100 END