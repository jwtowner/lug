10 REM N-BODY GRAVITATIONAL SIMULATION
20 PRINT "ENTER NUMBER OF PARTICLES: "
30 INPUT N
40 DEF FNS(X) = X * X
50 DIM M(N), X(N), Y(N), V1(N), V2(N), A1(N), A2(N)
60 GOSUB 1000 REM INITIALIZE PARTICLE PROPERTIES

100 REM MAIN SIMULATION LOOP
110 PRINT "ENTER NUMBER OF TIME STEPS: "
120 INPUT T
130 PRINT "ENTER TIME STEP SIZE (S): "
140 INPUT T0
150 FOR T1 = 1 TO T
160     GOSUB 2000 REM CALCULATE FORCES
170     GOSUB 3000 REM UPDATE POSITIONS AND VELOCITIES
180     GOSUB 4000 REM PRINT PARTICLE POSITIONS
190 NEXT T1
200 END

1000 REM INITIALIZE PARTICLE PROPERTIES
1010 FOR I = 1 TO N
1020    PRINT "ENTER MASS (KG) FOR PARTICLE", I, ": "
1030    INPUT M(I)
1040    PRINT "ENTER X POSITION (M) FOR PARTICLE", I, ": "
1050    INPUT X(I)
1060    PRINT "ENTER Y POSITION (M) FOR PARTICLE", I, ": "
1070    INPUT Y(I)
1080    PRINT "ENTER X VELOCITY (M/S) FOR PARTICLE", I, ": "
1090    INPUT V1(I)
1100    PRINT "ENTER Y VELOCITY (M/S) FOR PARTICLE", I, ": "
1110    INPUT V2(I)
1120 NEXT I
1130 RETURN

2000 REM CALCULATE FORCES
2010 FOR I = 1 TO N
2020    LET A1(I) = 0
2030    LET A2(I) = 0
2040    FOR J = 1 TO N
2050        IF I <> J THEN GOSUB 5000 REM CALCULATE GRAVITATIONAL FORCE
2060    NEXT J
2070 NEXT I
2080 RETURN

3000 REM UPDATE POSITIONS AND VELOCITIES
3010 FOR I = 1 TO N
3020    LET V1(I) = V1(I) + A1(I) * T0
3030    LET V2(I) = V2(I) + A2(I) * T0
3040    LET X(I) = X(I) + V1(I) * T0
3050    LET Y(I) = Y(I) + V2(I) * T0
3060 NEXT I
3070 RETURN

4000 REM PRINT PARTICLE POSITIONS
4010 PRINT "TIME STEP: ", T1 * T0, " S"
4020 FOR K = 1 TO N
4030    PRINT "PARTICLE", K, " POSITION: (", X(K), ",", Y(K), ") M"
4040 NEXT K
4050 RETURN

5000 REM CALCULATE GRAVITATIONAL FORCE
5020 LET G = 6.67430E-11
5030 LET X0 = X(J) - X(I)
5040 LET Y0 = Y(J) - Y(I)
5050 LET R = SQR(FNS(X0) + FNS(Y0))
5060 IF R > 0 THEN GOTO 5080
5070 RETURN
5080 LET F = G * M(I) * M(J) / (R * R)
5090 LET A1(I) = A1(I) + F * X0 / (R * M(I))
5100 LET A2(I) = A2(I) + F * Y0 / (R * M(I))
5110 RETURN