REM THIS PROGRAM MODELS A SIMPLE HARMONIC OSCILLATOR (A MASS ON A SPRING)
10 PRINT "Enter the spring constant (in N/m): "
20 INPUT K
30 PRINT "Enter the mass (in kg): "
40 INPUT M
50 PRINT "Enter the initial displacement (in m): "
60 INPUT X0
70 PRINT "Enter the simulation time (in s): "
80 INPUT T
90 PRINT "Enter the time step (in s): "
100 INPUT D
110 LET N = INT(T / D)
120 DIM X(N)
130 GOSUB 200 REM CALCULATE POSITIONS
140 PRINT "Time (s)", "	", "Position (m)"
150 FOR I = 1 TO N
160     PRINT I * D, "		", X(I)
170 NEXT I
180 END
200 REM SUBROUTINE TO CALCULATE POSITIONS
220 LET O = SQR(K / M)
230 FOR I = 1 TO N
240     LET X(I) = X0 * COS(O * (I * D))
250 NEXT I
260 RETURN