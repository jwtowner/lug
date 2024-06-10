REM THIS PROGRAM CALCULATES THE TRAJECTORY OF A PROJECTILE LAUNCHED
REM WITH A GIVEN INITIAL VELOCITY, LAUNCH ANGLE AND HEIGHT
10 PRINT "Enter the initial velocity (in m/s): "
20 INPUT V
30 PRINT "Enter the launch angle (in degrees): "
40 INPUT A
50 PRINT "Enter the initial height (in meters): "
60 INPUT H0
70 LET G = 9.81
80 GOSUB 200 REM CALCULATE TRAJECTORY
90 PRINT "Time of flight is ", T, " seconds"
100 PRINT "Maximum height is ", H, " meters"
110 PRINT "Range is ", R, " meters"
120 END
200 REM SUBROUTINE TO CALCULATE TRAJECTORY
210 LET R = A * 3.1415926535 / 180
220 LET T = (V * SIN(R) + SQR((V * SIN(R)) ^ 2 + 2 * G * H0)) / G 
230 LET H = H0 + V * V * SIN(R) * SIN(R) / (2 * G)
240 LET R = V * COS(R) * T
250 RETURN