      DOUBLE PRECISION A(- 8: 8,- 8: 8,-16:16)
      INTEGER L1,L2,M1,M2,L,M
      DOUBLE PRECISION DL1,DL2,DM1,DM2,DL,DM
      DOUBLE PRECISION FL1,FL2,FL,IQU
C
C     BERECHNEN VON CLEBSCH-GORDEN-KOEFFIZIENTEN
C     0 <= L1 <= L2  ,    L2 - L1 <= L <= L2 + L1
C
 1111 WRITE (6,*) 'ES MUSS GELTEN:  ',
     *            '0 <= L1 <= L2  ,    L2 - L1 <= L <= L2 + L1 '
      WRITE (6,*) 'L1,L2,L   (IF L1 < 0 : GOTO ENDE)'
      READ  (6,*) L1,L2,L
      IF (L1 .LT. 0) GOTO 8888
C
      DL1=L1
      DL2=L2
      DL =L
      FL =DL*(DL+1.D0)
      FL1=DL1*(DL1+1.D0)
      FL2=DL2*(DL2+1.D0)
C
  120 FORMAT (1X,'M1=',I2,2X,'M2=',I2,3X,'M=',I2,2X,'INT=',F15.10,
     *                      2X,'IQU=',F15.10)
  130 FORMAT (1X,'L1=',I2,2X,'L2=',I2,3X,'L=',I2)
      WRITE(6,*) ' '
      WRITE(6,130) L1,L2,L
      WRITE(6,*) ' '
C
C
C
      M1=L1
      M2=-L1
      M=M1+M2
      A(   M1,   M2,  M)=1.D0
      IQU=A(M1,M2,M)*A(M1,M2,M)
      WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU
C
C
      DO 4010 M1= L1-1,-L1 ,-1
        M2=-M1
        M=M1+M2
        DM=M
        DM1=M1
        DM2=M2
        DM =M
        A(   M1,   M2,  M)=( A(   M1+1,   M2-1,  M) *
     *    (FL - FL2- FL1 - 2.D0*(DM1+1.D0)*(DM2-1.D0))
     *    -  A(   M1+2,   M2-2,  M)
     *     * DSQRT(FL1 - (DM1+2.D0)*(DM1+1.D0))
     *     * DSQRT(FL2 - (DM2-2.D0)*(DM2-1.D0)) ) /
     *    DSQRT(FL1 - DM1*(DM1+1.D0)) / DSQRT(FL2 - DM2*(DM2-1.D0))
        IQU=A(M1,M2,M)*A(M1,M2,M)
        WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU
 4010 CONTINUE
C
C
      DO 4020 M=      1,  L          ,1
        M1=L1
        M2=-L1+M
        DM1=M1
        DM2=M2
        DM=M
        A(   M1,   M2,  M) = (        A(   M1-1,   M2,  M-1) *
     *     DSQRT( FL1 - DM1*(DM1-1.D0)) + A(   M1,   M2-1,  M-1)*
     *     DSQRT( FL2 -DM2*(DM2-1.D0))        )
     *     /  DSQRT( FL - DM*(DM-1.D0) )
C
C
        IQU=A(M1,M2,M)*A(M1,M2,M)
        WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU
        DO 4030 M1= L1-1,M-L2 , -1
          IF (M1 .LT. -L1) GOTO 4030
          M2=M-M1
          DM1=M1
          DM2=M2
          DM =M
          A(   M1,   M2,  M)=( A(   M1+1,   M2-1,  M) *
     *      (FL - FL2- FL1 - 2.D0*(DM1+1.D0)*(DM2-1.D0))
     *      -  A(   M1+2,   M2-2,  M)
     *       * DSQRT(FL1 - (DM1+2.D0)*(DM1+1.D0))
     *       * DSQRT(FL2 - (DM2-2.D0)*(DM2-1.D0)) ) /
     *       DSQRT(FL1 - DM1*(DM1+1.D0)) / DSQRT(FL2 - DM2*(DM2-1.D0))
          IQU=A(M1,M2,M)*A(M1,M2,M)
          WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU
 4030  CONTINUE
 4020 CONTINUE
C
C
      GOTO 1111
 8888 CONTINUE
C
C
      END
