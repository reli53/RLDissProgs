      DOUBLE PRECISION A(- 8: 8,- 8: 8,-16:16)                          00000100
      INTEGER L1,L2,M1,M2,L,M                                           00000200
      DOUBLE PRECISION DL1,DL2,DM1,DM2,DL,DM                            00000300
      DOUBLE PRECISION FL1,FL2,FL,IQU                                   00000400
C                                                                       00000500
C     BERECHNEN VON CLEBSCH-GORDEN-KOEFFIZIENTEN                        00000600
C     0 <= L1 <= L2  ,    L2 - L1 <= L <= L2 + L1                       00000700
C                                                                       00000800
 1111 WRITE (6,*) 'ES MUSS GELTEN:  ',                                  00000900
     *            '0 <= L1 <= L2  ,    L2 - L1 <= L <= L2 + L1 '        00001000
      WRITE (6,*) 'L1,L2,L   (IF L1 < 0 : GOTO ENDE)'                   00001100
      READ  (6,*) L1,L2,L                                               00001200
      IF (L1 .LT. 0) GOTO 8888                                          00001300
C                                                                       00001400
      DL1=L1                                                            00001500
      DL2=L2                                                            00001600
      DL =L                                                             00001700
      FL =DL*(DL+1.D0)                                                  00001800
      FL1=DL1*(DL1+1.D0)                                                00001900
      FL2=DL2*(DL2+1.D0)                                                00002000
C                                                                       00002100
  120 FORMAT (1X,'M1=',I2,2X,'M2=',I2,3X,'M=',I2,2X,'INT=',F15.10,      00002200
     *                      2X,'IQU=',F15.10)                           00002300
  130 FORMAT (1X,'L1=',I2,2X,'L2=',I2,3X,'L=',I2)                       00002400
      WRITE(6,*) ' '                                                    00002500
      WRITE(6,130) L1,L2,L                                              00002600
      WRITE(6,*) ' '                                                    00002700
C                                                                       00002800
C                                                                       00002900
C                                                                       00003000
      M1=L1                                                             00003100
      M2=-L1                                                            00003200
      M=M1+M2                                                           00003300
      A(   M1,   M2,  M)=1.D0                                           00003400
      IQU=A(M1,M2,M)*A(M1,M2,M)                                         00003500
      WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU                     00003600
C                                                                       00003700
C                                                                       00003800
      DO 4010 M1= L1-1,-L1 ,-1                                          00003900
        M2=-M1                                                          00004000
        M=M1+M2                                                         00004100
        DM=M                                                            00004200
        DM1=M1                                                          00004300
        DM2=M2                                                          00004400
        DM =M                                                           00004500
        A(   M1,   M2,  M)=( A(   M1+1,   M2-1,  M) *                   00004600
     *    (FL - FL2- FL1 - 2.D0*(DM1+1.D0)*(DM2-1.D0))                  00004700
     *    -  A(   M1+2,   M2-2,  M)                                     00004800
     *     * DSQRT(FL1 - (DM1+2.D0)*(DM1+1.D0))                         00004900
     *     * DSQRT(FL2 - (DM2-2.D0)*(DM2-1.D0)) ) /                     00005000
     *    DSQRT(FL1 - DM1*(DM1+1.D0)) / DSQRT(FL2 - DM2*(DM2-1.D0))     00005100
        IQU=A(M1,M2,M)*A(M1,M2,M)                                       00005200
        WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU                   00005300
 4010 CONTINUE                                                          00005400
C                                                                       00005500
C                                                                       00005600
      DO 4020 M=      1,  L          ,1                                 00005700
        M1=L1                                                           00005800
        M2=-L1+M                                                        00005900
        DM1=M1                                                          00006000
        DM2=M2                                                          00006100
        DM=M                                                            00006200
        A(   M1,   M2,  M) = (        A(   M1-1,   M2,  M-1) *          00006300
     *     DSQRT( FL1 - DM1*(DM1-1.D0)) + A(   M1,   M2-1,  M-1)*       00006400
     *     DSQRT( FL2 -DM2*(DM2-1.D0))        )                         00006500
     *     /  DSQRT( FL - DM*(DM-1.D0) )                                00006600
C                                                                       00006700
C                                                                       00006800
        IQU=A(M1,M2,M)*A(M1,M2,M)                                       00006900
        WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU                   00007000
        DO 4030 M1= L1-1,M-L2 , -1                                      00007100
          IF (M1 .LT. -L1) GOTO 4030                                    00007200
          M2=M-M1                                                       00007300
          DM1=M1                                                        00007400
          DM2=M2                                                        00007500
          DM =M                                                         00007600
          A(   M1,   M2,  M)=( A(   M1+1,   M2-1,  M) *                 00007700
     *      (FL - FL2- FL1 - 2.D0*(DM1+1.D0)*(DM2-1.D0))                00007800
     *      -  A(   M1+2,   M2-2,  M)                                   00007900
     *       * DSQRT(FL1 - (DM1+2.D0)*(DM1+1.D0))                       00008000
     *       * DSQRT(FL2 - (DM2-2.D0)*(DM2-1.D0)) ) /                   00008100
     *       DSQRT(FL1 - DM1*(DM1+1.D0)) / DSQRT(FL2 - DM2*(DM2-1.D0))  00008200
          IQU=A(M1,M2,M)*A(M1,M2,M)                                     00008300
          WRITE (6,120) M1,M2,M, A(   M1,   M2,  M),IQU                 00008400
 4030  CONTINUE                                                         00008500
 4020 CONTINUE                                                          00008600
C                                                                       00008700
C                                                                       00008800
      GOTO 1111                                                         00008900
 8888 CONTINUE                                                          00009000
C                                                                       00009100
C                                                                       00009200
      END                                                               00009300
