C                                                                       00000010
C     ******************************************************************00000020
C     ******************************************************************00000030
C     ****                                                          ****00000040
C     ****                    MA86(PHP2)                            ****00000050
C     ****        GRAPHIC-PROGRAMM IN GDV6 ZU MA86(D2AS2)           ****00000060
C     ****                                                          ****00000070
C     ******************************************************************00000080
C     ******************************************************************00000090
C                                                                       00000100
C                                                                       00000110
      INTEGER I,N,N9,I9,COUNT,L,C9                                      00000120
      DOUBLE PRECISION H1,H2,H3,H4,PI,T, GAMMA,C,RHOA,DELT,DFI          00000130
      DOUBLE PRECISION RHON(0:400),GPOT(0:400),VR(0:400),VFI(0:400)     00000140
      DOUBLE PRECISION R(0:21),  B(0:81),BPKT(0:81)                     00000150
      DOUBLE PRECISION XMAX,YMAX,X00,Y00,   X0,Y0,GRX,GRY               00000160
      CHARACTER FN*30,CNT*3                                             00000170
C                                                                       00000180
C                                                                       00000190
C                                                                       00000200
      PI=4.*DATAN(1.0D+00)                                              00000210
      C9=0                                                              00000220
      FN= 'F159.TES2.DATA'                                              00000230
  666 OPEN(21,FILE=FN)                                                  00000240
C                                                                       00000250
C                                                                       00000260
C                                                                       00000270
C                                                                       00000280
C     ***********  BILDSCHIRMPARAMETER *********                        00000290
C                                                                       00000300
      PS=.02                                                            00000310
      VL=75.                                                            00000320
C     >>>>>>>>>> INPUT DATA AND PLOT DATA <<<<<<<<<<                    00000330
  999 CONTINUE                                                          00000340
      REWIND 21                                                         00000350
   30 FORMAT (1X,A3)                                                    00000360
   31 FORMAT (1X,8X,D23.16,2(10X,D23.16))                               00000370
   32 FORMAT (1X,8X,I3,20X,10X,I3,20X,10X,D23.16)                       00000380
   35 FORMAT (1X,I3,2(2X,D23.16))                                       00000390
   40 FORMAT (1X,8X,I8,15X,2(10X,D23.16))                               00000400
   41 FORMAT (1X,I3,2X,I3,4(2X,D23.16))                                 00000410
   42 FORMAT (1X,I3,2(2X,D23.16))                                       00000420
      READ (21, 30) CNT                                                 00000430
      READ (21, 31) GAMMA,RHOA,C                                        00000440
      READ (21, 32) N9,I9,DFI                                           00000450
      DO 881 I=0,I9,1                                                   00000460
         READ (21, 35) N,R(I)                                           00000470
  881 CONTINUE                                                          00000480
C                                                                       00000490
C                                                                       00000500
      CALL PLOTI ('D14K',28.,18.,0,0,0)                                 00000510
      X00=2.                                                            00000520
      Y00=2.                                                            00000530
      XMAX=26.                                                          00000540
      YMAX=16.                                                          00000550
C     ****** PLOTTEN EINES RAHMENS ******                               00000551
      CALL PLOT (X00,Y00,3)                                             00000560
      CALL PLOT (X00,YMAX,2)                                            00000570
      CALL PLOT (XMAX,YMAX,2)                                           00000580
      CALL PLOT (XMAX,Y00,2)                                            00000590
      CALL PLOT (X00,Y00,2)                                             00000600
C                                                                       00000610
 2000 C9=C9+1                                                           00000620
      READ (21,40) COUNT,T,DELT                                         00000630
      DO 1855 I=0,N9*I9  ,1                                             00000640
         READ (21,41) L,L,RHON(I),GPOT(I),VR(I),VFI(I)                  00000650
 1855 CONTINUE                                                          00000660
      DO 1856 N=1,N9,1                                                  00000670
         READ (21,42) L,B(N),BPKT(N)                                    00000680
 1856 CONTINUE                                                          00000690
C                                                                       00000700
C                                                                       00000710
C     ****** PLOTTEN VON 6 BILDERN IM 2*3-FORMAT ******                 00000711
      GRX=1.                                                            00000720
      GRY=1.                                                            00000730
      IF (C9 .GT. 4) GOTO 9999                                          00000731
      IF (C9 .EQ. 1) X0=X00+  1.                                        00000740
      IF (C9 .EQ. 4) X0=X00+ 13.                                        00000750
      IF (C9 .EQ. 7) X0=X00+  1.                                        00000760
      IF (C9 .EQ. 2) X0=X00+ 13.                                        00000770
      IF (C9 .EQ. 5) X0=X00+ 13.                                        00000780
      IF (C9 .EQ. 8) X0=X00+ 13.                                        00000790
      IF (C9 .EQ. 3) X0=X00+  1.                                        00000800
      IF (C9 .EQ. 6) X0=X00+16.5                                        00000810
      IF (C9 .EQ. 9) X0=X00+16.5                                        00000820
      IF (C9 .EQ. 1) Y0=Y00+ 8.                                         00000830
      IF (C9 .EQ. 2) Y0=Y00+ 8.                                         00000840
      IF (C9 .EQ. 3) Y0=Y00+ 1.                                         00000850
      IF (C9 .EQ. 4) Y0=Y00+ 1.                                         00000860
      IF (C9 .EQ. 5) Y0=Y00+1.                                          00000870
      IF (C9 .EQ. 6) Y0=Y00+1.                                          00000880
      IF (C9 .EQ. 7) Y0=Y00+1.                                          00000890
      IF (C9 .EQ. 8) Y0=Y00+1.                                          00000900
      IF (C9 .EQ. 9) Y0=Y00+1.                                          00000910
      CALL PLOT (X0+B(1)*GRX,Y0*GRY,3)                                  00000920
      DO 1858 I=1,N9,1                                                  00000930
         H4=I-1                                                         00000940
         CALL PLOT (X0+B(I)*DCOS(DFI*H4)*GRX,Y0+B(I)*DSIN(DFI*H4)*GRY,2)00000950
 1858 CONTINUE                                                          00000960
      CALL PLOT (X0,Y0,3)                                               00000970
      CALL PLOT (X0+PS*GRX ,Y0,3)                                       00000980
      DO 1897 L=0,40 ,10                                                00000990
         H4=L                                                           00001000
         CALL PLOT (X0+DCOS(PI*H4/20.)*PS*GRX ,                         00001010
     *               Y0+DSIN(PI*H4/20.)*PS*GRY,2)                       00001020
 1897    CONTINUE                                                       00001030
                                                                        00001040
      DO 1921 I=1,I9,1                                                  00001050
      DO 1921 N=1,N9,1                                                  00001060
         H4=N-1                                                         00001070
         H1=DCOS(DFI*H4)                                                00001080
         H2=DSIN(DFI*H4)                                                00001090
         H3=R(I)*B(N)                                                   00001100
         CALL PLOT (X0+(H3*H1+PS)*GRX,Y0+(H3*H2)*GRY,3)                 00001110
         DO 1923 L=0,40 ,10                                             00001120
            H4=L                                                        00001130
            CALL PLOT (X0+(H3*H1+DCOS(PI*H4/20.)*PS)*GRX,               00001140
     *                  Y0+(H3*H2+DSIN(PI*H4/20.)*PS)*GRY,2)            00001150
 1923    CONTINUE                                                       00001160
         CALL PLOT (X0+H3*H1*GRX,Y0+H3*H2*GRY,3)                        00001170
         CALL PLOT (X0+(H3*H1+(VR(N+N9*(I-1))*H1-VFI(N+N9*(I-1))*H2)*VL 00001180
     * )*GRX,Y0+(H3*H2+(VR(N+N9*(I-1))*H2+VFI(N+N9*(I-1))*H1)*VL)*GRY,2)00001190
 1921 CONTINUE                                                          00001200
      IF (C9 .LT. 9) GOTO 2000                                          00001210
C                                                                       00001220
      CALL PLOTE                                                        00001230
C                                                                       00001240
 9999 CONTINUE                                                          00001250
      END                                                               00001260
