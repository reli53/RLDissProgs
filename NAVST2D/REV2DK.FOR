C     EIGENWERT-GLEICHUNG FUER HYDRODYNAMISCHES MODELL                  00000010
C     2-DIM (UM STATIONAEREN KREIS MIT RADIUS B0)                       00000020
C     ----> NUR REELLE EIGENWERTE (IN ABHAENGIGKEIT VON GAMMA)          00000030
C     LAMBS  :=  (RHO / ETA)  * LAMBDA *B0*B0                           00000040
C                                                                       00000050
      REAL*4    EVG                                                     00000060
      REAL*4    ETA,RHO,C,B0                                            00000070
      REAL*4    YMIN,YMAX,XMIN,XMAX                                     00000080
      REAL*4    IMIN,IMAX,ISTEP                                         00000090
      REAL*4    XC,X0,X1,X2,F0,F1,F2,ZST,ZSW,ZEND,X,Y,SSG               00000100
      INTEGER   I,K1,L                                                  00000110
      CHARACTER SCR*1                                                   00000120
      ETA=1.                                                            00000130
      C=2.                                                              00000140
      RHO=100.                                                          00000150
      L=2                                                               00000160
      XMIN=   0.005                                                     00000170
      XMAX=   0.01                                                      00000180
      YMIN= -10.                                                        00000190
      YMAX=   5.                                                        00000200
C                                                                       00000210
C     PI=4.  * ATAN(1.  )                                               00000220
C                                                                       00000230
C                                                                       00000240
      CALL INITT (960)                                                  00000250
      CALL WRITEK (40,'WHICH TEKTRONIX-TERMINAL IS USED ????')          00000260
      CALL NEWLIN                                                       00000270
      CALL WRITEK (40,'ENTER:  DEFAULT   IF TEKTONIX 4012')             00000280
      CALL NEWLIN                                                       00000290
      CALL WRITEK (40,'              2   IF TEKTONIX 4014')             00000300
      CALL NEWLIN                                                       00000310
      READ ( 5,'(A1)') SCR                                              00000320
      CALL SETBUF (3)                                                   00000330
      IF (SCR .EQ. '2') THEN                                            00000340
         K1=4                                                           00000350
         CALL TERM (3,4096)                                             00000360
      ELSE                                                              00000370
         K1=1                                                           00000380
         CALL TERM (3,1024)                                             00000390
      END IF                                                            00000400
C                                                                       00000410
 5555 CALL ERASE                                                        00000420
      CALL MOVABS(0,730*K1)                                             00000430
      CALL ANMODE                                                       00000440
 2001 FORMAT (1X,'   XMIN =',F10.5,'   XMAX =',F10.5)                   00000450
 2000 FORMAT (1X,'   YMIN =',F10.2,'   YMAX =',F10.2)                   00000460
 2002 FORMAT (1X,'      C =',F10.5,'    RHO =',F10.5)                   00000470
 2004 FORMAT (1X,'    ETA =',F10.5,'         ',F10.5)                   00000480
 2005 FORMAT (1X,'      L =  ',I3)                                      00000490
      WRITE ( 6,2001) XMIN,XMAX                                         00000500
      WRITE ( 6,2000) YMIN,YMAX                                         00000510
      WRITE ( 6,2002) C,RHO                                             00000520
      WRITE ( 6,2004) ETA                                               00000530
      WRITE ( 6,2005) L                                                 00000540
      WRITE ( 6,*) ' '                                                  00000550
      WRITE ( 6,*) ' DEFAULT = NO CHANGE '                              00000560
      SCR=' '                                                           00000570
      READ  ( 5,'(A1)') SCR                                             00000580
      IF (SCR .NE. ' ') THEN                                            00000590
         WRITE ( 6, *) ' '                                              00000600
         WRITE ( 6, *) '  8 WERTE DURCH KOMMATA GETRENNT '              00000610
         READ ( 5,*) XMIN,XMAX,YMIN,YMAX,C,RHO,ETA      ,L              00000620
         GOTO 5555                                                      00000630
      ENDIF                                                             00000640
C                                                                       00000650
      CALL ERASE                                                        00000660
      CALL TWINDO (250*K1,950*K1, 50*K1,750*K1)                         00000670
      CALL DWINDO ( XMIN,XMAX,YMIN ,YMAX)                               00000680
      CALL MOVEA  ( XMIN,YMIN )                                         00000690
      CALL DRAWA  ( XMAX,YMIN )                                         00000700
      CALL DRAWA  ( XMAX,YMAX )                                         00000710
      CALL DRAWA  ( XMIN,YMAX )                                         00000720
      CALL DRAWA  ( XMIN,YMIN )                                         00000730
      CALL MOVABS (0,730*K1)                                            00000740
      CALL ANMODE                                                       00000750
 2100 FORMAT ('   C =',F10.5)                                           00000760
 2101 FORMAT (' RHO =',F10.5)                                           00000770
 2102 FORMAT (' ETA =',F10.5)                                           00000780
C2103 FORMAT (' GAM =',F10.5)                                           00000790
 2104 FORMAT ('   L =  ',I3)                                            00000800
 2109 FORMAT (' XMIN =',F9.5)                                           00000810
 2110 FORMAT (' XMAX =',F9.5)                                           00000820
 2111 FORMAT (' YMIN =',F9.2)                                           00000830
 2112 FORMAT (' YMAX =',F9.2)                                           00000840
      WRITE ( 6,2109) XMIN                                              00000850
      WRITE ( 6,2110) XMAX                                              00000860
      WRITE ( 6,2111) YMIN                                              00000870
      WRITE ( 6,2112) YMAX                                              00000880
      WRITE ( 6,2100) C                                                 00000890
      WRITE ( 6,2101) RHO                                               00000900
      WRITE ( 6,2102) ETA                                               00000910
C     WRITE ( 6,2103) GAMMA                                             00000920
      WRITE ( 6,2104) L                                                 00000930
C                                                                       00000940
C                                                                       00000950
      CALL MOVEA (XMIN,0.)                                              00000960
      CALL DRAWA (XMAX,0.)                                              00000970
      CALL MOVEA (0.,YMIN)                                              00000980
      CALL DRAWA (0.,YMAX)                                              00000990
      DO 3000 I=0,2,1                                                   00001000
         IF (I .EQ.0) THEN                                              00001010
            IMIN=1.                                                     00001020
            IMAX=IMIN                                                   00001030
            ISTEP=1.                                                    00001040
            IMAX=IMIN                                                   00001050
            ZST=.0001                                                   00001060
            ZSW=.2                                                      00001070
            ZEND=1000.                                                  00001080
         ENDIF                                                          00001090
         IF (I .EQ. 1) THEN                                             00001100
            IMIN=XMIN                                                   00001110
            IMAX=XMAX                                                   00001120
            ISTEP=(XMAX-XMIN)/200.                                      00001130
            ZST=  YMIN *ABS(YMIN)                                       00001140
            ZEND= YMAX *ABS(YMAX)                                       00001150
            ZSW= (ZEND-ZST)/100.                                        00001160
         ENDIF                                                          00001170
         IF (I .EQ. 2) THEN                                             00001180
            IMIN=YMIN *ABS(YMIN)                                        00001190
            IMAX=YMAX *ABS(YMAX)                                        00001200
            ISTEP=(IMAX-IMIN)/1000.                                     00001210
            ZST= XMIN                                                   00001220
            ZEND=XMAX                                                   00001230
            ZSW= (XMAX-XMIN)/20.                                        00001240
         ENDIF                                                          00001250
C                                                                       00001260
         DO 3010 X=IMIN,IMAX,ISTEP                                      00001270
            X1=ZST                                                      00001280
 3019       IF(I.EQ.0) F1=SSG(X1,C)                                     00001290
            IF(I.EQ.1) F1=     EVG(X1,   C,RHO,ETA,X    ,B0,L)          00001300
            IF(I.EQ.2) F1=     EVG(X ,   C,RHO,ETA,X1   ,B0,L)          00001310
 3020       X2=X1+ZSW                                                   00001320
            IF(I.EQ.0) F2=SSG(X2,C)                                     00001330
            IF(I.EQ.1) F2=     EVG(X2,   C,RHO,ETA,X    ,B0,L)          00001340
            IF(I.EQ.2) F2=     EVG(X ,   C,RHO,ETA,X2   ,B0,L)          00001350
            IF(X1.GT.ZEND) GOTO 3010                                    00001360
            IF ( (F1.EQ.0.) .OR. (F1.GT.0. .AND. F2.LT.0.) .OR.         00001370
     +           (F1.LT.0. .AND. F2.GT.0.) ) THEN                       00001380
C           NULLSTELLENSUCHE NACH REGULA-FALSI                          00001390
              XC=X1                                                     00001400
              X0=X1                                                     00001410
              IF (F1. EQ. 0.) GOTO 3040                                 00001420
 3030         X0=X1-F1*(X2-X1)/(F2-F1)                                  00001430
              IF(I.EQ.0) F0=SSG(X0,C)                                   00001440
              IF(I.EQ.1) F0=     EVG(X0,   C,RHO,ETA,X    ,B0,L)        00001450
              IF(I.EQ.2) F0=     EVG(X ,   C,RHO,ETA,X0   ,B0,L)        00001460
              IF(XC.EQ.X0              ) GOTO 3040                      00001470
              XC=X0                                                     00001480
              IF ( (F0.LT.0. .AND. F1.LT.0.) .OR.                       00001490
     +             (F0.GT.0. .AND. F1.GT.0.) ) THEN                     00001500
                 X1=X0                                                  00001510
                 F1=F0                                                  00001520
              ELSE                                                      00001530
                 X2=X0                                                  00001540
                 F2=F0                                                  00001550
              ENDIF                                                     00001560
              GOTO 3030                                                 00001570
           ELSE                                                         00001580
              X1=X2                                                     00001590
              F1=F2                                                     00001600
              GOTO 3020                                                 00001610
           ENDIF                                                        00001620
 3040      IF (X0.GT.ZEND) GOTO 3010                                    00001630
           IF (I.EQ.0) THEN                                             00001640
              B0=X0                                                     00001650
              CALL MOVABS(0,500*K1)                                     00001660
              CALL ANMODE                                               00001670
 2120         FORMAT ('  B0 =',F10.5)                                   00001680
              WRITE ( 6,2120) B0                                        00001690
              GOTO 3010                                                 00001700
           ENDIF                                                        00001710
C                                                                       00001720
           IF (I .EQ. 1) THEN                                           00001730
              Y=SQRT(ABS(X0))                                           00001740
              IF (X0 .LT. 0. ) Y=-Y                                     00001750
              CALL PPOI99(X , Y, 0. ,3601)                              00001760
           ENDIF                                                        00001770
           IF (I .EQ. 2) THEN                                           00001780
              Y=SQRT(ABS(X ))                                           00001790
              IF (X  .LT. 0. ) Y=-Y                                     00001800
              CALL PPOI99(X0, Y, 0. ,3601)                              00001810
           ENDIF                                                        00001820
C                                                                       00001830
           X1=X0+ZSW/10.                                                00001840
           GOTO 3019                                                    00001850
 3010    CONTINUE                                                       00001860
 3000 CONTINUE                                                          00001870
C                                                                       00001880
      CALL VCURSR (ICH,XDUMMY,YDUMMY)                                   00001890
C     A=65 , B=66 , C=67 , ....                                         00001900
C     L=76 , M=77 , N=78 , ....                                         00001910
C     P=80 ---- PARAMETER AENDERN                                       00001920
      IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 5555                      00001930
      END                                                               00001940
C                                                                       00001950
C                                                                       00001960
      REAL FUNCTION SSG(X,C)                                            00001970
      REAL*4 X,C,I0,I1                                                  00001980
      INTEGER ICON                                                      00001990
      CALL BI1(X,I1,ICON)                                               00002000
      CALL BI0(X,I0,ICON)                                               00002010
      SSG=C* I1/I0 - X/2                                                00002020
      RETURN                                                            00002030
      END                                                               00002040
C                                                                       00002050
      REAL FUNCTION EVG(LAMBS,  C,RHO,ETA,GAMMA,B0,L)                   00002060
      REAL*4  LAMBS ,WLAMB                                              00002070
      REAL*4  JL2,JL, H1, H2 ,H4                                        00002080
      REAL*4  C,RHO,ETA,GAMMA,B0                                        00002090
      REAL*4  I1,I2,IL,IL1,IL2                                          00002100
      INTEGER L,ICON,N                                                  00002110
      WLAMB = SQRT(ABS(LAMBS))                                          00002120
      H4=1.                                                             00002130
      DO 100 N=1,L,1                                                    00002140
         H4=H4*WLAMB/FLOAT(N)/2.                                        00002150
  100 CONTINUE                                                          00002160
C                                                                       00002170
      IF (WLAMB .LT. 1.E-2) THEN                                        00002180
         H1=FLOAT(L)                                                    00002190
         H2=FLOAT(2*L*(L*L-1))                                          00002200
      ELSE                                                              00002210
         IF (LAMBS .GE. 0.) THEN                                        00002220
            CALL BIN( WLAMB ,L+2, IL2,ICON)                             00002230
            CALL BIN( WLAMB ,L  , IL ,ICON)                             00002240
            H1=( FLOAT(L)*IL + IL2 ) / H4                               00002250
            H2=( FLOAT(2*L*(L*L-1))* (IL +IL2) ) /H4                    00002260
            H2=H2+H1*WLAMB*WLAMB                                        00002270
         ELSE                                                           00002280
            CALL BJN( WLAMB ,L+2,JL2,ICON)                              00002290
            CALL BJN( WLAMB ,L  ,JL ,ICON)                              00002300
            H1= ( FLOAT(L)*JL-JL2  ) / H4                               00002310
            H2= ( FLOAT(2*L*(L*L-1)) *(JL-JL2) ) / H4                   00002320
            H2=H2-H1*WLAMB*WLAMB                                        00002330
         ENDIF                                                          00002340
      ENDIF                                                             00002350
C                                                                       00002360
      CALL BIN(B0,L+1,IL1,ICON)                                         00002370
      CALL BIN(B0,L  ,IL ,ICON)                                         00002380
      CALL BIN(B0,2  ,I2 ,ICON)                                         00002390
      CALL BIN(B0,1  ,I1 ,ICON)                                         00002400
C                                                                       00002410
      EVG= - (RHO-C) * (ETA/RHO/B0/B0)  * LAMBS    * H2                 00002420
     +     +  B0 * (I2/I1-IL1/IL)/2.               * H2                 00002430
     +     -  GAMMA*RHO* FLOAT(L*(L*L-1)) /B0/ETA  * H1                 00002440
C 111 FORMAT (1X,4(E12.5,2X))                                           00002450
C     WRITE (6,111) WLAMB,H1,H2,EVG                                     00002460
      RETURN                                                            00002470
      END                                                               00002480
C                                                                       00002490
      SUBROUTINE PPOI99(X,Y,PS,SW)                                      00002500
      REAL*4 X,Y,PS,H1,PI                                               00002510
      INTEGER SW,L                                                      00002520
      PI= 4.  *ATAN(1.  )                                               00002530
      CALL MOVEA (X+PS,Y)                                               00002540
      DO 100 L=0,3600,SW                                                00002550
         H1=FLOAT(L)*2.                                                 00002560
         CALL DRAWA (X+PS*COS(PI*H1/3600.),Y+PS,SIN(PI*H1/3600.))       00002570
  100 CONTINUE                                                          00002580
      CALL MOVEA (X,Y)                                                  00002590
      RETURN                                                            00002600
      END                                                               00002610
