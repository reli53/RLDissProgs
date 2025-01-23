C     EIGENWERT-GLEICHUNG FUER HYDRODYNAMISCHES MODELL                  00010000
C     2-DIM (UM STATIONAEREN KREIS MIT RADIUS B0)                       00020000
C     LAMBS  :=  (RHO / ETA) * LAMBDA  *B0*B0                           00030000
C                                                                       00040000
      COMPLEX   EVG                                                     00050000
      REAL*4    ETA,GAMMA,RHO,C,B0                                      00060000
      REAL*4    YMIN,YMAX,XMIN,XMAX                                     00070000
      REAL*4    IMIN,IMAX,ISTEP                                         00080000
      REAL*4    XC,X0,X1,X2,F0,F1,F2,ZST,ZSW,ZEND,  Y,SSG               00090000
      INTEGER   I,J,K1,L                                                00100000
      CHARACTER SCR*1                                                   00110000
      GAMMA=.01                                                         00120000
      ETA=1.                                                            00130000
      C=2.                                                              00140000
      RHO=100.                                                          00150000
      L=2                                                               00160000
      XMIN=- 20.                                                        00170000
      XMAX=  20.                                                        00180000
      YMIN=- 20.                                                        00190000
      YMAX=  20.                                                        00200000
C                                                                       00210000
C     PI=4.  * ATAN(1.  )                                               00220000
C                                                                       00230000
C                                                                       00240000
      CALL INITT (960)                                                  00250000
      CALL WRITEK (40,'WHICH TEKTRONIX-TERMINAL IS USED ????')          00260000
      CALL NEWLIN                                                       00270000
      CALL WRITEK (40,'ENTER:  DEFAULT   IF TEKTONIX 4012')             00280000
      CALL NEWLIN                                                       00290000
      CALL WRITEK (40,'              2   IF TEKTONIX 4014')             00300000
      CALL NEWLIN                                                       00310000
      READ ( 5,'(A1)') SCR                                              00320000
      CALL SETBUF (3)                                                   00330000
      IF (SCR .EQ. '2') THEN                                            00340000
         K1=4                                                           00350000
         CALL TERM (3,4096)                                             00360000
      ELSE                                                              00370000
         K1=1                                                           00380000
         CALL TERM (3,1024)                                             00390000
      END IF                                                            00400000
C                                                                       00410000
 5555 CALL ERASE                                                        00420000
      CALL MOVABS(0,730*K1)                                             00430000
      CALL ANMODE                                                       00440000
 2001 FORMAT (1X,'   XMIN =',F10.2,'   XMAX =',F10.2)                   00450000
 2000 FORMAT (1X,'   YMIN =',F10.2,'   YMAX =',F10.2)                   00460000
 2002 FORMAT (1X,'      C =',F10.5,'    RHO =',F10.5)                   00470000
 2004 FORMAT (1X,'    ETA =',F10.5,'  GAMMA =',F10.5)                   00480000
 2005 FORMAT (1X,'      L =  ',I3)                                      00490000
      WRITE ( 6,2001) XMIN,XMAX                                         00500000
      WRITE ( 6,2000) YMIN,YMAX                                         00510000
      WRITE ( 6,2002) C,RHO                                             00520000
      WRITE ( 6,2004) ETA,GAMMA                                         00530000
      WRITE ( 6,2005) L                                                 00540000
      WRITE ( 6,*) ' '                                                  00550000
      WRITE ( 6,*) ' DEFAULT = NO CHANGE '                              00560000
      SCR=' '                                                           00570000
      READ  ( 5,'(A1)') SCR                                             00580000
      IF (SCR .NE. ' ') THEN                                            00590000
         WRITE ( 6, *) ' '                                              00600000
         WRITE ( 6, *) '  9 WERTE DURCH KOMMATA GETRENNT '              00610000
         READ ( 5,*) XMIN,XMAX,YMIN,YMAX,C,RHO,ETA,GAMMA,L              00620000
         GOTO 5555                                                      00630000
      ENDIF                                                             00640000
C                                                                       00650000
 6666 CALL ERASE                                                        00660000
      CALL TWINDO (250*K1,950*K1, 50*K1,750*K1)                         00670000
      CALL DWINDO ( XMIN,XMAX,YMIN ,YMAX)                               00680000
      CALL MOVEA  ( XMIN,YMIN )                                         00690000
      CALL DRAWA  ( XMAX,YMIN )                                         00700000
      CALL DRAWA  ( XMAX,YMAX )                                         00710000
      CALL DRAWA  ( XMIN,YMAX )                                         00720000
      CALL DRAWA  ( XMIN,YMIN )                                         00730000
      CALL MOVABS (0,730*K1)                                            00740000
      CALL ANMODE                                                       00750000
 2100 FORMAT ('   C =',F10.5)                                           00760000
 2101 FORMAT (' RHO =',F10.5)                                           00770000
 2102 FORMAT (' ETA =',F10.5)                                           00780000
 2103 FORMAT (' GAM =',F10.5)                                           00790000
 2104 FORMAT ('   L =  ',I3)                                            00800000
 2109 FORMAT (' XMIN =',F9.2)                                           00810000
 2110 FORMAT (' XMAX =',F9.2)                                           00820000
 2111 FORMAT (' YMIN =',F9.2)                                           00830000
 2112 FORMAT (' YMAX =',F9.2)                                           00840000
      WRITE ( 6,2109) XMIN                                              00850000
      WRITE ( 6,2110) XMAX                                              00860000
      WRITE ( 6,2111) YMIN                                              00870000
      WRITE ( 6,2112) YMAX                                              00880000
      WRITE ( 6,2100) C                                                 00890000
      WRITE ( 6,2101) RHO                                               00900000
      WRITE ( 6,2102) ETA                                               00910000
      WRITE ( 6,2103) GAMMA                                             00920000
      WRITE ( 6,2104) L                                                 00930000
C                                                                       00940000
C                                                                       00950000
      CALL MOVEA (XMIN,0.)                                              00960000
      CALL DRAWA (XMAX,0.)                                              00970000
      CALL MOVEA (0.,YMIN)                                              00980000
      CALL DRAWA (0.,YMAX)                                              00990000
C                                                                       01000000
      DO 3000 I=0,2,1                                                   01010000
      DO 3000 J=1,2,1                                                   01020000
         IF (J .EQ.1) THEN                                              01030000
             IMIN=AMAX1(0.,YMIN)                                        01040000
             ISTEP=(YMAX-YMIN)/100.                                     01050000
             IMAX=YMAX                                                  01060000
             ZST=XMIN                                                   01070000
             ZSW=(XMAX-XMIN)/ 50.                                       01080000
             ZEND=XMAX                                                  01090000
         ELSE                                                           01100000
             IMIN=XMIN                                                  01110000
             ISTEP=(XMAX-XMIN)/100.                                     01120000
             IMAX=XMAX                                                  01130000
             ZST=AMIN1(0.,YMIN)                                         01140000
             ZSW=(YMAX-YMIN)/ 50.                                       01150000
             ZEND=YMAX                                                  01160000
         ENDIF                                                          01170000
         IF (I .EQ.0) THEN                                              01180000
             IMIN=1.                                                    01190000
             ISTEP=1.                                                   01200000
             IMAX=IMIN                                                  01210000
             ZST=.0001                                                  01220000
             ZSW=.2                                                     01230000
             ZEND=1000                                                  01240000
         ENDIF                                                          01250000
         IF (I .EQ. 0 .AND. J .EQ. 2) GOTO 3000                         01260000
C                                                                       01270000
         DO 3010 Y=IMIN,IMAX,ISTEP                                      01280000
            X1=ZST                                                      01290000
 3019       IF(I.EQ.0) F1=SSG(X1,C)                                     01300000
            IF (J .EQ. 1) THEN                                          01310000
              IF(I.EQ.1) F1=REAL(EVG(X1,Y,C,RHO,ETA,GAMMA,B0,L))        01320000
              IF(I.EQ.2) F1=AIMAG(EVG(X1,Y,C,RHO,ETA,GAMMA,B0,L))       01330000
            ELSE                                                        01340000
              IF(I.EQ.1) F1=REAL(EVG(Y,X1,C,RHO,ETA,GAMMA,B0,L))        01350000
              IF(I.EQ.2) F1=AIMAG(EVG(Y,X1,C,RHO,ETA,GAMMA,B0,L))       01360000
            ENDIF                                                       01370000
 3020       X2=X1+ZSW                                                   01380000
            IF(I.EQ.0) F2=SSG(X2,C)                                     01390000
            IF (J .EQ. 1) THEN                                          01400000
               IF(I.EQ.1) F2=REAL(EVG(X2,Y,C,RHO,ETA,GAMMA,B0,L))       01410000
               IF(I.EQ.2) F2=AIMAG(EVG(X2,Y,C,RHO,ETA,GAMMA,B0,L))      01420000
            ELSE                                                        01430000
               IF(I.EQ.1) F2=REAL(EVG(Y,X2,C,RHO,ETA,GAMMA,B0,L))       01440000
               IF(I.EQ.2) F2=AIMAG(EVG(Y,X2,C,RHO,ETA,GAMMA,B0,L))      01450000
            ENDIF                                                       01460000
            IF(X1.GT.ZEND) GOTO 3010                                    01470000
            IF ( (F1.EQ.0.) .OR. (F1.GT.0. .AND. F2.LT.0.) .OR.         01480000
     +           (F1.LT.0. .AND. F2.GT.0.) ) THEN                       01490000
C           NULLSTELLENSUCHE NACH REGULA-FALSI                          01500000
              XC=X1                                                     01510000
              X0=X1                                                     01520000
              IF (F1. EQ. 0.) GOTO 3040                                 01530000
 3030         X0=X1-F1*(X2-X1)/(F2-F1)                                  01540000
              IF(I.EQ.0) F0=SSG(X0,C)                                   01550000
              IF (J .EQ. 1) THEN                                        01560000
                 IF(I.EQ.1) F0=REAL(EVG(X0,Y,C,RHO,ETA,GAMMA,B0,L))     01570000
                 IF(I.EQ.2) F0=AIMAG(EVG(X0,Y,C,RHO,ETA,GAMMA,B0,L))    01580000
              ELSE                                                      01590000
                 IF(I.EQ.1) F0=REAL(EVG(Y,X0,C,RHO,ETA,GAMMA,B0,L))     01600000
                 IF(I.EQ.2) F0=AIMAG(EVG(Y,X0,C,RHO,ETA,GAMMA,B0,L))    01610000
              ENDIF                                                     01620000
              IF(XC .EQ. X0 ) GOTO 3040                                 01630000
              XC=X0                                                     01640000
              IF ( (F0.LT.0. .AND. F1.LT.0.) .OR.                       01650000
     +             (F0.GT.0. .AND. F1.GT.0.) ) THEN                     01660000
                 X1=X0                                                  01670000
                 F1=F0                                                  01680000
              ELSE                                                      01690000
                 X2=X0                                                  01700000
                 F2=F0                                                  01710000
              ENDIF                                                     01720000
              GOTO 3030                                                 01730000
           ELSE                                                         01740000
              X1=X2                                                     01750000
              F1=F2                                                     01760000
              GOTO 3020                                                 01770000
           ENDIF                                                        01780000
 3040      IF (X0.GT.ZEND) GOTO 3010                                    01790000
           IF (I.EQ.0) THEN                                             01800000
              B0=X0                                                     01810000
              CALL MOVABS(0,500*K1)                                     01820000
              CALL ANMODE                                               01830000
 2120         FORMAT ('  B0 =',F10.5)                                   01840000
              WRITE ( 6,2120) B0                                        01850000
              GOTO 3010                                                 01860000
           ENDIF                                                        01870000
C                                                                       01880000
           IF (J. EQ. 1) THEN                                           01890000
              CALL PPOI99(X0, Y,0.,3601)                                01900000
              CALL PPOI99(X0,-Y,0.,3601)                                01910000
              IF (I.EQ.1) CALL PPOI99(X0, Y,(XMAX-XMIN)/400.,900)       01920000
              IF (I.EQ.1) CALL PPOI99(X0,-Y,(XMAX-XMIN)/400.,900)       01930000
           ELSE                                                         01940000
              CALL PPOI99(Y, X0,0.,3601)                                01950000
              CALL PPOI99(Y,-X0,0.,3601)                                01960000
              IF (I.EQ.1) CALL PPOI99(Y, X0,(XMAX-XMIN)/400.,900)       01970000
              IF (I.EQ.1) CALL PPOI99(Y,-X0,(XMAX-XMIN)/400.,900)       01980000
           ENDIF                                                        01990000
C                                                                       02000000
           X1=X0+ZSW/10.                                                02010000
           IF (Y. EQ. 0.) X1=X0+ZSW*5.                                  02020000
           GOTO 3019                                                    02030000
 3010    CONTINUE                                                       02040000
 3000 CONTINUE                                                          02050000
C                                                                       02060000
      CALL VCURSR (ICH,XDUMMY,YDUMMY)                                   02070000
C     A=65 , B=66 , C=67 , ....                                         02080000
C     L=76 , M=77 , N=78 , ....                                         02090000
C     P=80 ---- PARAMETER AENDERN                                       02100000
      IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 5555                      02110000
      IF (ICH .EQ. 76 .OR. ICH .EQ. 108) L=L+1                          02120000
      IF (ICH .EQ. 76 .OR. ICH .EQ. 108) GOTO 6666                      02130000
      END                                                               02140000
C                                                                       02150000
C                                                                       02160000
      REAL FUNCTION SSG(X,C)                                            02170000
      REAL*4 X,C,I0,I1                                                  02180000
      INTEGER ICON                                                      02190000
      CALL BI1(X,I1,ICON)                                               02200000
      CALL BI0(X,I0,ICON)                                               02210000
      SSG=C* I1/I0 - X/2                                                02220000
      RETURN                                                            02230000
      END                                                               02240000
C                                                                       02250000
      COMPLEX FUNCTION EVG(X,Y,C,RHO,ETA,GAMMA,B0,L)                    02260000
      COMPLEX LAMBS                                                     02270000
      COMPLEX KB,CI,JL2,JL,CH1,CH2,CH4                                  02280000
      REAL*4  X,Y,C,RHO,ETA,GAMMA,B0                                    02290000
      REAL*4  I1,I2,IL,IL1                                              02300000
      INTEGER L,ICON,N                                                  02310000
      LAMBS =CMPLX(X,Y)                                                 02320000
      CI    =CMPLX(0.,1.)                                               02330000
      KB    =CI * CSQRT( LAMBS )                                        02340000
      CH4   = CMPLX(1.,0.)                                              02350000
      DO 100 N=1,L,1                                                    02360000
         CH4 = CH4 * KB/CMPLX( 2.*FLOAT(L) , 0.)                        02370000
  100 CONTINUE                                                          02380000
      IF (SQRT(SQRT(X*X+Y*Y)) .LT. 1.E-02) THEN                         02390000
         CH1= CMPLX( FLOAT(L) ,0.)                                      02400000
         CH2= CMPLX( FLOAT(2*L*(L*L-1)) ,0.)                            02410000
      ELSE                                                              02420000
         CALL CBJN(KB,L+2,JL2,ICON)                                     02430000
         CALL CBJN(KB,L  ,JL ,ICON)                                     02440000
         CH1=( CMPLX(FLOAT(L),0.)*JL-JL2 ) / CH4                        02450000
         CH2=( CMPLX(FLOAT(2*L*(L*L-1)),0.) * (JL-JL2)) / CH4           02460000
         CH2=CH2-CH1*KB*KB                                              02470000
      ENDIF                                                             02480000
C                                                                       02490000
      CALL BIN(B0,L+1,IL1,ICON)                                         02500000
      CALL BIN(B0,L  ,IL ,ICON)                                         02510000
      CALL BIN(B0,2  ,I2 ,ICON)                                         02520000
      CALL BIN(B0,1  ,I1 ,ICON)                                         02530000
C                                                                       02540000
      EVG= +CMPLX( (RHO-C) * ETA/RHO/B0/B0  , 0.) * KB*KB   *CH2        02550000
     +     +CMPLX( B0 * (I2/I1-IL1/IL)/2.  ,0.)             *CH2        02560000
     +     -CMPLX( GAMMA*RHO* FLOAT(L*(L*L-1)) /B0/ETA ,0.) *CH1        02570000
      RETURN                                                            02580000
      END                                                               02590000
C                                                                       02600000
      SUBROUTINE PPOI99(X,Y,PS,SW)                                      02610000
      REAL*4 X,Y,PS,H1,PI                                               02620000
      INTEGER SW,L                                                      02630000
      PI= 4.  *ATAN(1.  )                                               02640000
      CALL MOVEA (X+PS,Y)                                               02650000
      DO 100 L=0,3600,SW                                                02660000
         H1=FLOAT(L)*2.                                                 02670000
         CALL DRAWA (X+PS*COS(PI*H1/3600.),Y+PS*SIN(PI*H1/3600.))       02680000
  100 CONTINUE                                                          02690000
      CALL MOVEA (X,Y)                                                  02700000
      RETURN                                                            02710000
      END                                                               02720000
