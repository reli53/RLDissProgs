C     EIGENWERT-GLEICHUNG FUER MODELL A87                               00010010
C     MIT EULERGLEICHUNG (OHNE ZAEHIGKEIT)                              00020010
C                                                                       00030010
C     2-DIM (UM STATIONAEREN KREIS MIT RADIUS B0)                       00040005
C     ----> NUR REELLE EIGENWERTE (IN ABHAENGIGKEIT VON GAMMA)          00050003
C     LAMBS  :=  (1   / DK )  * LAMBDA *B0*B0                           00060004
C                                                                       00070003
      REAL*4    EVG                                                     00080003
      REAL*4    DK     ,C,B0                                            00090006
      REAL*4    YMIN,YMAX,XMIN,XMAX                                     00100003
      REAL*4    IMIN,IMAX,ISTEP                                         00110003
      REAL*4    XC,X0,X1,X2,F0,F1,F2,ZST,ZSW,ZEND,X,Y,SSG               00120000
      INTEGER   I,K1,L                                                  00130003
      CHARACTER SCR*1                                                   00140003
      C=2.                                                              00150003
      DK =1.                                                            00160004
      L=2                                                               00170003
      XMIN=   0.0                                                       00180007
      XMAX=   0.3                                                       00190009
      YMIN= -100.                                                       00200008
      YMAX=  100.                                                       00210008
C                                                                       00220003
C     PI=4.  * ATAN(1.  )                                               00230003
C                                                                       00240003
C                                                                       00250003
      CALL INITT (960)                                                  00260003
      CALL WRITEK (40,'WHICH TEKTRONIX-TERMINAL IS USED ????')          00270003
      CALL NEWLIN                                                       00280003
      CALL WRITEK (40,'ENTER:  DEFAULT   IF TEKTONIX 4012')             00290003
      CALL NEWLIN                                                       00300003
      CALL WRITEK (40,'              2   IF TEKTONIX 4014')             00310000
      CALL NEWLIN                                                       00320003
      READ ( 5,'(A1)') SCR                                              00330003
      CALL SETBUF (3)                                                   00340003
      IF (SCR .EQ. '2') THEN                                            00350003
         K1=4                                                           00360003
         CALL TERM (3,4096)                                             00370003
      ELSE                                                              00380003
         K1=1                                                           00390003
         CALL TERM (3,1024)                                             00400003
      END IF                                                            00410000
C                                                                       00420003
 5555 CALL ERASE                                                        00430003
      CALL MOVABS(0,730*K1)                                             00440003
      CALL ANMODE                                                       00450003
 2001 FORMAT (1X,'   XMIN =',F10.5,'   XMAX =',F10.5)                   00460003
 2000 FORMAT (1X,'   YMIN =',F10.2,'   YMAX =',F10.2)                   00470003
 2002 FORMAT (1X,'      C =',F10.5,'     DK =',F10.5)                   00480004
 2005 FORMAT (1X,'      L =  ',I3)                                      00490003
      WRITE ( 6,2001) XMIN,XMAX                                         00500000
      WRITE ( 6,2000) YMIN,YMAX                                         00510003
      WRITE ( 6,2002) C,DK                                              00520004
      WRITE ( 6,2005) L                                                 00530003
      WRITE ( 6,*) ' '                                                  00540003
      WRITE ( 6,*) ' DEFAULT = NO CHANGE '                              00550003
      SCR=' '                                                           00560003
      READ  ( 5,'(A1)') SCR                                             00570003
      IF (SCR .NE. ' ') THEN                                            00580003
         WRITE ( 6, *) ' '                                              00590000
         WRITE ( 6, *) '  7 WERTE DURCH KOMMATA GETRENNT '              00600003
         READ ( 5,*) XMIN,XMAX,YMIN,YMAX,C,DK     ,L                    00610004
         GOTO 5555                                                      00620003
      ENDIF                                                             00630004
C                                                                       00640003
 6666 CONTINUE                                                          00650005
      CALL ERASE                                                        00660003
      CALL TWINDO (250*K1,950*K1, 50*K1,750*K1)                         00670003
      CALL DWINDO ( XMIN,XMAX,YMIN ,YMAX)                               00680003
      CALL MOVEA  ( XMIN,YMIN )                                         00690003
      CALL DRAWA  ( XMAX,YMIN )                                         00700000
      CALL DRAWA  ( XMAX,YMAX )                                         00710003
      CALL DRAWA  ( XMIN,YMAX )                                         00720003
      CALL DRAWA  ( XMIN,YMIN )                                         00730003
      CALL MOVABS (0,730*K1)                                            00740003
      CALL ANMODE                                                       00750003
 2100 FORMAT ('   C =',F10.5)                                           00760003
 2101 FORMAT ('  DK =',F10.5)                                           00770004
C2103 FORMAT (' GAM =',F10.5)                                           00780003
 2104 FORMAT ('   L =  ',I3)                                            00790000
 2109 FORMAT (' XMIN =',F9.5)                                           00800003
 2110 FORMAT (' XMAX =',F9.5)                                           00810003
 2111 FORMAT (' YMIN =',F9.2)                                           00820003
 2112 FORMAT (' YMAX =',F9.2)                                           00830003
      WRITE ( 6,2109) XMIN                                              00840003
      WRITE ( 6,2110) XMAX                                              00850003
      WRITE ( 6,2111) YMIN                                              00860003
      WRITE ( 6,2112) YMAX                                              00870003
      WRITE ( 6,2100) C                                                 00880003
      WRITE ( 6,2101) DK                                                00890004
C     WRITE ( 6,2103) GAMMA                                             00900003
      WRITE ( 6,2104) L                                                 00910003
C                                                                       00920003
C                                                                       00930003
      CALL MOVEA (XMIN,0.)                                              00940003
      CALL DRAWA (XMAX,0.)                                              00950003
      CALL MOVEA (0.,YMIN)                                              00960003
      CALL DRAWA (0.,YMAX)                                              00970003
      DO 3000 I=0,2,1                                                   00980007
         IF (I .EQ.0) THEN                                              00990003
            IMIN=1.                                                     01000003
            IMAX=IMIN                                                   01010003
            ISTEP=1.                                                    01020003
            IMAX=IMIN                                                   01030003
            ZST=.0001                                                   01040003
            ZSW=.2                                                      01050003
            ZEND=1000.                                                  01060003
         ENDIF                                                          01070004
         IF (I .EQ. 1) THEN                                             01080000
            IMIN=XMIN                                                   01090003
            IMAX=XMAX                                                   01100003
            ISTEP=(XMAX-XMIN)/200.                                      01110009
C           ZST=  YMIN *ABS(YMIN)                                       01120007
C           ZEND= YMAX *ABS(YMAX)                                       01130007
            ZST=  YMIN                                                  01140007
            ZEND= YMAX                                                  01150007
            ZSW= (ZEND-ZST)/100.                                        01160007
         ENDIF                                                          01170004
         IF (I .EQ. 2) THEN                                             01180003
C           IMIN=YMIN *ABS(YMIN)                                        01190007
C           IMAX=YMAX *ABS(YMAX)                                        01200007
            IMIN=YMIN                                                   01210007
            IMAX=YMAX                                                   01220007
            ISTEP=(IMAX-IMIN)/1000.                                     01230003
            ZST= XMIN                                                   01240003
            ZEND=XMAX                                                   01250003
            ZSW= (XMAX-XMIN)/10.                                        01260009
         ENDIF                                                          01270003
C                                                                       01280003
         DO 3010 X=IMIN,IMAX,ISTEP                                      01290003
            X1=ZST                                                      01300003
 3019       IF(I.EQ.0) F1=SSG(X1,C)                                     01310003
            IF(I.EQ.1) F1=     EVG(X1,     DK     ,X    ,B0,L)          01320004
            IF(I.EQ.2) F1=     EVG(X ,     DK     ,X1   ,B0,L)          01330004
 3020       X2=X1+ZSW                                                   01340003
            IF(I.EQ.0) F2=SSG(X2,C)                                     01350003
            IF(I.EQ.1) F2=     EVG(X2,     DK     ,X    ,B0,L)          01360004
            IF(I.EQ.2) F2=     EVG(X ,     DK     ,X2   ,B0,L)          01370004
            IF(X1.GT.ZEND) GOTO 3010                                    01380003
            IF ( (F1.EQ.0.) .OR. (F1.GT.0. .AND. F2.LT.0.) .OR.         01390003
     +           (F1.LT.0. .AND. F2.GT.0.) ) THEN                       01400003
C           NULLSTELLENSUCHE NACH REGULA-FALSI                          01410003
              XC=X1                                                     01420000
              X0=X1                                                     01430003
              IF (F1. EQ. 0.) GOTO 3040                                 01440003
 3030         X0=X1-F1*(X2-X1)/(F2-F1)                                  01450003
              IF(I.EQ.0) F0=SSG(X0,C)                                   01460003
              IF(I.EQ.1) F0=     EVG(X0,     DK     ,X    ,B0,L)        01470004
              IF(I.EQ.2) F0=     EVG(X ,     DK     ,X0   ,B0,L)        01480004
              IF(XC.EQ.X0              ) GOTO 3040                      01490003
              XC=X0                                                     01500003
              IF ( (F0.LT.0. .AND. F1.LT.0.) .OR.                       01510003
     +             (F0.GT.0. .AND. F1.GT.0.) ) THEN                     01520000
                 X1=X0                                                  01530003
                 F1=F0                                                  01540003
              ELSE                                                      01550003
                 X2=X0                                                  01560003
                 F2=F0                                                  01570003
              ENDIF                                                     01580004
              GOTO 3030                                                 01590003
           ELSE                                                         01600003
              X1=X2                                                     01610003
              F1=F2                                                     01620000
              GOTO 3020                                                 01630003
           ENDIF                                                        01640003
 3040      IF (X0.GT.ZEND) GOTO 3010                                    01650003
           IF (I.EQ.0) THEN                                             01660003
              B0=X0                                                     01670003
              CALL MOVABS(0,500*K1)                                     01680003
              CALL ANMODE                                               01690003
 2120         FORMAT ('  B0 =',F10.5)                                   01700003
              WRITE ( 6,2120) B0                                        01710003
              GOTO 3010                                                 01720000
           ENDIF                                                        01730003
C                                                                       01740003
           IF (I .EQ. 1) THEN                                           01750003
C             Y=SQRT(ABS(X0))                                           01760007
C             IF (X0 .LT. 0. ) Y=-Y                                     01770007
              Y=X0                                                      01780007
              CALL PPOI99(X , Y, 0. ,3601)                              01790003
           ENDIF                                                        01800004
           IF (I .EQ. 2) THEN                                           01810003
C             Y=SQRT(ABS(X ))                                           01820007
C             IF (X  .LT. 0. ) Y=-Y                                     01830007
              Y=X                                                       01840007
              CALL PPOI99(X0, Y, 0. ,3601)                              01850003
           ENDIF                                                        01860003
C                                                                       01870003
           X1=X0+ZSW/10.                                                01880003
           GOTO 3019                                                    01890003
 3010    CONTINUE                                                       01900003
 3000 CONTINUE                                                          01910003
C                                                                       01920003
      CALL VCURSR (ICH,XDUMMY,YDUMMY)                                   01930003
C     A=65 , B=66 , C=67 , ....                                         01940000
C     L=76 , M=77 , N=78 , ....                                         01950003
C     P=80 ---- PARAMETER AENDERN                                       01960003
C     L=76 ---- NAECHSTES L                                             01970005
      IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 5555                      01980003
      IF (ICH .EQ. 76 .OR. ICH .EQ. 108) L=L+1                          01990005
      IF (ICH .EQ. 76 .OR. ICH .EQ. 108) GOTO 6666                      02000005
      END                                                               02010003
C                                                                       02020003
C                                                                       02030003
      REAL FUNCTION SSG(X,C)                                            02040003
      REAL*4 X,C,I0,I1                                                  02050003
      INTEGER ICON                                                      02060003
      CALL BI1(X,I1,ICON)                                               02070000
      CALL BI0(X,I0,ICON)                                               02080003
      SSG=C* I1/I0 - X/2                                                02090003
      RETURN                                                            02100003
      END                                                               02110003
C                                                                       02120003
      REAL FUNCTION EVG(LAMBS,    DK     ,GAMMA,B0,L)                   02130004
      REAL*4  LAMBS ,WLAMB                                              02140003
      REAL*4  JL1,JL, H1, H2 ,H4                                        02150003
      REAL*4    DK     ,GAMMA,B0                                        02160004
      REAL*4  I1,I2,IL,IL1                                              02170003
      INTEGER L,ICON,N                                                  02180003
      WLAMB = SQRT(ABS(B0*B0+LAMBS))                                    02190003
      H4=1.                                                             02200003
      DO 100 N=1,L,1                                                    02210003
         H4=H4*WLAMB/FLOAT(N)/2.                                        02220003
  100 CONTINUE                                                          02230003
C                                                                       02240003
      IF (WLAMB .LT. 1.E-2) THEN                                        02250003
         H1=1.                                                          02260003
         H2=1./FLOAT(2*(L+1))                                           02270003
      ELSE                                                              02280003
         IF (LAMBS .GE. -B0*B0) THEN                                    02290003
            CALL BIN( WLAMB ,L+1, IL1,ICON)                             02300003
            CALL BIN( WLAMB ,L  , IL ,ICON)                             02310003
            H1=  IL / H4                                                02320003
            H2= IL1    /H4/WLAMB                                        02330003
         ELSE                                                           02340003
            CALL BJN( WLAMB ,L+1,JL1,ICON)                              02350003
            CALL BJN( WLAMB ,L  ,JL ,ICON)                              02360000
            H1=   JL  / H4                                              02370003
            H2=    JL1 / H4 /WLAMB                                      02380003
         ENDIF                                                          02390004
      ENDIF                                                             02400004
C                                                                       02410003
      CALL BIN(B0,2  ,I2 ,ICON)                                         02420003
      CALL BIN(B0,1  ,I1 ,ICON)                                         02430000
C                                                                       02440003
      IF (L .GE. 2) THEN                                                02450006
         EVG= - LAMBS*LAMBS*DK*DK/B0/B0/B0/B0         * H1              02460006
     +        + LAMBS*DK/B0/B0  * .5*B0* (I2/I1* H1 - B0*H2)            02470006
     +        -  GAMMA*  FLOAT(L*(L*L-1)) /B0/B0/B0   * H1              02480006
      ELSE                                                              02490006
         EVG= - LAMBS      *DK   /B0/B0               * H1              02500006
     +        +                   .5*B0* (I2/I1* H1 - B0*H2)            02510006
      ENDIF                                                             02520006
C 111 FORMAT (1X,5(E12.5,2X))                                           02530007
C     WRITE (6,111) WLAMB,LAMBS,H1,H2,EVG                               02540007
      RETURN                                                            02550003
      END                                                               02560003
C                                                                       02570003
      SUBROUTINE PPOI99(X,Y,PS,SW)                                      02580000
      REAL*4 X,Y,PS,H1,PI                                               02590003
      INTEGER SW,L                                                      02600003
      PI= 4.  *ATAN(1.  )                                               02610003
      CALL MOVEA (X+PS,Y)                                               02620003
      DO 100 L=0,3600,SW                                                02630003
         H1=FLOAT(L)*2.                                                 02640003
         CALL DRAWA (X+PS*COS(PI*H1/3600.),Y+PS*SIN(PI*H1/3600.))       02650005
  100 CONTINUE                                                          02660003
      CALL MOVEA (X,Y)                                                  02670003
      RETURN                                                            02680000
      END                                                               02690003
