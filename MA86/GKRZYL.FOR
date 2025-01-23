C                                                                       00010014
C       ##################################################              00020014
C       ####   MODELL: MA86                           ####              00030014
C       ####   NAME DES PROGRAMMS:  MA86(GKRZYL)      ####              00040015
C       ####      LAPLACE(RHON) = RHON                ####              00050014
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00060014
C       ####      R.B. : RHON = C                     ####              00070014
C       ####           : PHIA = - GAMMA * KAPPA       ####              00080014
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00090014
C       ####      VA    =  GRAD( PHIA )               ####              00100014
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00110014
C       ##################################################              00120014
C        3 DIMENSIONAL UND UNENDLICH LANGER ZYLINDER                    00130014
C        AUSGERECHNET WIRD DER (VERMUTLICHE) KRITISCHE PARAMETER        00140014
C        UND ZUR SICHERHEIT WERDEN DIE MAXIMALEN EIGENWERTE             00150014
C        MLAMB BEI DER DAZUGEHOERNDEN WELLENZAHL MK BESTIMMT.           00160014
C                                                                       00170014
      DOUBLE PRECISION GKR,B0 ,K  ,LAMBDA ,MBQU                         00180014
      DOUBLE PRECISION MLAMB  ,MK ,KMIN   ,KMAX                         00190005
      DOUBLE PRECISION H0 ,H1 ,H2 ,H3 ,H9                               00200004
      INTEGER          L,IFNW                                           00210009
      CHARACTER        FNW*30                                           00220009
      FNW='D14K.TES6.DATA'                                              00230009
      IFNW=0                                                            00240009
   50 FORMAT (/,1X,'PROGRAMM: MA86(GKRZY) ')                            00250009
      WRITE ( 6,50)                                                     00260009
      IF (IFNW .EQ. 1) THEN                                             00270009
         OPEN (22,FILE=FNW)                                             00280009
         REWIND 22                                                      00290009
         WRITE (22,50)                                                  00300009
      ENDIF                                                             00310009
C  >>>IFNW = 1 : SCHREIBEN DER ERGEBNISSE IN DIE DATEI FNW              00320009
      DO 2200 B0=  5.D-01  ,  1.D+01 ,   5.D-01                         00330007
         H0 = MBQU(0,B0)                                                00340004
         H1 = MBQU(1,B0)                                                00350004
         GKR=B0*B0*B0*( (H1/H0 -1.D0)/4.D0 +1.D0/8.D0)                  00360004
         KMIN = 0.D0                                                    00370006
         KMAX = DMIN1( 100.D0/B0 , 100.D0) + KMIN                       00380005
C  >>>   GKR = VERMUTLICH KRITISCHES GAMMA , BESTIMMT AUS DER           00390009
C  >>>   GLEICHUNG (D(LAMBDA))**2/D(K**2) = 0 , K=0 , L = 1             00400009
         WRITE ( 6, 53  ) B0,GKR,KMAX                                   00410007
         IF (IFNW .EQ. 1) WRITE (22, 53  ) B0,GKR,KMAX                  00420009
   53    FORMAT (/,1X,'B0 = ',D14.7,'  GAMMA(KRITISCH) = ',D14.7        00430005
     *            ,'  KMAX = ',D14.7)                                   00440005
         DO 2220 L=0 , 5                                                00450007
C  >>>      MLAMB = MAXIMALES LAMBDA (ZUR MODE L )                      00460005
C  >>>      MK      IST DIE DAZUGEHOERENDEN WELLENZAHLEN K .            00470005
            MLAMB=-1.D+60                                               00480006
            MK   =-1.D+60                                               00490006
         DO 2100 K=KMIN ,KMAX , (KMAX-KMIN)/  200.D0                    00500007
            H9=DSQRT(1+K*K)                                             00510004
            H2 = MBQU(L  ,H9*B0)                                        00520004
            H3 = MBQU(L  , K*B0)                                        00530004
C           WRITE ( 6, *) K*B0 ,H3                                      00540004
            LAMBDA= K*B0* H3 * (   .5D0 - GKR/B0/B0/B0 *                00550004
     *          ( (L*L-1.D0) + K*K*B0*B0 )   )                          00560004
     *          - GKR/B0/B0/B0 * L * ( (L*L-1.D0) + K*K*B0*B0 )         00570004
     *          + B0/2 * (H1 - H9 * H2)                                 00580004
            MLAMB = DMAX1(MLAMB,LAMBDA)                                 00590004
C           IF (L .EQ. 1 .AND. K .LT. 5.D0) WRITE ( 6,*) K,LAMBDA       00600004
            IF (MLAMB .EQ. LAMBDA) THEN                                 00610004
               MK=DMAX1(K,MK)                                           00620004
            ENDIF                                                       00630001
 2100    CONTINUE                                                       00640000
         WRITE ( 6,55) L,MLAMB,MK                                       00650009
         IF (IFNW .EQ. 1) WRITE (22,55) L,MLAMB,MK                      00660009
   55    FORMAT (1X,'L = ',I3,'    MLAMB = ',D14.7,'    MK = ',D14.7)   00670009
 2220 CONTINUE                                                          00680007
 2200 CONTINUE                                                          00690007
      IF (IFNW .EQ. 1) CLOSE(22,STATUS='KEEP')                          00700009
      END                                                               00710000
C                                                                       00720004
      DOUBLE PRECISION FUNCTION MBESS(L,R)                              00730004
C     >>> COMPUTE THE MODIFIED BESSELFUNCTION OF THE 1. KIND            00740004
C     >>> ( OF INTEGER ORDER L >= 0)                                    00750004
                                                                        00760004
      DOUBLE PRECISION R,X7,X8,X9,XC,DK,DL                              00770004
      INTEGER          K,L                                              00780004
      DL=L                                                              00790004
      X9=1.D0                                                           00800004
      DO 10 K=1 ,L,1                                                    00810004
         DK=K                                                           00820004
         X9=X9*R/DK/2.D0                                                00830004
   10 CONTINUE                                                          00840004
      X8=1.D0                                                           00850004
      X7=1.D0                                                           00860004
      K=0                                                               00870004
   20 K=K+1                                                             00880004
      XC=X7                                                             00890004
      DK=K                                                              00900004
      X8=X8*.25D0*R*R/DK/(DK+DL)                                        00910004
      X7=X7+X8                                                          00920004
      IF (X7 .NE. XC) GOTO 20                                           00930004
      MBESS=X7*X9                                                       00940004
      RETURN                                                            00950004
      END                                                               00960004
C                                                                       00970004
      DOUBLE PRECISION FUNCTION MBQU(L,R)                               00980004
C     >>> COMPUTE THE QUOTIENT OF THE MODIFIED BESSELFUCTIONS           00990004
C     >>> OF THE 1. KIND ( ORDER L+1 / ORDER L) (L >=0)                 01000004
C     >>> DIE BERECHNUNG DES QUOTIENTEN MIT                             01010013
C     >>> MBESS(L+1,R)/MBESS(L,R) LIEFERT FUER KLEINE UND FUER          01020013
C     >>> GROSSE R ZU GROSSE FEHLER !!!!                                01030013
      DOUBLE PRECISION R,X8,X9,X1,X2,X7,XC,DK,DL                        01040007
      INTEGER          K,L                                              01050004
C     WRITE ( 6, *) '  R = ',R                                          01060012
      DL=L                                                              01070004
      MBQU=1.D0                                                         01080007
      X9=1.D0                                                           01090007
      X8=1.D0                                                           01100007
      X7=1.D0                                                           01110007
      K=0                                                               01120004
   20 K=K+1                                                             01130004
      XC=MBQU                                                           01140007
      DK=K                                                              01150004
      X1=R*R/4.D0/DK/(DK+DL+1.D0)                                       01160007
      X2=R*R/4.D0/DK/(DK+DL     )                                       01170007
      X7=X7*( 1.D0 + 1.D0/(DK+DL) )                                     01180007
      X8=X8*X1/(1.D0+X8*X1*X7)                                          01190007
      X9=X9*X2/(1.D0+X9*X2   )                                          01200007
      MBQU= X8  + MBQU * (1.D0 - X9)                                    01210007
C     WRITE ( 6, *) XC,MBQU-X8,MBQU                                     01220012
      IF (MBQU .NE. XC .OR. MBQU .NE. MBQU-X8) GOTO 20                  01230012
      IF (K .LT. 10) GOTO 20                                            01240010
      MBQU=MBQU * R /2.D0 /(DL+1.D0)                                    01250007
      RETURN                                                            01260004
      END                                                               01270004
