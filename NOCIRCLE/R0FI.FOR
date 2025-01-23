//D14KS    JOB RACF,CLASS=A,                                            00010048
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020048
//            EXEC FORTCLG,REGION.GO=1024K                              00030048
//FORT.SYSIN    DD *                                                    00040048
C                                                                       00050000
C                                                                       00060000
C                                                                       00070000
C       ##################################################              00080000
C       ####     BERECHNUNG EINES STEADY-STATES       ####              00090000
C       ####             KAPPA(S)                     ####              00100000
C       ####                                          ####              00110000
C       ####   NAME DES PROGRAMMES : NOCIRCLE(R0FI)   ####              00120000
C       ##################################################              00130000
C                                                                       00140000
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00150000
C                                                                       00160000
C                                                                       00170000
        INTEGER           I,J,POT                                       00180043
        INTEGER           L,N,M,N9,NDIS                                 00190011
        DOUBLE PRECISION  FNF1,FNF2,X1,A1,B1,B8,B9                      00200044
        DOUBLE PRECISION  PI,H1,H2,INT,DELK,XU,XO                       00210034
        DOUBLE PRECISION  K0(0:2000),S(0:2000)                          00220041
        DOUBLE PRECISION  K0MIN,GAMMA,B,DELK0M                          00230034
C                                                                       00240000
        DOUBLE PRECISION  X(0:2000),Y(0:2000)                           00250041
        DOUBLE PRECISION  THETA(0:2000),FI(0:2000),R0(0:2000)           00260041
        DOUBLE PRECISION  FIC(-1:2001),RC(-1:2001)                      00270041
        DOUBLE PRECISION  XCORR                                         00280007
        CHARACTER         FNW*30                                        00290026
C                                                                       00300000
        FNF1(A1,B1,X1)=1.0D0/DSQRT(1.0D0-4.0D0*A1*B1*DSIN(X1)           00310043
     *          *DSIN(X1)/(A1+B1)/(A1+B1))                              00320043
        FNF2(A1,B1,X1)=1.0D0/DSQRT(1.0D0-B1*B1/(A1*A1+B1*B1)            00330043
     *          *DSIN(X1)*DSIN(X1))                                     00340044
C                                                                       00350000
C       **************************************************              00360000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00370000
C       **************************************************              00380000
C                                                                       00390000
                 FNW   = 'D14K.R022.DATA'                               00400049
                 L     =        2                                       00410048
                 N     =      200                                       00420049
C        >>>>    L*N MUSS <= 1000 SEIN                                  00430041
                 GAMMA =         .125000D0                              00440049
                 B     =        1.000000D0                              00450034
                 POT   =        1                                       00460050
                 NDIS  =        1                                       00470024
                 M     =        8                                       00480041
                 N9    =    2*N*L                                       00490041
C                                                                       00500011
C                NDIS = 1 : NEUDISKRETISIERUNG                          00510011
C                L = MODE                                               00520000
C                N = ANZAHL DER STUETZPUNKTE FUER 1/2-PERIODE           00530000
C                M = ANZ. DER INTEGRATIONSPUNKTE VON STUETZPUNKT        00540000
C                    ZU STUETZPUNKT (M>=1)                              00550041
C                N9 = L*N*2 = ANZAHL DER STUETZPUNKTE FUER L-PERIODEN   00560000
C                GAMMA = KOEFFIZIENT DER OBERFLAECHENSPANNUNG           00570034
C                B = STEADY-STATE RADIUS                                00580034
C                K0MIN = MINIMALES KAPPA (WIRD BERECHNET)               00590034
C                POT = 1 :  V(X) = (X-1/B) -(X-1/B)**3                  00600043
C                POT = 2 :  V(X) = (X-1/B) +(X-1/B)**3                  00610043
C                                                                       00620000
                 H1=1.0D+00                                             00630032
                 PI=4.0D+00*DATAN(H1)                                   00640033
C                                                                       00650000
C       **********************************************************      00660034
C       *** SUCHE VON K0MIN, SO DASS DIE KURVE GESCHLOSSEN IST***       00670034
C       **********************************************************      00680034
C                                                                       00690000
      IF (L .EQ. 0) THEN                                                00700042
         L=1                                                            00710042
         N9=2*L*N                                                       00720042
         K0MIN=0.D0                                                     00730042
         H2=N9                                                          00740042
         DO 700 I=0,N9                                                  00750042
            R0(I)=B                                                     00760042
            H1=I                                                        00770042
            FI(I)=2.D0*PI*H1/H2                                         00780042
  700    CONTINUE                                                       00790042
         GOTO 5000                                                      00800042
      ENDIF                                                             00810042
C                                                                       00820042
      K0MIN =-.00000001D0                                               00830038
      DELK0M=-.99999998D0                                               00840038
      H1=L                                                              00850034
      B8=DSQRT(GAMMA)*H1                                                00860034
      IF ((B8 .GE. B .AND. POT .EQ. 1) .OR.                             00870046
     *    (B8 .LT. B .AND. POT .EQ. 2)) GOTO 9000                       00880046
 1000 CONTINUE                                                          00890034
      K0MIN=K0MIN+DELK0M                                                00900034
C                                                                       00910034
        K0(0) = K0MIN                                                   00920000
        K0(N) =-K0MIN                                                   00930030
        S(0)  = 0.0D0                                                   00940038
        H1    = N                                                       00950000
        DO 100 I=1,N-1                                                  00960030
           H2=I                                                         00970000
C          K0(I) = K0MIN - 2.D0*K0MIN*H2/H1                             00980034
           K0(I) = - DSIN((-1.0D0+2.0D0*H2/H1)*PI*.5D0)*K0MIN           00990034
  100   CONTINUE                                                        01000000
C                                                                       01010000
        IF (POT .EQ. 1) THEN                                            01020043
           A1=DSQRT(2.0D0-K0MIN*K0MIN)                                  01030043
           B1=-K0MIN                                                    01040043
        ENDIF                                                           01050043
        IF (POT .EQ. 2) THEN                                            01060043
           A1=DSQRT(2.0D0+K0MIN*K0MIN)                                  01070043
           B1=-K0MIN                                                    01080043
        ENDIF                                                           01090043
        INT=0.0D0                                                       01100034
C                                                                       01110000
        H1=M                                                            01120000
        DO 110 I=1,N                                                    01130000
           IF (POT .EQ. 1) THEN                                         01140043
           XU=DASIN(DSQRT((A1+B1)*(K0(I-1)+B1)/(B1+B1)/(K0(I-1)+A1)))   01150033
           XO=DASIN(DSQRT((A1+B1)*(K0(I  )+B1)/(B1+B1)/(K0(I  )+A1)))   01160033
           ENDIF                                                        01170043
           IF (POT .EQ. 2) THEN                                         01180043
              XU=PI/2 - DASIN(K0(I-1)/K0(0))                            01190045
              XO=PI/2 - DASIN(K0(I  )/K0(0))                            01200045
           ENDIF                                                        01210043
           DELK=(XO-XU)/H1                                              01220000
           DO 120 J=1,M                                                 01230000
              H2=J                                                      01240000
              IF (POT .EQ. 1) THEN                                      01250043
                INT=INT+DELK*(FNF1(A1,B1,XU+(H2-1.0D0)*DELK)+FNF1(A1,   01260043
     *            B1,XU+H2*DELK)+4.0D0*FNF1(A1,B1,XU+(H2-.5D0)*DELK))   01270043
              ENDIF                                                     01280043
              IF (POT .EQ. 2) THEN                                      01290043
                INT=INT+DELK*(FNF2(A1,B1,XU+(H2-1.0D0)*DELK)+FNF2(A1,   01300043
     *            B1,XU+H2*DELK)+4.0D0*FNF2(A1,B1,XU+(H2-.5D0)*DELK))   01310043
           ENDIF                                                        01320043
  120      CONTINUE                                                     01330000
           S(I)=INT/6.D0                                                01340043
           IF (POT .EQ. 1) THEN                                         01350043
              S(I)=2.0D+00*DSQRT(2.0D0*GAMMA)*S(I)/(A1+B1)              01360043
           ENDIF                                                        01370043
           IF (POT .EQ. 2) THEN                                         01380043
              S(I)=S(I)*DSQRT(2.0D0*GAMMA)/DSQRT(A1*A1+B1*B1)           01390043
           ENDIF                                                        01400043
  110   CONTINUE                                                        01410000
C                                                                       01420034
        H1=L                                                            01430034
        B9=H1*S(N)/PI                                                   01440034
        IF ((B9 .GT. B .AND. B8 .LT. B) .OR. (B9 .LT. B .AND.           01450034
     *       B8 . GT. B)) DELK0M=-DELK0M*.5D0                           01460034
       IF (DABS(B9-B ) .LT. 1.D-10 .OR. B9 .EQ. B8) GOTO 2000           01470039
       B8=B9                                                            01480039
       GOTO 1000                                                        01490039
C                                                                       01500034
C       ******************************************                      01510034
C       ***   BERECHNUNG VON KAPPA(S)          ***                      01520034
C       ******************************************                      01530034
C                                                                       01540000
 2000   CONTINUE                                                        01550040
        DO 130 I=1,N                                                    01560000
           K0(I+N)=K0(N-I)                                              01570000
           S(I+N) =S(N)+S(I)                                            01580000
  130   CONTINUE                                                        01590000
C                                                                       01600000
        DO 140 J=2,L                                                    01610000
           DO 140 I=1, N+N                                              01620000
              K0((J-1)*(N+N)+I) = K0(I)                                 01630000
              S ((J-1)*(N+N)+I) = S ((J-1)*(N+N)) + S(I)                01640000
  140   CONTINUE                                                        01650000
C                                                                       01660000
        DO 150 I=0,N9                                                   01670000
           K0(I)=-K0(I)+1.0D0/B                                         01680034
  150   CONTINUE                                                        01690000
C                                                                       01700000
C                                                                       01710000
C       **************************************************              01720000
C       ****   BERECHNUNG VON X(S),Y(S)               ****              01730000
C       **************************************************              01740000
C                                                                       01750000
C       X(0)=Y(0)=0.0    AN DIESEM PUNKT IST DIE KRUEMMUNG MAXIMAL.     01760000
C       X(N9)=X(0)       Y(N9)=Y(0)                                     01770000
C       ALLE ANDEREN X-WERTE SIND < 0 .                                 01780000
C       DIE KURVE WIRD ENTGEGENGESETZT DEM UHRZEIGERSINN DURCH-         01790000
C       LAUFEN. MIT XCORR WIRD DIE KURVE ZENTRIERT.                     01800000
C                                                                       01810000
        THETA(0) = 0.0D0                                                01820034
        DO 200 I=1,N9                                                   01830000
           THETA(I)=THETA(I-1) + (S(I)-S(I-1))*(K0(I-1)+K0(I))*.5D0     01840034
  200   CONTINUE                                                        01850000
C                                                                       01860000
        X(0) = 0.0D0                                                    01870034
        Y(0) = 0.0D0                                                    01880034
        DO 210 I=1,N9                                                   01890000
           H1=THETA(I-1)                                                01900000
           H2=THETA(I)                                                  01910000
           X(I)=X(I-1)-(S(I)-S(I-1))*(DSIN(H2)+DSIN(H1))*.5D0           01920034
           Y(I)=Y(I-1)+(S(I)-S(I-1))*(DCOS(H2)+DCOS(H1))*.5D0           01930034
  210   CONTINUE                                                        01940000
C                                                                       01950000
        H1=X(N+N)                                                       01960000
        H2=Y(N+N)                                                       01970000
        XCORR=-.5D0*(H1*H1+H2*H2)/H1                                    01980034
        DO 220 I=0,N9                                                   01990000
           X(I) = X(I) + XCORR                                          02000000
  220   CONTINUE                                                        02010000
C                                                                       02020000
C       **************************************************              02030000
C       ****     BERECHNEN VON RADIUS(FI)             ****              02040000
C       **************************************************              02050000
C                                                                       02060000
        DO 300 I=0,N9                                                   02070000
           R0(I)=DSQRT(X(I)*X(I)+Y(I)*Y(I))                             02080007
  300   CONTINUE                                                        02090000
C                                                                       02100000
        FI(0) = 0.0D0                                                   02110034
        FI(N9) = 2.0D0*PI                                               02120034
        FI(L*N) = PI                                                    02130000
        DO 310 I=1,L*N-1                                                02140000
           FI(I)=DATAN2(Y(I),X(I))                                      02150007
  310   CONTINUE                                                        02160000
        DO 320 I=L*N+1,N9                                               02170000
           FI(I)=2.0D0*PI + DATAN2(Y(I),X(I))                           02180034
  320   CONTINUE                                                        02190000
C                                                                       02200000
C       **************************************************              02210000
C       ****   NEUDISKRETISIERUNG                     ****              02220000
C       ****   AEQUIDISTANZ IN FI                     ****              02230000
C       **************************************************              02240000
C                                                                       02250000
        IF (NDIS .LT. 1 .OR. NDIS .GT. 1) GOTO 5000                     02260035
        H1=N9                                                           02270000
        DO 400 I=0,N9                                                   02280000
           H2=I                                                         02290000
           RC(I)=R0(I)                                                  02300000
           FIC(I)=FI(I)                                                 02310000
           FI(I)=2.0D0*PI*H2/H1                                         02320034
  400   CONTINUE                                                        02330000
        RC(-1)=R0(N9-1)                                                 02340000
        FIC(-1)=-FIC(1)                                                 02350000
        RC(N9+1)=R0(1)                                                  02360000
        FIC(N9+1)=2.0D0*PI+FIC(1)                                       02370034
C                                                                       02380000
        R0(0)=RC(0)                                                     02390000
        R0(N9)=RC(N9)                                                   02400000
        J=0                                                             02410000
        DO 410 I=1,N9-1                                                 02420000
  420      IF (FIC(J) .LT. FI(I) .AND. FIC(J+1) .GE. FI(I)) THEN        02430000
              CALL NEUDIS(RC(J-1),RC(J),RC(J+1),RC(J+2),FIC(J-1),       02440000
     *               FIC(J),FIC(J+1),FIC(J+2),R0(I),FI(I))              02450000
           ELSE                                                         02460000
              J=J+1                                                     02470000
              GOTO 420                                                  02480000
           END IF                                                       02490000
  410   CONTINUE                                                        02500000
C                                                                       02510000
C                                                                       02520000
C       **************************************************              02530000
C       ****      PRINTEN DER DATEN                   ****              02540000
C       **************************************************              02550000
C                                                                       02560000
 5000   CONTINUE                                                        02570035
        OPEN (22,FILE=FNW)                                              02580026
        REWIND 22                                                       02590026
   50   FORMAT(1X,'L     =',I6)                                         02600022
   51   FORMAT(1X,'GAMMA =',D23.16)                                     02610038
   52   FORMAT(1X,'B     =',D23.16)                                     02620038
   53   FORMAT(1X,'K0MIN =',D23.16)                                     02630038
   54   FORMAT(1X,'N     =',I6)                                         02640022
   59   FORMAT(1X,I6,1X,D23.16,1X,D23.16)                               02650038
        WRITE (22,50) L                                                 02660026
        WRITE (22,51) GAMMA                                             02670026
        WRITE (22,52) B                                                 02680026
        WRITE (22,53) K0MIN                                             02690026
        WRITE (22,54) N                                                 02700026
        DO 500 J=0 ,N9                                                  02710000
           WRITE (22,59) J,R0(J),FI(J)                                  02720026
  500   CONTINUE                                                        02730000
        CLOSE (22,STATUS='KEEP')                                        02740026
C       BACKSPACE 22                                                    02750026
C                                                                       02760000
 9000 CONTINUE                                                          02770035
      END                                                               02780035
C                                                                       02790000
C       **************************************************              02800000
C       ****      SUBROUTINES                         ****              02810000
C       **************************************************              02820000
C                                                                       02830000
        SUBROUTINE NEUDIS(F1,F2,F3,F4,X1,X2,X3,X4,F,X)                  02840000
C       APPROXIMIEREN DES FUNKTIONSWERTES AN DER STELLE X               02850000
C         MIT X1 < X2 <= X <= X3 < X4 DURCH DAS MITTELN                 02860000
C         UEBER ZWEI PARABELN                                           02870000
C         F = RETURNVARIABLE                                            02880000
        DOUBLE PRECISION F1,F2,F3,F4,X1,X2,X3,X4,F,X                    02890007
        DOUBLE PRECISION A1,A2,B1,B2,C1,C2                              02900007
        DOUBLE PRECISION D1,D2,D3                                       02910007
        D1=X2-X1                                                        02920000
        D2=X3-X2                                                        02930000
        D3=X4-X3                                                        02940000
        A1=((F3-F2)/D2+(F1-F2)/D1)/(D1+D2)                              02950000
        B1=(F2-F1)/D1+A1*D1-A1*X2*2.0D+00                               02960033
        C1=F2-B1*X2-A1*X2*X2                                            02970000
        A2=((F4-F3)/D3+(F2-F3)/D2)/(D2+D3)                              02980000
        B2=(F3-F2)/D2+A2*D2-A2*X3*2.0D+00                               02990033
        C2=F3-B2*X3-A2*X3*X3                                            03000000
        F=( (A1*X*X+B1*X+C1)*(X3-X)+(A2*X*X+B2*X+C2)*(X-X2)) / D2       03010000
        RETURN                                                          03020000
C                                                                       03030000
        END                                                             03040048
//GO.SYSIN DD *                                                         03050048
/*                                                                      03060048
/*      EXEC DRUCKE                                                     03070048
/*      END                                                             03080048
