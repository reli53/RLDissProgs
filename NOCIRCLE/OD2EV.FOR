C/D14KS    JOB RACF,CLASS=G,                                            00010000
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020000
C/E2          EXEC FORTCLG,REGION.FORT=1024K,REGION.GO=2048K            00030000
C/FORT.SYSIN    DD *                                                    00040000
C                                                                       00050000
C                                                                       00060000
C                                                                       00070000
C       ##################################################              00080000
C       ####     EIGENWERTE DES LOKALEN               ####              00090000
C       ####       OBERFLAECHEN-DYNAMIK-MODELLS (2D)  ####              00100000
C       ####     V(K)=(KAPPA-K0) - (KAPPA-K0)**3      ####              00110000
C       ####     (POT = 1)                            ####              00120000
C       ####     BESETZUNG DER MATRIZEN MIT           ####              00130000
C       ####              DFCOST  UND   DFSINT        ####              00140000
C       ####     EIGENWERTBERECHNUNG MIT              ####              00150000
C       ####              DEIG1                       ####              00160000
C       ####                                          ####              00170000
C       ####   NAME DES PROGRAMMES :  NOCIRCLE(OD2EV) ####              00180000
C       ##################################################              00190000
C                                                                       00200000
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00210000
C                                                                       00220000
C                                                                       00230000
        INTEGER           I,J,K,POT,NCOUNT,SD                           00240000
        INTEGER           L,N,M,N9,N8,N7,NN                             00250000
        INTEGER           ICON,MODE,CPUSEC                              00260000
        DOUBLE PRECISION  B8,B9,X9,X1                                   00270000
        DOUBLE PRECISION  INT,DELK,XU,XO                                00280000
        DOUBLE PRECISION  K0MIN,GAMMA,B,DELK0M                          00290000
        DOUBLE PRECISION  XCORR                                         00300000
        DOUBLE PRECISION  PI,H1,H2,H3,H4                                00310000
        DOUBLE PRECISION  B0,B1,B2,B3,B4                                00320000
        DOUBLE PRECISION  G0,G1,G2,      A0,A1,A2                       00330000
        DOUBLE PRECISION  WG0,  G02,G03,G05                             00340000
        DOUBLE PRECISION  C1,C2,C3,C4,C5,C6                             00350000
        DOUBLE PRECISION  D0,D1,D2,D3,D4,   E0,E1,E2                    00360000
        DOUBLE PRECISION  VS,K00                                        00370000
        DOUBLE PRECISION  SQGA ,SQGAA,SQGAE,SQGAS                       00380000
C                                                                       00390000
        DOUBLE PRECISION  K0(0:2048),S(0:2048)                          00400000
        DOUBLE PRECISION  X(0:2048),Y(0:2048)                           00410000
        DOUBLE PRECISION  THETA(0:2048),FI(0:2048),R0(0:2048)           00420000
        DOUBLE PRECISION  FIC(-1:2049),RC(-1:2049)                      00430000
        DOUBLE PRECISION  R1(0:2048),R2(0:2048)                         00440000
        DOUBLE PRECISION  R3(0:2048),R4(0:2048)                         00450000
        DOUBLE PRECISION  F0(0:2048),F1(0:2048)                         00460000
        DOUBLE PRECISION  F2(0:2048),F3(0:2048)                         00470000
        DOUBLE PRECISION  F4(0:2048),DFI(1:2048)                        00480000
        DOUBLE PRECISION  MC(0:100,0:100),MS(0:100,0:100)               00490000
C                                                                       00500000
        DOUBLE PRECISION  ACTR(2049),ASTR(2048),TAB(1023)               00510000
C                                                                       00520000
        DOUBLE PRECISION  A(  100,  100)                                00530000
        DOUBLE PRECISION  EV(  100,  100)                               00540000
        DOUBLE PRECISION  EI(  100),ER(  100)                           00550000
        DOUBLE PRECISION  VW(  100)                                     00560000
C                                                                       00570000
        DOUBLE PRECISION  ECSORT(  100), ESSORT(  100)                  00580000
        CHARACTER         FNW*30                                        00590000
C                                                                       00600000
        DOUBLE PRECISION  FNF1,FNF2                                     00610000
        DOUBLE PRECISION  FNVS1,FNVS2                                   00620000
C                                                                       00630000
        FNF1(A1,B1,X1)=1.0D0/DSQRT(1.0D0-4.0D0*A1*B1*DSIN(X1)           00640000
     *          *DSIN(X1)/(A1+B1)/(A1+B1))                              00650000
        FNF2(A1,B1,X1)=1.0D0/DSQRT(1.0D0-B1*B1/(A1*A1+B1*B1)            00660000
     *          *DSIN(X1)*DSIN(X1))                                     00670000
C                                                                       00680000
        FNVS1(B9,X9)=-3.0D0*(X9-1.0D0/B9)*(X9-1.0D0/B9)+1.0D0           00690000
        FNVS2(B9,X9)=+3.0D0*(X9-1.0D0/B9)*(X9-1.0D0/B9)+1.0D0           00700000
C                                                                       00710000
C                                                                       00720000
C       **************************************************              00730000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00740000
C       **************************************************              00750000
C                                                                       00760000
                 FNW   = 'D14K.TES0.DATA'                               00770000
                 B     =        1.000000D0                              00780000
                 POT   =        1                                       00790000
                 L     =        2                                       00800000
                 M     =        8                                       00810000
                 SD    =        0                                       00820000
                 SD    =        1                                       00830000
                 NN    =        2**10                                   00840000
                 N     =       200                                      00850000
                 N8    =        54                                      00860000
                 SQGAA =  1.0000D0                                      00870000
                 SQGAE =  6.0000D0                                      00880000
                 SQGAS =  0.02500D0 - 1.0D-16                           00890000
C                N8 = MATRIZENGROESSE (SOLLTE >= 36 SEIN)               00900000
C                SD=0 : REDUZ. AUSGABE AUF BILDSCHIRM                   00910000
C                L    = ANZAHL DER PERIODEN                             00920000
C                N    = ANZAHL DER STUETZPUNKTE F. 1/2 PERIODE          00930000
C                NN   = ANZAHL DER STUETZPUNKTE FUER DIE HALBE          00940000
C                       KURVE ( FUER DFCOST  UND  DFSINT)               00950000
C                       (MUSS DIE GESTALT 2**K HABEN)                   00960000
C                N9 =  ANZAHL DER STUETZPKTE FUER L PERIODEN            00970000
C                N9 =  2*L*N MUSS <= 2048 = 2**10 SEIN                  00980000
C                NN   MUSS <= 2048 = 2**10 SEIN                         00990000
C                M = ANZ. DER INTEGRATIONSPUNKTE VON STUETZPUNKT        01000000
C                    ZU STUETZPUNKT (M>=1)                              01010000
C                GAMMA = KOEFFIZIENT DER OBERFLAECHENSPANNUNG           01020000
C                B = STEADY-STATE RADIUS                                01030000
C                POT = 1 :  V(X) = (X-1/B) -(X-1/B)**3                  01040000
C                POT = 2 :  V(X) = (X-1/B) +(X-1/B)**3                  01050000
C                K0MIN = MINIMALES KAPPA (WIRD BERECHNET)               01060000
C        CPU-ZEITEN-TABELLE FUER VERSCHIEDENE PARAMETER                 01070000
C               (IN SEKUNDEN FUER EINEN SCHLEIFENDURCHLAUF)             01080000
C       ..... N9 .... NN .... N8 .... L ... DIALOG ... BATCH .....      01090000
C        2*2*200    1024      36      2     CA. 10         7            01100000
C        2*2*400    1024      36      2     CA. 17         ?            01110000
C        2*2*512    1024      50      2     CA.  ?         ?            01120000
C        3*2*300    1024      54      3     CA. 17        12            01130000
C        4*2*200    1024      72      4     CA. 15         ?            01140000
C                                                                       01150000
C                                                                       01160000
C                                                                       01170000
C       **********************************************************      01180000
C       *** ANFANG DER GAMMA-SCHLEIFE                          ***      01190000
C       **********************************************************      01200000
C                                                                       01210000
        PI=4.0D0*DATAN(1.D0)                                            01220000
        NCOUNT=0                                                        01230000
        OPEN(22,FILE=FNW)                                               01240000
        REWIND 22                                                       01250000
        WRITE (22,50) NCOUNT                                            01260000
C       >>>> NCOUNT IST DER EIGENWERT-ZAEHLER.                          01270000
C                                                                       01280000
      DO 9000 SQGA = SQGAA , SQGAE , SQGAS                              01290000
         GAMMA=1.D0/SQGA/SQGA                                           01300000
         L=0                                                            01310000
         IF (L .EQ. 2) THEN                                             01320000
            NN = 2**10                                                  01330000
            N  = 200                                                    01340000
            N8 =  36                                                    01350000
         ENDIF                                                          01360000
         IF (L .EQ. 3) THEN                                             01370000
            NN = 2**10                                                  01380000
            N  = 300                                                    01390000
            N8 =  54                                                    01400000
         ENDIF                                                          01410000
         IF (L .EQ. 4) THEN                                             01420000
            NN = 2**10                                                  01430000
            N  = 200                                                    01440000
            N8 =  72                                                    01450000
         ENDIF                                                          01460000
         N9 = 2*N*L                                                     01470000
         H1=L                                                           01480000
         IF (SQGA .LE. H1 .OR. L .EQ. 0) L=0                            01490000
C                                                                       01500000
C       **********************************************************      01510000
C       *** SUCHE VON K0MIN, SO DASS DIE KURVE GESCHLOSSEN IST ***      01520000
C       **********************************************************      01530000
C                                                                       01540000
                                                                        01550000
      IF (L .EQ. 0) THEN                                                01560000
         L=1                                                            01570000
         N8=36                                                          01580000
         N=2**6                                                         01590000
         N9=2*L*N                                                       01600000
         NN=  L*N                                                       01610000
         K0MIN=0.D0                                                     01620000
         H2=N9                                                          01630000
         DO 111 I=0,N9                                                  01640000
            R0(I)=B                                                     01650000
            H1=I                                                        01660000
            FI(I)=2.D0*PI*H1/H2                                         01670000
  111    CONTINUE                                                       01680000
         GOTO 5000                                                      01690000
      ENDIF                                                             01700000
C                                                                       01710000
      K0MIN =-.00000001D0                                               01720000
      DELK0M=-.99999998D0                                               01730000
      H1=L                                                              01740000
      B8=DSQRT(GAMMA)*H1                                                01750000
      IF ((B8 .GE. B .AND. POT .EQ. 1) .OR.                             01760000
     *    (B8 .LT. B .AND. POT .EQ. 2)) GOTO 4000                       01770000
 1000 CONTINUE                                                          01780000
      K0MIN=K0MIN+DELK0M                                                01790000
C                                                                       01800000
        K0(0) = K0MIN                                                   01810000
        K0(N) =-K0MIN                                                   01820000
        S(0)  = 0.0D0                                                   01830000
        H1    = N                                                       01840000
        DO 100 I=1,N-1                                                  01850000
           H2=I                                                         01860000
C          K0(I) = K0MIN - 2.D0*K0MIN*H2/H1                             01870000
           K0(I) = - DSIN((-1.0D0+2.0D0*H2/H1)*PI*.5D0)*K0MIN           01880000
  100   CONTINUE                                                        01890000
C                                                                       01900000
        IF (POT .EQ. 1) THEN                                            01910000
           A1=DSQRT(2.0D0-K0MIN*K0MIN)                                  01920000
           B1=-K0MIN                                                    01930000
        ENDIF                                                           01940000
        IF (POT .EQ. 2) THEN                                            01950000
           A1=DSQRT(2.0D0+K0MIN*K0MIN)                                  01960000
           B1=-K0MIN                                                    01970000
        ENDIF                                                           01980000
        INT=0.0D0                                                       01990000
C                                                                       02000000
        H1=M                                                            02010000
        DO 110 I=1,N                                                    02020000
           IF (POT .EQ. 1) THEN                                         02030000
           XU=DASIN(DSQRT((A1+B1)*(K0(I-1)+B1)/(B1+B1)/(K0(I-1)+A1)))   02040000
           XO=DASIN(DSQRT((A1+B1)*(K0(I  )+B1)/(B1+B1)/(K0(I  )+A1)))   02050000
           ENDIF                                                        02060000
           IF (POT .EQ. 2) THEN                                         02070000
              XU=PI/2 - DASIN(K0(I-1)/K0(0))                            02080000
              XO=PI/2 - DASIN(K0(I  )/K0(0))                            02090000
           ENDIF                                                        02100000
           DELK=(XO-XU)/H1                                              02110000
           DO 120 J=1,M                                                 02120000
              H2=J                                                      02130000
              IF (POT .EQ. 1) THEN                                      02140000
                INT=INT+DELK*(FNF1(A1,B1,XU+(H2-1.0D0)*DELK)+FNF1(A1,   02150000
     *            B1,XU+H2*DELK)+4.0D0*FNF1(A1,B1,XU+(H2-.5D0)*DELK))   02160000
              ENDIF                                                     02170000
              IF (POT .EQ. 2) THEN                                      02180000
                INT=INT+DELK*(FNF2(A1,B1,XU+(H2-1.0D0)*DELK)+FNF2(A1,   02190000
     *            B1,XU+H2*DELK)+4.0D0*FNF2(A1,B1,XU+(H2-.5D0)*DELK))   02200000
           ENDIF                                                        02210000
  120      CONTINUE                                                     02220000
           S(I)=INT/6.D0                                                02230000
           IF (POT .EQ. 1) THEN                                         02240000
              S(I)=2.0D0*DSQRT(2.0D0*GAMMA)*S(I)/(A1+B1)                02250000
           ENDIF                                                        02260000
           IF (POT .EQ. 2) THEN                                         02270000
              S(I)=S(I)*DSQRT(2.0D0*GAMMA)/DSQRT(A1*A1+B1*B1)           02280000
           ENDIF                                                        02290000
  110   CONTINUE                                                        02300000
C                                                                       02310000
        H1=L                                                            02320000
        B9=H1*S(N)/PI                                                   02330000
        IF ((B9 .GT. B .AND. B8 .LT. B) .OR. (B9 .LT. B .AND.           02340000
     *       B8 . GT. B)) DELK0M=-DELK0M*.5D0                           02350000
       IF (DABS(B9-B ) .LT. 1.D-10 .OR. B9 .EQ. B8) GOTO 2000           02360000
       B8=B9                                                            02370000
       GOTO 1000                                                        02380000
C                                                                       02390000
C       ******************************************                      02400000
C       ***   BERECHNUNG VON KAPPA(S)          ***                      02410000
C       ******************************************                      02420000
C                                                                       02430000
 2000   CONTINUE                                                        02440000
        DO 130 I=1,N                                                    02450000
           K0(I+N)=K0(N-I)                                              02460000
           S(I+N) =S(N)+S(I)                                            02470000
  130   CONTINUE                                                        02480000
C                                                                       02490000
        DO 140 J=2,L                                                    02500000
           DO 140 I=1, N+N                                              02510000
              K0((J-1)*(N+N)+I) = K0(I)                                 02520000
              S ((J-1)*(N+N)+I) = S ((J-1)*(N+N)) + S(I)                02530000
  140   CONTINUE                                                        02540000
C                                                                       02550000
C       >>>>> BIS HIER WAR BEI K0(I) DIE SCHWAECHSTE KRUEMMUNG          02560000
C       >>>>> AB  HIER IST BEI K0(I) DIE STAERKSTE   KRUEMMUNG          02570000
        DO 150 I=0,N9                                                   02580000
           K0(I)=-K0(I)+1.0D0/B                                         02590000
  150   CONTINUE                                                        02600000
C                                                                       02610000
C                                                                       02620000
C       **************************************************              02630000
C       ****   BERECHNUNG VON X(S),Y(S)               ****              02640000
C       **************************************************              02650000
C                                                                       02660000
C       X(0)=Y(0)=0.0    AN DIESEM PUNKT IST DIE KRUEMMUNG MAXIMAL.     02670000
C       X(N9)=X(0)       Y(N9)=Y(0)                                     02680000
C       ALLE ANDEREN X-WERTE SIND < 0 .                                 02690000
C       DIE KURVE WIRD ENTGEGENGESETZT DEM UHRZEIGERSINN DURCH-         02700000
C       LAUFEN.                                                         02710000
C       MIT XCORR WIRD DIE KURVE ZENTRIERT.                             02720000
C                                                                       02730000
        THETA(0) = 0.0D0                                                02740000
        DO 200 I=1,N9                                                   02750000
           THETA(I)=THETA(I-1) + (S(I)-S(I-1))*(K0(I-1)+K0(I))*.5D0     02760000
  200   CONTINUE                                                        02770000
C                                                                       02780000
        X(0) = 0.0D0                                                    02790000
        Y(0) = 0.0D0                                                    02800000
        DO 210 I=1,N9                                                   02810000
           H1=THETA(I-1)                                                02820000
           H2=THETA(I)                                                  02830000
           X(I)=X(I-1)-(S(I)-S(I-1))*(DSIN(H2)+DSIN(H1))*.5D0           02840000
           Y(I)=Y(I-1)+(S(I)-S(I-1))*(DCOS(H2)+DCOS(H1))*.5D0           02850000
  210   CONTINUE                                                        02860000
        X(N9) = 0.0D0                                                   02870000
        Y(N9) = 0.0D0                                                   02880000
C                                                                       02890000
        H1=X(N+N)                                                       02900000
        H2=Y(N+N)                                                       02910000
        XCORR=-.5D0*(H1*H1+H2*H2)/H1                                    02920000
        DO 220 I=0,N9                                                   02930000
           X(I) = X(I) + XCORR                                          02940000
  220   CONTINUE                                                        02950000
C                                                                       02960000
C       **************************************************              02970000
C       ****     BERECHNEN VON RADIUS(FI)             ****              02980000
C       **************************************************              02990000
C                                                                       03000000
        DO 300 I=0,N9                                                   03010000
           R0(I)=DSQRT(X(I)*X(I)+Y(I)*Y(I))                             03020000
  300   CONTINUE                                                        03030000
C                                                                       03040000
        FI(0) = 0.0D0                                                   03050000
        FI(N9) = 2.0D0*PI                                               03060000
        FI(L*N) = PI                                                    03070000
        DO 310 I=1,L*N-1                                                03080000
           FI(I)=DATAN2(Y(I),X(I))                                      03090000
  310   CONTINUE                                                        03100000
        DO 320 I=L*N+1,N9                                               03110000
           FI(I)=2.0D0*PI + DATAN2(Y(I),X(I))                           03120000
  320   CONTINUE                                                        03130000
C                                                                       03140000
C       **************************************************              03150000
C       ****   NEUDISKRETISIERUNG (AEQUIDIST. IN FI)  ****              03160000
C       ****   NN=2**10=1024 , NN=2**9=512,......     ****              03170000
C       ****   NN=ANZ. DER STUETZPKTE F. HALBE KURVE  ****              03180000
C       **************************************************              03190000
C                                                                       03200000
C       >>>> UMSCHAUFELN VON R0,FI NACH RC,FIC.                         03210000
        DO 400 I=0,N9                                                   03220000
           RC(I)=R0(I)                                                  03230000
           FIC(I)=FI(I)                                                 03240000
  400   CONTINUE                                                        03250000
        RC(-1)=R0(N9-1)                                                 03260000
        FIC(-1)=-FIC(1)                                                 03270000
        RC(N9+1)=R0(1)                                                  03280000
        FIC(N9+1)=2.0D0*PI+FIC(1)                                       03290000
C                                                                       03300000
C       >>>> NEUDISKRETISIERTE WERTE FUER R0 BEI AEQUIDISTANTEM FI.     03310000
        H1=NN*2                                                         03320000
        DO 402 I=0,NN*2                                                 03330000
           H2=I                                                         03340000
           FI(I)=2.0D0*PI*H2/H1                                         03350000
  402   CONTINUE                                                        03360000
C                                                                       03370000
        R0(0)=RC(0)                                                     03380000
        R0(2*NN)=RC(N9)                                                 03390000
        J=0                                                             03400000
        DO 410 I=1,NN*2-1                                               03410000
  420      IF (FIC(J) .LT. FI(I) .AND. FIC(J+1) .GE. FI(I)) THEN        03420000
              CALL NEUDIS(RC(J-1),RC(J),RC(J+1),RC(J+2),FIC(J-1),       03430000
     *               FIC(J),FIC(J+1),FIC(J+2),R0(I),FI(I))              03440000
           ELSE                                                         03450000
              J=J+1                                                     03460000
              GOTO 420                                                  03470000
           END IF                                                       03480000
  410   CONTINUE                                                        03490000
C                                                                       03500000
 5000   CONTINUE                                                        03510000
C                                                                       03520000
C       ##################################################              03530000
C       ####     BERECHNUNG DER MATRIZEN  BZGL.       ####              03540000
C       ####     DER COSINUS UND SINUS-MODEN          ####              03550000
C       ####     (UM STEADY-STATE R0(FI))             ####              03560000
C       ##################################################              03570000
C                                                                       03580000
C       N8 = MATRIZENGROESSE (SOLLTE >= 36 SEIN)                        03590000
C       MC(0:N8,0:N8)                                                   03600000
C       MS(0:N8,0:N8)                                                   03610000
C                                                                       03620000
C                                                                       03630000
C                                                                       03640000
C                L = MODE                                               03650000
C                NN = ANZAHL DER STUETZPUNKTE FUER 1/2-PERIODE          03660000
C                GAMMA = KOEFFIZIENT DER OBERFLAECHENSPANNUBG           03670000
C                B = STEADY-STATE RADIUS                                03680000
C                K0MIN= MINIMALES KAPPA                                 03690000
C                                                                       03700000
C       *************************************************               03710000
C       **** BERECHNEN DER 1. BIS 4. ABL. VON R0(FI) ****               03720000
C       *************************************************               03730000
C                                                                       03740000
        DO 500 I=1,NN*2                                                 03750000
           DFI(I)=FI(I)-FI(I-1)                                         03760000
  500   CONTINUE                                                        03770000
C                                                                       03780000
        CALL ABLEI(R0,DFI,NN*2,R1)                                      03790000
        CALL ABLEI(R1,DFI,NN*2,R2)                                      03800000
        CALL ABLEI(R2,DFI,NN*2,R3)                                      03810000
        CALL ABLEI(R3,DFI,NN*2,R4)                                      03820000
C                                                                       03830000
C       **************************************************              03840000
C       **** BERECHNEN DER FUNKT. F0(FI) BIS F4(FI)   ****              03850000
C       **************************************************              03860000
C                                                                       03870000
        DO 510 I=0,NN*2                                                 03880000
           B0=R0(I)                                                     03890000
           B1=R1(I)                                                     03900000
           B2=R2(I)                                                     03910000
           B3=R3(I)                                                     03920000
           B4=R4(I)                                                     03930000
           G0=B0*B0+B1*B1                                               03940000
           G1=2.D0*B1*(B0+B2)                                           03950000
           G2=2.D0*B2*(B0+B2)+2.D0*B1*(B1+B3)                           03960000
           A0=B0*B0+2.D0*B1*B1-B0*B2                                    03970000
           A1=2.D0*B0*B1+3.D0*B1*B2-B0*B3                               03980000
           A2=B2*(2.D0*B0+3.D0*B2)+2.D0*B1*(B1+B3)-B0*B4                03990000
           WG0=DSQRT(G0)                                                04000000
           G02=G0*G0                                                    04010000
           G03=G0*G02                                                   04020000
           G05=G02*G03                                                  04030000
           C1=(49.*G0*G1*A1+21.*G0*G2*A0-10.*G0*G0*A2-81.*G1*G1*A0)/    04040000
     *          4./G05                                                  04050000
           C2=(9.*G0*G1*A0-3.5D0*G0*G0*A1)/G05                          04060000
           C3=-1.5D0*A0/G03                                             04070000
           C4=(9.*G0*G1*G1-3.*G0*G0*G2)/G05/2.                          04080000
           C5=-3.5D0*G1/G03                                             04090000
           C6=1.0D0/G02                                                 04100000
           K00=A0/(WG0*WG0*WG0)                                         04110000
           D0=2.*C1*B0+2.*C2*B1+2.*C3*B2+C4*(2.*B0-B2)+C5*(2.*B1-B3)    04120000
     *          +C6*(2.*B2-B4)                                          04130000
           D1=C1*2.*B1+C2*2.*(B0+B2)+C3*2.*(2.*B1+B3)+C4*4.*B1          04140000
     *          +C5*(2.*B0+3.*B2)+C6*(4.*B1+2.*B3)                      04150000
           D2=C2*2.*B1+C3*2.*(B0+2.*B2)-C4*B0+C5*3.*B1+C6*(2.*B0+6.*B2) 04160000
           D3=C3*2.*B1-C5*B0+C6*2.*B1                                   04170000
           D4=-C6*B0                                                    04180000
           IF (POT .EQ. 1) THEN                                         04190000
              VS=FNVS1(B,K00)/G02                                       04200000
           ENDIF                                                        04210000
           IF (POT .EQ. 2) THEN                                         04220000
              VS=FNVS2(B,K00)/G02                                       04230000
           ENDIF                                                        04240000
           E0=VS*(G0*(2.*B0-B2)-3.*A0*B0)                               04250000
           E1=VS*(G0*4.*B1-3.*A0*B1)                                    04260000
           E2=-VS*G0*B0                                                 04270000
           F0(I)=(E0+GAMMA*D0)/B0                                       04280000
           F1(I)=(E1+GAMMA*D1)/B0                                       04290000
           F2(I)=(E2+GAMMA*D2)/B0                                       04300000
           F3(I)=    GAMMA*D3 /B0                                       04310000
           F4(I)=    GAMMA*D4 /B0                                       04320000
  510   CONTINUE                                                        04330000
C                                                                       04340000
C       **************************************************              04350000
C       **** BERECHNEN DER MATR. MC FUER COSINUS-MODE ****              04360000
C       **************************************************              04370000
C                                                                       04380000
        H1= NN                                                          04390000
        DO 550 J=0,N8                                                   04400000
           H3=J                                                         04410000
           H4=H3*H3                                                     04420000
           DO 560 K=0,NN                                                04430000
             ACTR(K+1)=( (F0(K)-H4*F2(K)+H4*H4*F4(K))*DCOS(H3*FI(K))    04440000
     *                  +H3*(H4*F3(K)-F1(K))*DSIN(H3*FI(K)) )           04450000
  560      CONTINUE                                                     04460000
           CALL DFCOST(ACTR,NN,TAB,ICON)                                04470000
           DO 570 I=0,N8                                                04480000
              MC(I,J)=ACTR(I+1)*2.D0 / H1                               04490000
  570      CONTINUE                                                     04500000
           MC(0,J)=MC(0,J)*.5D0                                         04510000
  550   CONTINUE                                                        04520000
C                                                                       04530000
C                                                                       04540000
C       **************************************************              04550000
C       **** BERECHNEN DER MATR. MS FUER   SINUS-MODE ****              04560000
C       **************************************************              04570000
C                                                                       04580000
        DO 600 J=0,N8                                                   04590000
           H3=J                                                         04600000
           H4=H3*H3                                                     04610000
           DO 610 K=0,NN - 1                                            04620000
             ASTR(K+1)=( (F0(K)-H4*F2(K)+H4*H4*F4(K))*DSIN(H3*FI(K))    04630000
     *                  -H3*(H4*F3(K)-F1(K))*DCOS(H3*FI(K)) )           04640000
  610      CONTINUE                                                     04650000
           CALL DFSINT(ASTR,NN,TAB,ICON)                                04660000
           DO 620 I=0,N8                                                04670000
              MS(I,J)=ASTR(I+1)*2.D0 / H1                               04680000
C             WRITE (6,*) 'MS(',I,J,')= ',MS(I,J)                       04690000
  620      CONTINUE                                                     04700000
  600   CONTINUE                                                        04710000
C                                                                       04720000
C                                                                       04730000
C       ##################################################              04740000
C       ####          EIGENWERTE BERECHNEN            ####              04750000
C       ##################################################              04760000
C                                                                       04770000
C                                                                       04780000
C                                                                       04790000
C       **************************************************              04800000
C       ****      PARAMETER PARAMETER PARAMETER       ****              04810000
C       **************************************************              04820000
C                                                                       04830000
        K=100                                                           04840000
        MODE = 1                                                        04850000
C                                                                       04860000
C       CALL DEIG1(A,K,N,MODE,ER,EI,EV,VW,ICON)                         04870000
C                                                                       04880000
C       K        : K MUSS DIE DEFINIERTE DIMENSION DER MATRIZEN SEIN    04890000
C                : (ALSO STETS N<=K)                                    04900000
C       N        : GROESSE DER MATRIX , MIT DER GERECHNET WIRD          04910000
C       MODE = 1 : AUSBALANZIEREN DER MATRIX , D.H. ABSOLUTE SUMME      04920000
C                  UEBER I-TE ZEILE = ABSL. SUMME UEBER I-TE SPALTE     04930000
C       MODE<>1  : KEIN AUSBALANZIEREN                                  04940000
C       A(K,K)   : A = MATRIX, BZGL. DER DIE EIGENWERTE BERECHNET       04950000
C                  WERDEN. ACHTUNG: MATRIX A WIRD VERAENDERT !!!!       04960000
C       ER(K)    : REALTEILE DER EIGENWERTE                             04970000
C       EI(K)    : IMAGINAERTEILE DER EIGENWERTE                        04980000
C       EV(K,K)  : EV(.,I) IST EIGENVEKTOR ZUM I-TEN EIGENWERT          04990000
C       VW(K)    : ARBEITSVEKTOR FUER EIG1(.....)                       05000000
C       ICON     : KONTROLLPARAMETER FUER EIG1(.....)                   05010000
C                : ICON = 0     NO ERROR                                05020000
C                : ICON = 10000 N=1                                     05030000
C                : ICON = 20000 KEINE REDUKT. AUF DREIECKSMATR. MOEGL.  05040000
C                : ICON = 30000 N<1 ODER K<N                            05050000
C                                                                       05060000
C                                                                       05070000
C       ***************************************************             05080000
C       ****  BERECHNEN UND SORTIEREN DER EIGENWERTE   ****             05090000
C       ***************************************************             05100000
C                                                                       05110000
C                                                                       05120000
        DO 720 I=0,N8                                                   05130000
        DO 720 J=0,N8                                                   05140000
           A(I+1,J+1)=MC(I,J)                                           05150000
  720   CONTINUE                                                        05160000
C                                                                       05170000
        N7 = N8+1                                                       05180000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    05190000
        CALL SORTIE(ER,ECSORT,N7)                                       05200000
C                                                                       05210000
        DO 740 I=1,N8                                                   05220000
        DO 740 J=1,N8                                                   05230000
           A(I,J) = MS(I,J)                                             05240000
  740   CONTINUE                                                        05250000
C                                                                       05260000
        N7 = N8                                                         05270000
        CALL DEIG1(A,100,N7  ,MODE,ER,EI,EV,VW,ICON)                    05280000
        CALL SORTIE(ER,ESSORT,N7)                                       05290000
C                                                                       05300000
   60   FORMAT (1X,'  L = ',I4,' NN*2=',I4,'  GAMMA = ',F10.5,          05310000
     *                                     '  K0MIN = ',F10.5)          05320000
   61   FORMAT (1X,'  CPU-ZEIT = ',I7,' 1/SQR(GA)=',F 9.4)              05330000
   62   FORMAT (1X,I4,2(1X,F12.5))                                      05340000
   50   FORMAT (1X,I8)                                                  05350000
   52   FORMAT (5(1X,D23.14))                                           05360000
        IF (L .EQ. 1)  L=0                                              05370000
        CALL CLOCK(CPUSEC)                                              05380000
        WRITE ( 6,60) L,NN*2,GAMMA,K0MIN                                05390000
        WRITE ( 6,61) CPUSEC,1.D0/DSQRT(GAMMA)                          05400000
        IF (SD .EQ. 1) THEN                                             05410000
           WRITE ( 6,62) 0,ECSORT(1)                                    05420000
           DO 770 I=1 ,6,1                                              05430000
              WRITE ( 6,62) I,ECSORT(I+1),ESSORT(I)                     05440000
  770      CONTINUE                                                     05450000
        ENDIF                                                           05460000
C                                                                       05470000
        DO 780 I=0,6                                                    05480000
           NCOUNT=NCOUNT+1                                              05490000
           WRITE (22,52) 1.D0/ DSQRT(GAMMA) ,ECSORT(I+1)                05500000
  780   CONTINUE                                                        05510000
        DO 790 I=1,6                                                    05520000
           NCOUNT=NCOUNT+1                                              05530000
           WRITE (22,52) 1.D0/ DSQRT(GAMMA) ,ESSORT(I)                  05540000
  790   CONTINUE                                                        05550000
C                                                                       05560000
C                                                                       05570000
 4000   CONTINUE                                                        05580000
C                                                                       05590000
 9000 CONTINUE                                                          05600000
C                                                                       05610000
C                                                                       05620000
C       *******************************************************         05630000
C       ****  SCHLIESSEN DER DATEI + BEENDEN DES PROGRAMMS ****         05640000
C       *******************************************************         05650000
C                                                                       05660000
C                                                                       05670000
      CLOSE (22,STATUS='KEEP')                                          05680000
      END                                                               05690000
C                                                                       05700000
C       **************************************************              05710000
C       ****      SUBROUTINES                         ****              05720000
C       **************************************************              05730000
C                                                                       05740000
        SUBROUTINE NEUDIS(F1,F2,F3,F4,X1,X2,X3,X4,F,X)                  05750000
C       APPROXIMIEREN DES FUNKTIONSWERTES AN DER STELLE X               05760000
C         MIT X1 < X2 <= X <= X3 < X4 DURCH DAS MITTELN                 05770000
C         UEBER ZWEI PARABELN                                           05780000
C         F = RETURNVARIABLE                                            05790000
        DOUBLE PRECISION F1,F2,F3,F4,X1,X2,X3,X4,F,X                    05800000
        DOUBLE PRECISION A1,A2,B1,B2,C1,C2                              05810000
        DOUBLE PRECISION D1,D2,D3                                       05820000
        D1=X2-X1                                                        05830000
        D2=X3-X2                                                        05840000
        D3=X4-X3                                                        05850000
        A1=((F3-F2)/D2+(F1-F2)/D1)/(D1+D2)                              05860000
        B1=(F2-F1)/D1+A1*D1-A1*X2*2.0D0                                 05870000
        C1=F2-B1*X2-A1*X2*X2                                            05880000
        A2=((F4-F3)/D3+(F2-F3)/D2)/(D2+D3)                              05890000
        B2=(F3-F2)/D2+A2*D2-A2*X3*2.0D0                                 05900000
        C2=F3-B2*X3-A2*X3*X3                                            05910000
        F=( (A1*X*X+B1*X+C1)*(X3-X)+(A2*X*X+B2*X+C2)*(X-X2)) / D2       05920000
        RETURN                                                          05930000
        END                                                             05940000
C                                                                       05950000
C       **************************************************              05960000
C                                                                       05970000
        SUBROUTINE ABLEI(FU,DELX,N7,FUA)                                05980000
C       >>>> BILDEN DER 1. ABLEITUNG.                                   05990000
C       >>>>  INPUT : FU , DELX, N7  (WERDEN NICHT VERAENDERT)          06000000
C       >>>>  OUTPUT: FUA                                               06010000
        DOUBLE PRECISION FU(0:2048),DELX(1:2048),FUA(0:2048)            06020000
        DOUBLE PRECISION D1,D2                                          06030000
        INTEGER N7,I                                                    06040000
        DO 2000 I=1,N7-1                                                06050000
           D1=DELX(I)                                                   06060000
           D2=DELX(I+1)                                                 06070000
           FUA(I)=((FU(I+1)-FU(I))*D1/D2+(FU(I)-FU(I-1))*D2/D1)/(D1+D2) 06080000
 2000   CONTINUE                                                        06090000
        D1=DELX(N7)                                                     06100000
        D2=DELX(1)                                                      06110000
        FUA(N7)=((FU(1)-FU(N7))*D1/D2+(FU(N7)-FU(N7-1))*D2/D1)/(D1+D2)  06120000
        FUA(0) =FUA(N7)                                                 06130000
        RETURN                                                          06140000
        END                                                             06150000
C                                                                       06160000
C       **************************************************              06170000
C                                                                       06180000
        SUBROUTINE SORTIE(IN,OUT,N)                                     06190000
C       >>>> SORTIEREN DER GROESSE NACH.                                06200000
C       >>>> INPUT : IN , N  (WIRD NICHT VERAENDERT)                    06210000
C       >>>> OUTPUT: OUT                                                06220000
        DOUBLE PRECISION IN(  100), OUT( 100) ,H1                       06230000
        INTEGER N,I,J,K                                                 06240000
        IF (N .LE. 0) RETURN                                            06250000
        IF (N .EQ. 1) OUT(1)=IN(1)                                      06260000
        IF (N .EQ. 1) RETURN                                            06270000
C                                                                       06280000
C      **** UMSPEICHERN VON IN NACH OUT                                 06290000
        DO 1000 I=1,N                                                   06300000
           OUT(I)=IN(I)                                                 06310000
 1000   CONTINUE                                                        06320000
C                                                                       06330000
C      **** SORTIEREN DER GROESSE NACH                                  06340000
        DO 3000 J=1,N-1                                                 06350000
           H1=OUT(J)                                                    06360000
           K=J                                                          06370000
           DO 2000 I=J+1,N                                              06380000
              IF (OUT(I) .GT. H1) THEN                                  06390000
                  H1=OUT(I)                                             06400000
                  K =I                                                  06410000
              ENDIF                                                     06420000
 2000      CONTINUE                                                     06430000
           H1=OUT(J)                                                    06440000
           OUT(J)=OUT(K)                                                06450000
           OUT(K)=H1                                                    06460000
 3000   CONTINUE                                                        06470000
        RETURN                                                          06480000
C       END                                                             06490000
C/LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              06500000
C/              DD  DSN=SYS1.FORTLIB,DISP=SHR                           06510000
C/GO.SYSIN DD *                                                         06520000
C*                                                                      06530000
C*      EXEC DRUCKE                                                     06540000
C*      END                                                             06550000
        END                                                             06560000
