C/D14KH    JOB RACF,CLASS=G,                                            00010000
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020000
C/E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030000
C/FORT.SYSIN    DD *                                                    00040000
C                                                                       00050000
C                                                                       00060001
C       ##################################################              00070001
C       ####   MODELL: MA86                           ####              00080001
C       ####   NAME DES PROGRAMMS:  MA86(XY23)        ####              00090001
C       ####      LAPLACE(RHON) = RHON                ####              00100001
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00110001
C       ####      R.B. : RHON = C                     ####              00120001
C       ####           : PHIA = - GAMMA * KAPPA       ####              00130001
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00140001
C       ####      VA    =  GRAD( PHIA )               ####              00150001
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00160001
C       ##################################################              00170001
C        3 DIMENSIONAL:  GERECHNET IN ZYLINDERKOORDINATEN (UNABHAENGIG  00180000
C                        VOM WINKEL FI) MIT SYMMETRIE BZGL. Z=0-EBENE   00190000
C        2 DIMENSIONAL:  GERECHNET IN KARTESISCHEN KOORDINATEN          00200000
C                        MIT SYMMETRIE BZGL. X=0-GERADEN                00210000
C        (GERECHNET WIRD AUF VIERTEL-KREIS.)                            00220000
C        VARIABLE OBERFLAECHENSPANNUNGS-KOEFFIZIENT MOEGLICH.           00230000
C        DIE DATEN WERDEN IN DER DATEI  FNW  ABGESPEICHERT.             00240000
C        GRAPHIK-PROGRAMM:       MA86(PIC2XY)                           00250002
C                                                                       00260000
        INTEGER           I9,I,N,ICNTL,COUNT,COUN9,CPRIN,N9(0:21)       00270000
        DOUBLE PRECISION  GAMMA,C,DELT,T9,TOP,TOPPKT,DIM                00280000
        DOUBLE PRECISION  B(0:21),BPKT(0:21),BSTRT(0:21),BST(0:12)      00290000
        DOUBLE PRECISION  BA1(0:21),BA2(0:21),G(0:21),WG(0:21)          00300000
        DOUBLE PRECISION  A1(0:21),A2(0:21),A3(0:21),A4(0:21),A5        00310000
        DOUBLE PRECISION  A9(0:21),KAPPA(0:21),GAM(0:21),KM(0:21)       00320000
        DOUBLE PRECISION  Z(0:21),DZ(0:21),DZ1(0:21),DZ2(0:21)          00330000
        DOUBLE PRECISION  R(0:21),DR(0:21),DR1(0:21),DR2(0:21)          00340000
        DOUBLE PRECISION  DZ1L(0:21),DZ2L(0:21),DZ1R(0:21),DZ2R(0:21)   00350000
        DOUBLE PRECISION  DR1L(0:21),DR2L(0:21),DR1R(0:21),DR2R(0:21)   00360000
        DOUBLE PRECISION  DRZLO(20,20),DRZLU(20,20),DRZRU(20,20)        00370000
        DOUBLE PRECISION  PI,H1,H2,H3,H4,NF1,NF2,NF0,TH1,TH2,TH0,THC    00380000
        DOUBLE PRECISION  FB(0:21)                                      00390000
C                                                                       00400000
        DOUBLE PRECISION  Z0(-1:21),R0(-1:21)                           00410000
        DOUBLE PRECISION  DS1L(0:21),DS1(0:21),DS1R(0:21),DS(0:21)      00420000
        DOUBLE PRECISION  DS2L(0:21),DS2(0:21),DS2R(0:21)               00430000
        DOUBLE PRECISION  Z1(-1:21),Z2(0:21),R1(-1:21),R2(0:21)         00440000
C                                                                       00450000
        DOUBLE PRECISION  RHON(161),GPOT(161)                           00460000
        DOUBLE PRECISION  GPAZ(161),GPAR(161)                           00470000
C                                                                       00480000
        DOUBLE PRECISION  A(161,161),AL(161,161),BL(161)                00490000
        DOUBLE PRECISION  VW(161),IP(161)                               00500000
        DOUBLE PRECISION  EPSZ,IS                                       00510000
        INTEGER           ICON,ISW                                      00520000
        CHARACTER         FNW*30                                        00530000
C                                                                       00540000
        DOUBLE PRECISION  F0(-1:21),F1(0:21),F2(0:21)                   00550000
        DOUBLE PRECISION  ZT,FNF0,FNP2                                  00560000
        FNP2(ZT)=(3.D0*ZT*ZT-1.D0) * .5D0                               00570000
        FNF0(ZT)=DSQRT(1.D0-ZT*ZT)                                      00580000
C                                                                       00590000
C       **************************************************              00600000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00610000
C       **************************************************              00620000
C                                                                       00630000
C    >>>>> DIM = 3.D0  ENTSPRICHT DEM 3 DIM. MODELL                     00640000
C    >>>>> DIM = 2.D0  ENTSPRICHT DEM 2 DIM. MODELL                     00650000
        FNW ='D14K.TES1.DATA'                                           00660000
        GAMMA     =     3.00000D0                                       00670000
C       GAMMA     =     1.00000D0                                       00680000
C       GAMMA     =     1.80000D0                                       00690000
        C         =     2.00000D0                                       00700000
        DIM       =     3.00000D0                                       00710000
        DELT      =     0.40960D0                                       00720000
        I9        =     8                                               00730000
        Z(0)      =       0.D-03                                        00740000
        R(0)      =       0.D-03                                        00750000
        Z(I9)     =    1000.D-03                                        00760000
        R(I9)     =    1000.D-03                                        00770000
C                                                                       00780000
        BST(0)    =     4.5000D0                                        00790000
C       BST(0)    =     3.2500D0                                        00800000
        BST(2)    =     0.0000D0                                        00810000
        CEND      = 40000                                               00820000
        CPRIN     =   500                                               00830000
        EPSZ      =     1.D-60                                          00840000
        ISW       =     1                                               00850000
C                                                                       00860000
C    >>>>> BERECHN. DER DISKRETIS. IN Z-RICHT. UND R-RICHTUNG           00870000
C    >>>>> HIER : AEQUIDISTANTE EINTEILUNG DES KREISBOGENS              00880000
        PI=4.0D0*DATAN(1.0D0)                                           00890000
        H1=I9                                                           00900000
        H1=PI/H1/2.D0                                                   00910000
        DO 101 I=1,I9-1,1                                               00920000
           H2=I9-I                                                      00930000
           Z(I)=DCOS(H1*H2)                                             00940000
  101   CONTINUE                                                        00950000
        DO 102 I=1,I9-1,   1                                            00960000
           R(I)=FNF0(Z(I9-I))                                           00970000
  102   CONTINUE                                                        00980000
C                                                                       00990000
C    >>>> BERECHNUNG DER STARTKURVE                                     01000000
        IF (DIM .EQ. 3.D0) BSTRT(0)=BST(0)+BST(2)*FNP2(0.D0)            01010000
        IF (DIM .EQ. 2.D0) BSTRT(0)=BST(0)+BST(2)*DCOS(PI)              01020000
        BSTRT(I9)=.0D0                                                  01030000
        IF (DIM .EQ. 3.D0) TOP=BST(0)+BST(2)*FNP2(1.D0)                 01040000
        IF (DIM .EQ. 2.D0) TOP=BST(0)+BST(2)*DCOS(0.D0)                 01050000
        DO 100 I=I9-1,1, -1                                             01060000
           TH1=0.D0                                                     01070000
           TH2=PI/2.D0                                                  01080000
           IF (DIM .EQ. 3.D0) THEN                                      01090000
              NF1=(BST(0)+BST(2)*FNP2(DCOS(TH1)))*DCOS(TH1)-Z(I)*TOP    01100000
              NF2=(BST(0)+BST(2)*FNP2(DCOS(TH2)))*DCOS(TH2)-Z(I)*TOP    01110000
           ENDIF                                                        01120000
           IF (DIM .EQ. 2.D0) THEN                                      01130000
              NF1=(BST(0)+BST(2)*DCOS(2.D0*TH1))*DCOS(TH1)-Z(I)*TOP     01140000
              NF2=(BST(0)+BST(2)*DCOS(2.D0*TH2))*DCOS(TH2)-Z(I)*TOP     01150000
           ENDIF                                                        01160000
           THC=TH1                                                      01170000
C          >>> NULLSTELLENSUCHE NACH REGULA-FALSI                       01180000
  105      TH0=TH1-NF1*(TH2-TH1)/(NF2-NF1)                              01190000
           IF (DIM .EQ. 3.D0) THEN                                      01200000
              NF0=(BST(0)+BST(2)*FNP2(DCOS(TH0)))*DCOS(TH0)-Z(I)*TOP    01210000
           ENDIF                                                        01220000
           IF (DIM .EQ. 2.D0) THEN                                      01230000
              NF0=(BST(0)+BST(2)*DCOS(2.D0*TH0))*DCOS(TH0)-Z(I)*TOP     01240000
           ENDIF                                                        01250000
           IF (THC .NE. TH0 .AND. NF0 .GT. 1.D-30) THEN                 01260000
              THC=TH0                                                   01270000
              IF (NF0*NF1 .GT. 0.D0) THEN                               01280000
                 TH1=TH0                                                01290000
                 NF1=NF0                                                01300000
              ELSE                                                      01310000
                 TH2=TH0                                                01320000
                 NF2=NF0                                                01330000
              ENDIF                                                     01340000
              GOTO 105                                                  01350000
           ELSE                                                         01360000
              IF (DIM .EQ. 3.D0) THEN                                   01370000
                 BSTRT(I)=(BST(0)+BST(2)*FNP2(DCOS(TH0)))*DSIN(TH0)     01380000
              ENDIF                                                     01390000
              IF (DIM .EQ. 2.D0) THEN                                   01400000
                 BSTRT(I)=(BST(0)+BST(2)*DCOS(2.D0*TH0))*DSIN(TH0)      01410000
              ENDIF                                                     01420000
           ENDIF                                                        01430000
  100   CONTINUE                                                        01440000
        DO 107 I=0,I9,1                                                 01450000
           GAM(I)=GAMMA                                                 01460000
  107   CONTINUE                                                        01470000
C                                                                       01480000
C    >>>> OPEN DATA-FILE UND PRINTEN DER PARAMETER                      01490000
        OPEN (22,FILE=FNW)                                              01500000
        REWIND 22                                                       01510000
C                                                                       01520000
   30   FORMAT (1X,'MA86(XY23)  2 DIM + 3 DIM MIT ROTATIONSSYMMETRIE')  01530000
   31   FORMAT (1X,'GAMMA = ',D23.16,'   DIM  = ',D23.16,'  C     = ',  01540000
     *          D23.16)                                                 01550000
   32   FORMAT (1X,'I9    = ',I3)                                       01560000
   35   FORMAT (1X,I3,4(2X,D23.16) )                                    01570000
        WRITE (22,30)                                                   01580000
        WRITE (22,31) GAMMA,DIM ,C                                      01590000
        WRITE (22,32) I9                                                01600000
        DO 128 I=1,  I9,1                                               01610000
           DZ(I)=Z(I)-Z(I-1)                                            01620000
           DR(I)=R(I)-R(I-1)                                            01630000
  128   CONTINUE                                                        01640000
        DZ(0)=DZ(1)                                                     01650000
        DR(0)=DR(1)                                                     01660000
        DO 130 I=0,  I9,1                                               01670000
           WRITE (22,35) I,Z(I),DZ(I),R(I),DR(I)                        01680000
  130   CONTINUE                                                        01690000
        DO 136 I=0,  I9-1,1                                             01700000
           H1=DZ(I)                                                     01710000
           H2=DZ(I+1)                                                   01720000
           DZ2L(I)= 2.D0/H1/(H1+H2)                                     01730000
           DZ2R(I)= 2.D0/H2/(H1+H2)                                     01740000
           DZ2 (I)=-(DZ2L(I)+DZ2R(I))                                   01750000
           DZ1R(I)= H1/H2/(H1+H2)                                       01760000
           DZ1L(I)=-H2/H1/(H1+H2)                                       01770000
           DZ1 (I)=-(DZ1L(I)+DZ1R(I))                                   01780000
           H1=DR(I)                                                     01790000
           H2=DR(I+1)                                                   01800000
           DR2L(I)= 2.D0/H1/(H1+H2)                                     01810000
           DR2R(I)= 2.D0/H2/(H1+H2)                                     01820000
           DR2 (I)=-(DR2L(I)+DR2R(I))                                   01830000
           DR1R(I)= H1/H2/(H1+H2)                                       01840000
           DR1L(I)=-H2/H1/(H1+H2)                                       01850000
           DR1 (I)=-(DR1L(I)+DR1R(I))                                   01860000
  136   CONTINUE                                                        01870000
C                                                                       01880000
C   >>> FUER D/DR(D/DZ(...)) 8-PUNKTE-APPROXIMATION                     01890000
        DO 140 I=1,  I9-2                                               01900000
        DO 140 N=1,I9-I-1                                               01910000
           H1=DZ(I)                                                     01920000
           H2=DZ(I+1)                                                   01930000
           H3=DR(N)                                                     01940000
           H4=DR(N+1)                                                   01950000
           DRZLO(I,N)=-H3/H4/H1/(H3+H4)                                 01960000
           DRZLU(I,N)=+(H2*H4-H1*H3)/H1/H3/(H1+H2)/(H3+H4)              01970000
           DRZRU(I,N)=-H1/H2/H3/(H1+H2)                                 01980000
  140   CONTINUE                                                        01990000
C                                                                       02000000
C    >>>>> N9(I9) = ANZAHL DER STUETZPUNKTE IM INNERN                   02010000
        N9(0)=0                                                         02020000
        DO 160 I=1,I9  ,1                                               02030000
           N9(I)=N9(I-1)+I9-I+1                                         02040000
  160   CONTINUE                                                        02050000
C                                                                       02060000
        DO 170 I=0,    I9   ,1                                          02070000
           B(I)=BSTRT(I)                                                02080000
  170   CONTINUE                                                        02090000
        ICNTL=0                                                         02100000
        COUNT=-1                                                        02110000
        COUN9=CPRIN-1                                                   02120000
        T9   =-DELT                                                     02130000
C                                                                       02140000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          02150000
C       >>>>         BEGINN DER ZEITSCHLEIFE              <<<<          02160000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          02170000
C                                                                       02180000
 2000   CONTINUE                                                        02190000
        COUNT=COUNT+1                                                   02200000
        COUN9=COUN9+1                                                   02210000
        T9   =T9   +DELT                                                02220000
C       ******************************************************          02230000
C       ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****          02240000
C       ******************************************************          02250000
C                                                                       02260000
        B(I9)=0.D0                                                      02270000
        BA1(0)=0.D0                                                     02280000
        BA2(0)=(B(1)-B(0))*2.D0/DZ(1)/DZ(1)                             02290000
        DO 220 I=1, I9-1 ,1                                             02300000
           BA1(I)= B(I-1)*DZ1L(I)+B(I)*DZ1(I)+B(I+1)*DZ1R(I)            02310000
           BA2(I)= B(I-1)*DZ2L(I)+B(I)*DZ2(I)+B(I+1)*DZ2R(I)            02320000
  220   CONTINUE                                                        02330000
C                                                                       02340000
        DO 228 I=0,I9-1                                                 02350000
           F0(I)  = FNF0(Z(I))                                          02360000
  228   CONTINUE                                                        02370000
C                                                                       02380000
        F0(I9)=0.D0                                                     02390000
        F1(0)=0.D0                                                      02400000
        F2(0)=(F0(1)-F0(0))*2.D0/DZ(1)/DZ(1)                            02410000
        DO 230 I=1,I9-1                                                 02420000
           F1(I)= DZ1L(I)*F0(I-1)+DZ1(I)*F0(I)+DZ1R(I)*F0(I+1)          02430000
           F2(I)= DZ2L(I)*F0(I-1)+DZ2(I)*F0(I)+DZ2R(I)*F0(I+1)          02440000
  230   CONTINUE                                                        02450000
C                                                                       02460000
        DO 232 I=0,I9-1                                                 02470000
           F1(I)= F1(I)/F0(I)-BA1(I)/B(I)                               02480000
           F2(I)= F2(I)/F0(I)-BA2(I)/B(I)-2.D0* F1(I) *BA1(I)/B(I)      02490000
           F0(I)= F0(I)/B(I)                                            02500000
  232   CONTINUE                                                        02510000
C                                                                       02520000
C                                                                       02530000
C       DO 234 I=0,I9-1                                                 02540000
C          F0(I)  = FNF0(Z(I))/B(I)                                     02550000
C 234   CONTINUE                                                        02560000
C                                                                       02570000
C       F0(I9)=F0(I9-1)                                                 02580000
C       F1(0)=0.D0                                                      02590000
C       F2(0)=(F0(1)-F0(0))*2.D0/DZ(1)/DZ(1)                            02600000
C       DO 236 I=1,I9-1                                                 02610000
C          F1(I)=(DZ1L(I)*F0(I-1)+DZ1(I)*F0(I)+DZ1R(I)*F0(I+1))/F0(I)   02620000
C          F2(I)=(DZ2L(I)*F0(I-1)+DZ2(I)*F0(I)+DZ2R(I)*F0(I+1))/F0(I)   02630000
C 236   CONTINUE                                                        02640000
C                                                                       02650000
C    >>> F0 = FNF0/B  ,  F1  =  FNF0'/FNF0 - B'/B  =  F0'/F0            02660000
C    >>> F2 = FNF0''/FNF0 - B''/B + F1*F1 + (B'/B)**2 - (FNF0'/FNF0)**2 02670000
C    >>>    = F0''/F0                                                   02680000
        A5 = 1.D0/TOP/TOP                                               02690000
        DO 244 I=0,I9-1, 1                                              02700000
           A1(I)=F0(I)*F0(I)                                            02710000
           A9(I)=F1(I)                                                  02720000
           A2(I)=F1(I)*F1(I)*A5                                         02730000
           A3(I)=F2(I)*A5                                               02740000
           A4(I)=2.D0*F1(I)*A5                                          02750000
  244   CONTINUE                                                        02760000
C                                                                       02770000
C    >>>> NUMERISCH HAT ES VORTEILE, DIE KRUEMMUNG UEBER DIE            02780000
C    >>>> BOGENLAENGE AUSZURECHNEN. DIE KURVE WIRD IM UHR-              02790000
C    >>>> ZEIGERSINN DURCHLAUFEN.                                       02800000
        R0(-1)=B(1)                                                     02810000
        R0(I9+1)=-B(I9-1)                                               02820000
        Z0(-1)=-Z(1)*TOP                                                02830000
        Z0(I9+1)=Z(I9-1)*TOP                                            02840000
        DO 250 I=0,I9,1                                                 02850000
           R0(I)=B(I)                                                   02860000
           H1=R0(I)-R0(I-1)                                             02870000
           H1=H1*H1                                                     02880000
           Z0(I)=Z(I)*TOP                                               02890000
           H2=Z0(I)-Z0(I-1)                                             02900000
           H2=H2*H2                                                     02910000
           DS(I)=DSQRT(H1+H2)                                           02920000
  250   CONTINUE                                                        02930000
        DS(I9+1)=DS(I9)                                                 02940000
        DO 255 I=0,I9,1                                                 02950000
           H1=DS(I)                                                     02960000
           H2=DS(I+1)                                                   02970000
           DS1R(I)= H1/H2/(H1+H2)                                       02980000
           DS1L(I)=-H2/H1/(H1+H2)                                       02990000
           DS1 (I)=-(DS1L(I)+DS1R(I))                                   03000000
           DS2L(I)= 2.D0/H1/(H1+H2)                                     03010000
           DS2R(I)= 2.D0/H2/(H1+H2)                                     03020000
           DS2 (I)=-(DS2L(I)+DS2R(I))                                   03030000
  255   CONTINUE                                                        03040000
C                                                                       03050000
        DO 260 I=0,I9,1                                                 03060000
           Z1(I)=DS1L(I)*Z0(I-1)+DS1(I)*Z0(I)+DS1R(I)*Z0(I+1)           03070000
           R1(I)=DS1L(I)*R0(I-1)+DS1(I)*R0(I)+DS1R(I)*R0(I+1)           03080000
           Z2(I)=DS2L(I)*Z0(I-1)+DS2(I)*Z0(I)+DS2R(I)*Z0(I+1)           03090000
           R2(I)=DS2L(I)*R0(I-1)+DS2(I)*R0(I)+DS2R(I)*R0(I+1)           03100000
           G(I)=Z1(I)*Z1(I)+R1(I)*R1(I)                                 03110000
           WG(I)=DSQRT(G(I))                                            03120000
           KAPPA(I)=-(Z1(I)*R2(I)-Z2(I)*R1(I))/G(I)/WG(I)               03130000
  260   CONTINUE                                                        03140000
C                                                                       03150000
C       **** KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS         03160000
C       **** KM = SUMME DER BEIDEN HAUPTKRUEMMUNGEN (DIM = 3.D0)        03170000
C       **** KM = KRUEMMUNG IN RICHTUNG DES KURVENPARAM. (DIM = 2.D0)   03180000
        DO 261 I=0,I9-1,1                                               03190000
           KM(I)= KAPPA(I) +Z1(I)/R0(I)/WG(I) * (DIM - 2.D0)            03200000
  261   CONTINUE                                                        03210000
        KM(I9)= KAPPA(I9) + KAPPA(I9) * (DIM - 2.D0)                    03220000
C                                                                       03230000
C       ***************************************************             03240000
C       ****   AUFSTELLEN DER MATRIX A( , )            ****             03250000
C       ***************************************************             03260000
C                                                                       03270000
        ICNTL=ICNTL+1                                                   03280000
        DO 300 I=1,  N9(I9)  ,1                                         03290000
        DO 300 N=1,  N9(I9),1                                           03300000
           A(I,N)=0.D0                                                  03310000
  300   CONTINUE                                                        03320000
C                                                                       03330000
C    >>> MI OB UN LO LU RU RE LI  (BEZUGSPUNKT = INNERER PUNKT)         03340000
        DO 320 I=1,  I9-3,1                                             03350000
        DO 320 N=1, I9-I-2 ,1                                           03360000
         H1=DRZLO(I,N)                                                  03370000
         H2=DRZLU(I,N)                                                  03380000
         H3=DRZRU(I,N)                                                  03390000
         A(N9(I)+N+1 , N9(I)+N+1 )=DR2(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03400000
     *    DR1(N)*A3(I)*R(N) + DZ2(I)*A5 + (H1+H2+H3)*A4(I)*R(N)         03410000
     *           +A1(I)*DR1(N)/R(N) * (DIM - 2.D0)                      03420000
         A(N9(I)+N+1 ,N9(I)+N+2 )=DR2R(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03430000
     *    DR1R(N)*A3(I)*R(N) + (-H1)*A4(I)*R(N)                         03440000
     *           +A1(I)*DR1R(N)/R(N) * (DIM - 2.D0)                     03450000
         A(N9(I)+N+1 ,N9(I)+N   )=DR2L(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03460000
     *    DR1L(N)*A3(I)*R(N) + (-H2-H3)*A4(I)*R(N)                      03470000
     *           +A1(I)*DR1L(N)/R(N) * (DIM - 2.D0)                     03480000
         A(N9(I)+N+1 ,N9(I-1)+N+2 )= H1*A4(I)*R(N)                      03490000
         A(N9(I)+N+1 ,N9(I-1)+N )  = H2*A4(I)*R(N)                      03500000
         A(N9(I)+N+1 ,N9(I+1)+N )  = H3*A4(I)*R(N)                      03510000
         A(N9(I)+N+1 ,N9(I+1)+N+1 )= A5*DZ2R(I) +(-H3)*A4(I)*R(N)       03520000
         A(N9(I)+N+1 ,N9(I-1)+N+1 )= A5*DZ2L(I) +(-H1-H2)*A4(I)*R(N)    03530000
  320   CONTINUE                                                        03540000
C                                                                       03550000
C    >>> MI    UN LO LU RU    LI (RE UND OB SIND RANDPUNKTE)            03560000
        DO 360 I=1,  I9-2,1                                             03570000
         N=I9-I-1                                                       03580000
         H1=DRZLO(I,N)                                                  03590000
         H2=DRZLU(I,N)                                                  03600000
         H3=DRZRU(I,N)                                                  03610000
         A(N9(I)+N+1 , N9(I)+N+1 )=DR2(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03620000
     *    DR1(N)*A3(I)*R(N) + DZ2(I)*A5 + (H1+H2+H3)*A4(I)*R(N)         03630000
     *           +A1(I)*DR1(N)/R(N) * (DIM - 2.D0)                      03640000
C        A(N9(I)+N+1 ,N9(I)+N+2 )=DR2R(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03650000
C    *    DR1R(N)*A3(I)*R(N) + (-H1)*A4(I)*R(N)                         03660000
C    *           +A1(I)*DR1R(N)/R(N) * (DIM - 2.D0)                     03670000
         A(N9(I)+N+1 ,N9(I)+N   )=DR2L(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03680000
     *    DR1L(N)*A3(I)*R(N) + (-H2-H3)*A4(I)*R(N)                      03690000
     *           +A1(I)*DR1L(N)/R(N) * (DIM - 2.D0)                     03700000
         A(N9(I)+N+1 ,N9(I-1)+N+2 )= H1*A4(I)*R(N)                      03710000
         A(N9(I)+N+1 ,N9(I-1)+N )  = H2*A4(I)*R(N)                      03720000
         A(N9(I)+N+1 ,N9(I+1)+N )  = H3*A4(I)*R(N)                      03730000
C        A(N9(I)+N+1 ,N9(I+1)+N+1 )= A5*DZ2R(I) +(-H3)*A4(I)*R(N)       03740000
         A(N9(I)+N+1 ,N9(I-1)+N+1 )= A5*DZ2L(I) +(-H1-H2)*A4(I)*R(N)    03750000
  360   CONTINUE                                                        03760000
C                                                                       03770000
C    >>> MI OB RE LI (BEZUGSPUNKT AUF Z-ACHSE)                          03780000
        DO 380 I=1,  I9-2,1                                             03790000
         N=0                                                            03800000
         A(N9(I)+N+1 , N9(I)+N+1 )= DR2(N)*A1(I)*(DIM - 1.D0) +         03810000
     *                        DZ2(I)*A5                                 03820000
         A(N9(I)+N+1 ,N9(I)+N+2 )=DR2R(N)*A1(I)*2.D0*(DIM - 1.D0)       03830000
         A(N9(I)+N+1 ,N9(I+1)+N+1 )=DZ2R(I)*A5                          03840000
         A(N9(I)+N+1 ,N9(I-1)+N+1 )=DZ2L(I)*A5                          03850000
  380   CONTINUE                                                        03860000
         I=I9-1                                                         03870000
         N=0                                                            03880000
         A(N9(I)+N+1 , N9(I)+N+1 )= DR2(N)*A1(I)*(DIM - 1.D0) +         03890000
     *                        DZ2(I)*A5                                 03900000
         A(N9(I)+N+1 ,N9(I-1)+N+1 )=DZ2L(I)*A5                          03910000
C                                                                       03920000
C    >>> MI OB UN RE (BEZUGSPUNKT AUF R-ACHSE)                          03930000
        DO 382 N=1,  I9-2,1                                             03940000
         I=0                                                            03950000
         A(N9(I)+N+1 , N9(I)+N+1 )=DR2(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03960000
     *    DZ2(I)*A5 + DR1(N)*A3(I)*R(N) + DR1(N)*A1(I)/R(N) *(DIM-2.D0) 03970000
         A(N9(I)+N+1 ,N9(I)+N+2 )=DR2R(N)*(A1(I)+R(N)*R(N)*A2(I)) +     03980000
     *    DR1R(N)*A3(I)*R(N) + DR1R(N)*A1(I)/R(N) * (DIM - 2.D0)        03990000
         A(N9(I)+N+1 ,N9(I)+N )=DR2L(N)*(A1(I)+R(N)*R(N)*A2(I)) +       04000000
     *    DR1L(N)*A3(I)*R(N) + DR1L(N)*A1(I)/R(N) * (DIM - 2.D0)        04010000
         A(N9(I)+N+1 ,N9(I+1)+N+1 )=DZ2L(I)*A5*2.D0                     04020000
  382   CONTINUE                                                        04030000
         N=I9-1                                                         04040000
         I=0                                                            04050000
         A(N9(I)+N+1 , N9(I)+N+1 )=DR2(N)*(A1(I)+R(N)*R(N)*A2(I)) +     04060000
     *    DZ2(I)*A5 + DR1(N)*A3(I)*R(N) + DR1(N)*A1(I)/R(N) *(DIM-2.D0) 04070000
         A(N9(I)+N+1 ,N9(I)+N )=DR2L(N)*(A1(I)+R(N)*R(N)*A2(I)) +       04080000
     *    DR1L(N)*A3(I)*R(N) + DR1L(N)*A1(I)/R(N) * (DIM - 2.D0)        04090000
C                                                                       04100000
C    >>>> MI OB RE  (BEZUGSPUNKT = NULLPUNKT)                           04110000
         A(1,1)   = DZ2(0)*A5 + DR2(0)*A1(0)*(DIM - 1.D0)               04120000
         A(1,2)   = DR2R(0)*A1(0)*2.D0*(DIM - 1.D0)                     04130000
         A(1,N9(1)+1) = DZ2R(0)*A5*2.D0                                 04140000
C                                                                       04150000
 2100   DO 510 I=1,N9(I9)                                               04160000
           BL(I)=0.0D0                                                  04170000
        DO 510 N=1,N9(I9)                                               04180000
           AL(I,N)=A(I,N)                                               04190000
  510   CONTINUE                                                        04200000
C                                                                       04210000
        IF (ICNTL .EQ. 1) THEN                                          04220000
           DO 560 I=1,  N9(I9)     ,1                                   04230000
              AL(I,I)=AL(I,I) - 1.D0                                    04240000
  560      CONTINUE                                                     04250000
        ENDIF                                                           04260000
C       ***************************************************             04270000
C       ****   AUFSTELLEN DER MATRIX BL(  )            ****             04280000
C       ***************************************************             04290000
C                                                                       04300000
        IF (ICNTL .EQ. 1) THEN                                          04310000
           DO 580 I=0,  I9  ,1                                          04320000
             FB(I)=C                                                    04330000
  580      CONTINUE                                                     04340000
        ELSE                                                            04350000
           DO 581 I=0,  I9  ,1                                          04360000
             FB(I)=-GAM(I)*KM(I)                                        04370000
  581      CONTINUE                                                     04380000
        ENDIF                                                           04390000
C                                                                       04400000
C    >>>>>>>> RANDPUNKT OBEN UND RECHTS                                 04410000
        DO 605 I=1,I9-2,1                                               04420000
         N=I9-I-1                                                       04430000
         H1=DRZLO(I,N)                                                  04440000
C        H2=DRZLU(I,N)                                                  04450000
         H3=DRZRU(I,N)                                                  04460000
         BL(N9(I)+N+1) = - (      DR2R(N)*(A1(I)+R(N)*R(N)*A2(I)) +     04470000
     *    DR1R(N)*A3(I)*R(N) + (-H1)*A4(I)*R(N)                         04480000
     *    +A1(I)*DR1R(N)/R(N) * (DIM - 2.D0)  ) * FB(I)                 04490000
     *    -  (     A5*DZ2R(I) +(-H3)*A4(I)*R(N)    ) * FB(I+1)          04500000
  605   CONTINUE                                                        04510000
         I=I9-1                                                         04520000
         N=I9-I-1                                                       04530000
         H1= DR2R(N)*A1(I)*2.D0 *(DIM - 1.D0) * FB(I)                   04540000
         H2= DZ2R(I)*A5 * FB(I+1)                                       04550000
         BL(N9(I)+N+1 ) = -H2-H1                                        04560000
         I=0                                                            04570000
         N=I9-I-1                                                       04580000
         H1=( DR2R(N)*(A1(I)+R(N)*R(N)*A2(I)) + DR1R(N)*A3(I)*R(N)      04590000
     *            + DR1R(N)*A1(I)/R(N) * (DIM - 2.D0) ) *FB(I)          04600000
         H2= DZ2R(I)*A5*2.D0 * FB(I+1)                                  04610000
         BL(N9(I)+N+1 ) = -H1-H2                                        04620000
C                                                                       04630000
C                                                                       04640000
        IF (ICNTL .EQ. 2) THEN                                          04650000
           DO 610 I=1,   N9(I9)  ,1                                     04660000
              BL(I)=BL(I) + (RHON(I)-1.D0)                              04670000
  610      CONTINUE                                                     04680000
        ENDIF                                                           04690000
C                                                                       04700000
C                                                                       04710000
        CALL DLAX(AL,161, N9(I9),BL,EPSZ,ISW,IS,VW,IP,ICON)             04720000
C                                                                       04730000
C    >>>> ABBRUCH BEI ICON > 0                                          04740000
        IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                       04750000
        IF (ICON .GT. 0) GOTO 9000                                      04760000
   50   FORMAT (1X,'ICON = ',I8,'  STEP = ',I8)                         04770000
C                                                                       04780000
        IF (ICNTL .EQ. 1) THEN                                          04790000
           DO 820 I=1,  N9(I9),1                                        04800000
              RHON(I)=BL(I)                                             04810000
  820      CONTINUE                                                     04820000
        ELSE                                                            04830000
           DO 821 I=1,  N9(I9),1                                        04840000
              GPOT(I)=BL(I)                                             04850000
  821      CONTINUE                                                     04860000
        ENDIF                                                           04870000
        IF (ICNTL. EQ. 1) THEN                                          04880000
           ICNTL=ICNTL+1                                                04890000
           GOTO 2100                                                    04900000
        ENDIF                                                           04910000
C                                                                       04920000
C                                                                       04930000
C       ******************************************************          04940000
C       ****         BEWEGUNG DES RANDES                  ****          04950000
C       ******************************************************          04960000
C                                                                       04970000
C    >>> 3 PUNKTE APPROXIMATION DES GRADIENTEN AM RANDE                 04980000
      DO 900 I=2,I9                                                     04990000
        N=I9-I                                                          05000000
        H3=-GAM(I)*KM(I)                                                05010000
        H1=GPOT(N9(I-1)+N+1)                                            05020000
        H2=GPOT(N9(I-2)+N+1)                                            05030000
        GPAZ(N9(I9)+I+1)=(H3-H1)*(DZ(I) + DZ(I-1))/DZ(I)/DZ(I-1)        05040000
     *        -(H3-H2)*DZ(I)/DZ(I-1)/(DZ(I) + DZ(I-1))                  05050000
  900 CONTINUE                                                          05060000
C                                                                       05070000
      DO 901 I=0,I9-2                                                   05080000
        N=I9-I                                                          05090000
        H3=-GAM(I)*KM(I)                                                05100000
        H1=GPOT(N9(I)+N  )                                              05110000
        H2=GPOT(N9(I)+N-1 )                                             05120000
        GPAR(N9(I9)+I+1)=(H3-H1)*(DR(N) + DR(N-1))/DR(N)/DR(N-1)        05130000
     *        -(H3-H2)*DR(N)/DR(N-1)/(DR(N) + DR(N-1))                  05140000
  901 CONTINUE                                                          05150000
C                                                                       05160000
        GPAR(N9(I9)+I9+1)=0.D0                                          05170000
        GPAZ(N9(I9)+1)=0.D0                                             05180000
C                                                                       05190000
C   >>> 5 PUNKTE APPROXIMATION FUER D/DZ (I=1) BZW. D/DR (I=I9-1)       05200000
        I=1                                                             05210000
        N=I9-I                                                          05220000
        H1=GPOT(N9(I-1)+N+1)                                            05230000
        H2=GPOT(N9(I-1)+N  )                                            05240000
        H3=GPOT(N9(I  )+N  )                                            05250000
        H4=(-GAM(I+1)*KM(I+1)*DZ(I)+H2*DZ(I+1))/(DZ(I)+DZ(I+1))-H3      05260000
        GPAZ(N9(I9)+I+1)=H4/DZ(I+1)-(H1+GAM(I)*KM(I))/DZ(I)             05270000
C                                                                       05280000
        I=I9-1                                                          05290000
        N=I9-I                                                          05300000
        H1=GPOT(N9(I-1)+N+1)                                            05310000
        H2=GPOT(N9(I-1)+N  )                                            05320000
        H3=GPOT(N9(I  )+N  )                                            05330000
        H4=(-GAM(I-1)*KM(I-1)*DR(N)+H2*DR(N+1))/(DR(N)+DR(N+1))-H1      05340000
        GPAR(N9(I9)+I+1)=H4/DR(N+1)-(H3+GAM(I)*KM(I))/DR(N)             05350000
C                                                                       05360000
C   >>> 3 PUNKTE APPROXIMATION FUER D/DZ (I=1) BZW. D/DR (I=I9-1)       05370000
        I=1                                                             05380000
        N=I9-I                                                          05390000
        H3=-GAM(I)*KM(I)                                                05400000
        H1=GPOT(N9(I-1)+N+1)                                            05410000
        H2=H3                                                           05420000
        GPAZ(N9(I9)+I+1)=(H3-H1)*(DZ(I) + DZ(I-1))/DZ(I)/DZ(I-1)        05430000
     *        -(H3-H2)*DZ(I)/DZ(I-1)/(DZ(I) + DZ(I-1))                  05440000
C                                                                       05450000
        I=I9-1                                                          05460000
        N=I9-I                                                          05470000
        H3=-GAM(I)*KM(I)                                                05480000
        H1=GPOT(N9(I)+N  )                                              05490000
        H2=H3                                                           05500000
        GPAR(N9(I9)+I+1)=(H3-H1)*(DR(N) + DR(N-1))/DR(N)/DR(N-1)        05510000
     *        -(H3-H2)*DR(N)/DR(N-1)/(DR(N) + DR(N-1))                  05520000
C                                                                       05530000
        TOPPKT=GPAZ(N9(I9)+I9+1)/TOP                                    05540000
C                                                                       05550000
        DO 904 I=0,I9-1,1                                               05560000
           N=I9-I                                                       05570000
           K=N9(I9)+I+1                                                 05580000
           H1= (GPAZ(K)+GPAR(K)*A9(I)*R(N))/TOP                         05590000
           H2= GPAR(K)*F0(I)                                            05600000
           BPKT(I)= H2 -  H1 * BA1(I) /TOP                              05610000
           BPKT(I)= BPKT(I) + BA1(I)*Z(I)*TOPPKT/TOP                    05620000
  904   CONTINUE                                                        05630000
C                                                                       05640000
C    >>> SOLLTE DIE BEWEGUNG DES RANDES ZU SCHNELL WERDEN, SO WIRD      05650000
C    >>> DELT = DELTA T HALBIERT.                                       05660000
        DO 910 I=0, I9-1,1                                              05670000
  909    IF (DABS(BPKT(I)*DELT).GT.DABS(B(I)/ 200.D0)) DELT=DELT/2.D0   05680000
         IF (DABS(BPKT(I)*DELT).GT.DABS(B(I)/ 200.D0)) GOTO 909         05690000
  910   CONTINUE                                                        05700000
  911   IF (DABS(TOPPKT*DELT) .GT. DABS(TOP/ 200.D0)) DELT=DELT/2.D0    05710000
        IF (DABS(TOPPKT*DELT) .GT. DABS(TOP/ 200.D0)) GOTO 911          05720000
C                                                                       05730000
   40   FORMAT (1X,'STEP  = ',I8,15X,'  T     = ',D23.16,'  DELT  = ',  05740000
     *          D23.16)                                                 05750000
   41   FORMAT (1X,I3,2X,I3,4(2X,D23.16))                               05760000
   42   FORMAT (1X,I3,2(2X,D23.16))                                     05770000
   43   FORMAT (1X,I3,5(1X,D14.7))                                      05780000
   44   FORMAT (1X,'STEP  = ',I8,5X,'  T  = ',D14.7,'  DELT  = ',D14.7) 05790000
C    >>>> AUSRECHNEN DES GESCHWINDIGKEITSFELDES IM INNERN               05800000
C    >>>> (NUR BEIM AUSDRUCKEN DER DATEN)                               05810000
        IF (COUN9 .EQ. CPRIN) THEN                                      05820000
           WRITE (22,40) COUNT,T9,DELT                                  05830000
           WRITE ( 6,44) COUNT,T9,DELT                                  05840000
           DO 927 I=1 ,I9-1,1                                           05850000
           DO 927 N=0,I9-I-1 ,1                                         05860000
              H3=GPOT(N9(I)+N+1)                                        05870000
              H1=GPOT(N9(I-1)+N+1)                                      05880000
              H2=-GAM(I+1)*KM(I+1)                                      05890000
              IF (N .LT. I9-I-1)  H2=GPOT(N9(I+1)+N+1)                  05900000
              GPAZ(N9(I)+N+1 )=  H3*DZ1(I)+H1*DZ1L(I)+H2*DZ1R(I)        05910000
  927      CONTINUE                                                     05920000
           DO 929 N=0,I9-1,1                                            05930000
              GPAZ(N+1)=0.D0                                            05940000
              GPAR(N9(N)+1)=0.D0                                        05950000
  929      CONTINUE                                                     05960000
           DO 912 I=0,I9-2,1                                            05970000
           DO 912 N=1,I9-I-1,1                                          05980000
              H3=GPOT(N9(I)+N+1)                                        05990000
              H1=GPOT(N9(I)+N)                                          06000000
              H2=-GAM(I)*KM(I)                                          06010000
              IF (N .LT. I9-I-1) H2=GPOT(N9(I)+N+2)                     06020000
              GPAR(N9(I)+N+1 )=  H3*DR1(N)+H1*DR1L(N)+H2*DR1R(N)        06030000
  912      CONTINUE                                                     06040000
           DO 914 I=0,I9-1,1                                            06050000
           DO 913 N=0,I9-I-1                                            06060000
              K=N9(I)+N+1                                               06070000
              WRITE (22,41) I,N,RHON(K),GPOT(K),(GPAZ(K)+GPAR(K)*A9(I)* 06080000
     *                 R(N))/TOP , GPAR(K)*F0(I)                        06090000
  913      CONTINUE                                                     06100000
              K=N9(I9)+I+1                                              06110000
              N=I9-I                                                    06120000
              WRITE (22,41) I,N,C,-GAM(I)*KM(I),(GPAZ(K)+GPAR(K)        06130000
     *           *A9(I)*R(N))/TOP , GPAR(K)*F0(I)                       06140000
  914      CONTINUE                                                     06150000
              I=I9                                                      06160000
              K=N9(I9)+I+1                                              06170000
              N=I9-I                                                    06180000
              WRITE (22,41) I,N,C,-GAM(I)*KM(I),(GPAZ(K)+GPAR(K)        06190000
     *           *A9(I)*R(N))/TOP , 0.D0                                06200000
C                                                                       06210000
           DO 916 I=0,I9-1,1                                            06220000
              WRITE (22,42) I,B(I),BPKT(I)                              06230000
              K=N9(I9)+I+1                                              06240000
              N=I9-I                                                    06250000
              H1= (GPAZ(K)+GPAR(K)*A9(I)*R(N))/TOP                      06260000
              H2= GPAR(K)*F0(I)                                         06270000
              WRITE ( 6,43) I,B(I),BPKT(I),KM(I),H1,H2                  06280000
  916      CONTINUE                                                     06290000
           WRITE (22,42) I9,TOP,TOPPKT                                  06300000
           K=N9(I9)+I9+1                                                06310000
           H1=  GPAZ(K) /TOP                                            06320000
           WRITE ( 6,43) I9,TOP,TOPPKT,KM(I9),H1,0.D0                   06330000
C                                                                       06340000
           WRITE ( 6, *) ' '                                            06350000
           DO 917 I=0,I9-1,1                                            06360000
              WRITE ( 6,43) I,F0(I),F1(I),F2(I)                         06370000
  917      CONTINUE                                                     06380000
           WRITE ( 6, *) ' '                                            06390000
        ENDIF                                                           06400000
C                                                                       06410000
C    >>>>BERECHNUNG DER NEUEN RANDPOSITION                              06420000
        DO 920 I=0,  I9-1,1                                             06430000
           B(I)=B(I)+BPKT(I)*DELT                                       06440000
  920   CONTINUE                                                        06450000
        TOP=TOP+TOPPKT*DELT                                             06460000
        ICNTL=0                                                         06470000
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   06480000
        IF (COUNT .EQ. CEND) GOTO 9000                                  06490000
        GOTO 2000                                                       06500000
C                                                                       06510000
 9000   CONTINUE                                                        06520000
        CLOSE (22,STATUS='KEEP')                                        06530000
C       END                                                             06540000
C/LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              06550000
C/              DD  DSN=SYS1.FORTLIB,DISP=SHR                           06560000
C/GO.SYSIN DD *                                                         06570000
C*                                                                      06580000
C*      EXEC DRUCKE                                                     06590000
C*      END                                                             06600000
        END                                                             06610000
