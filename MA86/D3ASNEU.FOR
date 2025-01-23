//D14KA    JOB RACF,CLASS=A,                                            00010000
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020000
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030000
//FORT.SYSIN    DD *                                                    00040000
C                                                                       00050000
C       ##################################################              00060001
C       ####   MODELL: MA86       (ERWEITERT)         ####              00070001
C       ####   NAME DES PROGRAMMS:  MA86(D3ASNEU)     ####              00080001
C       ####      LAPLACE(RHONI) = RHONI (INNENRAUM)  ####              00090001
C       ####      LAPLACE(RHONI) = 0.0  (AUSSENRAUM)  ####              00100001
C       ####      LAPLACE(PHIA) = RHONI - 1           ####              00110001
C       ####      R.B. : RHONA = C   (IM UNENDLICHEN) ####              00120001
C       ####           : (DN * GRAD(RHONI) -          ####              00130001
C       ####              GRAD(RHONA) ) * NORMALE = 0 ####              00140001
C       ####           : PHIA = - GAMMA * KAPPA       ####              00150001
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00160001
C       ####      VA    =  GRAD( PHIA )               ####              00170001
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00180001
C       ##################################################              00190001
C        GERECHNET WIRD IN KUGELKOORDINATEN MIT AZIMUTHALER             00200001
C        SYMMETRIE (UND SYMMETRIE BZGL. DER X-Y-EBENE).                 00210001
C        VARIABLER OBERFLAECHENSP.-KOEFFIZIENT IST MOEGLICH.            00220001
C        DATEN WERDEN IN DATA-FILE  FNW  ABGESPEICHERT.                 00230001
C        GRAPHIK-PROGRAMM:       MA86(PIC2AS) (??????)                  00240001
C                                                                       00250001
C                                                                       00260000
      INTEGER           I8,I9,N9,I,N,COUNT,COUN9,CPRIN,K                00270000
      DOUBLE PRECISION  GAMMA,DTH,C,DELT,T9,DN                          00280000
      DOUBLE PRECISION  B(0:21),BPKT(0:21),BSTRT(0:21),BST(0:12)        00290000
      DOUBLE PRECISION  BA1(0:21),BA2(0:21),G(0:21),WG                  00300000
      DOUBLE PRECISION  A1(0:21),A2(0:21),A3(0:21),A4(0:21),A5(0:21)    00310000
      DOUBLE PRECISION  B1(0:21),B2(0:21),B3(0:21),B4(0:21),B5(0:21)    00320000
      DOUBLE PRECISION  KAN,KAPPA(0:21),GAM(0:21)                       00330000
      DOUBLE PRECISION  R(0:21),DR(21),DR1(21),DR2(21)                  00340000
      DOUBLE PRECISION  DR1L(21),DR1R(21),DR2L(21),DR2R(21)             00350000
      DOUBLE PRECISION  TR(21),TR2(21),TR3(21),TR4(21)                  00360000
      DOUBLE PRECISION  DR1GL0,DR1GL1,DR1GL2                            00370000
      DOUBLE PRECISION  DR1GR0,DR1GR1,DR1GR2                            00380000
      DOUBLE PRECISION  PI,H1,H2,H3                                     00390000
      DOUBLE PRECISION  FB(0:21)                                        00400000
C                                                                       00410000
      DOUBLE PRECISION  RHON(201),GPOT(201)                             00420000
      DOUBLE PRECISION  GPAR(201),GPATH(201)                            00430000
      DOUBLE PRECISION  RHOA                                            00440000
C                                                                       00450000
      DOUBLE PRECISION  A(201,201),AGPOT(201,201),BL(201)               00460000
      DOUBLE PRECISION  VW(201),IP(201)                                 00470000
      DOUBLE PRECISION  EPSZ,IS                                         00480000
      INTEGER           ICON,ISW                                        00490000
      CHARACTER         FNW*30                                          00500000
C                                                                       00510000
      DOUBLE PRECISION X,FNP2                                           00520000
      FNP2(X) = (3.D0*X*X -1.D0) * .5D0                                 00530000
C                                                                       00540000
C     **************************************************                00550000
C     ****      PARAMETER PARAMETER PARAMETER       ****                00560000
C     **************************************************                00570000
C                                                                       00580000
      FNW ='D14K.TEST.DATA'                                             00590001
      GAMMA     =     0.00100D0                                         00600000
      C         =     2.00000D0                                         00610000
      RHOA      =   100.00000D0                                         00620000
      DN        =     1.00000D0                                         00630000
      N9        =    10                                                 00640000
      I8        =     4                                                 00650000
      I9        =     8                                                 00660000
      R(0)      =      .00D0                                            00670000
      R(1)      =      .30D0                                            00680000
      R(2)      =      .60D0                                            00690000
      R(3)      =      .90D0                                            00700000
      R(I8)     =     1.00D0                                            00710000
      R(5)      =     1.10D0                                            00720000
      R(6)      =     1.40D0                                            00730000
      R(7)      =     1.70D0                                            00740000
      R(I9)     =     2.00D0                                            00750000
      DELT      =     8.00D0                                            00760000
      BST(0)    =     1.5870D0                                          00770000
      BST(2)    =     0.0100D0                                          00780000
      CEND      =  7000                                                 00790000
      CPRIN     =  1000                                                 00800000
      EPSZ      =     1.D-60                                            00810000
      ISW       =     1                                                 00820000
C                                                                       00830000
      H1=1.0D0                                                          00840000
      PI=4.0D0*DATAN(H1)                                                00850000
      H1=N9-1                                                           00860000
      DTH=.5D0 * PI/H1                                                  00870000
      DO 100 N=1,  N9   ,1                                              00880000
         H2=N-1                                                         00890000
         BSTRT(N)=BST(0)+BST(2) * FNP2( DCOS(DTH*H2) )                  00900000
         GAM(N)=GAMMA                                                   00910000
  100 CONTINUE                                                          00920000
      GAM(N9+1 )=GAM(N9-1)                                              00930000
      GAM(0    )=GAM(2   )                                              00940000
C                                                                       00950000
      OPEN (22,FILE=FNW)                                                00960000
      REWIND 22                                                         00970000
C                                                                       00980000
   30 FORMAT (1X,'MA87(D3AS) AZIMUTH. SYMMETRIE - 3 DIMENSIONAL')       00990000
   31 FORMAT (1X,'GAMMA = ',D23.16,'  RHOA  = ',D23.16,'  C     = ',    01000000
     *        D23.16)                                                   01010000
   32 FORMAT (1X,'N9    = ',I3,20X,'  I9    = ',I3,20X,'  DTH   = ',    01020000
     *        D23.16)                                                   01030000
   35 FORMAT (1X,I3,2X,D23.16,2X,D23.16)                                01040000
      WRITE (22,30)                                                     01050000
      WRITE (22,31) GAMMA,RHOA,C                                        01060000
      WRITE (22,32) N9,I9,DTH                                           01070000
      WRITE (22,35) 0,0.D0                                              01080000
      DO 130 I=1,  I9,1                                                 01090000
         DR(I)=R(I)-R(I-1)                                              01100000
         WRITE (22,35) I,R(I),DR(I)                                     01110000
         TR(I)=1.D0/R(I)                                                01120000
         IF (I .GT. I8) TR(I)=2.D0-R(I)                                 01130000
         TR2(I)=TR(I)*TR(I)                                             01140000
         TR3(I)=TR2(I)*TR(I)                                            01150000
         TR4(I)=TR3(I)*TR(I)                                            01160000
  130 CONTINUE                                                          01170000
      DO 136 I=1,  I9-1,1                                               01180000
         H1=DR(I)                                                       01190000
         H2=DR(I+1)                                                     01200000
         DR2 (I)=-2.D0/H1/H2                                            01210000
         DR2L(I)= 2.D0/H1/(H1+H2)                                       01220000
         DR2R(I)= 2.D0/H2/(H1+H2)                                       01230000
         DR1R(I)= H1/H2/(H1+H2)                                         01240000
         DR1L(I)=-H2/H1/(H1+H2)                                         01250000
         DR1 (I)=-(DR1L(I)+DR1R(I))                                     01260000
  136 CONTINUE                                                          01270000
      DR1GL2=   DR(I8)/DR(I8-1)/( DR(I8)+DR(I8-1) )                     01280000
      DR1GL1=-( DR(I8)+DR(I8-1) ) / DR(I8) / DR(I8-1)                   01290000
      DR1GL0=-( DR1GL1+DR1GL2 )                                         01300000
      DR1GR2=-  DR(I8+1)/DR(I8+2)/( DR(I8+1)+DR(I8+2) )                 01310000
      DR1GR1= ( DR(I8+1)+DR(I8+2) ) / DR(I8+1) / DR(I8+2)               01320000
      DR1GR0=-( DR1GR1+DR1GR2 )                                         01330000
      DO 140 N=1,    N9   ,1                                            01340000
         B(N)=BSTRT(N)                                                  01350000
  140 CONTINUE                                                          01360000
      COUNT=-1                                                          01370000
      COUN9=CPRIN-1                                                     01380000
      T9   =-DELT                                                       01390000
C                                                                       01400000
C     >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<            01410000
C     >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<            01420000
C     >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<            01430000
C                                                                       01440000
 2000 CONTINUE                                                          01450000
        COUNT=COUNT+1                                                   01460000
        COUN9=COUN9+1                                                   01470000
        T9   =T9   +DELT                                                01480000
        DO 160 I=1,  N9*(I9-1)+1,1                                      01490000
           BL(I)=0.D0                                                   01500000
        DO 160 N=1,  N9*(I9-1)+1,1                                      01510000
           A(I,N)=0.D0                                                  01520000
  160   CONTINUE                                                        01530000
C       ******************************************************          01540000
C       ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****          01550000
C       ******************************************************          01560000
C                                                                       01570000
        B(0)     =B(2)                                                  01580000
        B(  N9+1)=B(N9-1)                                               01590000
C       **** N=1 ENTSPRICHT THETA=0. GRAD *****                         01600000
        N=1                                                             01610000
           BA1(N)=0.D0                                                  01620000
           BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DTH/DTH                     01630000
           KAN   =2.D0*B(N)*B(N)-2.D0*B(N)*BA2(N)                       01640000
           G(N)  =B(N)*B(N)                                             01650000
           WG    =DSQRT(G(N))                                           01660000
           KAPPA(N)=(-BA2(N)*B(N)+B(N)*B(N)) / G(N)/WG                  01670000
           H1=B(N)*B(N)                                                 01680000
           A1(N)=1.D0/H1                                                01690000
           A2(N)=KAN/H1/H1                                              01700000
           A3(N)=2.D0/H1                                                01710000
           A4(N)=0.D0                                                   01720000
           A5(N)=0.D0                                                   01730000
           B1(N)=A1(N)                                                  01740000
           B2(N)= -2.D0*B(N)*BA2(N)/H1/H1                               01750000
           B3(N)=A3(N)                                                  01760000
           B4(N)=0.D0                                                   01770000
           B5(N)=0.D0                                                   01780000
C       **** N=N9 ENTSPRICHT THETA=90. GRAD *****                       01790000
        DO 220 N=2,    N9,1                                             01800000
           H1= N-1                                                      01810000
           H1= H1*DTH                                                   01820000
           H2= DCOS(H1)/DSIN(H1)                                        01830000
           BA1(N)=(B(N+1)-B(N-1))/2.D0/DTH                              01840000
           BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DTH/DTH                     01850000
           KAN   =2.D0*B(N)*B(N)+2.D0*BA1(N)*BA1(N)-B(N)*BA2(N)         01860000
     *                 -B(N)*BA1(N)*H2                                  01870000
           G(N)  =B(N)*B(N)+BA1(N)*BA1(N)                               01880000
           WG    =DSQRT(G(N))                                           01890000
           KAPPA(N)=(-BA2(N)*B(N)+3.D0*BA1(N)*BA1(N)+2.D0*B(N)*B(N)     01900000
     *                 -BA1(N)*G(N)*H2/B(N) ) /2.D0 /G(N)/WG            01910000
           H1=B(N)*B(N)                                                 01920000
           A1(N)=G(N)/H1/H1                                             01930000
           A2(N)=KAN/H1/H1                                              01940000
           A3(N)=1.D0/H1                                                01950000
           A4(N)=-2.D0*BA1(N)/H1/B(N)                                   01960000
           A5(N)=H2/H1                                                  01970000
           B1(N)=A1(N)                                                  01980000
           B2(N)=(-B(N)*BA2(N)-B(N)*BA1(N)*H2)/H1/H1                    01990000
           B3(N)=A3(N)                                                  02000000
           B4(N)=A4(N)                                                  02010000
           B5(N)=A5(N)                                                  02020000
  220   CONTINUE                                                        02030000
        KAPPA(  N9+1)=KAPPA(N9-1)                                       02040000
        KAPPA(0)     =KAPPA(2)                                          02050000
C       DO 222 N=0,N9+1,1                                               02060000
C          IF (COUN9 .EQ. CPRIN) WRITE (22,11) N,KAPPA(N)               02070000
C 222   CONTINUE                                                        02080000
C  11   FORMAT (1X,I3,2X,F10.6)                                         02090000
C                                                                       02100000
C       ********************************************************        02110000
C       ****   AUFSTELLEN DER MATRIX A( , ) FUER INNENRAUM  ****        02120000
C       ********************************************************        02130000
C                                                                       02140000
C       >>>>>>>>>>>MITTLERER PUNKT<<<<<<<<<<                            02150000
        DO 300 I=1,  I8-1,1                                             02160000
        DO 300 N=1,    N9,1                                             02170000
         A(N+1+N9*(I-1),N+1+N9*(I-1))= A1(N)*DR2(I)+A2(N)*DR1(I)/R(I)   02180000
     *              -A3(N)*2.D0/DTH/DTH/R(I)/R(I)                       02190000
  300   CONTINUE                                                        02200000
C       >>>>>>>>>>>PUNKT DARUEBER<<<<<<<<<<                             02210000
        DO 310 I=1,  I8-1 ,1                                            02220000
        DO 310 N=1,    N9 ,1                                            02230000
         A(N+1+N9*(I-1),N+1+N9*(I  ))= A1(N)*DR2R(I)                    02240000
     *              +A2(N)*DR1R(I)/R(I)                                 02250000
  310   CONTINUE                                                        02260000
C       >>>>>>>>>>>PUNKT DARUNTER<<<<<<<<<<                             02270000
        DO 320 N=1,    N9,1                                             02280000
         A(N+1,  1 )=A1(N)*DR2L(1)+A2(N)*DR1L(1)/R(1)                   02290000
        DO 320 I=2,  I8-1,1                                             02300000
         A(N+1+N9*(I-1),N+1+N9*(I-2))= A1(N)*DR2L(I)                    02310000
     *              +A2(N)*DR1L(I)/R(I)                                 02320000
  320   CONTINUE                                                        02330000
C       >>>>>>>>>>>PUNKT LINKS  OBEN<<<<<<<<<<                          02340000
        DO 330 I=1,  I8-1 ,1                                            02350000
        DO 330 N=2,  N9-1,1                                             02360000
         A(N+1+N9*(I-1),N+2+  N9*(I  ))= A4(N)*DR1R(I)/2.D0/DTH/R(I)    02370000
  330   CONTINUE                                                        02380000
C       >>>>>>>>>>>PUNKT LINKS UNTEN<<<<<<<<<<                          02390000
        DO 340 I=2,  I8-1,1                                             02400000
        DO 340 N=2,    N9-1,1                                           02410000
         A(N+1+N9*(I-1),N+2+  N9*(I-2))= A4(N)*DR1L(I)/2.D0/DTH/R(I)    02420000
  340   CONTINUE                                                        02430000
C       >>>>>>>>>>>PUNKT RECHTS OBEN<<<<<<<<<<                          02440000
        DO 350 I=1,  I8-1,1                                             02450000
        DO 350 N=2,N9-1  ,1                                             02460000
         A(N+1+N9*(I-1),N  +  N9*(I  ))=-A4(N)*DR1R(I)/2.D0/DTH/R(I)    02470000
  350   CONTINUE                                                        02480000
C       >>>>>>>>>>>PUNKT RECHTS UNTEN<<<<<<<<<                          02490000
        DO 360 I=2,  I8-1,1                                             02500000
        DO 360 N=2, N9-1 ,1                                             02510000
         A(N+1+N9*(I-1),N  +  N9*(I-2))=-A4(N)*DR1L(I)/2.D0/DTH/R(I)    02520000
  360   CONTINUE                                                        02530000
C       >>>>>>>>>>> PUNKT RECHTS  <<<<<<<<<<                            02540000
        DO 370 I=1,  I8-1,1                                             02550000
         A(1+N9*I   , N9*I   )=2.D0* A3(  N9)/DTH/DTH/R(I)/R(I)         02560000
        DO 370 N=2, N9-1 ,1                                             02570000
         A(N+1+N9*(I-1),N  +  N9*(I-1))= A3(N)/DTH/DTH/R(I)/R(I)        02580000
     *            - A4(N)*DR1(I)/2.D0/DTH/R(I)                          02590000
     *            - A5(N)/DTH/2.D0/R(I)/R(I)                            02600000
  370   CONTINUE                                                        02610000
C       >>>>>>>>>>> PUNKT LINKS   <<<<<<<<<<                            02620000
        DO 380 I=1,  I8-1,1                                             02630000
         A(2+  N9*(I-1),  3+  N9*(I-1))= 2.D0*A3(1)/DTH/DTH/R(I)/R(I)   02640000
        DO 380 N=2,    N9-1 ,1                                          02650000
         A(N+1+N9*(I-1),N+2+  N9*(I-1))= A3(N)/DTH/DTH/R(I)/R(I)        02660000
     *            + A4(N)*DR1(I)/2.D0/DTH/R(I)                          02670000
     *            + A5(N)/DTH/2.D0/R(I)/R(I)                            02680000
  380   CONTINUE                                                        02690000
C       >>>>>>>>>>>  NULLPUNKT (IN KARTES. KOORDINA.) <<<<<<<<<<<<<     02700000
        A(  1  ,2     )=2.D0/DR(1)/DR(1)/B(1)/B(1)                      02710000
        A(  1  ,N9+1  )=4.D0/DR(1)/DR(1)/B(N9)/B(N9)                    02720000
        H1=0.D0                                                         02730000
        H1=H1-2.D0/B(1)/B(1) -4.D0/B(N9)/B(N9)                          02740000
        A(  1  ,1     )=H1   /DR(1)/DR(1)                               02750000
C                                                                       02760000
C                                                                       02770000
C       ********************************************************        02780000
C       ****   AUFSTELLEN DER MATRIX A( , ) FUER AUSSENRAUM ****        02790000
C       ********************************************************        02800000
C                                                                       02810000
C       >>>>>>>>>>>MITTLERER PUNKT<<<<<<<<<<                            02820000
        DO 400 I=I8+1,I9-1,1                                            02830000
        DO 400 N=1,    N9,1                                             02840000
         A(N+1+N9*(I-1),N+1+N9*(I-1))= B1(N)*DR2(I)*TR4(I)              02850000
     *           +B2(N)*DR1(I)*TR3(I)-B3(N)*TR2(I)*2.D0/DTH/DTH         02860000
  400   CONTINUE                                                        02870000
C       >>>>>>>>>>>PUNKT DARUEBER<<<<<<<<<<                             02880000
        DO 410 I=I8+1,  I9-2 ,1                                         02890000
        DO 410 N=1,    N9 ,1                                            02900000
         A(N+1+N9*(I-1),N+1+N9*(I  ))= B1(N)*DR2R(I)*TR4(I)             02910000
     *              +B2(N)*DR1R(I)*TR3(I)                               02920000
  410   CONTINUE                                                        02930000
C       >>>>>>>>>>>PUNKT DARUNTER<<<<<<<<<<                             02940000
        DO 420 N=1,    N9,1                                             02950000
        DO 420 I=I8+1,  I9-1,1                                          02960000
         A(N+1+N9*(I-1),N+1+N9*(I-2))= B1(N)*DR2L(I)*TR4(I)             02970000
     *              +B2(N)*DR1L(I)*TR3(I)                               02980000
  420   CONTINUE                                                        02990000
C       >>>>>>>>>>>PUNKT LINKS  OBEN<<<<<<<<<<                          03000000
        DO 430 I=I8+1,  I9-2 ,1                                         03010000
        DO 430 N=2,  N9-1,1                                             03020000
         A(N+1+N9*(I-1),N+2+  N9*(I  ))= B4(N)*DR1R(I)*TR3(I)/2.D0/DTH  03030000
  430   CONTINUE                                                        03040000
C       >>>>>>>>>>>PUNKT LINKS UNTEN<<<<<<<<<<                          03050000
        DO 440 I=I8+1,  I9-1,1                                          03060000
        DO 440 N=2,    N9-1,1                                           03070000
         A(N+1+N9*(I-1),N+2+  N9*(I-2))= B4(N)*DR1L(I)*TR3(I)/2.D0/DTH  03080000
  440   CONTINUE                                                        03090000
C       >>>>>>>>>>>PUNKT RECHTS OBEN<<<<<<<<<<                          03100000
        DO 450 I=I8+1,  I9-2,1                                          03110000
        DO 450 N=2,N9-1  ,1                                             03120000
         A(N+1+N9*(I-1),N  +  N9*(I  ))=-B4(N)*DR1R(I)*TR3(I)/2.D0/DTH  03130000
  450   CONTINUE                                                        03140000
C       >>>>>>>>>>>PUNKT RECHTS UNTEN<<<<<<<<<                          03150000
        DO 460 I=I8+1,  I9-1,1                                          03160000
        DO 460 N=2, N9-1 ,1                                             03170000
         A(N+1+N9*(I-1),N  +  N9*(I-2))=-B4(N)*DR1L(I)*TR3(I)/2.D0/DTH  03180000
  460   CONTINUE                                                        03190000
C       >>>>>>>>>>> PUNKT RECHTS  <<<<<<<<<<                            03200000
        DO 470 I=I8+1,  I9-1,1                                          03210000
         A(1+N9*I   , N9*I   )=2.D0* B3(  N9)*TR2(I)/DTH/DTH            03220000
        DO 470 N=2, N9-1 ,1                                             03230000
         A(N+1+N9*(I-1),N  +  N9*(I-1))= B3(N)*TR2(I)/DTH/DTH           03240000
     *            - B4(N)*DR1(I)*TR3(I)/2.D0/DTH                        03250000
     *            - B5(N)*TR2(I)/DTH/2.D0                               03260000
  470   CONTINUE                                                        03270000
C       >>>>>>>>>>> PUNKT LINKS   <<<<<<<<<<                            03280000
        DO 480 I=I8+1,  I9-1,1                                          03290000
         A(2+  N9*(I-1),  3+  N9*(I-1))= 2.D0*B3(1)*TR2(I)/DTH/DTH      03300000
        DO 480 N=2,    N9-1 ,1                                          03310000
         A(N+1+N9*(I-1),N+2+  N9*(I-1))= B3(N)*TR2(I)/DTH/DTH           03320000
     *            + B4(N)*DR1(I)*TR3(I)/2.D0/DTH                        03330000
     *            + B5(N)*TR2(I)/DTH/2.D0                               03340000
  480   CONTINUE                                                        03350000
C                                                                       03360000
C       ***********************************************************     03370000
C       ****   AUFSTELLEN DER MATRIX A( , ) FUER I=I8 (GRENZE) ****     03380000
C       ***********************************************************     03390000
C                                                                       03400000
C   >>> APPROXIMATION DER GRADIENTEN AM RANDE DURCH POLYNOME 2. GRADES  03410000
        DO 500 N=1,N9,1                                                 03420000
         H1=G(N)/B(N)/B(N)/B(N)                                         03430000
         A(N+1+N9*(I8-1),N+1+N9*(I8-1)) = (DN*DR1GR0 - DR1GL0)*H1       03440000
         A(N+1+N9*(I8-1),N+1+N9*(I8-2)) =  -DR1GL1*H1                   03450000
         A(N+1+N9*(I8-1),N+1+N9*(I8-3)) =  -DR1GL2*H1                   03460000
         A(N+1+N9*(I8-1),N+1+N9*(I8  )) =  DN*DR1GR1*H1                 03470000
         A(N+1+N9*(I8-1),N+1+N9*(I8+1)) =  DN*DR1GR2*H1                 03480000
  500   CONTINUE                                                        03490000
        DO 510 N=2,N9-1,1                                               03500000
         H1=-1.D0/B(N)/B(N)                                             03510000
         A(N+1+N9*(I8-1),N+2+N9*(I8-1)) =  (DN-1.D0)*H1/DTH/2.D0        03520000
         A(N+1+N9*(I8-1),N  +N9*(I8-1)) = -(DN-1.D0)*H1/DTH/2.D0        03530000
  510   CONTINUE                                                        03540000
C                                                                       03550000
C                                                                       03560000
        DO 550 I=1,N9*(I8-1)+1,1                                        03570000
        DO 550 N=1,N9*(I8-1)+1,1                                        03580000
           AGPOT(I,N)=A(I,N)                                            03590000
  550   CONTINUE                                                        03600000
        DO 560 I=1,  N9*(I8-1)+1,1                                      03610000
           A(I,I)=A(I,I) - 1.D0                                         03620000
  560   CONTINUE                                                        03630000
C                                                                       03640000
C       ***************************************************             03650000
C       ****   AUFSTELLEN DER MATRIX BL(  ) FUER RHON  ****             03660000
C       ***************************************************             03670000
C                                                                       03680000
        DO 580 N=0,  N9+1,1                                             03690000
           FB(N)=C                                                      03700000
  580   CONTINUE                                                        03710000
C       >>>>>>>>  RANDPUNKT DARUEBER(MITTE,LINKS,RECHTS) <<<<<<<        03720000
        I=I9-1                                                          03730000
        DO 590 N=1,    N9 ,1                                            03740000
           H1=( B1(N)*DR2R(I)*TR4(I) + B2(N)*DR1R(I)*TR3(I) ) *FB(N)    03750000
           H1=H1+ B4(N)*DR1R(I)*TR3(I)/2.D0/DTH * FB(N+1)               03760000
           H1=H1- B4(N)*DR1R(I)*TR3(I)/2.D0/DTH * FB(N-1)               03770000
           BL(N+1+N9*(I-1))=-H1                                         03780000
  590   CONTINUE                                                        03790000
C                                                                       03800000
        CALL DLAX(A,201,  N9*(I9-1)+1,BL,EPSZ,ISW,IS,VW,IP,ICON)        03810000
C       **** ABBRUCH BEI ICON > 0 *****                                 03820000
        IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                       03830000
        IF (ICON .GT. 0) GOTO 9000                                      03840000
   50   FORMAT (1X,'ICON = ',I8,'  STEP = ',I8)                         03850000
C                                                                       03860000
        DO 600 I=1,  N9*(I9-1)+1,1                                      03870000
           RHON(I)=BL(I)                                                03880000
           BL(I)=0.D0                                                   03890000
  600   CONTINUE                                                        03900000
C                                                                       03910000
C       ***************************************************             03920000
C       ****   AUFSTELLEN DER MATRIX BL(  ) FUER GPOT  ****             03930000
C       ***************************************************             03940000
C                                                                       03950000
        DO 610 N=0,  N9+1,1                                             03960000
           FB(N)=-GAM(N)*KAPPA(N)                                       03970000
  610   CONTINUE                                                        03980000
C                                                                       03990000
C       >>>>>>>>  RANDPUNKT DARUEBER(MITTE,LINKS,RECHTS) <<<<<<<        04000000
        I=I8-1                                                          04010000
        DO 620 N=1,    N9 ,1                                            04020000
           H1=( A1(N)*DR2R(I) + A2(N)*DR1R(I)/R(I) ) *FB(N)             04030000
           H1=H1+ A4(N)*DR1R(I)/2./DTH/R(I) * FB(N+1)                   04040000
           H1=H1- A4(N)*DR1R(I)/2./DTH/R(I) * FB(N-1)                   04050000
           BL(N+1+N9*(I-1))=-H1                                         04060000
  620   CONTINUE                                                        04070000
C                                                                       04080000
        DO 630 I=1,N9*(I8-1)+1 , 1                                      04090000
           BL(I)=BL(I)+(RHON(I)-1.D0)/RHOA                              04100000
  630   CONTINUE                                                        04110000
C                                                                       04120000
        CALL DLAX(AGPOT,201,  N9*(I8-1)+1,BL,EPSZ,ISW,IS,VW,IP,ICON)    04130000
C       **** ABBRUCH BEI ICON > 0 *****                                 04140000
        IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                       04150000
        IF (ICON .GT. 0) GOTO 9000                                      04160000
C                                                                       04170000
        DO 640 I=1,  N9*(I8-1)+1,1                                      04180000
           GPOT(I)=BL(I)                                                04190000
  640   CONTINUE                                                        04200000
C                                                                       04210000
C                                                                       04220000
C       ******************************************************          04230000
C       ****         BEWEGUNG DES RANDES                  ****          04240000
C       ******************************************************          04250000
C                                                                       04260000
        DO 900 N=1,   N9, 1                                             04270000
           H3=-GAM(N)*KAPPA(N)                                          04280000
           H1=GPOT(N+1+N9*(I8-2))                                       04290000
           H2=GPOT(N+1+N9*(I8-3))                                       04300000
           GPAR(N+1+N9*(I8-1))=H3*DR1GL0+H1*DR1GL1+H2*DR1GL2            04310000
           BPKT(N)= GPAR(N+1+N9*(I8-1)) * G(N)/B(N)/B(N)/B(N)           04320000
           GPATH(N+1+N9*(I8-1)) =-( GAM(N+1)*KAPPA(N+1)                 04330000
     *                           -GAM(N-1)*KAPPA(N-1) )/2.D0/DTH        04340000
           BPKT(N)= BPKT(N) - GPATH(N+1+N9*(I8-1)) *BA1(N)/B(N)/B(N)    04350000
  900   CONTINUE                                                        04360000
C       >>>>> SOLLTE DIE BEWEGUNG DES RANDES ZU SCHNELL WERDEN,         04370000
C       >>>>> SO WIRD DELT = DELTA T  HALBIERT.                         04380000
        DO 910 I=1, N9,1                                                04390000
  909      IF (DABS(BPKT(I)*DELT) .GT. DABS(B(I)/ 100.D0))  THEN        04400000
              DELT=DELT/2.D0                                            04410000
              GOTO 909                                                  04420000
           ENDIF                                                        04430000
  910   CONTINUE                                                        04440000
C                                                                       04450000
C       >>>> BERECHNEN DES GESCHWINDIGKEITSFELDES IM INNERN <<<<<       04460000
C       >>>> (NUR BEIM ABSPEICHERN DER DATEN) <<<<                      04470000
   40   FORMAT (1X,'STEP  = ',I8,15X,'  T     = ',D23.16,'  DELT  = ',  04480000
     *          D23.16)                                                 04490000
   41   FORMAT (1X,I3,2X,I3,4(2X,D23.16))                               04500000
   42   FORMAT (1X,I3,2(2X,D23.16))                                     04510000
        IF (COUN9 .EQ. CPRIN) THEN                                      04520000
           WRITE (22,40) COUNT,T9,DELT                                  04530000
           WRITE (22,41) 0,0,RHON(1),GPOT(1),0.D0,0.D0                  04540000
           DO 911 I=1,I8-1,1                                            04550000
           DO 911 N=1,N9  ,1                                            04560000
              H3=GPOT(N+1+N9*(I-1))                                     04570000
              H1=GPOT( 1  )                                             04580000
              H2=-GAM(N)*KAPPA(N)                                       04590000
              IF (I .GT. 1)     H1=GPOT(N+1+N9*(I-2))                   04600000
              IF (I .LT. I8-1)  H2=GPOT(N+1+N9*I)                       04610000
              GPAR(N+1+N9*(I-1))=  H3*DR1(I)+H1*DR1L(I)+H2*DR1R(I)      04620000
  911      CONTINUE                                                     04630000
           DO 912 I=1,I8-1,1                                            04640000
              GPATH(2+N9*(I-1))=0.D0                                    04650000
              GPATH(N9*I +1   )=0.D0                                    04660000
           DO 912 N=2,N9-1,1                                            04670000
              H1=GPOT(N  +N9*(I-1))                                     04680000
              H2=GPOT(N+2+N9*(I-1))                                     04690000
              GPATH(N+1+N9*(I-1)) = (H2-H1)/2.D0/DTH                    04700000
  912      CONTINUE                                                     04710000
           DO 913 I=1,I8-1,1                                            04720000
           DO 913 N=1,N9,1                                              04730000
              K=N+1+N9*(I-1)                                            04740000
              WRITE (22,41) I,N,RHON(K),GPOT(K),GPAR(K)/B(N),           04750000
     *                 (GPATH(K)-GPAR(K)*BA1(N)*R(I)/B(N)) /B(N)/R(I)   04760000
  913      CONTINUE                                                     04770000
           DO 914 N=1,N9,1                                              04780000
              K=N+1+N9*(I8-1)                                           04790000
              WRITE (22,41) I8,N,RHON(K),-GAM(N)*KAPPA(N),GPAR(K)/B(N), 04800000
     *                      (GPATH(K)-GPAR(K)*BA1(N)/B(N)) /B(N)        04810000
  914      CONTINUE                                                     04820000
           DO 915 I=I8+1,I9-1,1                                         04830000
           DO 915 N=1,N9,1                                              04840000
              K=N+1+N9*(I-1)                                            04850000
              WRITE (22,41) I,N,RHON(K),0.D0,0.D0,0.D0                  04860000
  915      CONTINUE                                                     04870000
           DO 916 N=1,N9,1                                              04880000
              K=N+1+N9*(I9-1)                                           04890000
              WRITE (22,41) I9,N,C,0.D0,0.D0,0.D0                       04900000
  916      CONTINUE                                                     04910000
           DO 918 N=1,N9,1                                              04920000
              WRITE (22,42) N,B(N),BPKT(N)                              04930000
  918      CONTINUE                                                     04940000
        ENDIF                                                           04950000
C       >>>>> BERECHNEN DER NEUEN RANDPOSITIONEN <<<<<                  04960000
        DO 920 I=1,  N9,1                                               04970000
           B(I)=B(I)+BPKT(I)*DELT                                       04980000
  920   CONTINUE                                                        04990000
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   05000000
        IF (COUNT .EQ. CEND) GOTO 9000                                  05010000
        GOTO 2000                                                       05020000
C                                                                       05030000
 9000 CONTINUE                                                          05040000
      CLOSE (22,STATUS='KEEP')                                          05050000
      END                                                               05060000
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              05070000
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           05080000
//GO.SYSIN DD *                                                         05090000
/*                                                                      05100000
/*      EXEC DRUCKE                                                     05110000
/*      END                                                             05120000
