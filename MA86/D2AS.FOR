C/D14KD    JOB RACF,CLASS=D,                                            00010009
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020009
C/E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030009
C/FORT.SYSIN    DD *                                                    00040009
C                                                                       00050008
C       ##################################################              00060012
C       ####   MODELL: MA86                           ####              00070012
C       ####   NAME DES PROGRAMMS:  MA86(D2AS)        ####              00080012
C       ####      LAPLACE(RHON) = RHON                ####              00090012
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00100012
C       ####      R.B. : RHON = C                     ####              00110012
C       ####           : PHIA = - GAMMA * KAPPA       ####              00120012
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00130012
C       ####      VA    =  GRAD( PHIA )               ####              00140012
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00150012
C       ##################################################              00160012
C      GERECHNET MIT AXIALER SYMMETRIE (VIERTEL-KREIS)                  00170000
C      UND VARIABLEM OBERFLAECHENSPANNUNGS-KOEFFIZIENT.                 00180008
C      DIE DATEN WERDEN IN DER DATEI  FNW  ABGESPEICHERT.               00190008
C        GRAPHIK-PROGRAMM:       MA86(PIC2AS)                           00200012
C                                                                       00210008
      INTEGER           I9,N9,I,N,ICNTL,COUNT,COUN9,CPRIN,K             00220008
      INTEGER           CPUSEC                                          00230009
      DOUBLE PRECISION  GAMMA,DFI,C,DELT,T9                             00240008
      DOUBLE PRECISION  B(0:41),BPKT(0:41),BSTRT(0:41),BST(0:12)        00250008
      DOUBLE PRECISION  BA1(0:41),BA2(0:41),G(0:41),WG(0:41)            00260008
      DOUBLE PRECISION  A1(0:41),A2(0:41),A3(0:41),A4(0:41)             00270008
      DOUBLE PRECISION  KAN(0:41),KAPPA(0:41),GAM(0:41)                 00280008
      DOUBLE PRECISION  X0(0:41),Y0(0:41),DS(41),DS2R(40)               00290000
      DOUBLE PRECISION  DS1(40),DS1L(40),DS1R(40),DS2(40),DS2L(40)      00300008
      DOUBLE PRECISION  R(21),DR(21),DR1(21),DR2(21)                    00310008
      DOUBLE PRECISION  PI,H1,H2,H3,X1,X2,Y1,Y2                         00320008
      DOUBLE PRECISION  FB(0:41)                                        00330008
C                                                                       00340008
      DOUBLE PRECISION  RHON(401),GPOT(401)                             00350008
      DOUBLE PRECISION  GPAR(401),GPAFI(401)                            00360008
      DOUBLE PRECISION  RHOA                                            00370008
C                                                                       00380008
      DOUBLE PRECISION  A(401,401),BL(401)                              00390000
      DOUBLE PRECISION  VW(401),IP(401)                                 00400008
      DOUBLE PRECISION  EPSZ,IS                                         00410008
      INTEGER           ICON,ISW                                        00420008
      CHARACTER         FNW*30                                          00430008
C                                                                       00440008
C     **************************************************                00450008
C     ****      PARAMETER PARAMETER PARAMETER       ****                00460008
C     **************************************************                00470008
C                                                                       00480008
      FNW ='D14K.TES1.DATA'                                             00490009
      GAMMA     =    12.40000D0 + 3.0D0 / 4.0D0 / 4.0D0                 00500011
      C         =     3.262500D0                                        00510011
      RHOA      =     1.00000D0                                         00520009
      N9        =    10                                                 00530008
      I9        =     9                                                 00540009
      R(0)      =      .000D0                                           00550008
      R(1)      =      .200D0                                           00560009
      R(2)      =      .400D0                                           00570009
      R(3)      =      .600D0                                           00580009
      R(4)      =      .700D0                                           00590009
      R(5)      =      .800D0                                           00600009
      R(6)      =      .900D0                                           00610009
      R(I9-2)   =      .950D0                                           00620008
      R(I9-1)   =      .980D0                                           00630008
      R(I9)     =     1.000D0                                           00640009
      DELT      =     0.0400D0                                          00650011
      BST(0)    =     5.99999300D0 +0.01D0                              00660011
      BST(2)    =     0.8000D0                                          00670011
      BST(4)    =     0.0000D0                                          00680008
      CEND      =  4000                                                 00690009
      CPRIN     =   200                                                 00700011
      EPSZ      =     1.D-60                                            00710000
      ISW       =     1                                                 00720008
C                                                                       00730009
C     ********  PARAMETERTABELLE (ANALYTISCH) ( RHOA = 1.0 ) *******    00740011
C     B0        C (TAU)   GAMMA2    GAMMA3     4*ALPHA2                 00750009
C      3.0      1.8519     0.9481                0.11477                00760009
C      3.5      2.0806     1.7616                0.01370                00770009
C      4.0      2.3161     2.9525               -0.75306                00780009
C      5.0      2.7984     6.7280               -0.22015                00790009
C      6.0      3.2882    12.7771               -0.?????                00800011
C                                                                       00810008
C                                                                       00820011
C     ********  PARAMETERTABELLE (NUMERISCH ) ( RHOA = 1.0 ) *******    00830011
C      R(0)=0.00 ** R(1)=0.20 ** R(2)=0.40 ** R(3)=0.60 ** R(4)=0.7     00840011
C      R(5)=0.80 ** R(6)=0.90 ** R(7)=0.95 ** R(8)=0.98 ** R(9)=1.0     00850011
C     B0        C (TAU)   GAMMA2    GAMMA3     4*ALPHA2(NUMERISCH!)     00860011
C      4.0      2.303072   2.8750                ?.?????                00870011
C      6.0      3.2625    12.4                   POSITIV !!!!!          00880011
C                                                                       00890011
C     ***********  RECHENZEITEN - TABELLE  **************************   00900009
C     N9        I9        #ZEITSCHRITTE  CPU-ZEIT(SEK)                  00910009
C     10         9           1000          318                          00920009
C                                                                       00930009
      H1=1.0D0                                                          00940008
      PI=4.0D0*DATAN(H1)                                                00950008
      H1=N9-1.D0                                                        00960008
      DFI=.5D0 * PI/H1                                                  00970008
      DO 100 N=1,  N9   ,1                                              00980008
         H2=N-1.D0                                                      00990008
         BSTRT(N)=BST(0)+BST(2)*DCOS(DFI*H2*2.D0)                       01000008
     *                  +BST(4)*DCOS(DFI*H2*4.D0)                       01010000
         GAM(N)=GAMMA                                                   01020008
  100 CONTINUE                                                          01030008
C     GAM(N9)=.04D0                                                     01040008
C     GAM(N9-1)=.03D0                                                   01050008
      GAM(N9+1)=GAM(N9-1)                                               01060008
      GAM(0   )=GAM(2   )                                               01070008
C     BSTRT(1)=BSTRT(1)-.1000D+00                                       01080008
C                                                                       01090008
      OPEN (22,FILE=FNW)                                                01100008
      REWIND 22                                                         01110000
C                                                                       01120008
   30 FORMAT (1X,'MA86(D2AS) AXIALE SYMMETRIE - 2 DIMENSIONAL')         01130008
   31 FORMAT (1X,'GAMMA = ',D23.16,'  RHOA  = ',D23.16,'  C     = ',    01140008
     *        D23.16)                                                   01150008
   32 FORMAT (1X,'N9    = ',I3,20X,'  I9    = ',I3,20X,'  DFI   = ',    01160008
     *        D23.16)                                                   01170008
   35 FORMAT (1X,I3,2X,D23.16,2X,D23.16)                                01180008
      WRITE (22,30)                                                     01190008
      WRITE (22,31) GAMMA,RHOA,C                                        01200008
      WRITE (22,32) N9,I9,DFI                                           01210000
      WRITE (22,35) 0,0.D0                                              01220008
      DO 130 I=1,  I9,1                                                 01230008
         DR(I)=R(I)-R(I-1)                                              01240008
         WRITE (22,35) I,R(I),DR(I)                                     01250008
  130 CONTINUE                                                          01260008
      DO 136 I=1,  I9-1,1                                               01270008
         DR1(I)=1.D0/DR(I)/(DR(I+1)+DR(I))                              01280008
         DR2(I)=1.D0/DR(I+1)/(DR(I+1)+DR(I))                            01290008
  136 CONTINUE                                                          01300008
      DO 140 N=1,    N9   ,1                                            01310000
         B(N)=BSTRT(N)                                                  01320008
  140 CONTINUE                                                          01330008
      ICNTL=0                                                           01340008
      COUNT=-1                                                          01350008
      COUN9=CPRIN-1                                                     01360008
      T9   =-DELT                                                       01370008
C                                                                       01380008
C     >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<            01390008
C     >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<            01400008
C     >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<            01410000
C                                                                       01420008
 2000 CONTINUE                                                          01430008
      COUNT=COUNT+1                                                     01440008
      COUN9=COUN9+1                                                     01450008
      T9   =T9   +DELT                                                  01460008
C     ******************************************************            01470008
C     ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****            01480008
C     ******************************************************            01490008
C                                                                       01500008
      B(0)     =B(2)                                                    01510000
      B(  N9+1)=B(N9-1)                                                 01520008
      DO 220 N=1,    N9,1                                               01530008
         BA1(N)=(B(N+1)-B(N-1))/2.D0/DFI                                01540008
         BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DFI/DFI                       01550008
         KAN(N)=B(N)*B(N)+2.D0*BA1(N)*BA1(N)-B(N)*BA2(N)                01560008
         G(N)  =B(N)*B(N)+BA1(N)*BA1(N)                                 01570008
         WG(N) =DSQRT(G(N))                                             01580008
         KAPPA(N)=KAN(N)/G(N)/WG(N)                                     01590008
         H1=B(N)*B(N)                                                   01600008
         A1(N)=G(N)/H1/H1                                               01610000
         A2(N)=KAN(N)/H1/H1                                             01620008
         A3(N)=1.D0/H1                                                  01630008
         A4(N)=-2.D0*BA1(N)/H1/B(N)                                     01640008
  220 CONTINUE                                                          01650008
      KAPPA(  N9+1)=KAPPA(N9-1)                                         01660008
      KAPPA(0)     =KAPPA(2)                                            01670008
C                                                                       01680008
C     BERECHNUNG VON KAPPA MIT HILFE DER BOGENLAENGE                    01690008
      DO 230 N=1,N9,1                                                   01700008
         H1=N-1                                                         01710000
         X0(N)=B(N)*DCOS(H1*DFI)                                        01720008
         Y0(N)=B(N)*DSIN(H1*DFI)                                        01730008
  230 CONTINUE                                                          01740008
      X0(0)   = X0(2)                                                   01750008
      Y0(0)   =-Y0(2)                                                   01760008
      X0(N9+1)=-X0(N9-1)                                                01770008
      Y0(N9+1)= Y0(N9-1)                                                01780008
      DO 240 N=1,N9+1,1                                                 01790008
         H1=X0(N)-X0(N-1)                                               01800008
         H2=Y0(N)-Y0(N-1)                                               01810000
         DS(N)=DSQRT(H1*H1+H2*H2)                                       01820008
  240 CONTINUE                                                          01830008
      DO 250 N=1,N9,1                                                   01840008
         H1=DS(N)                                                       01850008
         H2=DS(N+1)                                                     01860008
         DS2 (N)= -2.D0/H1/H2                                           01870008
         DS2L(N)=  2.D0/H1/(H1+H2)                                      01880008
         DS2R(N)=  2.D0/H2/(H1+H2)                                      01890008
         DS1R(N)=    H1/H2/(H1+H2)                                      01900008
         DS1L(N)=   -H2/H1/(H1+H2)                                      01910000
         DS1 (N)= -(DS1L(N)+DS1R(N))                                    01920008
  250 CONTINUE                                                          01930008
      DO 260 N=1,N9,1                                                   01940008
         X1=DS1L(N)*X0(N-1)+DS1(N)*X0(N)+DS1R(N)*X0(N+1)                01950008
         X2=DS2L(N)*X0(N-1)+DS2(N)*X0(N)+DS2R(N)*X0(N+1)                01960008
         Y1=DS1L(N)*Y0(N-1)+DS1(N)*Y0(N)+DS1R(N)*Y0(N+1)                01970008
         Y2=DS2L(N)*Y0(N-1)+DS2(N)*Y0(N)+DS2R(N)*Y0(N+1)                01980008
         H1=X1*X1+Y1*Y1                                                 01990008
         KAPPA(N)= (X1*Y2-X2*Y1)/H1/DSQRT(H1)                           02000008
  260 CONTINUE                                                          02010000
      KAPPA(  N9+1)=KAPPA(N9-1)                                         02020008
      KAPPA(0)     =KAPPA(2)                                            02030008
C                                                                       02040008
C     ***************************************************               02050008
C     ****   AUFSTELLEN DER MATRIX A( , )            ****               02060008
C     ***************************************************               02070008
C                                                                       02080008
 2100 CONTINUE                                                          02090008
      ICNTL=ICNTL+1                                                     02100008
      DO 300 I=1,  N9*(I9-1)+1,1                                        02110000
         BL(I)=0.D0                                                     02120008
      DO 300 N=1,  N9*(I9-1)+1,1                                        02130008
         A(I,N)=0.D0                                                    02140008
  300 CONTINUE                                                          02150008
C     >>>>>>>>>>>MITTLERER PUNKT<<<<<<<<<<                              02160008
      DO 320 I=1,  I9-1,1                                               02170008
      DO 320 N=1,    N9,1                                               02180008
         A(N+  N9*(I-1),N+  N9*(I-1))=-A1(N)*2.D0*(DR1(I)+DR2(I))+A2(N) 02190008
     *  *(DR(I+1)*DR1(I)-DR(I)*DR2(I))/R(I)-A3(N)*2.D0/DFI/DFI/R(I)/R(I)02200008
  320 CONTINUE                                                          02210000
C     >>>>>>>>>>>PUNKT DARUEBER<<<<<<<<<<                               02220008
      DO 340 I=1,  I9-2 ,1                                              02230008
      DO 340 N=1,    N9 ,1                                              02240008
         A(N+  N9*(I-1),N+  N9*(I  ))= A1(N)*2.D0* DR2(I)               02250008
     *    +A2(N)*(DR(I)*DR2(I))/R(I)                                    02260008
  340 CONTINUE                                                          02270008
C     >>>>>>>>>>>PUNKT DARUNTER<<<<<<<<<<                               02280008
      DO 360 I=2,  I9-1,1                                               02290008
      DO 360 N=1,    N9,1                                               02300008
         A(N+  N9*(I-1),N+  N9*(I-2))= A1(N)*2.D0* DR1(I)               02310000
     *    -A2(N)*(DR(I+1)*DR1(I))/R(I)                                  02320008
  360 CONTINUE                                                          02330008
      DO 370 N=1,    N9,1                                               02340008
         A(N ,   N9*(I9-1)+1)=A1(N)*2.D0*DR1(1)-A2(N)*DR(2)*DR1(1)/R(1) 02350008
  370 CONTINUE                                                          02360008
C     >>>>>>>>>>>PUNKT LINKS  OBEN<<<<<<<<<<                            02370008
      DO 400 I=1,  I9-2 ,1                                              02380008
      DO 400 N=2,  N9-1,1                                               02390008
         A(N+  N9*(I-1),N+1+  N9*(I))= A4(N)*DR(I)*DR2(I)/2.D0/DFI/R(I) 02400008
  400 CONTINUE                                                          02410000
C     >>>>>>>>>>>PUNKT LINKS UNTEN<<<<<<<<<<                            02420008
      DO 420 I=2,  I9-1,1                                               02430008
      DO 420 N=2,    N9-1,1                                             02440008
         A(N+  N9*(I-1),N+1+  N9*(I-2))=-A4(N)*DR(I+1)*DR1(I)/2.D0/DFI  02450008
     *                                /R(I)                             02460008
  420 CONTINUE                                                          02470008
C     >>>>>>>>>>>PUNKT RECHTS OBEN<<<<<<<<<<                            02480008
      DO 380 I=1,  I9-2,1                                               02490008
      DO 380 N=2,N9-1  ,1                                               02500008
         A(N+  N9*(I-1),N-1+  N9*(I))=-A4(N)*DR(I)*DR2(I)/2.D0/DFI/R(I) 02510000
  380 CONTINUE                                                          02520008
C     >>>>>>>>>>>PUNKT RECHTS UNTEN<<<<<<<<<                            02530008
      DO 440 I=2,  I9-1,1                                               02540008
      DO 440 N=2, N9-1 ,1                                               02550008
         A(N+  N9*(I-1),N-1+  N9*(I-2))= A4(N)*DR(I+1)*DR1(I)/2.D0/DFI  02560008
     *                                 /R(I)                            02570008
  440 CONTINUE                                                          02580008
C     >>>>>>>>>>> PUNKT RECHTS  <<<<<<<<<<                              02590008
      DO 460 I=1,  I9-1,1                                               02600008
         A(1+  N9*(I-1), 2 +  N9*(I-1))= 2.D0* A3(1)/DFI/DFI/R(I)/R(I)  02610000
      DO 460 N=2, N9-1 ,1                                               02620008
         A(N+  N9*(I-1),N-1+  N9*(I-1))= A3(N)/DFI/DFI/R(I)/R(I)        02630008
     *            - A4(N)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2.D0/DFI/R(I)   02640008
  460 CONTINUE                                                          02650008
C     >>>>>>>>>>> PUNKT LINKS   <<<<<<<<<<                              02660008
      DO 480 I=1,  I9-1,1                                               02670008
         A(  N9*I   , N9*I -1      )=2.D0* A3(  N9)/DFI/DFI/R(I)/R(I)   02680008
      DO 480 N=2,    N9-1 ,1                                            02690008
         A(N+  N9*(I-1),N+1+  N9*(I-1))= A3(N)/DFI/DFI/R(I)/R(I)        02700008
     *            + A4(N)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2.D0/DFI/R(I)   02710000
  480 CONTINUE                                                          02720008
C     >>>>>>>>>>>  NULLPUNKT  <<<<<<<<<<<<<                             02730008
C     H2=N9-1                                                           02740008
      H2=1.D0                                                           02750008
C     DO 500 N=2, N9-1, 1                                               02760008
C        A(  N9*(I9-1)+1,N    )=4./DR(1)/DR(1)/ B(N) /H2/B(N)           02770008
C 500 CONTINUE                                                          02780008
      A(  N9*(I9-1)+1,1     )=2.D0/DR(1)/DR(1)/B(1)/B(1)/H2             02790008
      A(  N9*(I9-1)+1,N9    )=2.D0/DR(1)/DR(1)/B(N9)/B(N9)/H2           02800008
      H1=0.0D0                                                          02810000
C     DO 540 N=2,N9-1,1                                                 02820008
C        H1=H1- 4./B(N )/ B(N )                                         02830008
C 540 CONTINUE                                                          02840008
      H1=H1-2.D0/B(1)/B(1) -2.D0/B(N9)/B(N9)                            02850008
      A(  N9*(I9-1)+1,  N9*(I9-1)+1)=H1/H2/DR(1)/DR(1)                  02860008
C                                                                       02870008
      DO 560 I=1,  N9*(I9-1)+1,1                                        02880008
         IF (ICNTL .EQ. 1)   A(I,I)=A(I,I) - 1.D0                       02890008
  560 CONTINUE                                                          02900008
C     ***************************************************               02910000
C     ****   AUFSTELLEN DER MATRIX BL(  )            ****               02920008
C     ***************************************************               02930008
C                                                                       02940008
      DO 580 N=0,  N9+1,1                                               02950008
         IF (ICNTL .EQ. 1)   FB(N)=C                                    02960008
         IF (ICNTL .EQ. 2)   FB(N)=-GAM(N)*KAPPA(N)                     02970008
  580 CONTINUE                                                          02980008
C                                                                       02990008
C     >>>>>>>>  RANDPUNKT DARUEBER(MITTE,LINKS,RECHTS) <<<<<<<          03000008
      I=I9-1                                                            03010000
      DO 600 N=1,    N9 ,1                                              03020008
         H1=(A1(N)*2.D0* DR2(I)                                         03030008
     *    +A2(N)*(DR(I)*DR2(I))/R(I))   * FB(N)                         03040008
         H1=H1+ A4(N)*DR(I)*DR2(I)/2.D0/DFI/R(I) * FB(N+1)              03050008
         H1=H1- A4(N)*DR(I)*DR2(I)/2.D0/DFI/R(I) * FB(N-1)              03060008
         BL(N+  N9*(I-1))=-H1                                           03070008
  600 CONTINUE                                                          03080008
      DO 610 I=1,   N9*(I9-1)+1,1                                       03090008
         IF (ICNTL .EQ. 2)   BL(I)=BL(I)+(RHON(I)-1.D0)/RHOA            03100008
  610 CONTINUE                                                          03110000
C                                                                       03120008
C                                                                       03130008
C                                                                       03140008
      CALL DLAX(A,401,  N9*(I9-1)+1,BL,EPSZ,ISW,IS,VW,IP,ICON)          03150008
C     >>>>> ABBRUCH BEI ICON > 0  <<<<<<                                03160008
      IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                         03170008
      IF (ICON .GT. 0) GOTO 9000                                        03180008
   50 FORMAT (1X,'ICON = ',I8,'  STEP = ',I8)                           03190008
C                                                                       03200008
      DO 820 I=1,  N9*(I9-1)+1,1                                        03210000
         IF (ICNTL .EQ. 1)   RHON(I)=BL(I)                              03220008
         IF (ICNTL .EQ. 2)   GPOT(I)=BL(I)                              03230008
  820 CONTINUE                                                          03240008
      IF (ICNTL. EQ. 1) GOTO 2100                                       03250008
C                                                                       03260008
C                                                                       03270008
C     ******************************************************            03280008
C     ****         BEWEGUNG DES RANDES          ****                    03290008
C     ******************************************************            03300008
C                                                                       03310000
      DO 900 N=1,   N9, 1                                               03320008
         H3=-GAM(N)*KAPPA(N)                                            03330008
         H1=GPOT(N+  N9*(I9-2))                                         03340008
         H2=GPOT(N+  N9*(I9-3))                                         03350008
         GPAR(N+N9*(I9-1))=(H3-H1)*(DR(I9)+DR(I9-1))/DR(I9)/DR(I9-1)    03360008
     *     -(H3-H2)*DR(I9)/DR(I9-1)/(DR(I9)+DR(I9-1))                   03370008
         BPKT(N)= GPAR(N+N9*(I9-1)) * G(N)/B(N)/B(N)/B(N)               03380008
         GPAFI(N+N9*(I9-1)) = - ( GAM(N+1)*KAPPA(N+1)                   03390008
     *                           -GAM(N-1)*KAPPA(N-1) )/2.D0/DFI        03400008
         BPKT(N)= BPKT(N) - GPAFI(N+N9*(I9-1)) *BA1(N)/B(N)/B(N)        03410000
  900 CONTINUE                                                          03420008
      DO 910 I=1, N9,1                                                  03430008
  909    IF (DABS(BPKT(I)*DELT) .GT. DABS(B(I)/100.))  DELT=DELT/2.D0   03440008
         IF (DABS(BPKT(I)*DELT) .GT. DABS(B(I)/100.))  GOTO 909         03450008
  910 CONTINUE                                                          03460008
C                                                                       03470008
   40 FORMAT (1X,'STEP  = ',I8,15X,'  T     = ',D23.16,'  DELT  = ',    03480008
     *        D23.16)                                                   03490008
   41 FORMAT (1X,I3,2X,I3,4(2X,D23.16))                                 03500008
   42 FORMAT (1X,I3,2(2X,D23.16))                                       03510000
   55 FORMAT (1X,'STEP  = ',I8,2X,'T = ',F10.6,'  DELT = ',             03520009
     *        F10.6,'  CPU-ZEIT(SEK) = ',I6)                            03530009
   51 FORMAT (1X,'N    B(N)        BPKT(N)     KAPPA(N)')               03540009
   52 FORMAT (1X,I3,5(2X,F10.6))                                        03550009
      IF (COUN9 .EQ. CPRIN) THEN                                        03560008
         WRITE (22,40) COUNT,T9,DELT                                    03570008
         K=N9*(I9-1)+1                                                  03580008
         WRITE (22,41) 0,0,RHON(K),GPOT(K),0.D0,0.D0                    03590008
         DO 911 I=1,I9-1,1                                              03600008
         DO 911 N=1,N9  ,1                                              03610008
            H3=GPOT(N+N9*(I-1))                                         03620008
            H1=GPOT(N9*(I9-1)+1)                                        03630008
            H2=-GAM(N)*KAPPA(N)                                         03640008
            IF (I .GT. 1)     H1=GPOT(N+N9*(I-2))                       03650000
            IF (I .LT. I9-1)  H2=GPOT(N+N9*I)                           03660008
            GPAR(N+N9*(I-1))=  (H2-H3)* DR(I)*DR2(I)                    03670008
     *                 +(H3-H1)*DR(I+1)*DR1(I)                          03680008
  911    CONTINUE                                                       03690008
         DO 912 I=1,I9-1,1                                              03700008
            GPAFI(1+N9*(I-1))=0.D0                                      03710008
            GPAFI(N9*I      )=0.D0                                      03720008
         DO 912 N=2,N9-1,1                                              03730008
            H1=GPOT(N-1+N9*(I-1))                                       03740008
            H2=GPOT(N+1+N9*(I-1))                                       03750000
            GPAFI(N+N9*(I-1)) = (H2-H1)/2.D0/DFI                        03760008
  912    CONTINUE                                                       03770008
         DO 913 I=1,I9-1,1                                              03780008
         DO 913 N=1,N9,1                                                03790008
            K=N+N9*(I-1)                                                03800008
            WRITE (22,41) I,N,RHON(K),GPOT(K),GPAR(K)/B(N),             03810008
     *               (GPAFI(K)-GPAR(K)*BA1(N)*R(I)/B(N)) /B(N)/R(I)     03820008
  913    CONTINUE                                                       03830008
         DO 914 N=1,N9,1                                                03840008
            K=N+N9*(I9-1)                                               03850000
            WRITE (22,41) I9,N,C,-GAM(N)*KAPPA(N),GPAR(K)/B(N),         03860008
     *                    (GPAFI(K)-GPAR(K)*BA1(N)/B(N)) /B(N)          03870008
  914    CONTINUE                                                       03880008
         CALL CLOCK(CPUSEC)                                             03890009
         WRITE ( 6,55) COUNT,T9,DELT,CPUSEC                             03900009
         WRITE ( 6,51)                                                  03910009
         DO 916 N=1,N9,1                                                03920008
            WRITE (22,42) N,B(N),BPKT(N)                                03930008
            WRITE ( 6,52) N,B(N),BPKT(N),KAPPA(N)                       03940009
  916    CONTINUE                                                       03950008
      ENDIF                                                             03960008
      DO 920 I=1,  N9,1                                                 03970008
         B(I)=B(I)+BPKT(I)*DELT                                         03980008
  920 CONTINUE                                                          03990000
      ICNTL=0                                                           04000008
      IF (COUN9 .EQ. CPRIN) COUN9=0                                     04010008
      IF (COUNT .EQ. CEND) GOTO 9000                                    04020008
      GOTO 2000                                                         04030008
C                                                                       04040008
 9000 CONTINUE                                                          04050008
      CLOSE (22,STATUS='KEEP')                                          04060008
C     END                                                               04070009
C/LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              04080009
C/              DD  DSN=SYS1.FORTLIB,DISP=SHR                           04090009
C/GO.SYSIN DD *                                                         04100009
C*                                                                      04110009
C*      EXEC DRUCKE                                                     04120009
C*      END                                                             04130009
      END                                                               04140009
