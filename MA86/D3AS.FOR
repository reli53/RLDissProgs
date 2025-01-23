//D14KF    JOB RACF,CLASS=F,                                            00010014
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020006
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030006
//FORT.SYSIN    DD *                                                    00040006
C                                                                       00050006
C       ##################################################              00060006
C       ####   MODELL: MA86                           ####              00070015
C       ####   NAME DES PROGRAMMS:  MA86(D3AS)        ####              00080006
C       ####      LAPLACE(RHON) = RHON                ####              00081015
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00082015
C       ####      R.B. : RHON = C                     ####              00083015
C       ####           : PHIA = - GAMMA * KAPPA       ####              00084015
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00085015
C       ####      VA    =  GRAD( PHIA )               ####              00086015
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00087015
C       ##################################################              00090006
C        GERECHNET WIRD IN KUGELKOORDINATEN MIT AZIMUTHALER             00100000
C        SYMMETRIE (UND SYMMETRIE BZGL. DER X-Y-EBENE).                 00110006
C        VARIABLER OBERFLAECHENSP.-KOEFFIZIENT IST MOEGLICH.            00120006
C        DATEN WERDEN IN DATA-FILE  FNW  ABGESPEICHERT.                 00130006
C        GRAPHIK-PROGRAMM:       MA86(PIC2AS)                           00131016
C                                                                       00140006
        INTEGER           I9,N9,I,N,ICNTL,COUNT,COUN9,CPRIN,K           00150006
        DOUBLE PRECISION  GAMMA,DTH,C,DELT,T9                           00160006
        DOUBLE PRECISION  B(0:21),BPKT(0:21),BSTRT(0:21),BST(0:12)      00170006
        DOUBLE PRECISION  BA1(0:21),BA2(0:21),G(0:21),WG                00180006
        DOUBLE PRECISION  A1(0:21),A2(0:21),A3(0:21),A4(0:21),A5(0:21)  00190006
        DOUBLE PRECISION  KAN,KAPPA(0:21),GAM(0:21)                     00200000
        DOUBLE PRECISION  R(0:21),DR(21),DR1(21),DR2(21)                00210006
        DOUBLE PRECISION  DR1L(21),DR1R(21),DR2L(21),DR2R(21)           00220006
        DOUBLE PRECISION  DR1GL0,DR1GL1,DR1GL2                          00230006
        DOUBLE PRECISION  PI,H1,H2,H3                                   00240006
        DOUBLE PRECISION  FB(0:21)                                      00250006
C                                                                       00260006
        DOUBLE PRECISION  R0(0:21),R1(20),R2(20)                        00270009
        DOUBLE PRECISION  Z0(0:21),Z1(20),Z2(20)                        00280009
        DOUBLE PRECISION  DS(1:21),DS2(20),DS1(20)                      00290009
        DOUBLE PRECISION  DS1R(20),DS1L(20),DS2R(20),DS2L(20)           00300009
C                                                                       00310009
        DOUBLE PRECISION  RHON(401),GPOT(401)                           00320006
        DOUBLE PRECISION  GPAR(401),GPATH(401)                          00330006
        DOUBLE PRECISION  RHOA                                          00340006
C                                                                       00350000
        DOUBLE PRECISION  A(401,401),BL(401)                            00360006
        DOUBLE PRECISION  VW(401),IP(401)                               00370006
        DOUBLE PRECISION  EPSZ,IS                                       00380006
        INTEGER           ICON,ISW                                      00390006
        CHARACTER         FNW*30                                        00400006
C                                                                       00410006
        DOUBLE PRECISION X,FNP2                                         00420006
        FNP2(X) = (3.D0*X*X -1.D0) * .5D0                               00430006
C                                                                       00440006
C       **************************************************              00450000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00460006
C       **************************************************              00470006
C                                                                       00480006
        FNW ='D14K.TES4.DATA'                                           00490013
        GAMMA     =     5.10000D0                                       00500014
        C         =     2.00000D0                                       00510006
        RHOA      =     1.00000D0                                       00520006
        N9        =    10                                               00530006
        I9        =     7                                               00540006
        R(0)      =      .0                                             00550000
        R(1)      =      .200D0                                         00560006
        R(2)      =      .400D0                                         00570006
        R(3)      =      .600D0                                         00580006
        R(4)      =      .800D0                                         00590006
        R(I9-2)   =      .950D0                                         00600006
        R(I9-1)   =      .980D0                                         00610006
        R(I9)     =     1.000D0                                         00620006
        DELT      =     4.000D0                                         00630006
        BST(0)    =     4.8700D0                                        00640006
        BST(2)    =     0.1000D0                                        00650013
        CEND      = 12000                                               00660014
        CPRIN     =  1000                                               00670006
        EPSZ      =     1.D-60                                          00680006
        ISW       =     1                                               00690006
C                                                                       00700006
C    >>>>> BERECHNEN DER STARTPOSITION                                  00710006
        H1=1.0D0                                                        00720006
        PI=4.0D0*DATAN(H1)                                              00730006
        H1=N9-1                                                         00740006
        DTH=.5D0 * PI/H1                                                00750000
        DO 100 N=1,  N9   ,1                                            00760006
           H2=N-1                                                       00770006
           BSTRT(N)=BST(0)+BST(2) * FNP2( DCOS(DTH*H2) )                00780006
           GAM(N)=GAMMA                                                 00790006
  100   CONTINUE                                                        00800006
        GAM(N9+1 )=GAM(N9-1)                                            00810006
        GAM(0    )=GAM(2   )                                            00820006
C                                                                       00830006
        OPEN (22,FILE=FNW)                                              00840006
        REWIND 22                                                       00850000
C                                                                       00860006
   30   FORMAT (1X,'MA86(D3AS) AZIMUTH. SYMMETRIE - 3 DIMENSIONAL')     00870006
   31   FORMAT (1X,'GAMMA = ',D23.16,'  RHOA  = ',D23.16,'  C     = ',  00880006
     *          D23.16)                                                 00890006
   32   FORMAT (1X,'N9    = ',I3,20X,'  I9    = ',I3,20X,'  DTH   = ',  00900006
     *          D23.16)                                                 00910006
   35   FORMAT (1X,I3,2X,D23.16,2X,D23.16)                              00920006
        WRITE (22,30)                                                   00930006
        WRITE (22,31) GAMMA,RHOA,C                                      00940006
        WRITE (22,32) N9,I9,DTH                                         00950000
        WRITE (22,35) 0,0.                                              00960006
        DO 130 I=1,  I9,1                                               00970006
           DR(I)=R(I)-R(I-1)                                            00980006
           WRITE (22,35) I,R(I),DR(I)                                   00990006
  130   CONTINUE                                                        01000006
        DO 136 I=1,  I9-1,1                                             01010006
           H1=DR(I)                                                     01020006
           H2=DR(I+1)                                                   01030006
           DR2 (I)=-2./H1/H2                                            01040006
           DR2L(I)= 2./H1/(H1+H2)                                       01050000
           DR2R(I)= 2./H2/(H1+H2)                                       01060006
           DR1R(I)= H1/H2/(H1+H2)                                       01070006
           DR1L(I)=-H2/H1/(H1+H2)                                       01080006
           DR1 (I)=-(DR1L(I)+DR1R(I))                                   01090006
  136   CONTINUE                                                        01100006
        DR1GL2=   DR(I9)/DR(I9-1)/( DR(I9)+DR(I9-1) )                   01110006
        DR1GL1=-( DR(I9)+DR(I9-1) ) / DR(I9) / DR(I9-1)                 01120006
        DR1GL0=-( DR1GL1+DR1GL2 )                                       01130006
        DO 140 N=1,    N9   ,1                                          01140006
           B(N)=BSTRT(N)                                                01150000
  140   CONTINUE                                                        01160006
        ICNTL=0                                                         01170006
        COUNT=-1                                                        01180006
        COUN9=CPRIN-1                                                   01190006
        T9   =-DELT                                                     01200006
C                                                                       01210006
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          01220006
C       >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<          01230006
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          01240006
C                                                                       01250000
 2000   CONTINUE                                                        01260006
        COUNT=COUNT+1                                                   01270006
        COUN9=COUN9+1                                                   01280006
        T9   =T9   +DELT                                                01290006
C       ******************************************************          01300006
C       ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****          01310006
C       ******************************************************          01320006
C                                                                       01330006
        B(0)     =B(2)                                                  01340006
        B(  N9+1)=B(N9-1)                                               01350000
C    >>>>>> N=1 ENTSPRICHT THETA=0. GRAD                                01360006
        N=1                                                             01370006
           BA1(N)=0.D0                                                  01380006
           BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DTH/DTH                     01390006
           KAN   =2.*B(N)*B(N)-2.*B(N)*BA2(N)                           01400006
           G(N)  =B(N)*B(N)                                             01410006
           WG    =DSQRT(G(N))                                           01420006
           KAPPA(N)=(-BA2(N)*B(N)+B(N)*B(N)) / G(N)/WG                  01430006
           H1=B(N)*B(N)                                                 01440006
           A1(N)=1./H1                                                  01450000
           A2(N)=KAN/H1/H1                                              01460006
           A3(N)=2./H1                                                  01470006
           A4(N)=0.D0                                                   01480006
           A5(N)=0.D0                                                   01490006
C    >>>>>> N=N9 ENTSPRICHT THETA=90. GRAD                              01500006
        DO 220 N=2,    N9,1                                             01510006
           H1= N-1                                                      01520006
           H1= H1*DTH                                                   01530006
           H2= DCOS(H1)/DSIN(H1)                                        01540006
           BA1(N)=(B(N+1)-B(N-1))/2./DTH                                01550000
           BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DTH/DTH                     01560006
           KAN   =2.*B(N)*B(N)+2.*BA1(N)*BA1(N)-B(N)*BA2(N)             01570006
     *                 -B(N)*BA1(N)*H2                                  01580006
           G(N)  =B(N)*B(N)+BA1(N)*BA1(N)                               01590006
           WG    =DSQRT(G(N))                                           01600006
           KAPPA(N)=.5D+00*(-BA2(N)*B(N)+3.*BA1(N)*BA1(N)+2.*B(N)*B(N)  01610006
     *                 -BA1(N)*G(N)*H2/B(N) ) /G(N)/WG                  01620006
           H1=B(N)*B(N)                                                 01630006
           A1(N)=G(N)/H1/H1                                             01640006
           A2(N)=KAN/H1/H1                                              01650000
           A3(N)=1./H1                                                  01660006
           A4(N)=-2.*BA1(N)/H1/B(N)                                     01670006
           A5(N)=H2/H1                                                  01680006
  220   CONTINUE                                                        01690006
C                                                                       01700009
C  >>>> BERECHNENUNG DER MITTLEREN KRUEMMUNG UEBER KURVENPARAMETER      01710009
C  >>>> DURCHLAUFEN DER KURVE ENTGEGEN DEM UHRZEIGERSINN                01720009
        DO 230 N=1,N9,1                                                 01730009
           H1=N-1                                                       01740009
           Z0(N)=B(N)*DCOS(H1*DTH)                                      01750009
           R0(N)=B(N)*DSIN(H1*DTH)                                      01760009
  230   CONTINUE                                                        01770009
        R0(0)=-R0(2)                                                    01780009
        Z0(0)= Z0(2)                                                    01790009
        R0(N9+1)= R0(N9-1)                                              01800009
        Z0(N9+1)=-Z0(N9-1)                                              01810009
        DO 240 N=1,N9+1,1                                               01820009
           H1=R0(N)-R0(N-1)                                             01830009
           H2=Z0(N)-Z0(N-1)                                             01840009
           DS(N)=DSQRT(H1*H1+H2*H2)                                     01850009
  240   CONTINUE                                                        01860009
        DO 250 N=1,N9,1                                                 01870009
           H1=DS(N  )                                                   01880009
           H2=DS(N+1)                                                   01890009
           DS2 (N)=-2./H1/H2                                            01900012
           DS2L(N)= 2./H1/(H1+H2)                                       01910012
           DS2R(N)= 2./H2/(H1+H2)                                       01920012
           DS1R(N)= H1/H2/(H1+H2)                                       01930012
           DS1L(N)=-H2/H1/(H1+H2)                                       01940012
           DS1 (N)=-(DS1L(N)+DS1R(N))                                   01950012
  250   CONTINUE                                                        01960009
        DO 260 N=1,N9,1                                                 01970009
           Z1(N)=DS1L(N)*Z0(N-1)+DS1(N)*Z0(N)+DS1R(N)*Z0(N+1)           01980009
           R1(N)=DS1L(N)*R0(N-1)+DS1(N)*R0(N)+DS1R(N)*R0(N+1)           01990009
           Z2(N)=DS2L(N)*Z0(N-1)+DS2(N)*Z0(N)+DS2R(N)*Z0(N+1)           02000009
           R2(N)=DS2L(N)*R0(N-1)+DS2(N)*R0(N)+DS2R(N)*R0(N+1)           02010009
           H1=Z1(N)*Z1(N) + R1(N)*R1(N)                                 02020014
           KAPPA(N)= ( Z1(N)*R2(N)-Z2(N)*R1(N) ) / H1 / DSQRT(H1)       02030014
  260   CONTINUE                                                        02040009
        DO 270 N=2,N9,1                                                 02050009
           H1=Z1(N)*Z1(N) + R1(N)*R1(N)                                 02060014
           KAPPA(N)=( KAPPA(N) - Z1(N)/R0(N)/DSQRT(H1) ) / 2.D0         02070014
  270   CONTINUE                                                        02080009
C                                                                       02090009
        KAPPA(  N9+1)=KAPPA(N9-1)                                       02100009
        KAPPA(0)     =KAPPA(2)                                          02110009
C                                                                       02120006
C       ***************************************************             02130006
C       ****   AUFSTELLEN DER MATRIX A( , )            ****             02140006
C       ***************************************************             02150006
C                                                                       02160006
 2100   CONTINUE                                                        02170006
        ICNTL=ICNTL+1                                                   02180006
        DO 300 I=1,  N9*(I9-1)+1,1                                      02190006
           BL(I)=0.                                                     02200006
        DO 300 N=1,  N9*(I9-1)+1,1                                      02210000
           A(I,N)=0.                                                    02220006
  300   CONTINUE                                                        02230006
C    >>>>>>>>>>>MITTLERER PUNKT                                         02240006
        DO 320 I=1,  I9-1,1                                             02250006
        DO 320 N=1,    N9,1                                             02260006
         A(N+  N9*(I-1),N+  N9*(I-1))= A1(N)*DR2(I)+A2(N)*DR1(I)/R(I)   02270006
     *              -A3(N)*2./DTH/DTH/R(I)/R(I)                         02280006
  320   CONTINUE                                                        02290006
C    >>>>>>>>>>>PUNKT DARUEBER                                          02300006
        DO 340 I=1,  I9-2 ,1                                            02310000
        DO 340 N=1,    N9 ,1                                            02320006
         A(N+  N9*(I-1),N+  N9*(I  ))= A1(N)*DR2R(I)                    02330006
     *              +A2(N)*DR1R(I)/R(I)                                 02340006
  340   CONTINUE                                                        02350006
C    >>>>>>>>>>>PUNKT DARUNTER                                          02360006
        DO 360 I=2,  I9-1,1                                             02370006
        DO 360 N=1,    N9,1                                             02380006
         A(N+  N9*(I-1),N+  N9*(I-2))= A1(N)*DR2L(I)                    02390006
     *              +A2(N)*DR1L(I)/R(I)                                 02400006
  360   CONTINUE                                                        02410000
        DO 370 N=1,    N9,1                                             02420006
         A(N ,   N9*(I9-1)+1)=A1(N)*DR2L(1)+A2(N)*DR1L(1)/R(1)          02430006
  370   CONTINUE                                                        02440006
C    >>>>>>>>>>>PUNKT LINKS  OBEN                                       02450006
        DO 400 I=1,  I9-2 ,1                                            02460006
        DO 400 N=2,  N9-1,1                                             02470006
         A(N+  N9*(I-1),N+1+  N9*(I  ))= A4(N)*DR1R(I)/2./DTH/R(I)      02480006
  400   CONTINUE                                                        02490006
C    >>>>>>>>>>>PUNKT LINKS UNTEN                                       02500006
        DO 420 I=2,  I9-1,1                                             02510000
        DO 420 N=2,    N9-1,1                                           02520006
         A(N+  N9*(I-1),N+1+  N9*(I-2))= A4(N)*DR1L(I)/2./DTH/R(I)      02530006
  420   CONTINUE                                                        02540006
C    >>>>>>>>>>>PUNKT RECHTS OBEN                                       02550006
        DO 380 I=1,  I9-2,1                                             02560006
        DO 380 N=2,N9-1  ,1                                             02570006
         A(N+  N9*(I-1),N-1+  N9*(I  ))=-A4(N)*DR1R(I)/2./DTH/R(I)      02580006
  380   CONTINUE                                                        02590006
C    >>>>>>>>>>>PUNKT RECHTS UNTEN                                      02600006
        DO 440 I=2,  I9-1,1                                             02610000
        DO 440 N=2, N9-1 ,1                                             02620006
         A(N+  N9*(I-1),N-1+  N9*(I-2))=-A4(N)*DR1L(I)/2./DTH/R(I)      02630006
  440   CONTINUE                                                        02640006
C    >>>>>>>>>>>PUNKT RECHTS                                            02650006
        DO 460 I=1,  I9-1,1                                             02660006
         A(  N9*I   , N9*I -1      )=2.* A3(  N9)/DTH/DTH/R(I)/R(I)     02670006
        DO 460 N=2, N9-1 ,1                                             02680006
         A(N+  N9*(I-1),N-1+  N9*(I-1))= A3(N)/DTH/DTH/R(I)/R(I)        02690006
     *            - A4(N)*DR1(I)/2./DTH/R(I)                            02700006
     *            - A5(N)/DTH/2./R(I)/R(I)                              02710000
  460   CONTINUE                                                        02720006
C    >>>>>>>>>>> PUNKT LINKS                                            02730006
        DO 480 I=1,  I9-1,1                                             02740006
         A(1+  N9*(I-1),1+1+  N9*(I-1))= 2.*A3(1)/DTH/DTH/R(I)/R(I)     02750006
        DO 480 N=2,    N9-1 ,1                                          02760006
         A(N+  N9*(I-1),N+1+  N9*(I-1))= A3(N)/DTH/DTH/R(I)/R(I)        02770006
     *            + A4(N)*DR1(I)/2./DTH/R(I)                            02780006
     *            + A5(N)/DTH/2./R(I)/R(I)                              02790006
  480   CONTINUE                                                        02800006
C     >>>>>>>>>>>  NULLPUNKT (IN KARTES. KOORDINA.)                     02810000
        A(  N9*(I9-1)+1,1     )=2./DR(1)/DR(1)/B(1)/B(1)                02820006
        A(  N9*(I9-1)+1,N9    )=4./DR(1)/DR(1)/B(N9)/B(N9)              02830006
        H1=0.0                                                          02840006
        H1=H1-2./B(1)/B(1) -4./B(N9)/B(N9)                              02850006
        A(  N9*(I9-1)+1,  N9*(I9-1)+1)=H1   /DR(1)/DR(1)                02860006
C                                                                       02870006
        DO 560 I=1,  N9*(I9-1)+1,1                                      02880006
         IF (ICNTL .EQ. 1)   A(I,I)=A(I,I) - 1.                         02890006
  560   CONTINUE                                                        02900006
C       ***************************************************             02910000
C       ****   AUFSTELLEN DER MATRIX BL(  )            ****             02920006
C       ***************************************************             02930006
C                                                                       02940006
        IF (ICNTL .EQ. 1) THEN                                          02950006
           DO 580 N=0,  N9+1,1                                          02960006
              FB(N)=C                                                   02970006
  580      CONTINUE                                                     02980006
        ENDIF                                                           02990006
        IF (ICNTL .EQ. 2) THEN                                          03000006
           DO 590 N=0,  N9+1,1                                          03010000
              FB(N)=-GAM(N)*KAPPA(N)                                    03020006
  590      CONTINUE                                                     03030006
        ENDIF                                                           03040006
C                                                                       03050006
C    >>>>>>>>  RANDPUNKT DARUEBER(MITTE,LINKS,RECHTS)                   03060006
        I=I9-1                                                          03070006
        DO 600 N=1,    N9 ,1                                            03080006
         H1=( A1(N)*DR2R(I) + A2(N)*DR1R(I)/R(I) ) *FB(N)               03090006
         H1=H1+ A4(N)*DR1R(I)/2./DTH/R(I) * FB(N+1)                     03100006
         H1=H1- A4(N)*DR1R(I)/2./DTH/R(I) * FB(N-1)                     03110000
         BL(N+  N9*(I-1))=-H1                                           03120006
  600   CONTINUE                                                        03130006
        IF (ICNTL .EQ. 2) THEN                                          03140006
           DO 610 I=1,   N9*(I9-1)+1,1                                  03150006
              BL(I)=BL(I)+(RHON(I)-1.)/RHOA                             03160006
  610      CONTINUE                                                     03170006
        ENDIF                                                           03180006
C                                                                       03190006
C                                                                       03200006
C                                                                       03210000
        CALL DLAX(A,401,  N9*(I9-1)+1,BL,EPSZ,ISW,IS,VW,IP,ICON)        03220006
        IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                       03230006
        IF (ICON .GT. 0) GOTO 9000                                      03240006
   50   FORMAT (1X,'ICON = ',I8,'  STEP = ',I8)                         03250006
C                                                                       03260006
        IF (ICNTL .EQ. 1) THEN                                          03270006
           DO 820 I=1,  N9*(I9-1)+1,1                                   03280006
              RHON(I)=BL(I)                                             03290006
  820      CONTINUE                                                     03300006
        ENDIF                                                           03310000
        IF (ICNTL .EQ. 2) THEN                                          03320006
           DO 830 I=1,  N9*(I9-1)+1,1                                   03330006
              GPOT(I)=BL(I)                                             03340006
  830      CONTINUE                                                     03350006
        ENDIF                                                           03360006
        IF (ICNTL. EQ. 1) GOTO 2100                                     03370006
C                                                                       03380006
C                                                                       03390006
C       ******************************************************          03400006
C       ****         BEWEGUNG DES RANDES          ****                  03410000
C       ******************************************************          03420006
C                                                                       03430006
        DO 900 N=1,   N9, 1                                             03440006
           H3=-GAM(N)*KAPPA(N)                                          03450006
           H1=GPOT(N+  N9*(I9-2))                                       03460006
           H2=GPOT(N+  N9*(I9-3))                                       03470006
           GPAR(N+N9*(I9-1))=H3*DR1GL0+H1*DR1GL1+H2*DR1GL2              03480006
           BPKT(N)= GPAR(N+N9*(I9-1)) * G(N)/B(N)/B(N)/B(N)             03490006
           GPATH(N+N9*(I9-1)) =-( GAM(N+1)*KAPPA(N+1)                   03500006
     *                           -GAM(N-1)*KAPPA(N-1) )/2.D0/DTH        03510000
           BPKT(N)= BPKT(N) - GPATH(N+N9*(I9-1)) *BA1(N)/B(N)/B(N)      03520006
  900   CONTINUE                                                        03530006
        DO 910 I=1, N9,1                                                03540006
  909      IF (DABS(BPKT(I)*DELT) .GT. DABS(B(I)/ 90.))  DELT=DELT/2.   03550006
           IF (DABS(BPKT(I)*DELT) .GT. DABS(B(I)/ 90.))  GOTO 909       03560006
  910   CONTINUE                                                        03570006
C                                                                       03580006
C    >>>> BERECHNEN DES GESCHWINDIGKEITSFELDES IM INNERN                03590006
C    >>>> (NUR BEIM ABSPEICHERN DER DATEN)                              03600006
   40   FORMAT (1X,'STEP  = ',I8,15X,'  T     = ',D23.16,'  DELT  = ',  03610000
     *          D23.16)                                                 03620006
   41   FORMAT (1X,I3,2X,I3,4(2X,D23.16))                               03630006
   42   FORMAT (1X,I3,2(2X,D23.16))                                     03640006
        IF (COUN9 .EQ. CPRIN) THEN                                      03650006
           WRITE (22,40) COUNT,T9,DELT                                  03660006
           K=N9*(I9-1)+1                                                03670006
           WRITE (22,41) 0,0,RHON(K),GPOT(K),0.,0.                      03680006
           DO 911 I=1,I9-1,1                                            03690006
           DO 911 N=1,N9  ,1                                            03700006
              H3=GPOT(N+N9*(I-1))                                       03710000
              H1=GPOT(N9*(I9-1)+1)                                      03720006
              H2=-GAM(N)*KAPPA(N)                                       03730006
              IF (I .GT. 1)     H1=GPOT(N+N9*(I-2))                     03740006
              IF (I .LT. I9-1)  H2=GPOT(N+N9*I)                         03750006
              GPAR(N+N9*(I-1))=  H3*DR1(I)+H1*DR1L(I)+H2*DR1R(I)        03760006
  911      CONTINUE                                                     03770006
           DO 912 I=1,I9-1,1                                            03780006
              GPATH(1+N9*(I-1))=0.                                      03790006
              GPATH(N9*I      )=0.                                      03800006
           DO 912 N=2,N9-1,1                                            03810000
              H1=GPOT(N-1+N9*(I-1))                                     03820006
              H2=GPOT(N+1+N9*(I-1))                                     03830006
              GPATH(N+N9*(I-1)) = (H2-H1)/2./DTH                        03840006
  912      CONTINUE                                                     03850006
           DO 913 I=1,I9-1,1                                            03860006
           DO 913 N=1,N9,1                                              03870006
              K=N+N9*(I-1)                                              03880006
              WRITE (22,41) I,N,RHON(K),GPOT(K),GPAR(K)/B(N),           03890006
     *                 (GPATH(K)-GPAR(K)*BA1(N)*R(I)/B(N)) /B(N)/R(I)   03900006
  913      CONTINUE                                                     03910000
           DO 914 N=1,N9,1                                              03920006
              K=N+N9*(I9-1)                                             03930006
              WRITE (22,41) I9,N,C,-GAM(N)*KAPPA(N),GPAR(K)/B(N),       03940006
     *                      (GPATH(K)-GPAR(K)*BA1(N)/B(N)) /B(N)        03950006
  914      CONTINUE                                                     03960006
           DO 916 N=1,N9,1                                              03970006
              WRITE (22,42) N,B(N),BPKT(N)                              03980006
  916      CONTINUE                                                     03990006
        ENDIF                                                           04000006
C    >>>> BERECHNUNG DER NEUEN RANDPOSITION                             04010000
        DO 920 I=1,  N9,1                                               04020006
           B(I)=B(I)+BPKT(I)*DELT                                       04030006
  920   CONTINUE                                                        04040006
        ICNTL=0                                                         04050006
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   04060006
        IF (COUNT .EQ. CEND) GOTO 9000                                  04070006
        GOTO 2000                                                       04080006
C                                                                       04090006
 9000   CONTINUE                                                        04100006
        CLOSE (22,STATUS='KEEP')                                        04110000
        END                                                             04120006
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              04130006
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           04140006
//GO.SYSIN DD *                                                         04150006
/*                                                                      04160006
/*      EXEC DRUCKE                                                     04170006
/*      END                                                             04180006
