//D14KG    JOB RACF,CLASS=G,                                            00010015
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020013
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030013
//FORT.SYSIN    DD *                                                    00040013
C                                                                       00050006
C       ##################################################              00060016
C       ####   MODELL: MA86                           ####              00070016
C       ####   NAME DES PROGRAMMS:  MA86(D2SCHL)      ####              00080016
C       ####      LAPLACE(RHON) = RHON                ####              00090016
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00100016
C       ####      R.B. : RHON = C                     ####              00110016
C       ####           : PHIA = - GAMMA * KAPPA       ####              00120016
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00130016
C       ####      VA    =  GRAD( PHIA )               ####              00140016
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00150016
C       ##################################################              00160016
C        GERECHNET WIRD EIN 2-DIM. SCHLAUCH MIT PERIODISCHER            00170016
C        RANDBEDINGUNG                                                  00180016
C        DIE DATEN WERDEN IN DER DATEI  FNW  ABGESPEICHERT              00190016
C        GRAPHIK-PROGRAMM:       MA86(PIC2SC)                           00200016
C                                                                       00210006
        INTEGER           X9,Y9,I,J,IR,IL,ICNTL,COUNT,COUN9,CPRIN       00220006
        DOUBLE PRECISION  GAMMA,C,DELT,T9,          WGO,WGU             00230006
        DOUBLE PRECISION  TRX,WK                                        00240007
        DOUBLE PRECISION  BO(0:21),BOPKT(0:21),BOSTRT(0:21),BOST(0:12)  00250006
        DOUBLE PRECISION  BU(0:21),BUPKT(0:21),BUSTRT(0:21),BUST(0:12)  00260006
        DOUBLE PRECISION  BU1(0:21),BU2(0:21),BO1(0:21),BO2(0:21)       00270006
        DOUBLE PRECISION  A1(0:21),A2(0:21),A3(0:21),A4(0:21),A5(0:21)  00280006
        DOUBLE PRECISION  KAPPAO(0:21),KAPPAU(0:21)                     00290000
        DOUBLE PRECISION  X(0:21),DX      ,DX1      ,DX2      ,LX       00300006
        DOUBLE PRECISION  Y(0:21),DY(0:21),DY1(0:21),DY2(0:21)          00310006
        DOUBLE PRECISION  DX1L      ,DX2L      ,DX1R      ,DX2R         00320006
        DOUBLE PRECISION  DY1L(0:21),DY2L(0:21),DY1R(0:21),DY2R(0:21)   00330006
        DOUBLE PRECISION  PI,H1,H2,H3                                   00340006
        DOUBLE PRECISION  FBO(0:21),FBU(0:21)                           00350006
C                                                                       00360006
C       DOUBLE PRECISION  X0(-1:21),Y0(-1:21)                           00370006
C       DOUBLE PRECISION  X1(0:21),X2(0:21),Y1(0:21),Y2(0:21)           00380006
C       DOUBLE PRECISION  DS(0:21)                                      00390000
C       DOUBLE PRECISION  DS1L(0:21),DS1(0:21),DS1R(0:21)               00400006
C       DOUBLE PRECISION  DS2L(0:21),DS2(0:21),DS2R(0:21)               00410006
C                                                                       00420006
        DOUBLE PRECISION  RHON(401),GPOT(401)                           00430006
        DOUBLE PRECISION  GPAX(401),GPAY(401)                           00440006
        DOUBLE PRECISION  RHOA                                          00450006
C                                                                       00460006
        DOUBLE PRECISION  A(401,401),BL(401)                            00470006
        DOUBLE PRECISION  VW(401),IP(401)                               00480006
        DOUBLE PRECISION  EPSZ,IS                                       00490000
        INTEGER           ICON,ISW                                      00500006
        CHARACTER         FNW*30                                        00510006
C                                                                       00520006
C       **************************************************              00530006
C       ****      PARAMETER PARAMETER PARAMETER       ****              00540006
C       **************************************************              00550006
C                                                                       00560006
        FNW ='D14K.TES3.DATA'                                           00570015
        PI=4.0D0*DATAN(1.0D0)                                           00580007
        GAMMA     =     0.20000D0                                       00590007
        C         =     2.00000D0                                       00600000
        RHOA      =     1.00000D0                                       00610007
        X9        =    12                                               00620015
        Y9        =     7                                               00630006
        LX        =     2.D0*PI  *  4.D0/3.D0                           00640009
        WK        =     2.D0*PI/LX                                      00650008
        TRX       =     PI/2.D0                                         00660007
        TRX       =     0.00000D0                                       00670015
        DELT      =     0.02000D0                                       00680013
        BOST(0)   =     1.98300D0                                       00690006
        BOST(2)   =     0.20000D0                                       00700013
        BUST(0)   =    -BOST(0)                                         00710006
        BUST(2)   =     BOST(2)                                         00720006
        CEND      =  8000                                               00730013
        CPRIN     =   400                                               00740013
        EPSZ      =     1.D-60                                          00750006
        ISW       =     1                                               00760006
C                                                                       00770006
C    >>>>> BERECHN. DER DISKRETIS. IN Y-RICHT. UND X-RICHTUNG <<<<<     00780006
C    >>>>> UND DER STARTKURVE                                 <<<<<     00790006
        H1  =X9                                                         00800006
        DX  =LX/ H1                                                     00810006
        X(1)=-LX/2.D0                                                   00820000
        DO 101 I=2 ,X9                                                  00830006
           X(I)=X(I-1)+DX                                               00840006
  101   CONTINUE                                                        00850006
        X(X9+1)=X(1)+LX                                                 00860006
C                                                                       00870006
        Y(0)=   -1000.D-03                                              00880006
        Y(1)=   - 950.D-03                                              00890006
        Y(2)=   - 800.D-03                                              00900006
        Y(3)=   - 400.D-03                                              00910006
        Y(4)=       0.D-03                                              00920000
        Y(5)=     400.D-03                                              00930006
        Y(6)=     800.D-03                                              00940006
        Y(7)=     950.D-03                                              00950006
        Y(8)=    1000.D-03                                              00960006
C                                                                       00970006
        DO 100 I=1  ,X9, 1                                              00980006
           BOSTRT(I)=BOST(0)+BOST(2)*DCOS(X(I)*WK+TRX)                  00990007
           BUSTRT(I)=BUST(0)+BUST(2)*DCOS(X(I)*WK+TRX)                  01000007
  100   CONTINUE                                                        01010006
C                                                                       01020000
C                                                                       01030006
        OPEN (22,FILE=FNW)                                              01040006
        REWIND 22                                                       01050006
C                                                                       01060006
   30   FORMAT (1X,'MA86(D2SCHL) PERIOD. RANDBED. - 2 DIMENSIONAL')     01070006
   31   FORMAT (1X,'GAMMA = ',D23.16,'  RHOA  = ',D23.16,'  C     = ',  01080006
     *          D23.16)                                                 01090006
   32   FORMAT (1X,'X9    = ',I3,20X,'  Y9    = ',I3,20X,'  LX    = ',  01100006
     *          D23.16)                                                 01110006
   35   FORMAT (1X,I3,4(2X,D23.16) )                                    01120000
        WRITE (22,30)                                                   01130006
        WRITE (22,31) GAMMA,RHOA,C                                      01140006
        WRITE (22,32) X9,Y9,LX                                          01150006
        DO 128 J=1,  Y9+1,1                                             01160006
           DY(J)=Y(J)-Y(J-1)                                            01170006
  128   CONTINUE                                                        01180006
        WRITE (22,35) 0,Y(0),0.D0                                       01190006
        DO 130 J=1,Y9+1,1                                               01200006
           WRITE (22,35) J,Y(J),DY(J)                                   01210006
  130   CONTINUE                                                        01220000
        DO 131 I=1,X9+1,1                                               01230006
           WRITE (22,35) I,X(I),DX                                      01240006
  131   CONTINUE                                                        01250006
C                                                                       01260006
           DX2    =-2.D0/DX/DX                                          01270006
           DX2L   = 1.D0/DX/DX                                          01280006
           DX2R   = 1.D0/DX/DX                                          01290006
           DX1R   = .5D0/DX                                             01300006
           DX1L   =-.5D0/DX                                             01310006
           DX1    =-(DX1L   +DX1R   )                                   01320000
        DO 136 J=1,  Y9  ,1                                             01330006
           H1=DY(J)                                                     01340006
           H2=DY(J+1)                                                   01350006
           DY2 (J)=-2.D0/H1/H2                                          01360006
           DY2L(J)= 2.D0/H1/(H1+H2)                                     01370006
           DY2R(J)= 2.D0/H2/(H1+H2)                                     01380006
           DY1R(J)= H1/H2/(H1+H2)                                       01390006
           DY1L(J)=-H2/H1/(H1+H2)                                       01400006
           DY1 (J)=-(DY1L(J)+DY1R(J))                                   01410006
  136   CONTINUE                                                        01420000
                                                                        01430006
        DO 140 I=1,    X9   ,1                                          01440006
           BO(I)=BOSTRT(I)                                              01450006
           BU(I)=BUSTRT(I)                                              01460006
  140   CONTINUE                                                        01470006
        ICNTL=0                                                         01480006
        COUNT=-1                                                        01490006
        COUN9=CPRIN-1                                                   01500006
        T9   =-DELT                                                     01510006
C                                                                       01520000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          01530006
C       >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<          01540006
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          01550006
C                                                                       01560006
 2000   CONTINUE                                                        01570006
        COUNT=COUNT+1                                                   01580006
        COUN9=COUN9+1                                                   01590006
        T9   =T9   +DELT                                                01600006
C       ******************************************************          01610006
C       ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****          01620000
C       ******************************************************          01630006
C                                                                       01640006
        BO(0)=BO(X9)                                                    01650006
        BO(X9+1)=BO(1)                                                  01660006
        BU(0)=BU(X9)                                                    01670006
        BU(X9+1)=BU(1)                                                  01680006
        DO 220 I=1, X9 ,1                                               01690006
           BO1(I)=(BO(I-1)*DX1L   +BO(I)*DX1   +BO(I+1)*DX1R   )        01700006
           BO2(I)=(BO(I-1)*DX2L   +BO(I)*DX2   +BO(I+1)*DX2R   )        01710006
           BU1(I)=(BU(I-1)*DX1L   +BU(I)*DX1   +BU(I+1)*DX1R   )        01720000
           BU2(I)=(BU(I-1)*DX2L   +BU(I)*DX2   +BU(I+1)*DX2R   )        01730006
           WGO    =DSQRT(1.D0+BO1(I)*BO1(I))                            01740006
           KAPPAO(I)=-BO2(I)/WGO/WGO/WGO                                01750006
           WGU    =DSQRT(1.D0+BU1(I)*BU1(I))                            01760006
           KAPPAU(I)= BU2(I)/WGU/WGU/WGU                                01770006
  220   CONTINUE                                                        01780006
C                                                                       01790006
        DO 222 I=1,X9  , 1                                              01800006
           H1=1.D0/(BO(I)-BU(I))                                        01810006
           A1(I)=H1*H1*4.D0                                             01820000
           A2(I)=-(BO1(I)+BU1(I))*H1                                    01830006
           A3(I)=-(BO1(I)-BU1(I))*H1                                    01840006
           A4(I)=-(BO2(I)+BU2(I))*H1+2.D0*A2(I)*A3(I)                   01850006
           A5(I)=-(BO2(I)-BU2(I))*H1+2.D0*A2(I)*A2(I)                   01860006
  222   CONTINUE                                                        01870006
C       >>>>> BERECHNUNG DER BOGENLAENGE UND KRUEMMUNG <<<<<            01880006
C       Y0(-1)=B(1)                                                     01890006
C       Y0(I9+1)=-B(I9-1)                                               01900006
C       X0(-1)=-DX(1)*TOP                                               01910006
C       X0(I9+1)=(1.D0-DX(I9))*TOP                                      01920000
C       DO 250 I=0,I9 ,1                                                01930006
C          Y0(I)=B(I)                                                   01940006
C          H1=Y0(I)-Y0(I-1)                                             01950006
C          H1=H1*H1                                                     01960006
C          X0(I)=X0(I-1)+DX(I)*TOP                                      01970006
C          H2= X0(I)-X0(I-1)                                            01980006
C          H2=H2*H2                                                     01990006
C          DS(I)=DSQRT(H1+H2)                                           02000006
C 250   CONTINUE                                                        02010006
C       DS(I9+1)=DS(I9)                                                 02020000
C       DO 255 I=0, I9,1                                                02030006
C          H1=DS(I)                                                     02040006
C          H2=DS(I+1)                                                   02050006
C          DS2 (I)=-2.D0/H1/H2                                          02060006
C          DS2L(I)= 2.D0/H1/(H1+H2)                                     02070006
C          DS2R(I)= 2.D0/H2/(H1+H2)                                     02080006
C          DS1R(I)= H1/H2/(H1+H2)                                       02090006
C          DS1L(I)=-H2/H1/(H1+H2)                                       02100006
C          DS1 (I)=-(DS1L(I)+DS1R(I))                                   02110006
C 255   CONTINUE                                                        02120000
C          H1=DSQRT(X0(-1)*X0(-1)+Y0(-1)*Y0(-1))                        02130006
C          IF (COUN9 .EQ. CPRIN) WRITE  (22,11) -1,X0(-1),Y0(-1),H1     02140006
C       DO 260 I=0,I9,1                                                 02150006
C          X1(I)=DS1L(I)*X0(I-1)+DS1(I)*X0(I)+DS1R(I)*X0(I+1)           02160006
C          X2(I)=DS2L(I)*X0(I-1)+DS2(I)*X0(I)+DS2R(I)*X0(I+1)           02170006
C          Y1(I)=DS1L(I)*Y0(I-1)+DS1(I)*Y0(I)+DS1R(I)*Y0(I+1)           02180006
C          Y2(I)=DS2L(I)*Y0(I-1)+DS2(I)*Y0(I)+DS2R(I)*Y0(I+1)           02190006
C          H1=X1(I)*X1(I)+Y1(I)*Y1(I)                                   02200006
C          KAPPA(I)=-(X1(I)*Y2(I)-X2(I)*Y1(I))/H1/DSQRT(H1)             02210006
C  11      FORMAT (1X,I3,5(1X,F10.5))                                   02220000
C          H1=DSQRT(X0(I)*X0(I)+Y0(I)*Y0(I))                            02230006
C          IF (COUN9 .EQ. CPRIN)                                        02240006
C    *     WRITE  (22,11) I,X0(I),Y0(I),H1,DS(I),KAPPA(I)               02250006
C 260   CONTINUE                                                        02260006
C          H1=DSQRT(X0(I9+1)*X0(I9+1)+Y0(I9+1)*Y0(I9+1))                02270006
C          IF (COUN9 .EQ. CPRIN)                                        02280006
C    *     WRITE  (22,11) I9+1,X0(I9+1),Y0(I9+1),H1,DS(I9+1)            02290006
C                                                                       02300006
C       ***************************************************             02310006
C       ****   AUFSTELLEN DER MATRIX A( , )            ****             02320000
C       ***************************************************             02330006
C                                                                       02340006
 2100   CONTINUE                                                        02350006
        ICNTL=ICNTL+1                                                   02360006
        DO 300 I=1,  X9*Y9   ,1                                         02370006
           BL(I)=0.D0                                                   02380006
        DO 300 N=1,  X9*Y9 ,1                                           02390006
           A(I,N)=0.D0                                                  02400006
  300   CONTINUE                                                        02410006
C       >>> MI OB UN LO LU RO RU RE LI <<< (BEZUGSPUNKT = INNERER PUNKT)02420000
        DO 320 I=1,  X9   ,1                                            02430006
        DO 320 J=2,  Y9-1 ,1                                            02440006
         IR=I+1                                                         02450006
         IL=I-1                                                         02460006
         IF (IR.EQ.X9+1) IR=1                                           02470006
         IF (IL.EQ.0)    IL=X9                                          02480006
         H1=(A2(I)+Y(J)*A3(I))*2.D0                                     02490006
         H2=A4(I)+Y(J)*A5(I)                                            02500006
         A( (I-1)*Y9+J, (I-1)*Y9+J   )=DY2 (J)*A1(I) +DX2               02510006
     *             +DY1 (J)*H2 +DX1 *DY1 (J)*H1                         02520000
         A( (I-1)*Y9+J, (I-1)*Y9+J+1 )=DY2R(J)*A1(I)                    02530006
     *             +DY1R(J)*H2 +DX1 *DY1R(J)*H1                         02540006
         A( (I-1)*Y9+J, (I-1)*Y9+J-1 )=DY2L(J)*A1(I)                    02550006
     *             +DY1L(J)*H2 +DX1 *DY1L(J)*H1                         02560006
         A( (I-1)*Y9+J, (IL-1)*Y9+J+1 )=DX1L *DY1R(J) *H1               02570006
         A( (I-1)*Y9+J, (IL-1)*Y9+J-1 )=DX1L *DY1L(J) *H1               02580006
         A( (I-1)*Y9+J, (IR-1)*Y9+J+1 )=DX1R *DY1R(J) *H1               02590006
         A( (I-1)*Y9+J, (IR-1)*Y9+J-1 )=DX1R *DY1L(J) *H1               02600006
         A( (I-1)*Y9+J, (IR-1)*Y9+J   )=DX2R + DX1R *DY1(J) *H1         02610006
         A( (I-1)*Y9+J, (IL-1)*Y9+J   )=DX2L + DX1L *DY1(J) *H1         02620000
  320   CONTINUE                                                        02630006
C       >>> MI    UN    LU    RU RE LI (OBERER RAND) <<<<               02640006
        DO 340 I=1, X9   ,1                                             02650006
         J=Y9                                                           02660006
         H1=(A2(I)+Y(J)*A3(I))*2.D0                                     02670006
         H2=A4(I)+Y(J)*A5(I)                                            02680006
         IR=I+1                                                         02690006
         IL=I-1                                                         02700006
         IF (IR.EQ.X9+1) IR=1                                           02710006
         IF (IL.EQ.0)    IL=X9                                          02720000
         A( (I-1)*Y9+J, (I-1)*Y9+J   )=DY2 (J)*A1(I) +DX2               02730006
     *             +DY1 (J)*H2 +DX1 *DY1 (J)*H1                         02740006
         A( (I-1)*Y9+J, (I-1)*Y9+J-1 )=DY2L(J)*A1(I)                    02750006
     *             +DY1L(J)*H2 +DX1 *DY1L(J)*H1                         02760006
         A( (I-1)*Y9+J, (IL-1)*Y9+J-1 )=DX1L *DY1L(J) *H1               02770006
         A( (I-1)*Y9+J, (IR-1)*Y9+J-1 )=DX1R *DY1L(J) *H1               02780006
         A( (I-1)*Y9+J, (IR-1)*Y9+J   )=DX2R + DX1R *DY1(J) *H1         02790006
         A( (I-1)*Y9+J, (IL-1)*Y9+J   )=DX2L + DX1L *DY1(J) *H1         02800006
  340   CONTINUE                                                        02810006
C       >>> MI OB    LO    RO    RE LI (UNTERER RAND) <<<<              02820000
        DO 360 I=1, X9   ,1                                             02830006
         J=1                                                            02840006
         H1=(A2(I)+Y(J)*A3(I))*2.D0                                     02850006
         H2=A4(I)+Y(J)*A5(I)                                            02860006
         IR=I+1                                                         02870006
         IL=I-1                                                         02880006
         IF (IR.EQ.X9+1) IR=1                                           02890006
         IF (IL.EQ.0)    IL=X9                                          02900006
         A( (I-1)*Y9+J, (I-1)*Y9+J   )=DY2 (J)*A1(I) +DX2               02910006
     *             +DY1 (J)*H2 +DX1 *DY1 (J)*H1                         02920000
         A( (I-1)*Y9+J, (I-1)*Y9+J+1 )=DY2R(J)*A1(I)                    02930006
     *             +DY1R(J)*H2 +DX1 *DY1R(J)*H1                         02940006
         A( (I-1)*Y9+J, (IL-1)*Y9+J+1 )=DX1L *DY1R(J) *H1               02950006
         A( (I-1)*Y9+J, (IR-1)*Y9+J+1 )=DX1R *DY1R(J) *H1               02960006
         A( (I-1)*Y9+J, (IR-1)*Y9+J   )=DX2R + DX1R *DY1(J) *H1         02970006
         A( (I-1)*Y9+J, (IL-1)*Y9+J   )=DX2L + DX1L *DY1(J) *H1         02980006
  360   CONTINUE                                                        02990006
C                                                                       03000006
C                                                                       03010006
C                                                                       03020000
        DO 560 I=1,  X9*Y9      ,1                                      03030006
         IF (ICNTL .EQ. 1)   A(I,I)=A(I,I) - 1.D0                       03040006
  560   CONTINUE                                                        03050006
C       ***************************************************             03060006
C       ****   AUFSTELLEN DER MATRIX BL(  )            ****             03070006
C       ***************************************************             03080006
C                                                                       03090006
        DO 580 I=1,  X9  ,1                                             03100006
           IF (ICNTL .EQ. 1)   FBO(I)=C                                 03110006
           IF (ICNTL .EQ. 1)   FBU(I)=C                                 03120000
           IF (ICNTL .EQ. 2)   FBO(I)=-GAMMA*KAPPAO(I)                  03130006
           IF (ICNTL .EQ. 2)   FBU(I)=-GAMMA*KAPPAU(I)                  03140006
  580   CONTINUE                                                        03150006
C                                                                       03160006
C       >>>>>>>>  RANDPUNKT OBEN <<<<<<<                                03170006
        DO 600 I=1, X9   ,1                                             03180006
         J=Y9                                                           03190006
         H1=(A2(I)+Y(J)*A3(I))*2.D0                                     03200006
         IR=I+1                                                         03210006
         IL=I-1                                                         03220000
         IF (IR.EQ.X9+1) IR=1                                           03230006
         IF (IL.EQ.0)    IL=X9                                          03240006
         BL( (I-1)*Y9+J ) = -FBO(I)*DY2R(J)*A1(I)                       03250006
     *          - (DX1R*FBO(IR) + DX1L*FBO(IL)) * DY1R(J) * H1          03260006
  600   CONTINUE                                                        03270006
C       >>>>>>>>  RANDPUNKT UNTEN <<<<<<<                               03280006
        DO 605 I=1, X9   ,1                                             03290006
         J=1                                                            03300006
         H1=(A2(I)+Y(J)*A3(I))*2.D0                                     03310006
         IR=I+1                                                         03320000
         IL=I-1                                                         03330006
         IF (IR.EQ.X9+1) IR=1                                           03340006
         IF (IL.EQ.0)    IL=X9                                          03350006
         BL( (I-1)*Y9+J ) = -FBU(I)*DY2L(J)*A1(I)                       03360006
     *          - (DX1R*FBU(IR) + DX1L*FBU(IL)) * DY1L(J) * H1          03370006
  605   CONTINUE                                                        03380006
C                                                                       03390006
C                                                                       03400006
        DO 610 I=1,   X9*Y9   ,1                                        03410006
           IF (ICNTL .EQ. 2)   BL(I)=BL(I)+(RHON(I)-1.D0)/RHOA          03420000
  610   CONTINUE                                                        03430006
C                                                                       03440006
C                                                                       03450006
C                                                                       03460006
        CALL DLAX(A,401, X9*Y9  ,BL,EPSZ,ISW,IS,VW,IP,ICON)             03470006
        IF (ICON .GT. 0) WRITE (22,50) ICON,COUNT                       03480006
        IF (ICON .GT. 0) GOTO 9000                                      03490006
   50   FORMAT (1X,'ICON = ',I8,'  STEP = ',I8)                         03500006
C                                                                       03510006
        DO 820 I=1,  X9*Y9 ,1                                           03520000
           IF (ICNTL .EQ. 1)   RHON(I)=BL(I)                            03530006
           IF (ICNTL .EQ. 2)   GPOT(I)=BL(I)                            03540006
  820   CONTINUE                                                        03550006
        IF (ICNTL. EQ. 1) GOTO 2100                                     03560006
C                                                                       03570006
C                                                                       03580006
C       ******************************************************          03590006
C       ****         BEWEGUNG DES RANDES          ****                  03600006
C       ******************************************************          03610006
C                                                                       03620000
        DO 900 I=1,   X9, 1                                             03630006
           IR=I+1                                                       03640006
           IL=I-1                                                       03650006
           IF (IR.EQ.X9+1) IR=1                                         03660006
           IF (IL.EQ.0)    IL=X9                                        03670006
           N=X9*Y9                                                      03680006
           GPAX(N+I)=-GAMMA* (KAPPAU(I)*DX1   + KAPPAU(IR)*DX1R         03690006
     *                                        + KAPPAU(IL)*DX1L   )     03700006
           N=X9*Y9+X9                                                   03710006
           GPAX(N+I)=-GAMMA* (KAPPAO(I)*DX1   + KAPPAO(IR)*DX1R         03720000
     *                                        + KAPPAO(IL)*DX1L   )     03730006
  900   CONTINUE                                                        03740006
        DO 902 I=1,X9  ,1                                               03750006
           N=X9*Y9                                                      03760006
           J=1                                                          03770006
           H3=-GAMMA*KAPPAU(I)                                          03780006
           H1=GPOT((I-1)*Y9+J  )                                        03790006
           H2=GPOT((I-1)*Y9+J+1)                                        03800006
           GPAY(N+I)=-(H3-H1)*(DY(J+1 )+DY(J  ))/DY(J+1)/DY(J   )       03810006
     *       +(H3-H2)*DY(J   )/DY(J+1 )/(DY(J+1 )+DY(J   ))             03820000
           N=X9*Y9+X9                                                   03830006
           J=Y9                                                         03840006
           H3=-GAMMA*KAPPAO(I)                                          03850006
           H1=GPOT((I-1)*Y9+J  )                                        03860006
           H2=GPOT((I-1)*Y9+J-1)                                        03870006
           GPAY(N+I)=+(H3-H1)*(DY(J   )+DY(J+1))/DY(J  )/DY(J+1 )       03880006
     *       -(H3-H2)*DY(J+1 )/DY(J   )/(DY(J   )+DY(J+1 ))             03890006
  902   CONTINUE                                                        03900006
C                                                                       03910006
        DO 904 I=1, X9 ,1                                               03920000
           N=X9*Y9                                                      03930006
           BUPKT(I)= GPAY(N+I)*2.D0/(BO(I)-BU(I))                       03940006
     *         -BU1(I) * ( GPAX(N+I)+GPAY(N+I)*(A2(I)+Y(0   )*A3(I)) )  03950006
           N=X9*Y9+X9                                                   03960006
           BOPKT(I)= GPAY(N+I)*2.D0/(BO(I)-BU(I))                       03970006
     *         -BO1(I) * ( GPAX(N+I)+GPAY(N+I)*(A2(I)+Y(Y9+1)*A3(I)) )  03980006
  904   CONTINUE                                                        03990006
        DO 910 I=1, X9  ,1                                              04000006
  909      IF (  (DABS(BOPKT(I)*DELT).GT.(BO(I)-BU(I))/400.D0) .OR.     04010013
     *           (DABS(BUPKT(I)*DELT).GT.(BO(I)-BU(I))/400.D0)  ) THEN  04020013
              DELT=DELT/2.D0                                            04030006
              GOTO 909                                                  04040006
           ENDIF                                                        04050006
  910   CONTINUE                                                        04060006
C                                                                       04070006
   39   FORMAT (1X,'STEP  = ',I8,2X,'  T = ',D23.16,'  DELT = ',        04080011
     *          D23.16)                                                 04090006
   40   FORMAT (1X,'STEP  = ',I8,15X,'  T     = ',D23.16,'  DELT  = ',  04100011
     *          D23.16)                                                 04110011
   41   FORMAT (1X,I3,2X,I3,4(2X,D23.16))                               04120006
   42   FORMAT (1X,I3,2(2X,D23.16))                                     04130006
        IF (COUN9 .EQ. CPRIN) THEN                                      04140000
           WRITE (22,40) COUNT,T9,DELT                                  04150006
           WRITE (6 ,39) COUNT,T9,DELT                                  04160011
           DO 927 I=1 ,X9  ,1                                           04170006
           DO 927 J=1, Y9  ,1                                           04180006
              IR=I+1                                                    04190006
              IL=I-1                                                    04200006
              IF (IR.EQ.X9+1) IR=1                                      04210006
              IF (IL.EQ.0)    IL=X9                                     04220006
              H3=GPOT((I -1) *Y9+J )                                    04230006
              H1=GPOT((IL-1) *Y9+J )                                    04240006
              H2=GPOT((IR-1) *Y9+J )                                    04250000
              GPAX((I-1)*Y9+J)=  H3*DX1   +H1*DX1L   +H2*DX1R           04260006
  927      CONTINUE                                                     04270006
           DO 912 I=1,X9  ,1                                            04280006
           DO 912 J=1,Y9  ,1                                            04290006
              H3=GPOT((I-1)*Y9+J  )                                     04300006
              H1=-GAMMA*KAPPAU(I)                                       04310006
              H2=-GAMMA*KAPPAO(I)                                       04320006
              IF (J .LT. Y9) H2=GPOT((I-1)*Y9+J+1 )                     04330006
              IF (J .GT.  1) H1=GPOT((I-1)*Y9+J-1 )                     04340006
              GPAY((I-1)*Y9+J)=  H3*DY1(J)+H1*DY1L(J)+H2*DY1R(J)        04350000
  912      CONTINUE                                                     04360006
           DO 914 IL=1,X9+1,1                                           04370006
              I=IL                                                      04380006
              IF (IL.EQ.X9+1) I=1                                       04390006
              N=X9*Y9                                                   04400006
              WRITE (22,41) IL,0,C,-GAMMA*KAPPAU(I),GPAX(N+I) +         04410006
     *            GPAY(N+I)*(A2(I)-A3(I)),GPAY(N+I)*2.D0/(BO(I)-BU(I))  04420006
           DO 913 J=1,Y9,1                                              04430006
              N=(I-1)*Y9+J                                              04440006
              WRITE (22,41) IL,J,RHON(N),GPOT(N),GPAX(N)+GPAY(N)*       04450000
     *             (A2(I)+Y(J)*A3(I)) , GPAY(N)*2.D0/(BO(I)-BU(I))      04460006
  913      CONTINUE                                                     04470006
              N=X9*Y9+X9                                                04480006
              WRITE (22,41) IL,Y9+1,C,-GAMMA*KAPPAO(I),GPAX(N+I) +      04490006
     *            GPAY(N+I)*(A2(I)+A3(I)),GPAY(N+I)*2.D0/(BO(I)-BU(I))  04500006
  914      CONTINUE                                                     04510006
C                                                                       04520006
           DO 916 I=1,X9  ,1                                            04530006
              WRITE (22,42) I,BU(I),BUPKT(I)                            04540006
C             WRITE (6 ,42) I,BU(I),BUPKT(I)                            04550011
  916      CONTINUE                                                     04560000
           WRITE (22,42) X9+1,BU(1),BUPKT(1)                            04570006
           DO 917 I=1,X9  ,1                                            04580006
              WRITE (22,42) I,BO(I),BOPKT(I)                            04590006
C             WRITE (6 ,42) I,BO(I),BOPKT(I)                            04600011
  917      CONTINUE                                                     04610006
           WRITE (22,42) X9+1,BO(1),BOPKT(1)                            04620006
        ENDIF                                                           04630006
        DO 920 I=1,  X9  ,1                                             04640006
           BU(I)=BU(I)+BUPKT(I)*DELT                                    04650006
           BO(I)=BO(I)+BOPKT(I)*DELT                                    04660006
  920   CONTINUE                                                        04670000
        ICNTL=0                                                         04680006
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   04690006
        IF (COUNT .EQ. CEND) GOTO 9000                                  04700006
        GOTO 2000                                                       04710006
C                                                                       04720006
 9000   CONTINUE                                                        04730006
        CLOSE (22,STATUS='KEEP')                                        04740006
        END                                                             04750013
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              04760013
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           04770013
//GO.SYSIN DD *                                                         04780013
/*                                                                      04790013
/*      EXEC DRUCKE                                                     04800013
/*      END                                                             04810013
