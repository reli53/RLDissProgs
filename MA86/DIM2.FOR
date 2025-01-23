//D14KR    JOB RACF,CLASS=C,                                            00010005
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020005
//E2          EXEC  FORTCLG,REGION.FORT=1024K,REGION.GO=2048K           00030005
//FORT.SYSIN    DD *                                                    00040005
C                                                                       00050005
C       ##################################################              00060005
C       ####   MODELL: MA86                           ####              00070005
C       ####   NAME DES PROGRAMMS:  MA86(DIM2)        ####              00080005
C       ####      LAPLACE(RHON) = RHON                ####              00090005
C       ####      LAPLACE(PHIA) = RHON - 1            ####              00100005
C       ####      R.B. : RHON = C                     ####              00110005
C       ####           : PHIA = - GAMMA * KAPPA       ####              00120005
C       ####             (KAPPA = 2* MITTL. KRUEMMUNG)####              00130005
C       ####      VA    =  GRAD( PHIA )               ####              00140005
C       ####             (RHOA AUF 1 SKALIERT !)      ####              00150005
C       ##################################################              00160005
C       GERECHNET WIRD DER VOLLE KREIS (OHNE SYMMETRIE) (2 DIMENS.)     00170000
C        DATEN WERDEN IN DATA-FILE  FNW  ABGESPEICHERT.                 00180005
C        GRAPHIK-PROGRAMM:       ???????                                00190005
C                                                                       00200005
        INTEGER           I9,N9,I,N,ICNTL,COUNT,COUN9,CPRIN             00210005
        DOUBLE PRECISION  GAMMA,DFI,C,DELT,T9                           00220005
        DOUBLE PRECISION  B(0:81),BPKT(0:81),BSTRT(0:81),BST(0:12)      00230005
        DOUBLE PRECISION  BA1(0:81),BA2(0:81),G(0:81),WG(0:81)          00240005
        DOUBLE PRECISION  A1(0:81),A2(0:81),A3(0:81),A4(0:81)           00250005
        DOUBLE PRECISION  KAN(0:81),KAPPA(0:81)                         00260005
        DOUBLE PRECISION  R(21),DR(21),DR1(21),DR2(21)                  00270005
        DOUBLE PRECISION  PI,H1,H2,H3                                   00280005
        DOUBLE PRECISION  FB(0:81)                                      00290000
C                                                                       00300005
        DOUBLE PRECISION  RHON(401),GPOT(401)                           00310005
        DOUBLE PRECISION  RHOA                                          00320005
C                                                                       00330005
        DOUBLE PRECISION  A(401,401),BL(401)                            00340005
        DOUBLE PRECISION  VW(401),IP(401)                               00350005
        DOUBLE PRECISION  EPSZ,IS                                       00360005
        INTEGER           ICON,ISW                                      00370005
C       CHARACTER         FNR*30 , FN1*60                               00380005
C                                                                       00390000
C       **************************************************              00400005
C       ****      PARAMETER PARAMETER PARAMETER       ****              00410005
C       **************************************************              00420005
C                                                                       00430005
C       FNR ='F159.M023.DATA'                                           00440005
        GAMMA     =     0.01000D+00                                     00450005
        C         =     2.00000D+00                                     00460005
        RHOA      =   100.00000D+00                                     00470005
        N9        =     1                                               00480005
        I9        =     7                                               00490000
        R(0)      =      .0                                             00500005
        R(1)      =      .20D+00                                        00510005
        R(2)      =      .40D+00                                        00520005
        R(3)      =      .60D+00                                        00530005
        R(4)      =      .80D+00                                        00540005
        R(I9-2)   =      .950D+00                                       00550005
        R(I9-1)   =      .980D+00                                       00560005
        R(I9)     =     1.                                              00570005
        DELT      =    10.00D+00                                        00580005
        BST(0)    =     3.4000D+00                                      00590000
        CEND      =  1000                                               00600005
        CPRIN     =   500                                               00610005
        EPSZ      =     1.D-08                                          00620005
        ISW       =     1                                               00630005
C                                                                       00640005
        DO 100 N=1,  N9*4 ,1                                            00650005
           BSTRT(N)=BST(0)                                              00660005
  100   CONTINUE                                                        00670005
C       H1=I9                                                           00680005
C       DO 120 I=2,  I9-3,1                                             00690000
C          H2=I                                                         00700005
C          R(I)=(R(I9-2)-R(1))/(H1-3.) * (H2-1.) +R(1)                  00710005
C 120   CONTINUE                                                        00720005
        H1=1.0D+00                                                      00730005
        PI=4.0D+00*DATAN(H1)                                            00740005
        H1=N9*4.                                                        00750005
        DFI=2.*PI/H1                                                    00760005
C                                                                       00770005
        WRITE (6,40) 0,0.                                               00780005
        DO 130 I=1,  I9,1                                               00790000
           DR(I)=R(I)-R(I-1)                                            00800005
           WRITE (6,40) I,R(I),DR(I)                                    00810005
  130   CONTINUE                                                        00820005
        DO 136 I=1,  I9-1,1                                             00830005
           DR1(I)=1./DR(I)/(DR(I+1)+DR(I))                              00840005
           DR2(I)=1./DR(I+1)/(DR(I+1)+DR(I))                            00850005
  136   CONTINUE                                                        00860005
        DO 140 N=1,  4*N9   ,1                                          00870005
           B(N)=BSTRT(N)                                                00880005
  140   CONTINUE                                                        00890000
        ICNTL=0                                                         00900005
        COUNT=-1                                                        00910005
        COUN9=CPRIN-1                                                   00920005
        T9   =-DELT                                                     00930005
C                                                                       00940005
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          00950005
C       >>>>     ZEITSCHLEIFE   (ANFANG)                  <<<<          00960005
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<          00970005
C                                                                       00980005
 2000   CONTINUE                                                        00990000
        COUNT=COUNT+1                                                   01000005
        COUN9=COUN9+1                                                   01010005
        T9   =T9   +DELT                                                01020005
C       ******************************************************          01030005
C       ****    BERECHNUNG DER ABLEITUNGEN AM RANDE       ****          01040005
C       ******************************************************          01050005
C                                                                       01060005
        B(0)     =B(4*N9)                                               01070005
        B(4*N9+1)=B(1)                                                  01080005
        DO 220 N=1,  4*N9,1                                             01090000
           BA1(N)=(B(N+1)-B(N-1))/2./DFI                                01100005
           BA2(N)=(B(N+1)+B(N-1)-B(N)-B(N))/DFI/DFI                     01110005
           KAN(N)=B(N)*B(N)+2.*BA1(N)*BA1(N)-B(N)*BA2(N)                01120005
           G(N)  =B(N)*B(N)+BA1(N)*BA1(N)                               01130005
           WG(N) =DSQRT(G(N))                                           01140005
           KAPPA(N)=KAN(N)/G(N)/WG(N)                                   01150005
           H1=B(N)*B(N)                                                 01160005
           A1(N)=G(N)/H1/H1                                             01170005
           A2(N)=KAN(N)/H1/H1                                           01180005
           A3(N)=1./H1                                                  01190000
           A4(N)=-2.*BA1(N)/H1/B(N)                                     01200005
  220   CONTINUE                                                        01210005
        KAPPA(4*N9+1)=KAPPA(1)                                          01220005
        KAPPA(0)     =KAPPA(4*N9)                                       01230005
C                                                                       01240005
C       ***************************************************             01250005
C       ****   AUFSTELLEN DER MATRIX A( , )            ****             01260005
C       ***************************************************             01270005
C                                                                       01280005
 2100   CONTINUE                                                        01290000
        ICNTL=ICNTL+1                                                   01300005
        DO 300 I=1,4*N9*(I9-1)+1,1                                      01310005
           BL(I)=0.                                                     01320005
        DO 300 N=1,4*N9*(I9-1)+1,1                                      01330005
           A(I,N)=0.                                                    01340005
  300   CONTINUE                                                        01350005
C       >>>>>>>>>>>MITTLERER PUNKT<<<<<<<<<<                            01360005
        DO 320 I=1,  I9-1,1                                             01370005
        DO 320 N=1,  4*N9,1                                             01380005
         A(N+4*N9*(I-1),N+4*N9*(I-1))=-A1(N)*2.*(DR1(I)+DR2(I))+A2(N)   01390000
     *    *(DR(I+1)*DR1(I)-DR(I)*DR2(I))/R(I)-A3(N)*2./DFI/DFI/R(I)/R(I)01400005
  320   CONTINUE                                                        01410005
C       >>>>>>>>>>>PUNKT DARUEBER<<<<<<<<<<                             01420005
        DO 340 I=1,  I9-2 ,1                                            01430005
        DO 340 N=1,  4*N9 ,1                                            01440005
         A(N+4*N9*(I-1),N+4*N9*(I  ))= A1(N)*2.* DR2(I)                 01450005
     *    +A2(N)*(DR(I)*DR2(I))/R(I)                                    01460005
  340   CONTINUE                                                        01470005
C       >>>>>>>>>>>PUNKT DARUNTER<<<<<<<<<<                             01480005
        DO 360 I=2,  I9-1,1                                             01490000
        DO 360 N=1,  4*N9,1                                             01500005
         A(N+4*N9*(I-1),N+4*N9*(I-2))= A1(N)*2.* DR1(I)                 01510005
     *    -A2(N)*(DR(I+1)*DR1(I))/R(I)                                  01520005
  360   CONTINUE                                                        01530005
        DO 370 N=1,  4*N9,1                                             01540005
         A(N , 4*N9*(I9-1)+1)=A1(N)*2.*DR1(1)-A2(N)*DR(2)*DR1(1)/R(1)   01550005
  370   CONTINUE                                                        01560005
C       >>>>>>>>>>>PUNKT LINKS  OBEN<<<<<<<<<<                          01570005
        DO 400 I=1,  I9-2 ,1                                            01580005
         A(4*N9+4*N9*(I-1),1+4*N9*(I))= A4(4*N9)*DR(I)*DR2(I)/2./DFI    01590000
     *                                  /R(I)                           01600005
        DO 400 N=1,  4*N9-1,1                                           01610005
         A(N+4*N9*(I-1),N+1+4*N9*(I  ))= A4(N)*DR(I)*DR2(I)/2./DFI/R(I) 01620005
  400   CONTINUE                                                        01630005
C       >>>>>>>>>>>PUNKT LINKS UNTEN<<<<<<<<<<                          01640005
        DO 420 I=2,  I9-1,1                                             01650005
         A(4*N9+4*N9*(I-1),1+4*N9*(I-2))=-A4(4*N9)*DR(I+1)*DR1(I)/2./DFI01660005
     *                                 /R(I)                            01670005
        DO 420 N=1,  4*N9-1,1                                           01680005
         A(N+4*N9*(I-1),N+1+4*N9*(I-2))=-A4(N)*DR(I+1)*DR1(I)/2./DFI    01690000
     *                                /R(I)                             01700005
  420   CONTINUE                                                        01710005
C       >>>>>>>>>>>PUNKT RECHTS OBEN<<<<<<<<<<                          01720005
        DO 380 I=1,  I9-2,1                                             01730005
         A(1+4*N9*(I-1),4*N9 +4*N9*(I))=-A4(1)*DR(I)*DR2(I)/2./DFI/R(I) 01740005
        DO 380 N=2,  4*N9,1                                             01750005
         A(N+4*N9*(I-1),N-1+4*N9*(I  ))=-A4(N)*DR(I)*DR2(I)/2./DFI/R(I) 01760005
  380   CONTINUE                                                        01770005
C       >>>>>>>>>>>PUNKT RECHTS UNTEN<<<<<<<<<                          01780005
        DO 440 I=2,  I9-1,1                                             01790000
         A(1+4*N9*(I-1),4*N9+4*N9*(I-2))=A4(1)*DR(I+1)*DR1(I)/2./DFI    01800005
     *                                 /R(I)                            01810005
        DO 440 N=2,  4*N9,1                                             01820005
         A(N+4*N9*(I-1),N-1+4*N9*(I-2))= A4(N)*DR(I+1)*DR1(I)/2./DFI    01830005
     *                                 /R(I)                            01840005
  440   CONTINUE                                                        01850005
C       >>>>>>>>>>> PUNKT RECHTS  <<<<<<<<<<                            01860005
        DO 460 I=1,  I9-1,1                                             01870005
         A(1+4*N9*(I-1),4*N9 +4*N9*(I-1))= A3(1)/DFI/DFI/R(I)/R(I)      01880005
     *            - A4(1)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2./DFI/R(I)     01890000
        DO 460 N=2,  4*N9,1                                             01900005
         A(N+4*N9*(I-1),N-1+4*N9*(I-1))= A3(N)/DFI/DFI/R(I)/R(I)        01910005
     *            - A4(N)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2./DFI/R(I)     01920005
  460   CONTINUE                                                        01930005
C       >>>>>>>>>>> PUNKT LINKS   <<<<<<<<<<                            01940005
        DO 480 I=1,  I9-1,1                                             01950005
         A(4*N9+4*N9*(I-1),1  +4*N9*(I-1))= A3(4*N9)/DFI/DFI/R(I)/R(I)  01960005
     *            + A4(4*N9)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2./DFI/R(I)  01970005
        DO 480 N=1,  4*N9-1 ,1                                          01980005
         A(N+4*N9*(I-1),N+1+4*N9*(I-1))= A3(N)/DFI/DFI/R(I)/R(I)        01990000
     *            + A4(N)*(DR(I+1)*DR1(I)-DR(I)*DR2(I))/2./DFI/R(I)     02000005
  480   CONTINUE                                                        02010005
C       >>>>>>>>>>>  NULLPUNKT  <<<<<<<<<<<<<                           02020005
        H2=N9                                                           02030005
        DO 500 N=1, 2*N9, 1                                             02040005
         A(4*N9*(I9-1)+1,N    )=2./DR(1)/DR(1)/(B(N)+B(2*N9+N))/H2/B(N) 02050005
         A(4*N9*(I9-1)+1,2*N9+N)=2./DR(1)/DR(1)/(B(N)+B(2*N9+N))/H2     02060005
     *                             /B(2*N9+N)                           02070005
  500   CONTINUE                                                        02080005
        H1=0.0                                                          02090000
        DO 540 N=1,2*N9,1                                               02100005
           H1=H1-(1./B(N    )+1./B(2*N9+N))/(B(N    )+B(2*N9+N))        02110005
  540   CONTINUE                                                        02120005
        A(4*N9*(I9-1)+1,4*N9*(I9-1)+1)=H1*2./H2/DR(1)/DR(1)             02130005
C                                                                       02140005
        DO 560 I=1,4*N9*(I9-1)+1,1                                      02150005
         IF (ICNTL .EQ. 1)   A(I,I)=A(I,I) - 1.                         02160005
  560   CONTINUE                                                        02170005
C       ***************************************************             02180005
C       ****   AUFSTELLEN DER MATRIX BL(  )            ****             02190000
C       ***************************************************             02200005
C                                                                       02210005
        DO 580 N=0,4*N9+1,1                                             02220005
           IF (ICNTL .EQ. 1)   FB(N)=C                                  02230005
           IF (ICNTL .EQ. 2)   FB(N)=-GAMMA*KAPPA(N)                    02240005
  580   CONTINUE                                                        02250005
C                                                                       02260005
C       >>>>>>>>  RANDPUNKT DARUEBER(MITTE,LINKS,RECHTS) <<<<<<<        02270005
        I=I9-1                                                          02280005
        DO 600 N=1,  4*N9 ,1                                            02290000
         H1=(A1(N)*2.* DR2(I)                                           02300005
     *    +A2(N)*(DR(I)*DR2(I))/R(I))   * FB(N)                         02310005
         H1=H1+ A4(N)*DR(I)*DR2(I)/2./DFI/R(I) * FB(N+1)                02320005
         H1=H1- A4(N)*DR(I)*DR2(I)/2./DFI/R(I) * FB(N-1)                02330005
         BL(N+4*N9*(I-1))=-H1                                           02340005
  600   CONTINUE                                                        02350005
        DO 610 I=1, 4*N9*(I9-1)+1,1                                     02360005
           IF (ICNTL .EQ. 2)   BL(I)=BL(I)+(RHON(I)-1.)/RHOA            02370005
  610   CONTINUE                                                        02380005
C                                                                       02390000
C                                                                       02400005
C                                                                       02410005
        CALL DLAX(A,401,4*N9*(I9-1)+1,BL,EPSZ,ISW,IS,VW,IP,ICON)        02420005
C                                                                       02430005
        DO 820 I=1,4*N9*(I9-1)+1,1                                      02440005
           IF (ICNTL .EQ. 1)   RHON(I)=BL(I)                            02450005
           IF (ICNTL .EQ. 2)   GPOT(I)=BL(I)                            02460005
  820   CONTINUE                                                        02470005
        IF (ICNTL. EQ. 1) GOTO 2100                                     02480005
C                                                                       02490000
        IF (COUN9 .EQ. CPRIN) THEN                                      02500005
           WRITE (6,42) T9,COUNT                                        02510005
           I=4*N9*(I9-1)+1                                              02520005
           WRITE (6,40) 0,GPOT(I),RHON(I)                               02530005
           DO 7777 I=1, 4*N9*(I9-1)  , 1                                02540005
              WRITE (6,40) I,GPOT(I),RHON(I)                            02550005
 7777      CONTINUE                                                     02560005
           DO 7778 I=1,4*N9,1                                           02570005
              WRITE (6,40) 4*N9*(I9-1)+I,-GAMMA*KAPPA(I),C              02580005
 7778      CONTINUE                                                     02590000
        ENDIF                                                           02600005
   40   FORMAT (1X,I8,2X,D23.16,2X,D23.16)                              02610005
   42   FORMAT (1X,'T = ',D23.16,2X,'SCHRITT = ',I8)                    02620005
C                                                                       02630005
C       ******************************************************          02640005
C       ****         BEWEGUNG DES RANDES          ****                  02650005
C       ******************************************************          02660005
C                                                                       02670005
        DO 900 N=1, 4*N9, 1                                             02680005
           H3=-GAMMA*KAPPA(N)                                           02690000
           H1=GPOT(N+4*N9*(I9-2))                                       02700005
           H2=GPOT(N+4*N9*(I9-3))                                       02710005
           BPKT(N)= G(N) *( (H3-H1)*(DR(I9)+DR(I9-1))/DR(I9)/DR(I9-1)   02720005
     *       -(H3-H2)*DR(I9)/DR(I9-1)/(DR(I9)+DR(I9-1)) )/B(N)/B(N)/B(N)02730005
           BPKT(N)=BPKT(N) +BA1(N)*GAMMA* (KAPPA(N+1)-KAPPA(N-1))/2./DFI02740005
     *                                     /B(N)/B(N)                   02750005
  900   CONTINUE                                                        02760005
        DO 920 I=1,4*N9,1                                               02770005
           IF (COUN9 .EQ. CPRIN) WRITE (6,40) I,B(I),BPKT(I)            02780005
           B(I)=B(I)+BPKT(I)*DELT                                       02790000
  920   CONTINUE                                                        02800005
        ICNTL=0                                                         02810005
        IF (COUN9 .EQ. CPRIN) COUN9=0                                   02820005
        IF (COUNT .EQ. CEND) GOTO 9000                                  02830005
        GOTO 2000                                                       02840005
 9000   END                                                             02850005
//LKED.SYSLIB   DD  DSN=SYS1.SSL2,DISP=SHR                              02860005
//              DD  DSN=SYS1.FORTLIB,DISP=SHR                           02870005
//GO.SYSIN DD *                                                         02880005
/*                                                                      02890000
/*      EXEC DRUCKE                                                     02900005
/*      END                                                             02910005
