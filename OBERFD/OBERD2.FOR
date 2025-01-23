C/D14KD    JOB RACF,CLASS=C,                                            00010002
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020002
C/            EXEC FORTCLG                                              00030002
C/FORT.SYSIN  DD *                                                      00040002
C                                                                       00050000
C       #############################################################   00060000
C       ####   OBERFLAECHEN-DYNAMIK  (2-DIMENSIONAL)    MIT      ####   00070000
C       ####      DIFFUSIONSGLEICHUNG AUF DER OBERFLAECHE        ####   00080000
C       ####                                                     ####   00090000
C       ####     (DX/DT)*NORMALE = U(KAPPA)                      ####   00100000
C       #### U(KAPPA) = FNV(KAPPA)+EPS*FNEPSA(A)*(DDKAPPA/DS/DS) ####   00110000
C       ####                      +FNW(A)                        ####   00120000
C       #### DA/DT = DA * DDA/DS/DS + FNG(A) - A*KAPPA*U(KAPPA)  ####   00130000
C       ####                                                     ####   00140000
C       ####        NAME DES PROGRAMMES : OBERFD(OBERD2)         ####   00150000
C       #############################################################   00160000
C                                                                       00170000
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00180000
C                                                                       00190000
C                                                                       00200000
        INTEGER           I,J,K,N,T7,T9,ZSEND,ZSPRT,T99,ND,NDEND,APR    00210000
        INTEGER           NURNR,CPUSEC                                  00220002
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF                    00230000
        DOUBLE PRECISION  AKO,BKO,CKO                                   00240000
        DOUBLE PRECISION  EPS,DA,AGES,K0                                00250004
        DOUBLE PRECISION  FNV,XD,FNW,FNG,FNEPSA                         00260005
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8                   00270000
        DOUBLE PRECISION  AMPLI(0:20)                                   00280000
        DOUBLE PRECISION  X(0:201),Y(0:201)                             00290004
        DOUBLE PRECISION  XT(0:201),YT(0:201)                           00300004
        DOUBLE PRECISION  X1(0:201),Y1(0:201)                           00310004
        DOUBLE PRECISION  X2(0:201),Y2(0:201)                           00320004
        DOUBLE PRECISION  DS(0:201),DSPKT(0:201)                        00330000
        DOUBLE PRECISION  GAM(0:201),GAMPKT(0:201)                      00340000
        DOUBLE PRECISION  G(0:201),WG(0:201)                            00350000
        DOUBLE PRECISION  KAPPA(0:201),KASS(0:201)                      00360004
        DOUBLE PRECISION  KA1(0:201),KA2(0:201)                         00370004
        DOUBLE PRECISION  AKOR(0:201)                                   00380000
        DOUBLE PRECISION  AKO1(0:201),BKO1(0:201),CKO1(0:201)           00390000
        DOUBLE PRECISION  AKO2(0:201),BKO2(0:201),CKO2(0:201)           00400000
        DOUBLE PRECISION  A(0:201),AT(0:201)                            00410003
        DOUBLE PRECISION  A1(0:201),A2(0:201)                           00420004
        DOUBLE PRECISION  HI1(0: 4001),HI2(0: 4001),HI3(0: 4000)        00430000
        CHARACTER         DNW*30                                        00440000
C       FNV(XD,K0)=(-.2D0   *(XD-K0)    +1.D0)*(XD-K0)                  00450005
        FNV(XD,K0)=( -(XD-K0)*(XD-K0) + 1.D0 )*(XD-K0)                  00460005
C       FNV(XD,K0)=DTANH(XD-K0)                                         00470005
C       FNV(XD,K0)=(XD-K0)*.10D0                                        00480005
        FNW(XD)=(XD-1.D0)*0.0D0                                         00490005
        FNG(XD)=(-1.D0*XD+1.D0)*0.0D0                                   00500005
        FNEPSA(XD)=1.00000D0                                            00510005
C                                                                       00520000
C       **************************************************              00530000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00540000
C       **************************************************              00550000
C                                                                       00560000
                 DNW   = 'D14K.TES1.DATA'                               00570005
                 DELT  =      0.00001000D0 *    4.D0                    00580006
                 N     =     72                                         00590006
                 NURNR =      0                                         00600000
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.          00610000
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00620000
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG            00630000
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.            00640000
C       NURNR=2 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00650000
C                 KEIN PUNKT IST HIERBEI AUSGEZEICHNET.                 00660000
C       N : ANZ. DER STUETZPUNKTE  / DELT=DELTA T                       00670000
C       BRAUCHBARE WERTE FUER DELTA T (DELT):                           00680005
C                B = 1/K0 = 1.0 , N=40  :  0.00001D0 * 32.D0            00690005
C                B = 1/K0 = 1.0 , N=72  :  0.00001D0 *  4.D0            00700006
C       CPU-ZEITEN IM INTERAKTIVEN BETRIEB:                             00710005
C                FUER N=40 , NURNR=0 , 10000 ITERAT.:  58 SEKUNDEN      00720005
C                FUER N=80 , NURNR=0 , 10000 ITERAT.: 116 SEKUNDEN      00730005
C                                                                       00740001
C                                                                       00750000
                 ZSEND = 500000                                         00760007
                 ZSPRT =  50000                                         00770007
C                                                                       00780000
                 K0    =      1.00000000D0                              00790004
                 EPS   =      0.12500000D+00                            00800002
                 DA    =      0.00000000D+00                            00810000
C                                                                       00820000
                 ND    =   4000                                         00830000
                 NDEND =     50                                         00840002
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN     00850000
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN               00860000
                 ND1=ND                                                 00870000
                 N1=N                                                   00880000
                 DGAM=1.D0/N1                                           00890000
                 PI=4.D0*DATAN(1.D0)                                    00900000
C                                                                       00910000
C       *************************************************               00920000
C       ****    STARTKONZENTRATION UND START-RAND    ****               00930000
C       *************************************************               00940000
C                                                                       00950000
        AMPLI(0) = 1.D0/K0                                              00960004
        AMPLI(1) = 0.0000D0                                             00970002
        AMPLI(2) = 0.0500D0                                             00980002
        AMPLI(3) = 0.0000D0                                             00990000
        AMPLI(4) = 0.0000D0                                             01000000
        AMPLI(5) = 0.0000D0                                             01010000
        AMPLI(6) = 0.0000D0                                             01020000
        AMPLI(7) = 0.0000D0                                             01030000
        AMPLI(8) = 0.0000D0                                             01040000
        AMPLI(9) = 0.0000D0                                             01050000
        AMPLI(10)= 0.0000D0                                             01060000
        AMPLI(11)= 0.0000D0                                             01070000
        AMPLI(12)= 0.0000D0                                             01080000
        AMPLI(13)= 0.0000D0                                             01090000
        AMPLI(14)= 0.0000D0                                             01100000
        AMPLI(15)= 0.0000D0                                             01110000
        AMPLI(16)= 0.0000D0                                             01120000
        AMPLI(17)= 0.0000D0                                             01130000
        AMPLI(18)= 0.0000D0                                             01140000
        AMPLI(19)= 0.0000D0                                             01150000
        AMPLI(20)= 0.0000D0                                             01160000
C                                                                       01170000
        DO 140 J=0, N+1, 1                                              01180000
           H1=J                                                         01190000
C          H2=DCOS(2.D0*PI*H1/N1)                                       01200000
           H2=0.0                                                       01210000
           A(J)=1.D0  + .5D0 *H2                                        01220000
  140   CONTINUE                                                        01230000
        A(0)=1.0D0                                                      01240000
        A(N)=A(0)                                                       01250000
C                                                                       01260000
        DO 100 J=0 , ND+1 , 1                                           01270000
           H1=J                                                         01280000
           H3=0.D0                                                      01290000
           DO 102 K=0, 20 , 1                                           01300000
              H2=K                                                      01310000
              H3=H3 + AMPLI(K)*DCOS(2.D0*H2*PI*H1/ND1)                  01320000
  102      CONTINUE                                                     01330000
           HI1(J)=DCOS(2.D0*PI*H1/ ND1)*H3                              01340000
           HI2(J)=DSIN(2.D0*PI*H1/ ND1)*H3                              01350000
  100   CONTINUE                                                        01360000
        HI1(0)=HI1(0)                                                   01370000
        HI1(ND)=HI1(0)                                                  01380000
                                                                        01390000
C                                                                       01400000
        HI3(0)=0.0                                                      01410000
        DO 110 J=1, ND  ,1                                              01420000
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        01430000
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  01440000
  110   CONTINUE                                                        01450000
C                                                                       01460000
        STOT=HI3( ND  )                                                 01470000
        J=0                                                             01480000
        DO 120 K=1, N-1, 1                                              01490000
           H1=K                                                         01500000
  130      J=J+1                                                        01510000
           IF (J .GT. ND+2) GOTO 120                                    01520000
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         01530000
              H5=HI3(J)-HI3(J-1)                                        01540000
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             01550000
              H4=(HI3(J) - H1*STOT/N1)/H5                               01560000
              X(K)=HI1(J)*H3 + HI1(J-1)*H4                              01570004
              Y(K)=HI2(J)*H3 + HI2(J-1)*H4                              01580004
  120   CONTINUE                                                        01590000
        X(0)=HI1(0)                                                     01600004
        Y(0)=HI2(0)                                                     01610004
        X(N)=X(0)                                                       01620004
        Y(N)=Y(0)                                                       01630004
        X(N+1)=X(1)                                                     01640004
        Y(N+1)=Y(1)                                                     01650004
C                                                                       01660000
C                                                                       01670000
C                                                                       01680000
C       **************************************************              01690000
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              01700000
C       **************************************************              01710000
C                                                                       01720000
        OPEN (22,FILE=DNW)                                              01730000
        REWIND 22                                                       01740000
   10   FORMAT (///' PROGRAMM-NAME :  OBERFD(OBERD2)')                  01750000
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                01760000
     *        ,F10.7,'    N =',I4)                                      01770000
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          01780000
   40   FORMAT (/,' EPS =',F10.5,'  DA =',F10.5,'   B =',F10.5)         01790000
        WRITE (6,10)                                                    01800000
        WRITE (6,20) DGAM,DELT,N                                        01810000
        WRITE (6,30) ZSEND,ZSPRT                                        01820000
        WRITE (6,40) EPS,DA,1.D0/K0                                     01830004
C                                                                       01840000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01850000
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              01860000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01870000
C                                                                       01880000
        T9=-1                                                           01890000
        T7=0                                                            01900000
        T8     = -DELT                                                  01910000
        APR=0                                                           01920000
 1000   T9=T9+1                                                         01930000
        T8=T8+DELT                                                      01940000
C                                                                       01950000
C       **************************************************              01960000
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****              01970000
C       *****                                         ****              01980000
C       **************************************************              01990000
C                                                                       02000000
 1500   STOT=0.0D0                                                      02010000
        DO 200 J=1, N   ,1                                              02020000
            DS(J)=DSQRT(( X(J   )- X(J-1))*( X(J    )- X(J-1))+         02030004
     *            ( Y(J   )- Y(J-1))*( Y(J    )- Y(J-1)))               02040004
        STOT=STOT+DS(J)                                                 02050000
  200   CONTINUE                                                        02060000
        DS(0)=DS(N)                                                     02070000
        DS(N+1)=DS(1)                                                   02080000
C                                                                       02090000
        GAM(0)=0.D0                                                     02100000
        DO 210 J=1,N,1                                                  02110000
           GAM(J)=GAM(J-1)+DS(J)/STOT                                   02120000
  210   CONTINUE                                                        02130000
        GAM(N+1)=1.D0+GAM(1)                                            02140000
C                                                                       02150000
C                                                                       02160000
        DO 220 J=1,  N, 1                                               02170000
           H1=DS(J)                                                     02180000
           H2=DS(J+1)                                                   02190000
           H3=H1+H2                                                     02200000
           X2(J)=(X(J+1)/H2/H3-X(J)/H1/H2+X(J-1)/H1/H3)*2.D0            02210004
           Y2(J)=(Y(J+1)/H2/H3-Y(J)/H1/H2+Y(J-1)/H1/H3)*2.D0            02220004
           X1(J)=(X(J)-X(J-1))/H1 + X2(J)*H1/2.D0                       02230004
           Y1(J)=(Y(J)-Y(J-1))/H1 + Y2(J)*H1/2.D0                       02240004
           G(J)=X1(J)*X1(J)+Y1(J)*Y1(J)                                 02250004
           WG(J)=DSQRT(G(J))                                            02260000
  220   CONTINUE                                                        02270000
        G(0)=G(N)                                                       02280000
        G(N+1)=G(1)                                                     02290000
        X1(0)=X1(N)                                                     02300004
        Y1(0)=Y1(N)                                                     02310004
        X2(0)=X2(N)                                                     02320004
        Y2(0)=Y2(N)                                                     02330004
        WG(0)=WG(N)                                                     02340000
C                                                                       02350000
        DO 230 J=1,N,1                                                  02360000
           KAPPA(J)=(-Y1(J)*X2(J)+X1(J)*Y2(J))/WG(J)/G(J)               02370004
  230   CONTINUE                                                        02380000
        KAPPA(N+1)=KAPPA(1)                                             02390000
        KAPPA(0)=KAPPA(N)                                               02400000
C                                                                       02410000
        DO 240 J=1,N,1                                                  02420000
           H1=DS(J)                                                     02430000
           H2=DS(J+1)                                                   02440000
           H3=H1+H2                                                     02450000
           KA2(J)= KAPPA(J+1)/H2/H3-KAPPA(J)/H1/H2+KAPPA(J-1)/H1/H3     02460004
           KA2(J)=KA2(J)*2.D0                                           02470004
           KA1(J)=( KAPPA(J)- KAPPA(J-1))/H1 + KA2(J)*H1/2.D0           02480004
           KASS(J)=KA2(J)/G(J) - KA1(J)*                                02490004
     *               (X2(J)*X1(J)+Y2(J)*Y1(J))/G(J)/G(J)                02500004
  240   CONTINUE                                                        02510000
        KASS(0)=KASS(N)                                                 02520004
        KASS(N+1)=KASS(1)                                               02530004
C                                                                       02540000
        STPKT=0.0D0                                                     02550000
        DO 250 J=1,N,1                                                  02560000
          H1 = KAPPA(J-1) * (     FNV(KAPPA(J-1),K0) +                  02570005
     *         EPS*FNEPSA( A(J-1) )*KASS(J-1)  +  FNW( A(J-1))     )    02580005
          H2 = KAPPA(J  ) * (     FNV(KAPPA(J  ),K0) +                  02590005
     *         EPS*FNEPSA( A(J  ) )*KASS(J  )  +  FNW( A(J  ))     )    02600005
          DSPKT(J)= (H1+H2)*DS(J)/2.D0                                  02610000
          STPKT   = STPKT + DSPKT(J)                                    02620000
  250   CONTINUE                                                        02630000
        DSPKT(0)=DSPKT(N)                                               02640000
C                                                                       02650000
        IF (NURNR .EQ. 2) THEN                                          02660000
        DO 261 I=1, N, 1                                                02670000
           GAMPKT(I)=0.0D0                                              02680000
           DO 260 J=1, N, 1                                             02690000
              K=I+J                                                     02700000
              IF (K .GT. N) K=K-N                                       02710000
              H1=J+J-N-1                                                02720000
              GAMPKT(I)=GAMPKT(I)+H1*(DSPKT(K)-DS(K)*STPKT/STOT)        02730000
  260      CONTINUE                                                     02740000
           H1=N+N                                                       02750000
           GAMPKT(I)=GAMPKT(I)/H1/STOT                                  02760000
  261   CONTINUE                                                        02770000
        ENDIF                                                           02780000
        IF (NURNR .EQ. 0) THEN                                          02790000
        H1=0.D0                                                         02800000
        DO 270 I=1, N, 1                                                02810000
           H1=H1+DSPKT(I)                                               02820000
           GAMPKT(I)=(H1-STPKT*GAM(I))/STOT                             02830000
  270   CONTINUE                                                        02840000
        ENDIF                                                           02850000
C                                                                       02860000
C       ***************************************************             02870000
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             02880000
C       ***************************************************             02890000
C                                                                       02900000
 1300   T99=0                                                           02910000
        H2=DGAM/4.D0                                                    02920000
        DO 300 J=1, N , 1                                               02930000
           H1=J                                                         02940000
           H3=H1*DGAM                                                   02950000
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))        02960000
     *            T99=1                                                 02970000
  300   CONTINUE                                                        02980000
        MDIFF=0.0D0                                                     02990000
        DO 310 J=1, N ,1                                                03000000
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))                     03010004
  310   CONTINUE                                                        03020000
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1                        03030000
C                                                                       03040000
C       T99=0                                                           03050000
        IF (T99 .EQ. 0) GOTO 1400                                       03060000
C                                                                       03070000
C                                                                       03080000
C       ***************************************************             03090000
C       ****          NEUE DISKRETISIERUNG             ****             03100000
C       ***************************************************             03110000
C                                                                       03120000
        T7=T7+1                                                         03130000
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  03140000
     *          I11)                                                    03150000
        WRITE (6,90) T7,T9                                              03160000
        IF (T7 .GT. NDEND) GOTO 1200                                    03170000
C       ** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN             03180000
C       ** EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE          03190000
        DO 400 J=1,  N  , 1                                             03200000
           H2=GAM(J+1)-GAM(J)                                           03210000
           H1=GAM(J)-GAM(J-1)                                           03220000
           H3=H1+H2                                                     03230000
           AKO1(J)=X(J-1)/H1/H3-X(J)/H1/H2+X(J+1)/H2/H3                 03240004
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X(J)-X(J-1))*H2/H1/H3          03250004
           BKO1(J)=BKO1(J)+(X(J+1)-X(J))*H1/H2/H3                       03260004
           CKO1(J)=X(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)                 03270004
           AKO2(J)=Y(J-1)/H1/H3-Y(J)/H1/H2+Y(J+1)/H2/H3                 03280004
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(Y(J)-Y(J-1))*H2/H1/H3          03290004
           BKO2(J)=BKO2(J)+(Y(J+1)-Y(J))*H1/H2/H3                       03300004
           CKO2(J)=Y(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)                 03310004
  400   CONTINUE                                                        03320000
           H2=GAM(1)-GAM(0)                                             03330000
           H1=GAM(N)-GAM(N-1)                                           03340000
           H3=H1+H2                                                     03350000
           AKO1(0)=X(N-1)/H1/H3-X(0)/H1/H2+X(     1)/H2/H3              03360004
           BKO1(0)=-2.*GAM(0)*AKO1(0)+(X(0)-X(N-1))*H2/H1/H3            03370004
           BKO1(0)=BKO1(0)+(X(   1)-X(0))*H1/H2/H3                      03380004
           CKO1(0)=X(0)-(AKO1(0)*GAM(0)+BKO1(0))*GAM(0)                 03390004
           AKO2(0)=Y(N-1)/H1/H3-Y(0)/H1/H2+Y(     1)/H2/H3              03400004
           BKO2(0)=-2.*GAM(0)*AKO2(0)+(Y(0)-Y(N-1))*H2/H1/H3            03410004
           BKO2(0)=BKO2(0)+(Y(   1)-Y(0))*H1/H2/H3                      03420004
           CKO2(0)=Y(0)-(AKO2(0)*GAM(0)+BKO2(0))*GAM(0)                 03430004
C                                                                       03440000
        HI1(0)=X(0)                                                     03450004
        HI2(0)=Y(0)                                                     03460004
        J=0                                                             03470000
        DO 410 K=1, N,1                                                 03480000
  420      J=J+1                                                        03490000
           IF (J .GT. ND+2) GOTO 410                                    03500000
           H1=J                                                         03510000
           H2=H1/ND1                                                    03520000
           IF (H2 .LT. GAM(K)) THEN                                     03530000
              H5=GAM(K)-GAM(K-1)                                        03540000
              H3=(GAM(K)-H2)/H5                                         03550000
              H4=(H2-GAM(K-1))/H5                                       03560000
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        03570000
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     03580000
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        03590000
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     03600000
              GOTO 420                                                  03610000
           ELSE                                                         03620000
              J=J-1                                                     03630000
              GOTO 410                                                  03640000
           ENDIF                                                        03650000
  410   CONTINUE                                                        03660000
        HI1(ND  )=HI1(0)                                                03670000
        HI2(ND  )=HI2(0)                                                03680000
        HI1(ND+1)=HI1(1)                                                03690000
        HI2(ND+1)=HI2(1)                                                03700000
C                                                                       03710000
C       ** BERECHNEN DER NEUEN KONZENTRATIONSWERTE                      03720000
        DO 430 J=1,  N-1, 1                                             03730000
           H2=GAM(J+1)-GAM(J)                                           03740000
           H1=GAM(J)-GAM(J-1)                                           03750000
           H3=H1+H2                                                     03760000
           AKO=A    (J-1)/H1/H3-A    (J)/H1/H2+A    (J+1)/H2/H3         03770000
           BKO=-2.D0*GAM(J)*AKO+(A    (J)-A    (J-1))*H2/H1/H3          03780000
           BKO=BKO+(A    (J+1)-A    (J))*H1/H2/H3                       03790000
           CKO=A    (J)-(AKO*GAM(J)+BKO)*GAM(J)                         03800000
           H1=J                                                         03810000
           H2=DGAM*H1                                                   03820000
           AKOR(J)=AKO*H2*H2+BKO*H2+CKO                                 03830000
  430   CONTINUE                                                        03840000
C                                                                       03850000
        DO 440 J=1, N-1, 1                                              03860000
           A(J)=AKOR(J)                                                 03870004
  440   CONTINUE                                                        03880000
        A(N+1)=A(1)                                                     03890000
C                                                                       03900000
C       ***** BERECHNEN DER NEUEN KOORDINATENPUNKTE                     03910000
        HI3(0)=0.0D0                                                    03920000
        DO 450 J=1, ND  ,1                                              03930000
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        03940000
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  03950000
  450   CONTINUE                                                        03960000
C                                                                       03970000
        STOT=HI3( ND  )                                                 03980000
        J=0                                                             03990000
        DO 460 K=1, N-1, 1                                              04000000
           H1=K                                                         04010000
  470      J=J+1                                                        04020000
           IF (J .GT. ND+2) GOTO 460                                    04030000
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         04040000
              H5=HI3(J)-HI3(J-1)                                        04050000
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             04060000
              H4=(HI3(J) - H1*STOT/N1  )/H5                             04070000
              X(K)=HI1(J)*H3 + HI1(J-1)*H4                              04080004
              Y(K)=HI2(J)*H3 + HI2(J-1)*H4                              04090004
  460   CONTINUE                                                        04100000
        X(0)=HI1(0)                                                     04110004
        Y(0)=HI2(0)                                                     04120004
        X(N)=X(0)                                                       04130004
        Y(N)=Y(0)                                                       04140004
        X(N+1)=X(1)                                                     04150004
        Y(N+1)=Y(1)                                                     04160004
        GOTO 1500                                                       04170000
C                                                                       04180000
C       ***************************************************             04190000
C       ****    ABLEITUNGEN DER DIFFUSIONSGLEICHUNG    ****             04200000
C       ****           AUF DER OBERFLAECHE             ****             04210000
C       ***************************************************             04220000
C                                                                       04230000
 1400   DO 500 J=1, N, 1                                                04240000
           H1=DS(J)                                                     04250000
           H2=DS(J+1)                                                   04260000
           H3=H1+H2                                                     04270000
           A2(J)=(A(J+1)/H2/H3-A(J)/H1/H2+A(J-1)/H1/H3)*2.D0            04280004
           A1(J)=(A(J)-A(J-1))/H1 + A2(J)*H1/2.D0                       04290004
  500   CONTINUE                                                        04300000
        A1(0)=A1(N)                                                     04310004
        A2(0)=A2(N)                                                     04320004
C                                                                       04330000
C       ***************************************************             04340003
C       **** BERECHNUNG DER ZEITABLEITUNGEN            ****             04350003
C       ***************************************************             04360003
C                                                                       04370003
        DO 510 J= 1,N,1                                                 04380003
           H1=FNV(KAPPA(J),K0)+EPS*FNEPSA(A(J))*KASS(J)+FNW(A(J))       04390005
           XT(J) =     Y1(J)*H1/WG(J) - X1(J)*STOT*GAMPKT(J)            04400004
           YT(J) =    -X1(J)*H1/WG(J) - Y1(J)*STOT*GAMPKT(J)            04410004
           H2=DA*A2(J)+FNG(A(J))-A(J)*KAPPA(J)*H1                       04420004
           AT(J)  =     H2 - A1(J)*STOT*GAMPKT(J)                       04430004
  510   CONTINUE                                                        04440003
C                                                                       04450003
C       ***************************************************             04460003
C       **** BERECHNUNG VON DELTA T (DELT)             ****             04470003
C       ***************************************************             04480003
C                                                                       04490003
        H2 = STOT*STOT/N1/N1/2.D0/PI/2.D0/PI                            04491006
        H1 = 0.D0                                                       04500004
        DO 520 J=1,N                                                    04510004
           H1 = H1 + DSQRT( XT(J)*XT(J) + YT(J)*YT(J) ) * DELT          04520004
  520   CONTINUE                                                        04530003
C       IF (H1 .LT. H2*0.1000D0) DELT=DELT*2.D0                         04540006
        DO 530 J=1,N                                                    04550004
        DO 530 I=0,30                                                   04570005
           H1 = DSQRT( XT(J)*XT(J) + YT(J)*YT(J) ) * DELT               04571006
           IF (H1 .GT. H2*0.2000D0) DELT=DELT*.5D0                      04580006
  530   CONTINUE                                                        04590004
C                                                                       04600003
C       ***************************************************             04610000
C       ****            PRINT PRINT PRINT              ****             04620000
C       ***************************************************             04630000
C                                                                       04640000
C                                                                       04650000
        H2=T9                                                           04660000
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          04670000
C                                                                       04680000
        AGES=0.0D0                                                      04690000
        DO 99 J=1, N ,1                                                 04700000
           AGES=(A(J-1)+A(J))*.5D0*DS(J) + AGES                         04710000
   99   CONTINUE                                                        04720000
C                                                                       04730000
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    04740000
     *     F12.6)                                                       04750000
   59   FORMAT (' CPU-ZEIT(SEC) =',I5,'    AGES =',F13.5,'  DELTA T=',  04760005
     *     F14.8)                                                       04770005
   60   FORMAT(/,T5,'J',T17,'X',T29,'Y',T37,'  DGA',T49,'KAPPA',        04780004
     *               T64,'VN',T74,'KASS')                               04790006
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.4,T56,F10.5, 04800005
     *           T68,F10.3)                                             04810005
   61   FORMAT(1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5)            04820000
   71   FORMAT(1X,'>>>>',I8,3X,I10)                                     04830000
        CALL CLOCK(CPUSEC)                                              04840002
        APR=APR+1                                                       04850000
        WRITE (6,50) T8,T9,STOT                                         04860000
        WRITE (6,59) CPUSEC,AGES,DELT                                   04870002
        WRITE (6,60)                                                    04880000
        WRITE (22,71) APR,N                                             04890000
        WRITE (22,61) T8,STOT                                           04900000
        DO 550 J=0 ,N ,1                                                04910000
           H1=FNV(KAPPA(J),K0)+EPS*FNEPSA(A(J))*KASS(J)+FNW(A(J))       04911006
           WRITE (6,70) J,X(J),Y(J),DS(J)/STOT,KAPPA(J),                04920006
     *                  H1,KASS(J)                                      04921006
           WRITE (22,61) X(J),Y(J),GAM(J),KAPPA(J),A(J)                 04930004
  550   CONTINUE                                                        04940000
        WRITE (22,71) 0,0                                               04950000
        BACKSPACE 22                                                    04960000
C       IF (T99 .EQ. 1) GOTO 1700                                       04970000
C                                                                       04980000
C       **************************************************              04990000
C       ****      BERECHNEN DER NEUEN WERTE           ****              05000000
C       **************************************************              05010000
C                                                                       05020000
 1100   DO 600 J= 1,N,1                                                 05030000
           X(J)=X(J) + XT(J)*DELT                                       05040004
           Y(J)=Y(J) + YT(J)*DELT                                       05050004
           A(J) = A(J) +  AT(J)*DELT                                    05060003
  600   CONTINUE                                                        05070000
        X(0)=X(N)                                                       05080004
        Y(0)=Y(N)                                                       05090004
        A (0)=A (N)                                                     05100000
        X(N+1)=X(1)                                                     05110004
        Y(N+1)=Y(1)                                                     05120004
        A (N+1)=A (1)                                                   05130000
C                                                                       05140000
C                                                                       05150000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              05160000
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              05170000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              05180000
C                                                                       05190000
        IF (T9 .GE. ZSEND) GOTO 1200                                    05200000
        GOTO 1000                                                       05210000
 1200   CONTINUE                                                        05220000
        CLOSE(22,STATUS='KEEP')                                         05230000
C       END                                                             05240002
C/GO.SYSIN    DD *                                                      05250002
C*                                                                      05260002
C*      EXEC DRUCKE                                                     05270002
        END                                                             05280002
