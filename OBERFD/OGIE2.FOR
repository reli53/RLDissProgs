//D14KOGIE JOB RACF,                                                    00010000
//      CLASS=D,MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=D14K                   00020000
//      EXEC FORTCLG                                                    00030000
//FORT.SYSIN DD *                                                       00040000
C                                                                       00050000
C                                                                       00060000
C                                                                       00070000
C                                                                       00080000
C                                                                       00090000
C       ##################################################              00100000
C       ####          OBERFLAECHEN-DYNAMIK            ####              00110000
C       ####             (2-DIMENSIONAL)              ####              00120000
C       ####     MIT DIFFUSIONSGLEICHUNG AUF DER      ####              00130000
C       ####               OBERFLAECHE                ####              00140000
C       ####                                          ####              00150000
C       ####   NAME DES PROGRAMMES :   RELI(OGIE2)    ####              00160000
C       ##################################################              00170000
C                                                                       00180000
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00190000
C                                                                       00200000
C                                                                       00210000
        INTEGER           J,N,T9,ZU,K,T7,ZSEND,ZSPRT,T99,ND,NDEND,AB,APR00220000
        DOUBLE PRECISION  DGAM,DELT,     STOT,MDIFF                     00230000
        DOUBLE PRECISION  EPS,DA,DH                                     00240000
        DOUBLE PRECISION  FNV,FNVA,FNFA,FNFH,X,Y                        00250000
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8                   00260000
        DOUBLE PRECISION  X1(0:201),X2(0:201)                           00270000
        DOUBLE PRECISION  X1GA2(0:201),X2GA2(0:201)                     00280000
        DOUBLE PRECISION  XZ1(0:201),XZ2(0:201)                         00290000
        DOUBLE PRECISION  TAU1(0:201),TAU2(0:201)                       00300000
        DOUBLE PRECISION  G(0:201),WG(0:201),S(0:201)                   00310000
        DOUBLE PRECISION  SPKT(0:201),KAPPA(0:201),KAS2(0:201)          00320000
        DOUBLE PRECISION  AKOR(0:201)                                   00330000
        DOUBLE PRECISION  AKO1(0:201),BKO1(0:201),CKO1(0:201)           00340000
        DOUBLE PRECISION  AKO2(0:201),BKO2(0:201),CKO2(0:201)           00350000
        DOUBLE PRECISION  DGA(0:201)                                    00360000
        DOUBLE PRECISION  GAPU(0:201),GAMMA(0:201)                      00370000
        DOUBLE PRECISION  A(0:201),AZ(0:201)                            00380000
        DOUBLE PRECISION  H(0:201),HZ(0:201)                            00390000
        DOUBLE PRECISION  AGA(0:201),AGA2(0:201)                        00400000
        DOUBLE PRECISION  HGA(0:201),HGA2(0:201)                        00410000
        DOUBLE PRECISION  HI1(0: 4001),HI2(0: 4001),HI3(0: 4000)        00420000
        FNV(X)    =  0.0                                                00430000
        FNVA(X)   =  (X-1.0)*0.5                                        00440000
        FNFA(X,Y) =  1.0*X*X/Y   -1.0*X                                 00450000
        FNFH(X,Y) =  1.0*X*X     -1.0*Y                                 00460000
C                                                                       00470000
C       **************************************************              00480000
C       ****      PARAMETER PARAMETER PARAMETER       ****              00490000
C       **************************************************              00500000
C                                                                       00510000
                 DELT  =      0.001                                     00520000
                 N     =     60                                         00530000
C                                                                       00540000
                 ZSEND =  30000                                         00550000
                 ZSPRT =   2000                                         00560000
                 ZU    =     30                                         00570000
C                                                                       00580000
                 EPS   =      0.1                                       00590000
                 DA    =      0.08                                      00600000
                 DH    =      1.0                                       00610000
C                                                                       00620000
                 ND    =   4000                                         00630000
                 NDEND =    500                                         00640000
C                                                                       00650000
                 ND1=ND                                                 00660000
                 N1=N                                                   00670000
                 DGAM=1./N1                                             00680000
                 PI=3.1415926536                                        00690000
C                                                                       00700000
C       *************************************************               00710000
C       ****     STARTFUNKTIONEN UND START-RAND      ****               00720000
C       *************************************************               00730000
C                                                                       00740000
C                                                                       00750000
        DO 140 J=0, N+1, 1                                              00760000
           H(J)=1.                                                      00770000
           H1=J                                                         00780000
           A(J)=1.+DCOS(8.*PI*H1/N1)*.5                                 00790000
  140   CONTINUE                                                        00800000
C                                                                       00810000
        DO 100 J=0 , ND+1 , 1                                           00820000
           H1=J                                                         00830000
           H2=0.0                                                       00840000
C          H2=DCOS(8.*PI*H1/ ND1)*.5                                    00850000
           HI1(J)=DCOS(2.*PI*H1/ ND1)*(2.0 + H2)                        00860000
           HI2(J)=DSIN(2.*PI*H1/ ND1)*(2.0 + H2)                        00870000
  100   CONTINUE                                                        00880000
C                                                                       00890000
        HI3(0)=0.0                                                      00900000
        DO 110 J=1, ND  ,1                                              00910000
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        00920000
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  00930000
  110   CONTINUE                                                        00940000
C                                                                       00950000
        STOT=HI3( ND  )                                                 00960000
        J=0                                                             00970000
        DO 120 K=1, N-1, 1                                              00980000
           H1=K                                                         00990000
  130      J=J+1                                                        01000000
           IF (J .GT. ND+2) GOTO 120                                    01010000
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         01020000
              H5=HI3(J)-HI3(J-1)                                        01030000
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             01040000
              H4=(HI3(J) - H1*STOT/N1)/H5                               01050000
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             01060000
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             01070000
  120   CONTINUE                                                        01080000
        X1(0)=HI1(0)                                                    01090000
        X2(0)=HI2(0)                                                    01100000
        X1(N)=X1(0)                                                     01110000
        X2(N)=X2(0)                                                     01120000
        X1(N+1)=X1(1)                                                   01130000
        X2(N+1)=X2(1)                                                   01140000
C                                                                       01150000
C                                                                       01160000
C       **************************************************              01170000
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              01180000
C       **************************************************              01190000
C                                                                       01200000
        REWIND 21                                                       01210000
   11   FORMAT (3X,I10,3X,I10)                                          01220000
        WRITE (21,11) N                                                 01230000
   10   FORMAT (///' PROGRAMM-NAME :  RELI(OBIE2)')                     01240000
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                01250000
     *        ,F10.7,'    N =',I4)                                      01260000
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          01270000
   40   FORMAT (/,' EPS =',F10.5,'  DA =',F10.5,'  DH =',F10.5)         01280000
        WRITE (6,10)                                                    01290000
        WRITE (6,20) DGAM,DELT,N                                        01300000
        WRITE (6,30) ZSEND,ZSPRT                                        01310000
        WRITE (6,40) EPS,DA,DH                                          01320000
C                                                                       01330000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01340000
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              01350000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01360000
C                                                                       01370000
        T9=-1                                                           01380000
        T7=0                                                            01390000
        T8     = -DELT                                                  01400000
        APR=0                                                           01410000
 1000   T9=T9+1                                                         01420000
        T8=T8+DELT                                                      01430000
C                                                                       01440000
C       **************************************************              01450000
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****              01460000
C       *****                                         ****              01470000
C       **************************************************              01480000
C                                                                       01490000
 1500   S(0)=0.0                                                        01500000
        DO 200 J=1, N   ,1                                              01510000
           S  (J)=DSQRT(( X1(J  )- X1(J-1))*( X1(J  )- X1(J-1))+        01520000
     *               ( X2(J  )- X2(J-1))*( X2(J  )- X2(J-1)))+  S(J-1)  01530000
  200   CONTINUE                                                        01540000
        STOT=S(N)                                                       01550000
C                                                                       01560000
        GAMMA(0)=0.0                                                    01570000
        DO 210 J=1, N,1                                                 01580000
           GAMMA(J)=S(J)/STOT                                           01590000
           DGA(J)=GAMMA(J)-GAMMA(J-1)                                   01600000
  210   CONTINUE                                                        01610000
        GAMMA(N+1)=1.0+GAMMA(1)                                         01620000
        DGA(0)=DGA(N)                                                   01630000
        DGA(N+1)=DGA(1)                                                 01640000
C                                                                       01650000
        DO 220 J=1,  N, 1                                               01660000
           H1=DGA(J)                                                    01670000
           H2=DGA(J+1)                                                  01680000
           H3=H1+H2                                                     01690000
           X1GA2(J)=(X1(J+1)/H2/H3-X1(J)/H1/H2+X1(J-1)/H1/H3)*2.        01700000
           X2GA2(J)=(X2(J+1)/H2/H3-X2(J)/H1/H2+X2(J-1)/H1/H3)*2.        01710000
           TAU1(J)=(X1(J)-X1(J-1))/H1 + X1GA2(J)*H1/2.                  01720000
           TAU2(J)=(X2(J)-X2(J-1))/H1 + X2GA2(J)*H1/2.                  01730000
           G(J)=TAU1(J)*TAU1(J)+TAU2(J)*TAU2(J)                         01740000
           WG(J)=DSQRT(G(J))                                            01750000
  220   CONTINUE                                                        01760000
        G(0)=G(N)                                                       01770000
        G(N+1)=G(1)                                                     01780000
        TAU1(0)=TAU1(N)                                                 01790000
        TAU2(0)=TAU2(N)                                                 01800000
        X1GA2(0)=X1GA2(N)                                               01810000
        X2GA2(0)=X2GA2(N)                                               01820000
        WG(0)=WG(N)                                                     01830000
C                                                                       01840000
        STOTQU=STOT * STOT                                              01850000
        DO 230 J=1,N,1                                                  01860000
           KAPPA(J)=(-TAU2(J)*X1GA2(J)+TAU1(J)*X2GA2(J))/WG(J)/STOTQU   01870000
  230   CONTINUE                                                        01880000
        KAPPA(N+1)=KAPPA(1)                                             01890000
        KAPPA(0)=KAPPA(N)                                               01900000
C                                                                       01910000
        DO 240 J=1,N,1                                                  01920000
           H1=DGA(J)                                                    01930000
           H2=DGA(J+1)                                                  01940000
           H3=H1+H2                                                     01950000
           KAS2(J)= KAPPA(J+1)/H2/H3-KAPPA(J)/H1/H2+KAPPA(J-1)/H1/H3    01960000
           KAS2(J)=KAS2(J)*2./STOTQU                                    01970000
  240   CONTINUE                                                        01980000
        KAS2(0)=KAS2(N)                                                 01990000
        KAS2(N+1)=KAS2(1)                                               02000000
C                                                                       02010000
        SPKT(0)=0.0                                                     02020000
        DO 250 J=1,N,1                                                  02030000
           H1=KAPPA(J-1)*(FNV(KAPPA(J-1))+EPS*KAS2(J-1)+FNVA(A(J-1)))   02040000
           H2=KAPPA(J  )*(FNV(KAPPA(J  ))+EPS*KAS2(J  )+FNVA(A(J  )))   02050000
           SPKT(J)=SPKT(J-1)+STOT*(H1+H2)*DGA(J)/2.                     02060000
  250   CONTINUE                                                        02070000
        STPKT=SPKT(N)                                                   02080000
C                                                                       02090000
        GAPU(0)=0.0                                                     02100000
        DO 260 J=1, N, 1                                                02110000
           GAPU(J)=(SPKT(J)-STPKT*GAMMA(J))/STOT                        02120000
  260   CONTINUE                                                        02130000
C                                                                       02140000
C       ***************************************************             02150000
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             02160000
C       ***************************************************             02170000
C                                                                       02180000
        T99=0                                                           02190000
        DO 300 J=1, N , 1                                               02200000
           H2=DGAM/4.                                                   02210000
           H1=J                                                         02220000
           H3=H1*DGAM                                                   02230000
           IF ((GAMMA(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAMMA(J+1)-H2))    02240000
     *            T99=1                                                 02250000
  300   CONTINUE                                                        02260000
        MDIFF=0.0                                                       02270000
        DO 310 J=1, N ,1                                                02280000
           MDIFF=DMAX1(MDIFF,DABS(DGA(J)-DGAM))                         02290000
  310   CONTINUE                                                        02300000
        IF (MDIFF .GT. DGAM/2.)    T99=1                                02310000
C                                                                       02320000
        IF (T99 .EQ. 0) GOTO 1400                                       02330000
C                                                                       02340000
C                                                                       02350000
C       ***************************************************             02360000
C       ****          NEUE DISKRETISIERUNG             ****             02370000
C       ***************************************************             02380000
C                                                                       02390000
        T7=T7+1                                                         02400000
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  02410000
     *          I11)                                                    02420000
        WRITE (6,90) T7,T9                                              02430000
        IF (T7 .GT. NDEND) GOTO 1200                                    02440000
C       ** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN             02450000
C       ** EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE          02460000
        DO 400 J=1,  N  , 1                                             02470000
           H2=DGA(J+1)                                                  02480000
           H1=DGA(J)                                                    02490000
           H3=H1+H2                                                     02500000
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3              02510000
           BKO1(J)=-2.*GAMMA(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3        02520000
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3                     02530000
           CKO1(J)=X1(J)-(AKO1(J)*GAMMA(J)+BKO1(J))*GAMMA(J)            02540000
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3              02550000
           BKO2(J)=-2.*GAMMA(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3        02560000
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3                     02570000
           CKO2(J)=X2(J)-(AKO2(J)*GAMMA(J)+BKO2(J))*GAMMA(J)            02580000
  400   CONTINUE                                                        02590000
           H2=DGA(  1)                                                  02600000
           H1=DGA(0)                                                    02610000
           H3=H1+H2                                                     02620000
           AKO1(0)=X1(N-1)/H1/H3-X1(0)/H1/H2+X1(  1)/H2/H3              02630000
           BKO1(0)=-2.*GAMMA(0)*AKO1(0)+(X1(0)-X1(N-1))*H2/H1/H3        02640000
           BKO1(0)=BKO1(0)+(X1(  1)-X1(0))*H1/H2/H3                     02650000
           CKO1(0)=X1(0)-(AKO1(0)*GAMMA(0)+BKO1(0))*GAMMA(0)            02660000
           AKO2(0)=X2(N-1)/H1/H3-X2(0)/H1/H2+X2(  1)/H2/H3              02670000
           BKO2(0)=-2.*GAMMA(0)*AKO2(0)+(X2(0)-X2(N-1))*H2/H1/H3        02680000
           BKO2(0)=BKO2(0)+(X2(  1)-X2(0))*H1/H2/H3                     02690000
           CKO2(0)=X2(0)-(AKO2(0)*GAMMA(0)+BKO2(0))*GAMMA(0)            02700000
C                                                                       02710000
        HI1(0)=X1(0)                                                    02720000
        HI2(0)=X2(0)                                                    02730000
        J=0                                                             02740000
        DO 410 K=1, N,1                                                 02750000
  420      J=J+1                                                        02760000
           IF (J .GT. ND+2) GOTO 410                                    02770000
           H1=J                                                         02780000
           H2=H1/ND1                                                    02790000
           IF (H2 .LT. GAMMA(K)) THEN                                   02800000
              H5=GAMMA(K)-GAMMA(K-1)                                    02810000
              H3=(GAMMA(K)-H2)/H5                                       02820000
              H4=(H2-GAMMA(K-1))/H5                                     02830000
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        02840000
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     02850000
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        02860000
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     02870000
              GOTO 420                                                  02880000
           ELSE                                                         02890000
              J=J-1                                                     02900000
              GOTO 410                                                  02910000
           ENDIF                                                        02920000
  410   CONTINUE                                                        02930000
        HI1(ND  )=HI1(0)                                                02940000
        HI2(ND  )=HI2(0)                                                02950000
        HI1(ND+1)=HI1(1)                                                02960000
        HI2(ND+1)=HI2(1)                                                02970000
C                                                                       02980000
C       ** BERECHNEN DER NEUEN KONZENTRATIONSWERTE                      02990000
C                                                                       03000000
        CALL KORKON(N,DGAM,DGA,GAMMA,A,AKOR)                            03010000
        DO 440 J=1, N-1, 1                                              03020000
           A    (J)=AKOR(J)                                             03030000
  440   CONTINUE                                                        03040000
        A(N+1)=A(1)                                                     03050000
C                                                                       03060000
        CALL KORKON(N,DGAM,DGA,GAMMA,H,AKOR)                            03070000
        DO 445 J=1, N-1, 1                                              03080000
           H    (J)=AKOR(J)                                             03090000
  445   CONTINUE                                                        03100000
        H(N+1)=H(1)                                                     03110000
C                                                                       03120000
C       ***** BERECHNEN DER NEUEN KOORDINATENPUNKTE                     03130000
        HI3(0)=0.0                                                      03140000
        DO 450 J=1, ND  ,1                                              03150000
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        03160000
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  03170000
  450   CONTINUE                                                        03180000
C                                                                       03190000
        STOT=HI3( ND  )                                                 03200000
        J=0                                                             03210000
        DO 460 K=1, N-1, 1                                              03220000
           H1=K                                                         03230000
  470      J=J+1                                                        03240000
           IF (J .GT. ND+2) GOTO 460                                    03250000
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         03260000
              H5=HI3(J)-HI3(J-1)                                        03270000
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             03280000
              H4=(HI3(J) - H1*STOT/N1  )/H5                             03290000
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             03300000
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             03310000
  460   CONTINUE                                                        03320000
        X1(0)=HI1(0)                                                    03330000
        X2(0)=HI2(0)                                                    03340000
        X1(N)=X1(0)                                                     03350000
        X2(N)=X2(0)                                                     03360000
        X1(N+1)=X1(1)                                                   03370000
        X2(N+1)=X2(1)                                                   03380000
        GOTO 1500                                                       03390000
C                                                                       03400000
C       ***************************************************             03410000
C       ****    ABLEITUNGEN DER DIFFUSIONSGLEICHUNG    ****             03420000
C       ****           AUF DER OBERFLAECHE             ****             03430000
C       ***************************************************             03440000
C                                                                       03450000
 1400   DO 500 J=1, N, 1                                                03460000
           H1=DGA(J)                                                    03470000
           H2=DGA(J+1)                                                  03480000
           H3=H1+H2                                                     03490000
           AGA2(J)=(A(J+1)/H2/H3-A(J)/H1/H2+A(J-1)/H1/H3)*2.            03500000
           AGA(J)=(A(J)-A(J-1))/H1 + AGA2(J)*H1/2.                      03510000
           HGA2(J)=(H(J+1)/H2/H3-H(J)/H1/H2+H(J-1)/H1/H3)*2.            03520000
           HGA(J)=(H(J)-H(J-1))/H1 + HGA2(J)*H1/2.                      03530000
  500   CONTINUE                                                        03540000
        AGA(0)=AGA(N)                                                   03550000
        AGA2(0)=AGA2(N)                                                 03560000
        HGA(0)=HGA(N)                                                   03570000
        HGA2(0)=HGA2(N)                                                 03580000
C                                                                       03590000
C                                                                       03600000
C       ***************************************************             03610000
C       ****            PRINT PRINT PRINT              ****             03620000
C       ***************************************************             03630000
C                                                                       03640000
C                                                                       03650000
        H2=T9                                                           03660000
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          03670000
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    03680000
     *           F12.6)                                                 03690000
   60   FORMAT(/,T5,'J',T16,'X1',T28,'X2',T37,'  DGA',T53,'A',T65,'H')  03700000
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.5,T56,F10.5) 03710000
   61   FORMAT(1X,F12.5,1X,F12.5)                                       03720000
   71   FORMAT(1X,'>>>>',I8,1X,F12.5,1X,F12.5)                          03730000
        APR=APR+1                                                       03740000
        WRITE (6,50) T8,T9,STOT                                         03750000
        WRITE (6,60)                                                    03760000
        WRITE (21,71) APR,T8,STOT                                       03770000
        DO 550 J=0 ,N ,1                                                03780000
           WRITE (6,70) J,X1 (J),  X2(J),DGA  (J),A(J),H(J)             03790000
           WRITE (21,61) X1(J),X2(J)                                    03800000
  550   CONTINUE                                                        03810000
        WRITE (21,71) 0,0.0,0.0                                         03820000
        BACKSPACE 21                                                    03830000
C       IF (T99 .EQ. 1) GOTO 1700                                       03840000
C                                                                       03850000
C       **************************************************              03860000
C       ****      BERECHNEN DER NEUEN WERTE           ****              03870000
C       **************************************************              03880000
C                                                                       03890000
 1100   DO 600 J= 1,N,1                                                 03900000
           H1=FNV(KAPPA(J))+EPS*KAS2(J)+FNVA(A(J))                      03910000
           X1(J)=X1(J)+( TAU2(J)*H1/WG(J)-TAU1(J)*GAPU(J))*DELT         03920000
           X2(J)=X2(J)+(-TAU1(J)*H1/WG(J)-TAU2(J)*GAPU(J))*DELT         03930000
           H2=DA*AGA2(J)/STOTQU+FNFA(A(J),H(J))-A(J)*KAPPA(J)*H1        03940000
           A(J)=A(J)+(H2-AGA(J)*GAPU(J))*DELT                           03950000
           H2=DH*HGA2(J)/STOTQU+FNFH(A(J),H(J))-H(J)*KAPPA(J)*H1        03960000
           H(J)=H(J)+(H2-HGA(J)*GAPU(J))*DELT                           03970000
  600   CONTINUE                                                        03980000
        X1(0)=X1(N)                                                     03990000
        X2(0)=X2(N)                                                     04000000
        A (0)=A (N)                                                     04010000
        H (0)=H (N)                                                     04020000
        X1(N+1)=X1(1)                                                   04030000
        X2(N+1)=X2(1)                                                   04040000
        A (N+1)=A (1)                                                   04050000
        H (N+1)=H (1)                                                   04060000
C                                                                       04070000
C       **************************************************              04080000
C       ****       AENDERN DES INDIZES               *****              04090000
C       **************************************************              04100000
C                                                                       04110000
        IF (ZU .EQ. 0) GOTO 1300                                        04120000
        DO 700 J=0 , N-1-ZU, 1                                          04130000
           XZ1(J)=X1(ZU+J)                                              04140000
           XZ2(J)=X2(ZU+J)                                              04150000
           AZ (J)=A (ZU+J)                                              04160000
           HZ (J)=H (ZU+J)                                              04170000
  700   CONTINUE                                                        04180000
        DO 710 J=N-ZU,N-1,1                                             04190000
           XZ1(J)=X1(J-N+ZU)                                            04200000
           XZ2(J)=X2(J-N+ZU)                                            04210000
           AZ (J)=A (J-N+ZU)                                            04220000
           HZ (J)=H (J-N+ZU)                                            04230000
  710   CONTINUE                                                        04240000
        XZ1(N)=XZ1(0)                                                   04250000
        XZ2(N)=XZ2(0)                                                   04260000
        AZ (N)=AZ (0)                                                   04270000
        HZ (N)=HZ (0)                                                   04280000
        XZ1(N+1)=XZ1(1)                                                 04290000
        XZ2(N+1)=XZ2(1)                                                 04300000
        AZ (N+1)=AZ (1)                                                 04310000
        HZ (N+1)=HZ (1)                                                 04320000
C                                                                       04330000
        DO 720 J=0,N+1,1                                                04340000
           X1(J)=XZ1(J)                                                 04350000
           X2(J)=XZ2(J)                                                 04360000
           A (J)=AZ (J)                                                 04370000
           H (J)=HZ (J)                                                 04380000
  720   CONTINUE                                                        04390000
C                                                                       04400000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              04410000
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              04420000
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              04430000
C                                                                       04440000
 1300   IF (T9 .GE. ZSEND) GOTO 1200                                    04450000
        GOTO 1000                                                       04460000
 1200   CONTINUE                                                        04470000
        END                                                             04480000
C                                                                       04490000
C       @@@@@@@@@@@@@@@@@@@@@@@ SUBROUTINES @@@@@@@@@@@@@@@@@@@@        04500000
C                                                                       04510000
        SUBROUTINE KORKON(N,DGAM,DGA,GAMMA,A,AKOR)                      04520000
        DOUBLE PRECISION DGA(0:201),GAMMA(0:201),A(0:201),AKOR(0:201)   04530000
        DOUBLE PRECISION DGAM,H1,H2,H3,AKO,BKO,CKO                      04540000
        INTEGER N,J                                                     04550000
        DO 3000 J=1,  N-1, 1                                            04560000
           H2=DGA(J+1)                                                  04570000
           H1=DGA(J)                                                    04580000
           H3=H1+H2                                                     04590000
           AKO=A    (J-1)/H1/H3-A    (J)/H1/H2+A    (J+1)/H2/H3         04600000
           BKO=-2.*GAMMA(J)*AKO+(A    (J)-A    (J-1))*H2/H1/H3          04610000
           BKO=BKO+(A    (J+1)-A    (J))*H1/H2/H3                       04620000
           CKO=A    (J)-(AKO*GAMMA(J)+BKO)*GAMMA(J)                     04630000
           H1=J                                                         04640000
           H2=DGAM*H1                                                   04650000
           AKOR(J)=AKO*H2*H2+BKO*H2+CKO                                 04660000
 3000   CONTINUE                                                        04670000
        RETURN                                                          04680000
        END                                                             04690000
//GO.SYSIN DD *                                                         04700000
//FT21F001 DD DSN=F159.TEST.DATA,DISP=OLD                               04710000
/*                                                                      04720000
/*      EXEC DRUCKE                                                     04730000
/*      END                                                             04740000
