//D14KG    JOB RACF,CLASS=G,
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K
//            EXEC FORTCLG
//FORT.SYSIN  DD *
C
C       #############################################################
C       ####   OBERFLAECHEN-DYNAMIK  (2-DIMENSIONAL)    MIT      ####
C       ####      DIFFUSIONSGLEICHUNG AUF DER OBERFLAECHE        ####
C       ####                                                     ####
C       ####     (DX/DT)*NORMALE = U(KAPPA)                      ####
C       #### U(KAPPA) = FNV(KAPPA)+EPS*FNEPSA(A)*(DDKAPPA/DS/DS) ####
C       ####                      +FNW(A)                        ####
C       #### DA/DT = DA * DDA/DS/DS + FNG(A) - A*KAPPA*U(KAPPA)  ####
C       ####                                                     ####
C       ####        NAME DES PROGRAMMES : OBERFD(PROBD2)         ####
C       #############################################################
C
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE
C
C
        INTEGER           I,J,K,N,T7,T9,ZSEND,ZSPRT,T99,ND,NDEND,APR
        INTEGER           NURNR,CPUSEC
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF
        DOUBLE PRECISION  AKO,BKO,CKO
        DOUBLE PRECISION  EPS,DA,AGES,K0
        DOUBLE PRECISION  FNV,XD,FNW,FNG,FNEPSA
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8
        DOUBLE PRECISION  AMPLI(0:20)
        DOUBLE PRECISION  X(0:201),Y(0:201)
        DOUBLE PRECISION  XT(0:201),YT(0:201)
        DOUBLE PRECISION  X1(0:201),Y1(0:201)
        DOUBLE PRECISION  X2(0:201),Y2(0:201)
        DOUBLE PRECISION  DS(0:201),DSPKT(0:201)
        DOUBLE PRECISION  GAM(0:201),GAMPKT(0:201)
        DOUBLE PRECISION  G(0:201),WG(0:201)
        DOUBLE PRECISION  KAPPA(0:201),KASS(0:201)
        DOUBLE PRECISION  KA1(0:201),KA2(0:201)
        DOUBLE PRECISION  AKOR(0:201)
        DOUBLE PRECISION  AKO1(0:201),BKO1(0:201),CKO1(0:201)
        DOUBLE PRECISION  AKO2(0:201),BKO2(0:201),CKO2(0:201)
        DOUBLE PRECISION  A(0:201),AT(0:201)
        DOUBLE PRECISION  A1(0:201),A2(0:201)
        DOUBLE PRECISION  HI1(0: 4001),HI2(0: 4001),HI3(0: 4000)
        CHARACTER         DNW*30
C       FNV(XD,K0)=(-.2D0   *(XD-K0)    +1.D0)*(XD-K0)
        FNV(XD,K0)=( -(XD-K0)*(XD-K0) + 1.D0 )*(XD-K0)
C       FNV(XD,K0)=DTANH(XD-K0)
C       FNV(XD,K0)=(XD-K0)*.10D0
        FNW(XD)=(XD-1.D0)*0.0D0
        FNG(XD)=(-1.D0*XD+1.D0)*0.0D0
        FNEPSA(XD)=1.00000D0
C
C       **************************************************
C       ****      PARAMETER PARAMETER PARAMETER       ****
C       **************************************************
C
                 DNW   = 'D14K.TES1.DATA'
                 DELT  =      0.00001000D0 *    8.D0
                 N     =     60
                 NURNR =      0
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.
C       NURNR=2 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.
C                 KEIN PUNKT IST HIERBEI AUSGEZEICHNET.
C       N : ANZ. DER STUETZPUNKTE  / DELT=DELTA T
C       BRAUCHBARE WERTE FUER DELTA T (DELT):
C                B = 1/K0 = 1.0 , N=40  :  0.00001D0 * 32.D0
C                B = 1/K0 = 1.0 , N=60  :  0.00001D0 *  8.D0
C                B = 1/K0 = 1.0 , N=72  :  0.00001D0 *  4.D0
C       CPU-ZEITEN IM INTERAKTIVEN BETRIEB:
C                FUER N=40 , NURNR=0 , 10000 ITERAT.:     SEKUNDEN
C                FUER N=60 , NURNR=0 , 10000 ITERAT.: 178 SEKUNDEN
C                FUER N=80 , NURNR=0 , 10000 ITERAT.:     SEKUNDEN
C
C
                 ZSEND = 480000
                 ZSPRT =  40000
C
                 K0    =      1.00000000D0
                 EPS   =      0.12500000D+00
                 DA    =      0.00000000D+00
C
                 ND    =   4000
                 NDEND =     50
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN
                 ND1=ND
                 N1=N
                 DGAM=1.D0/N1
                 PI=4.D0*DATAN(1.D0)
C
C       *************************************************
C       ****    STARTKONZENTRATION UND START-RAND    ****
C       *************************************************
C
        AMPLI(0) = 1.D0/K0
        AMPLI(1) = 0.0000D0
        AMPLI(2) = 0.0500D0
        AMPLI(3) = 0.0000D0
        AMPLI(4) = 0.0000D0
        AMPLI(5) = 0.0000D0
        AMPLI(6) = 0.0000D0
        AMPLI(7) = 0.0000D0
        AMPLI(8) = 0.0000D0
        AMPLI(9) = 0.0000D0
        AMPLI(10)= 0.0000D0
        AMPLI(11)= 0.0000D0
        AMPLI(12)= 0.0000D0
        AMPLI(13)= 0.0000D0
        AMPLI(14)= 0.0000D0
        AMPLI(15)= 0.0000D0
        AMPLI(16)= 0.0000D0
        AMPLI(17)= 0.0000D0
        AMPLI(18)= 0.0000D0
        AMPLI(19)= 0.0000D0
        AMPLI(20)= 0.0000D0
C
        DO 140 J=0, N+1, 1
           H1=J
C          H2=DCOS(2.D0*PI*H1/N1)
           H2=0.0
           A(J)=1.D0  + .5D0 *H2
  140   CONTINUE
        A(0)=1.0D0
        A(N)=A(0)
C
        DO 100 J=0 , ND+1 , 1
           H1=J
           H3=0.D0
           DO 102 K=0, 20 , 1
              H2=K
              H3=H3 + AMPLI(K)*DCOS(2.D0*H2*PI*H1/ND1)
  102      CONTINUE
           HI1(J)=DCOS(2.D0*PI*H1/ ND1)*H3
           HI2(J)=DSIN(2.D0*PI*H1/ ND1)*H3
  100   CONTINUE
        HI1(0)=HI1(0)
        HI1(ND)=HI1(0)

C
        HI3(0)=0.0
        DO 110 J=1, ND  ,1
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)
  110   CONTINUE
C
        STOT=HI3( ND  )
        J=0
        DO 120 K=1, N-1, 1
           H1=K
  130      J=J+1
           IF (J .GT. ND+2) GOTO 120
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130
              H5=HI3(J)-HI3(J-1)
              H3=(H1*STOT/N1 - HI3(J-1))/H5
              H4=(HI3(J) - H1*STOT/N1)/H5
              X(K)=HI1(J)*H3 + HI1(J-1)*H4
              Y(K)=HI2(J)*H3 + HI2(J-1)*H4
  120   CONTINUE
        X(0)=HI1(0)
        Y(0)=HI2(0)
        X(N)=X(0)
        Y(N)=Y(0)
        X(N+1)=X(1)
        Y(N+1)=Y(1)
C
C
C
C       **************************************************
C       ****      PRINTEN DER PARAMETER-TABELLE       ****
C       **************************************************
C
        OPEN (22,FILE=DNW)
        REWIND 22
   10   FORMAT (///' PROGRAMM-NAME :  OBERFD(OBERD2)')
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='
     *        ,F10.7,'    N =',I4)
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)
   40   FORMAT (/,' EPS =',F10.5,'  DA =',F10.5,'   B =',F10.5)
        WRITE (6,10)
        WRITE (6,20) DGAM,DELT,N
        WRITE (6,30) ZSEND,ZSPRT
        WRITE (6,40) EPS,DA,1.D0/K0
C
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C
        T9=-1
        T7=0
        T8     = -DELT
        APR=0
 1000   T9=T9+1
        T8=T8+DELT
C
C       **************************************************
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****
C       *****                                         ****
C       **************************************************
C
 1500   STOT=0.0D0
        DO 200 J=1, N   ,1
            DS(J)=DSQRT(( X(J   )- X(J-1))*( X(J    )- X(J-1))+
     *            ( Y(J   )- Y(J-1))*( Y(J    )- Y(J-1)))
        STOT=STOT+DS(J)
  200   CONTINUE
        DS(0)=DS(N)
        DS(N+1)=DS(1)
C
        GAM(0)=0.D0
        DO 210 J=1,N,1
           GAM(J)=GAM(J-1)+DS(J)/STOT
  210   CONTINUE
        GAM(N+1)=1.D0+GAM(1)
C
C
        DO 220 J=1,  N, 1
           H1=DS(J)
           H2=DS(J+1)
           H3=H1+H2
           X2(J)=(X(J+1)/H2/H3-X(J)/H1/H2+X(J-1)/H1/H3)*2.D0
           Y2(J)=(Y(J+1)/H2/H3-Y(J)/H1/H2+Y(J-1)/H1/H3)*2.D0
           X1(J)=(X(J)-X(J-1))/H1 + X2(J)*H1/2.D0
           Y1(J)=(Y(J)-Y(J-1))/H1 + Y2(J)*H1/2.D0
           G(J)=X1(J)*X1(J)+Y1(J)*Y1(J)
           WG(J)=DSQRT(G(J))
  220   CONTINUE
        G(0)=G(N)
        G(N+1)=G(1)
        X1(0)=X1(N)
        Y1(0)=Y1(N)
        X2(0)=X2(N)
        Y2(0)=Y2(N)
        WG(0)=WG(N)
C
        DO 230 J=1,N,1
           KAPPA(J)=(-Y1(J)*X2(J)+X1(J)*Y2(J))/WG(J)/G(J)
  230   CONTINUE
        KAPPA(N+1)=KAPPA(1)
        KAPPA(0)=KAPPA(N)
C
        DO 240 J=1,N,1
           H1=DS(J)
           H2=DS(J+1)
           H3=H1+H2
           KA2(J)= KAPPA(J+1)/H2/H3-KAPPA(J)/H1/H2+KAPPA(J-1)/H1/H3
           KA2(J)=KA2(J)*2.D0
           KA1(J)=( KAPPA(J)- KAPPA(J-1))/H1 + KA2(J)*H1/2.D0
           KASS(J)=KA2(J)/G(J) - KA1(J)*
     *               (X2(J)*X1(J)+Y2(J)*Y1(J))/G(J)/G(J)
  240   CONTINUE
        KASS(0)=KASS(N)
        KASS(N+1)=KASS(1)
C
        STPKT=0.0D0
        DO 250 J=1,N,1
          H1 = KAPPA(J-1) * (     FNV(KAPPA(J-1),K0) +
     *         EPS*FNEPSA( A(J-1) )*KASS(J-1)  +  FNW( A(J-1))     )
          H2 = KAPPA(J  ) * (     FNV(KAPPA(J  ),K0) +
     *         EPS*FNEPSA( A(J  ) )*KASS(J  )  +  FNW( A(J  ))     )
          DSPKT(J)= (H1+H2)*DS(J)/2.D0
          STPKT   = STPKT + DSPKT(J)
  250   CONTINUE
        DSPKT(0)=DSPKT(N)
C
        IF (NURNR .EQ. 2) THEN
        DO 261 I=1, N, 1
           GAMPKT(I)=0.0D0
           DO 260 J=1, N, 1
              K=I+J
              IF (K .GT. N) K=K-N
              H1=J+J-N-1
              GAMPKT(I)=GAMPKT(I)+H1*(DSPKT(K)-DS(K)*STPKT/STOT)
  260      CONTINUE
           H1=N+N
           GAMPKT(I)=GAMPKT(I)/H1/STOT
  261   CONTINUE
        ENDIF
        IF (NURNR .EQ. 0) THEN
        H1=0.D0
        DO 270 I=1, N, 1
           H1=H1+DSPKT(I)
           GAMPKT(I)=(H1-STPKT*GAM(I))/STOT
  270   CONTINUE
        ENDIF
C
C       ***************************************************
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****
C       ***************************************************
C
 1300   T99=0
        H2=DGAM/4.D0
        DO 300 J=1, N , 1
           H1=J
           H3=H1*DGAM
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))
     *            T99=1
  300   CONTINUE
        MDIFF=0.0D0
        DO 310 J=1, N ,1
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))
  310   CONTINUE
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1
C
C       T99=0
        IF (T99 .EQ. 0) GOTO 1400
C
C
C       ***************************************************
C       ****          NEUE DISKRETISIERUNG             ****
C       ***************************************************
C
        T7=T7+1
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',
     *          I11)
        WRITE (6,90) T7,T9
        IF (T7 .GT. NDEND) GOTO 1200
C       ** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN
C       ** EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE
        DO 400 J=1,  N  , 1
           H2=GAM(J+1)-GAM(J)
           H1=GAM(J)-GAM(J-1)
           H3=H1+H2
           AKO1(J)=X(J-1)/H1/H3-X(J)/H1/H2+X(J+1)/H2/H3
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X(J)-X(J-1))*H2/H1/H3
           BKO1(J)=BKO1(J)+(X(J+1)-X(J))*H1/H2/H3
           CKO1(J)=X(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)
           AKO2(J)=Y(J-1)/H1/H3-Y(J)/H1/H2+Y(J+1)/H2/H3
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(Y(J)-Y(J-1))*H2/H1/H3
           BKO2(J)=BKO2(J)+(Y(J+1)-Y(J))*H1/H2/H3
           CKO2(J)=Y(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)
  400   CONTINUE
           H2=GAM(1)-GAM(0)
           H1=GAM(N)-GAM(N-1)
           H3=H1+H2
           AKO1(0)=X(N-1)/H1/H3-X(0)/H1/H2+X(     1)/H2/H3
           BKO1(0)=-2.*GAM(0)*AKO1(0)+(X(0)-X(N-1))*H2/H1/H3
           BKO1(0)=BKO1(0)+(X(   1)-X(0))*H1/H2/H3
           CKO1(0)=X(0)-(AKO1(0)*GAM(0)+BKO1(0))*GAM(0)
           AKO2(0)=Y(N-1)/H1/H3-Y(0)/H1/H2+Y(     1)/H2/H3
           BKO2(0)=-2.*GAM(0)*AKO2(0)+(Y(0)-Y(N-1))*H2/H1/H3
           BKO2(0)=BKO2(0)+(Y(   1)-Y(0))*H1/H2/H3
           CKO2(0)=Y(0)-(AKO2(0)*GAM(0)+BKO2(0))*GAM(0)
C
        HI1(0)=X(0)
        HI2(0)=Y(0)
        J=0
        DO 410 K=1, N,1
  420      J=J+1
           IF (J .GT. ND+2) GOTO 410
           H1=J
           H2=H1/ND1
           IF (H2 .LT. GAM(K)) THEN
              H5=GAM(K)-GAM(K-1)
              H3=(GAM(K)-H2)/H5
              H4=(H2-GAM(K-1))/H5
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)
              GOTO 420
           ELSE
              J=J-1
              GOTO 410
           ENDIF
  410   CONTINUE
        HI1(ND  )=HI1(0)
        HI2(ND  )=HI2(0)
        HI1(ND+1)=HI1(1)
        HI2(ND+1)=HI2(1)
C
C       ** BERECHNEN DER NEUEN KONZENTRATIONSWERTE
        DO 430 J=1,  N-1, 1
           H2=GAM(J+1)-GAM(J)
           H1=GAM(J)-GAM(J-1)
           H3=H1+H2
           AKO=A    (J-1)/H1/H3-A    (J)/H1/H2+A    (J+1)/H2/H3
           BKO=-2.D0*GAM(J)*AKO+(A    (J)-A    (J-1))*H2/H1/H3
           BKO=BKO+(A    (J+1)-A    (J))*H1/H2/H3
           CKO=A    (J)-(AKO*GAM(J)+BKO)*GAM(J)
           H1=J
           H2=DGAM*H1
           AKOR(J)=AKO*H2*H2+BKO*H2+CKO
  430   CONTINUE
C
        DO 440 J=1, N-1, 1
           A(J)=AKOR(J)
  440   CONTINUE
        A(N+1)=A(1)
C
C       ***** BERECHNEN DER NEUEN KOORDINATENPUNKTE
        HI3(0)=0.0D0
        DO 450 J=1, ND  ,1
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)
  450   CONTINUE
C
        STOT=HI3( ND  )
        J=0
        DO 460 K=1, N-1, 1
           H1=K
  470      J=J+1
           IF (J .GT. ND+2) GOTO 460
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470
              H5=HI3(J)-HI3(J-1)
              H3=(H1*STOT/N1 - HI3(J-1))/H5
              H4=(HI3(J) - H1*STOT/N1  )/H5
              X(K)=HI1(J)*H3 + HI1(J-1)*H4
              Y(K)=HI2(J)*H3 + HI2(J-1)*H4
  460   CONTINUE
        X(0)=HI1(0)
        Y(0)=HI2(0)
        X(N)=X(0)
        Y(N)=Y(0)
        X(N+1)=X(1)
        Y(N+1)=Y(1)
        GOTO 1500
C
C       ***************************************************
C       ****    ABLEITUNGEN DER DIFFUSIONSGLEICHUNG    ****
C       ****           AUF DER OBERFLAECHE             ****
C       ***************************************************
C
 1400   DO 500 J=1, N, 1
           H1=DS(J)
           H2=DS(J+1)
           H3=H1+H2
           A2(J)=(A(J+1)/H2/H3-A(J)/H1/H2+A(J-1)/H1/H3)*2.D0
           A1(J)=(A(J)-A(J-1))/H1 + A2(J)*H1/2.D0
  500   CONTINUE
        A1(0)=A1(N)
        A2(0)=A2(N)
C
C       ***************************************************
C       **** BERECHNUNG DER ZEITABLEITUNGEN            ****
C       ***************************************************
C
        DO 510 J= 1,N,1
           H1=FNV(KAPPA(J),K0)+EPS*FNEPSA(A(J))*KASS(J)+FNW(A(J))
           XT(J) =     Y1(J)*H1/WG(J) - X1(J)*STOT*GAMPKT(J)
           YT(J) =    -X1(J)*H1/WG(J) - Y1(J)*STOT*GAMPKT(J)
           H2=DA*A2(J)+FNG(A(J))-A(J)*KAPPA(J)*H1
           AT(J)  =     H2 - A1(J)*STOT*GAMPKT(J)
  510   CONTINUE
C
C       ***************************************************
C       **** BERECHNUNG VON DELTA T (DELT)             ****
C       ***************************************************
C
        H2 = STOT*STOT/N1/N1/2.D0/PI/2.D0/PI
        H1 = 0.D0
        DO 520 J=1,N
           H1 = H1 + DSQRT( XT(J)*XT(J) + YT(J)*YT(J) ) * DELT
  520   CONTINUE
C       IF (H1 .LT. H2*0.1000D0) DELT=DELT*2.D0
        DO 530 J=1,N
        DO 530 I=0,30
           H1 = DSQRT( XT(J)*XT(J) + YT(J)*YT(J) ) * DELT
           IF (H1 .GT. H2*0.5000D0) DELT=DELT*.5D0
  530   CONTINUE
C
C       ***************************************************
C       ****            PRINT PRINT PRINT              ****
C       ***************************************************
C
C
        H2=T9
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100
C
        AGES=0.0D0
        DO 99 J=1, N ,1
           AGES=(A(J-1)+A(J))*.5D0*DS(J) + AGES
   99   CONTINUE
C
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',
     *     F12.6)
   59   FORMAT (' CPU-ZEIT(SEC) =',I5,'    AGES =',F13.5,'  DELTA T=',
     *     F14.8)
   60   FORMAT(/,T5,'J',T17,'X',T29,'Y',T37,'  DGA',T49,'KAPPA',
     *               T64,'VN',T74,'KASS')
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.4,T56,F10.5,
     *           T68,F10.3)
   61   FORMAT(1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5)
   71   FORMAT(1X,'>>>>',I8,3X,I10)
        CALL CLOCK(CPUSEC)
        APR=APR+1
        WRITE (6,50) T8,T9,STOT
        WRITE (6,59) CPUSEC,AGES,DELT
        WRITE (6,60)
        WRITE (22,71) APR,N
        WRITE (22,61) T8,STOT
        DO 550 J=0 ,N ,1
           H1=FNV(KAPPA(J),K0)+EPS*FNEPSA(A(J))*KASS(J)+FNW(A(J))
           WRITE (6,70) J,X(J),Y(J),DS(J)/STOT,KAPPA(J),
     *                  H1,KASS(J)
           WRITE (22,61) X(J),Y(J),GAM(J),KAPPA(J),A(J)
  550   CONTINUE
        WRITE (22,71) 0,0
        BACKSPACE 22
C       IF (T99 .EQ. 1) GOTO 1700
C
C       **************************************************
C       ****      BERECHNEN DER NEUEN WERTE           ****
C       **************************************************
C
 1100   DO 600 J= 1,N,1
           X(J)=X(J) + XT(J)*DELT
           Y(J)=Y(J) + YT(J)*DELT
           A(J) = A(J) +  AT(J)*DELT
  600   CONTINUE
        X(0)=X(N)
        Y(0)=Y(N)
        A (0)=A (N)
        X(N+1)=X(1)
        Y(N+1)=Y(1)
        A (N+1)=A (1)
C
C
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C
        IF (T9 .GE. ZSEND) GOTO 1200
        GOTO 1000
 1200   CONTINUE
        CLOSE(22,STATUS='KEEP')
        END
//GO.SYSIN    DD *
/*
//      EXEC DRUCKE
/*      END
