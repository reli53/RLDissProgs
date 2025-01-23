C//D14KC    JOB RACF,CLASS=C,
C//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K
C//            EXEC FORTCLG
C//FORT.SYSIN  DD *
C
C       #############################################################
C       ####   OBERFLAECHEN-DYNAMIK  (3-DIMENSIONAL)             ####
C       ####      (ROTATIONSSYMMETRISCH UM Z-ACHSE)              ####
C       ####                                                     ####
C       ####              (DX/DT)*NORMALE = U(KM)                ####
C       ####     U(KM) = FNV(KM)+EPS*(LAPLACE-BELTRAMI)(KM)      ####
C       ####        KM = 2 * MITTLERE KRUEMMUNG                  ####
C       ####       (NACH ABSCHNUERUNG WIRD MIT DER RECHTEN       ####
C       ####             HAELFTE WEITERGERECHNET.)               ####
C       ####       (BEIM UEBERGANG ZUM TORUS WIRD MIT DER OBEREN ####
C       ####             HAELFTE WEITERGERECHNET.)               ####
C       ####                                                     ####
C       ####        NAME DES PROGRAMMES : NOCIRCLE(PROBD3)       ####
C       ####               14.09.1989 (LIERMANN)                 ####
C       #############################################################
C
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE
C
C
        INTEGER           I,J,K,N,T7,T9,T99,ND,NDEND,NTORUS,VERGR
        INTEGER           NURNR,ZSEND,ZSPRT,APR,NI,NA,TT
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF
        DOUBLE PRECISION  FNV,X,K0,EPS,FNP2
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8
        DOUBLE PRECISION  OBERFL,VOLUME,VNOB,VERGRT
        DOUBLE PRECISION  AMPLI(0:10)
        DOUBLE PRECISION  Z(-1:201),R(-1:201)
        DOUBLE PRECISION  Z1(0:200),R1(0:200)
        DOUBLE PRECISION  Z2(0:200),R2(0:200)
        DOUBLE PRECISION  ZT(0:200),RT(0:200)
        DOUBLE PRECISION  X1(-1:201),X2(-1:201)
        DOUBLE PRECISION  DS(0:200),DSPKT(0:200)
        DOUBLE PRECISION  GAM(-1:201),GAMPKT(0:200)
        DOUBLE PRECISION  G(0:200),WG(0:200)
        DOUBLE PRECISION  KAPPA(0:200),KM(-1:201),KM1(0:200)
        DOUBLE PRECISION  KM2(0:200),LBKM(0:200)
        DOUBLE PRECISION  VN(0:200),OBFEL(0:200),VOLEL(0:200)
        DOUBLE PRECISION  AKO1(0:200),BKO1(0:200),CKO1(0:200)
        DOUBLE PRECISION  AKO2(0:200),BKO2(0:200),CKO2(0:200)
        DOUBLE PRECISION  HI1(0: 4000),HI2(0: 4000),HI3(0: 4000)
        CHARACTER         DNW*30,TORUS*1,DSWRIT*1,VOLPRE*1
C       FNV(X,K0)=X-K0
        FNV(X,K0)=(X-K0) - (X-K0)*(X-K0)*(X-K0)*( 1.D0 )
        FNP2(X)=.5D0*(3.D0*X*X-1.D0)
C
C       **************************************************
C       ****      PARAMETER PARAMETER PARAMETER       ****
C       **************************************************
C
                 DNW   = 'D14K.TES1.DATA'
                 DELT  =      0.00001000D0 *  64.D0
                 N     =     24
                 NTORUS=     30
                 NURNR =      0
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.
C       N       : ANZ. DER STUETZPUNKTE (FUER OBERE HAELFTE DER KURVE)
C       NTORUS  : ANZ. DER STUETZPUNKTE BEIM TORUS (OBERE HAELFTE)
C       DELT    : DELTA T
C       VERGR   : DIE OBERE GRENZE DER ZEITSCHRITTE, BIS ZU DER
C                 DIE DYNAMIK (FALLS SEHR LANGSAM) VERGROESSERT WIRD
C       VERGRT  : DIE OBERE GRENZE FUER DIE ZEIT   , BIS ZU DER
C                 DIE DYNAMIK (FALLS SEHR LANGSAM) VERGROESSERT WIRD
C       VOLPRE  : = 'J' : EINFUEGEN EINES VOLUMENERHALTENDEN TERMES
C       DSWRIT  : = 'J' : SCHREIBEN AUF DATA-FILE   DSW.
C       TORUS   : = 'N' :  KEIN TORUS
C                 = 'J' :  TORUS (  *** UNVOLLSTAENDIG ***  )
C       FUER N=20 UND NURNR=0 UND ZSEND=30000 IST DIE CPU-ZEIT
C                 CIRCA 1 MINUTE (CLASS C).
C       FUER N=36 UND NURNR=0 UND ZSEND=50000 IST DIE CPU-ZEIT
C                 CIRCA 5 MINUTE (CLASS D).
C
C
                 ZSEND = 400000
                 ZSPRT =   4000
                 VERGR =      0
                 VERGRT=      0.1D0
                 TORUS = 'N'
                 DSWRIT= 'N'
                 VOLPRE= 'N'
C
                 K0    =      2.00000000D0
C                EPS   =      1.D0 / 6.D0 - 0.1D0 * 2.0D0/21.0D0
                 EPS   =      1.D0 / 6.D0
C                EPS   =      3.D0 / 24.D0
C
                 ND    =   1000
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
        AMPLI(0) = 2.D0/K0
        AMPLI(1) = 0.0000D0
        AMPLI(2) =-0.2000D0
        AMPLI(3) = 0.0000D0
        AMPLI(4) = 0.0000D0
        AMPLI(5) = 0.0000D0
        AMPLI(6) = 0.0000D0
        AMPLI(7) = 0.0000D0
        AMPLI(8) = 0.0000D0
        AMPLI(9) = 0.0000D0
        AMPLI(10)= 0.0000D0
C
        DO 100 J=0 , ND , 1
           H1=J
           H3=0.D0
           H3=H3 + AMPLI(0)
           H3=H3+AMPLI(2)*FNP2( DCOS(H1*PI/ND1) )
           HI1(J)=DCOS(     PI*H1/ ND1)*H3
           HI2(J)=DSIN(     PI*H1/ ND1)*H3
  100   CONTINUE
C
        HI3(0)=0.D0
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
           IF (J .GT. ND) GOTO 120
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130
              H5=HI3(J)-HI3(J-1)
              H3=(H1*STOT/N1 - HI3(J-1))/H5
              H4=(HI3(J) - H1*STOT/N1)/H5
              Z(K)=HI1(J)*H3 + HI1(J-1)*H4
              R(K)=HI2(J)*H3 + HI2(J-1)*H4
  120   CONTINUE
        Z(0)=HI1(0)
        R(0)=HI2(0)
        Z(N)=HI1(ND)
        R(N)=HI2(ND)
        Z(-1)=Z(1)
        R(-1)=-R(1)
        Z(N+1)=Z(N-1)
        R(N+1)=-R(N-1)
C
C
C
C       **************************************************
C       ****      PRINTEN DER PARAMETER-TABELLE       ****
C       **************************************************
C
        IF (DSWRIT .EQ. 'J') OPEN (22,FILE=DNW)
        IF (DSWRIT .EQ. 'J') REWIND 22
   10   FORMAT (///' PROGRAMM-NAME :  NOCIRCLE(OBERD3)')
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='
     *        ,F10.7,'    N =',I4)
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)
   40   FORMAT (/,' EPS =',F10.5,' XXX =',F10.5,'   B =',F10.5)
        WRITE (6,10)
        WRITE (6,20) DGAM,DELT,N
        WRITE (6,30) ZSEND,ZSPRT
        WRITE (6,40) EPS,1.D0,2.D0/K0
C
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C
        T9=-1
        T7=0
        T8     = -DELT
        APR=0
        NI=N
        NA=N
        TT=0
 1000   T9=T9+1
        T8=T8+DELT
C
C       ***************************************************
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****
C       ***************************************************
C
 1300   T99=0
C       STOT=TOTALE BOGENLAENGE (FUER HALBE KURVE)
        STOT=0.0D0
        DO 305 J=1, NI  ,1
            DS(J)=DSQRT(( Z(J)-Z(J-1) )*( Z(J)-Z(J-1) )+
     *            ( R(J)-R(J-1) )*( R(J)-R(J-1) ))
        STOT=STOT+DS(J)
  305   CONTINUE
        DS(0)=DS(1)
        DS(NI+1)=DS(NI)
C
        GAM(0)=0.D0
        DO 307 J=1,NI,1
           GAM(J)=GAM(J-1)+DS(J)/STOT
  307   CONTINUE
        GAM(-1)=-GAM(1)
        GAM(NI+1)=2.D0-GAM(NI-1)
C
        H2=NI
        DGAM=1.D0/H2
        H2=DGAM/4.D0
        DO 300 J=1, NI-1 , 1
           H1=J
           H3=H1*DGAM
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))
     *            T99=1
  300   CONTINUE
        MDIFF=0.0D0
        DO 310 J=1, NI ,1
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))
  310   CONTINUE
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1
        IF (TT .NE. 0) T99=1
        TT=0
C
        IF (T99 .EQ. 0) GOTO 1500
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
C
C
        DO 401 J=-1,NI+1,1
           X1(J)=Z(J)
           X2(J)=R(J)
  401   CONTINUE
C       *** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN
C       **  EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE
        DO 400 J=0,  NI  , 1
           H2=GAM(J+1)-GAM(J)
           H1=GAM(J)-GAM(J-1)
           H3=H1+H2
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3
           CKO1(J)=X1(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3
           CKO2(J)=X2(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)
  400   CONTINUE
C
C       *** BESTIMMEN DERJENIGEN PUNKTE HI1(J),HI2(J) ,0<J<ND , FUER
C       **  DIE DIE BOGENLAENGE (VON HI1(0),HI2(0) AUS BERECHNET)
C       **  DEN WERT STOT*J/ND ANNIMMT.
        HI1(0)=X1(0)
        HI2(0)=X2(0)
        HI1(ND)=X1(NI)
        HI2(ND)=X2(NI)
        J=0
        DO 410 K=1, NI,1
  420      J=J+1
           IF (J .GT. ND  ) GOTO 410
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
C
C
C       *** BERECHNEN DER NEUEN KOORDINATENPUNKTE
        HI3(0)=0.0D0
        DO 450 J=1, ND  ,1
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)
  450   CONTINUE
        STOT=HI3( ND  )
C
        X1(NA)=HI1(ND)
        X2(NA)=HI2(ND)
        N1=NA
        J=0
        DO 460 K=1, NA-1, 1
           H1=K
  470      J=J+1
           IF (J .GT. ND  ) GOTO 460
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470
              H5=HI3(J)-HI3(J-1)
              H3=(H1*STOT/N1 - HI3(J-1))/H5
              H4=(HI3(J) - H1*STOT/N1  )/H5
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4
  460   CONTINUE
        X1(-1)=X1(1)
        X2(-1)=-X2(1)
        X1(NA+1)=X1(NA-1)
        X2(NA+1)=-X2(NA-1)
        IF (TORUS .EQ. 'J') THEN
          X1(-1)=-X1(1)
          X2(-1)= X2(1)
          X1(NA+1)=-X1(NA-1)
          X2(NA+1)= X2(NA-1)
        ENDIF
        DO 480 J=-1,NA+1,1
           Z(J)=X1(J)
           R(J)=X2(J)
  480   CONTINUE
        NI=N
        NA=N
        IF (TORUS .EQ. 'J') THEN
           NI=NTORUS
           NA=NTORUS
        ENDIF
        GOTO 1300
C
C       **************************************************
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE     ****
C       *****                                         ****
C       **************************************************
C
 1500   CONTINUE
        DO 220 J=0,  NA, 1
           H1=DS(J)
           H2=DS(J+1)
           H3=H1+H2
           Z2(J)=(Z(J+1)/H2/H3-Z(J)/H1/H2+Z(J-1)/H1/H3)*2.D0
           R2(J)=(R(J+1)/H2/H3-R(J)/H1/H2+R(J-1)/H1/H3)*2.D0
           Z1(J)=(Z(J)-Z(J-1))/H1 + Z2(J)*H1/2.D0
           R1(J)=(R(J)-R(J-1))/H1 + R2(J)*H1/2.D0
           G(J)=Z1(J)*Z1(J)+R1(J)*R1(J)
           WG(J)=DSQRT(G(J))
  220   CONTINUE
C
C       KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS
C       KM    = 2 * MITTLERE KRUEMMUNG
        DO 230 J=0,NA,1
           GAMPKT(J)=0.D0
           KAPPA(J)=(-R1(J)*Z2(J)+Z1(J)*R2(J))/WG(J)/G(J)
  230   CONTINUE
        DO 231 J=1,NA-1,1
           KM(J)=KAPPA(J)-Z1(J)/R(J)/WG(J)
  231   CONTINUE
        IF (TORUS .EQ. 'N') THEN
          KM(0)=KAPPA(0)-Z2(0)/R1(0)/WG(0)
          KM(NA)=KAPPA(NA)-Z2(NA)/R1(NA)/WG(NA)
        ENDIF
        IF (TORUS .EQ. 'J') THEN
           KM(0)=KAPPA(0)-Z1(0)/R(0)/WG(0)
           KM(NA)=KAPPA(NA)-Z1(NA)/R(NA)/WG(NA)
        ENDIF
        KM(NA+1)=KM(NA-1)
        KM(-1)=KM(1)
C
C       LBKM = LAPLACE-BELTRAMI AUF 2*MITTLERE KRUEMMUNG ANGEWANDT
        DO 240 J=0,NA ,1
           H1=DS(J)
           H2=DS(J+1)
           H3=H1+H2
           KM2(J)=( KM(J+1)/H2/H3-KM(J)/H1/H2+KM(J-1)/H1/H3 )*2.D0
           KM1(J)=( KM(J)-KM(J-1) )/H1 + KM2(J)*H1/2.D0
  240   CONTINUE
        DO 241 J=1,NA-1,1
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/
     *               G(J)/G(J)
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)
  241   CONTINUE
        LBKM(0)=2.D0*KM2(0)/G(0)
        LBKM(NA)=2.D0*KM2(NA)/G(NA)
        IF (TORUS .EQ. 'J') THEN
           J=0
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/
     *               G(J)/G(J)
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)
           J=NA
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/
     *               G(J)/G(J)
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)
        ENDIF
C
        DO 249 J=0,NA,1
          VN(J)= FNV( KM(J) , K0 ) + EPS*LBKM(J)
 249    CONTINUE
C
C       **** COMPUTE OBERFLAECHEN- UND  VOLUMENELEMENTE ****
        OBFEL(0 ) = DS(1) /8.D0 * R(1) * WG(1) * 2.D0 *PI
        OBFEL(NA) = DS(NA)/8.D0 * R(NA)* WG(NA)*2.D0*PI
        VOLEL(0 ) = DS(1) /12.D0*R(1)*(R1(1) *Z(1) - Z1(1) * R(1) )*PI
        VOLEL(NA) = DS(NA)/12.D0*R(NA)*(R1(NA)*Z(NA)- Z1(NA)* R(NA))*PI
        DO 251 J=1,NA-1,1
          H5=  (DS(J)+DS(J+1)) * (DS(J)*DS(J+1))
          H1=  (H5 - 3.D0*DS(J+1)*DS(J+1)) / ( 24.D0* DS(J  ))
          H3=  (H5 - 3.D0*DS(J  )*DS(J  )) / ( 24.D0* DS(J+1))
          H2=  ( DS(J)+DS(J+1) ) / 2.D0 -H1 -H3
          OBFEL(J)= ( H1 * R(J-1)*WG(J-1) + H2 * R(J) * WG(J)
     &              + H3 * R(J+1)*WG(J+1) ) *2.D0*PI
          VOLEL(J)= ( H1 * R(J-1)*(R1(J-1)*Z(J-1) - Z1(J-1)*R(J-1))
     &              + H3 * R(J+1)*(R1(J+1)*Z(J+1) - Z1(J+1)*R(J+1))
     &              + H2 * R(J)*(R1(J)*Z(J) - Z1(J)*R(J)) )*2.D0*PI/3.D0
  251   CONTINUE
C
C       **** COMPUTE INTEGRAL UEBER VN * OBERFLAECHENELEM. + OBERFLAECHE
        OBERFL = 0.D0
        VOLUME = 0.D0
        VNOB   = 0.D0
        DO 253 J=0,NA,1
          OBERFL = OBERFL + OBFEL(J)
          VOLUME = VOLUME + VOLEL(J)
          VNOB   = VNOB   + OBFEL(J)*VN(J)
  253   CONTINUE
C
C       **** COMPUTE NEUE (VOLUMENERHALTENDE) NORMALENGESCHWINDIGKEIT
        DO 255 J=0,NA,1
          IF (VOLPRE .EQ. 'J')  VN(J) = VN(J) - VNOB / OBERFL
  255   CONTINUE
C
        STPKT=0.0D0
        DO 260 J=1,NA,1
          H1=KAPPA(J-1) * VN(J-1) / WG(J-1)
          H2=KAPPA(J  ) * VN(J  ) / WG(J  )
          DSPKT(J)= (H1+H2)*DS(J)/2.D0
          STPKT   = STPKT + DSPKT(J)
  260   CONTINUE
C
C       *** GAMPKT IST DER KORREKTUR-TERM IN TANGENTIELLER
C       **  RICHTUNG.
        IF (NURNR .EQ. 0) THEN
           H1=0.D0
           DO 270 I=1, NA-1,1
              H1=H1+DSPKT(I)
              GAMPKT(I)=(H1-STPKT*GAM(I))/STOT
  270      CONTINUE
        ENDIF
C
        DO 280 J= 0,NA,1
           ZT(J)=  R1(J)*VN(J)/WG(J)-Z1(J)*STOT*GAMPKT(J)
           RT(J)= -Z1(J)*VN(J)/WG(J)-R1(J)*STOT*GAMPKT(J)
  280   CONTINUE
C
C       *** VERGROESSERUNG BZW. VERKLEINERUNG VON  DELT , FALLS
C       **  DYNAMIK ZU LANGSAM BZW. ZU SCHNELL WIRD.
        H1=0.D0
        DO 291 J=0,NA,1
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT + H1
  291   CONTINUE
C       *** FUER DIE STEADY-STEATE-SUCHE SOLLTE  DELT  NICHT
C       **  VERGROESSERT WERDEN !
        IF (T9  .LT. VERGR .OR. T8 .LT. VERGRT) THEN
           IF (H1 .LT. STOT*.0002D0/N1 ) DELT=DELT*2.D0
        ENDIF
        N1=NA
        DO 290 K=1,10,1
        DO 290 J=0,NA,1
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT
           IF (H1 .GT. STOT*.001D0/N1) DELT=DELT*.5D0
  290   CONTINUE
C
C       ***************************************************
C       ****            PRINT PRINT PRINT              ****
C       ***************************************************
C
C
C       *** FALLS R(J) FUER EIN 0<J<N SEHR KLEIN WIRD (TEILUNG),
C       **  WIRD MIT DER RECHTEN HAELFTE WEITERGERECHNET.
C       **                (WIEDER MIT N PUNKTEN)
C       *** FALLS Z(0)-Z(N) SEHR KLEIN WIRD (UEBERGANG ZUM
C       **  TORUS), WIRD MIT DER OBEREN HAELFTE WEITERGERECHNET.
C       **                (MIT N+N PUNKTEN)
        IF (TORUS .EQ. 'N') THEN
          DO 510 J=NA-1,1,-1
             IF (R(J) .LT. .01D0) TT=J
  510     CONTINUE
          IF (Z(0)-Z(NA) .LT. .005D0) TT=NA
          IF (TT .NE. 0) GOTO 1700
        ENDIF
        H2=T9
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100
C
C
 1700   CONTINUE
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',
     *     F12.6)
   51   FORMAT (' OBERFLAECHE =',F13.5,'    VOLUME =',F13.5)
   59   FORMAT (' DELT =',D14.7)
   60   FORMAT(/,T5,'J',T16,'X1',T28,'X2',T37,'  DGA',T52,'KM',
     *           T66,'  VN')
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.5,T56,D14.7)
   61   FORMAT(5(1X,F12.5))
   71   FORMAT(1X,'>>>>',I8,3X,I10)
        APR=APR+1
        WRITE (6,50) T8,T9,STOT
        WRITE (6,51) OBERFL,VOLUME
        WRITE (6,59) DELT
        WRITE (6,60)
        IF (TORUS .EQ. 'N') THEN
          IF (DSWRIT .EQ. 'J') WRITE (22,71) APR,N+N
        ENDIF
        IF (TORUS .EQ. 'J') THEN
          IF (DSWRIT .EQ. 'J') WRITE (22,71) APR,NTORUS
        ENDIF
        IF (DSWRIT .EQ. 'J') WRITE (22,61) T8,STOT,OBERFL,VOLUME
        IF (TORUS .EQ. 'N') THEN
          DO 550 J=0 ,N ,1
             WRITE (6,70) J,Z  (J),  R (J),DS(J)/STOT,KM(J),VN(J)
             IF (DSWRIT .EQ. 'J')
     *            WRITE (22,61) Z (J),R (J),GAM(J),KM(J),VN(J)
  550     CONTINUE
          DO 551 J=N+1,N+N,1
             K=N+N-J
             IF (DSWRIT .EQ. 'J')
     *            WRITE (22,61) Z (K),-R(K),2.D0-GAM(K),KM(K),VN(K)
  551     CONTINUE
        ENDIF
        IF (TORUS .EQ. 'J') THEN
          DO 555 J=0 ,NTORUS,1
             WRITE (6,70) J,Z  (J),  R (J),DS(J)/STOT,KM(J),VN(J)
             IF (DSWRIT .EQ. 'J')
     *            WRITE (22,61) Z (J),R (J),GAM(J),KM(J), VN(J)
  555     CONTINUE
        ENDIF
        IF (DSWRIT .EQ. 'J') WRITE (22,71) 0,0
        IF (DSWRIT .EQ. 'J') BACKSPACE 22
        IF (TT .NE. 0 .AND. TT .NE. N) THEN
            NI=TT
            NA=N
            T9=0
            R(NI)=0.D0
            R(NI+1)=-R(NI-1)
            Z(NI+1)=Z(NI-1)
            T8=0.D0
            GOTO 1300
        ENDIF
        IF (TT .EQ. N) THEN
            NI=N
            NA=NTORUS
            T9=0
            R(0)=0.01D0
            Z(0)=(Z(0) + Z(NI))*.5D0
            R(NI+1)= R(NI-1)
            Z(NI+1)=-Z(NI-1)
            R(NI)=R(0)
            Z(NI)=Z(0)
            Z(-1)=-Z(1)
            R(-1)= R(1)
            TORUS='J'
            T8=0.D0
            GOTO 1300
        ENDIF
C
C       **************************************************
C       ****      BERECHNEN DER NEUEN WERTE           ****
C       **************************************************
C
 1100   DO 600 J= 0,NA,1
           Z(J)=Z(J)+ZT(J)*DELT
           R(J)=R(J)+RT(J)*DELT
  600   CONTINUE
        Z(-1)=Z(1)
        Z(NA+1)=Z(NA-1)
        R(-1)=-R(1)
        R(NA+1)=-R(NA-1)
C
        IF (TORUS .EQ. 'J') THEN
          Z(-1)=-Z(1)
          Z(NA+1)=-Z(NA-1)
          R(-1)= R(1)
          R(NA+1)= R(NA-1)
        ENDIF
C
C
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<
C
        IF (T9 .GE. ZSEND) GOTO 1200
        GOTO 1000
 1200   CONTINUE
        IF (DSWRIT .EQ. 'J') CLOSE(22,STATUS='KEEP')
C        END
C//GO.SYSIN    DD *
C/*
C/*      EXEC DRUCKE
C/*      END
        END
