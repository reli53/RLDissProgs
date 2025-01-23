C//D14KC    JOB RACF,CLASS=C,                                           00010067
C//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                            00020067
C//            EXEC FORTCLG                                             00030067
C//FORT.SYSIN  DD *                                                     00040067
C                                                                       00050067
C       #############################################################   00060067
C       ####   OBERFLAECHEN-DYNAMIK  (3-DIMENSIONAL)             ####   00070067
C       ####      (ROTATIONSSYMMETRISCH UM Z-ACHSE)              ####   00080067
C       ####                                                     ####   00090067
C       ####              (DX/DT)*NORMALE = U(KM)                ####   00100067
C       ####     U(KM) = FNV(KM)+EPS*(LAPLACE-BELTRAMI)(KM)      ####   00110067
C       ####        KM = 2 * MITTLERE KRUEMMUNG                  ####   00120067
C       ####       (NACH ABSCHNUERUNG WIRD MIT DER RECHTEN       ####   00130067
C       ####             HAELFTE WEITERGERECHNET.)               ####   00140067
C       ####       (BEIM UEBERGANG ZUM TORUS WIRD MIT DER OBEREN ####   00150067
C       ####             HAELFTE WEITERGERECHNET.)               ####   00160067
C       ####                                                     ####   00170067
C       ####        NAME DES PROGRAMMES : NOCIRCLE(OBERD3)       ####   00180067
C       #############################################################   00190067
C                                                                       00200067
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00210067
C                                                                       00220067
C                                                                       00230067
        INTEGER           I,J,K,N,T7,T9,T99,ND,NDEND,NTORUS,VERGR       00240067
        INTEGER           NURNR,ZSEND,ZSPRT,APR,NI,NA,TT                00250067
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF                    00260067
        DOUBLE PRECISION  FNV,X,K0,EPS,FNP2                             00270067
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8                   00280067
        DOUBLE PRECISION  AMPLI(0:10)                                   00290067
        DOUBLE PRECISION  Z(-1:201),R(-1:201)                           00300067
        DOUBLE PRECISION  Z1(0:200),R1(0:200)                           00310067
        DOUBLE PRECISION  Z2(0:200),R2(0:200)                           00320067
        DOUBLE PRECISION  ZT(0:200),RT(0:200)                           00330067
        DOUBLE PRECISION  X1(-1:201),X2(-1:201)                         00340067
        DOUBLE PRECISION  DS(0:200),DSPKT(0:200)                        00350067
        DOUBLE PRECISION  GAM(-1:201),GAMPKT(0:200)                     00360067
        DOUBLE PRECISION  G(0:200),WG(0:200)                            00370067
        DOUBLE PRECISION  KAPPA(0:200),KM(-1:201),KM1(0:200)            00380067
        DOUBLE PRECISION  KM2(0:200),LBKM(0:200)                        00390067
        DOUBLE PRECISION  AKO1(0:200),BKO1(0:200),CKO1(0:200)           00400067
        DOUBLE PRECISION  AKO2(0:200),BKO2(0:200),CKO2(0:200)           00410067
        DOUBLE PRECISION  HI1(0: 4000),HI2(0: 4000),HI3(0: 4000)        00420067
        CHARACTER         DNW*30,TORUS*1                                00430067
C       FNV(X,K0)=X-K0                                                  00440073
        FNV(X,K0)=(X-K0) - (X-K0)*(X-K0)*(X-K0)*( 1.D0 )                00450076
        FNP2(X)=.5D0*(3.D0*X*X-1.D0)                                    00460067
C                                                                       00470067
C       **************************************************              00480067
C       ****      PARAMETER PARAMETER PARAMETER       ****              00490067
C       **************************************************              00500067
C                                                                       00510067
                 DNW   = 'D14K.TES1.DATA'                               00520067
                 DELT  =      0.00001000D0 *  64.D0                     00530068
                 N     =     24                                         00540067
                 NTORUS=     30                                         00550067
                 NURNR =      0                                         00560067
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.          00570067
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00580067
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG            00590067
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.            00600067
C       N       : ANZ. DER STUETZPUNKTE (FUER HALBE KURVE)              00610067
C       DELT    : DELTA T                                               00620067
C       VERGR   : DIE OBERE GRENZE DER ZEITSCHRITTE, BIS ZU DER         00630067
C                 DIE DYNAMIK (FALLS SEHR LANGSAM) VERGROESSERT WIRD    00640067
C       FUER N=20 UND NURNR=0 UND ZSEND=30000 IST DIE CPU-ZEIT          00650067
C                 CIRCA 1 MINUTE (CLASS C).                             00660067
C       FUER N=36 UND NURNR=0 UND ZSEND=50000 IST DIE CPU-ZEIT          00670067
C                 CIRCA 5 MINUTE (CLASS D).                             00680067
C                                                                       00690067
C                                                                       00700067
                 ZSEND = 400000                                         00710067
                 ZSPRT =  40000                                         00720068
                 VERGR =      0                                         00730069
                 TORUS = 'N'                                            00740067
C                                                                       00750067
                 K0    =      2.00000000D0                              00760067
C                EPS   =      1.D0 / 6.D0 - 0.1D0 * 2.0D0/21.0D0        00770073
C                EPS   =      1.D0 / 6.D0                               00771076
                 EPS   =      3.D0 / 24.D0                              00772076
C                                                                       00780067
                 ND    =   1000                                         00790067
                 NDEND =     50                                         00800067
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN     00810067
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN               00820067
                 ND1=ND                                                 00830067
                 N1=N                                                   00840067
                 DGAM=1.D0/N1                                           00850067
                 PI=4.D0*DATAN(1.D0)                                    00860067
C                                                                       00870067
C       *************************************************               00880067
C       ****    STARTKONZENTRATION UND START-RAND    ****               00890067
C       *************************************************               00900067
C                                                                       00910067
        AMPLI(0) = 2.D0/K0                                              00920067
        AMPLI(1) = 0.0000D0                                             00930067
        AMPLI(2) = 0.2000D0                                             00940074
        AMPLI(3) = 0.0000D0                                             00950067
        AMPLI(4) = 0.0000D0                                             00960067
        AMPLI(5) = 0.0000D0                                             00970067
        AMPLI(6) = 0.0000D0                                             00980067
        AMPLI(7) = 0.0000D0                                             00990067
        AMPLI(8) = 0.0000D0                                             01000067
        AMPLI(9) = 0.0000D0                                             01010067
        AMPLI(10)= 0.0000D0                                             01020067
C                                                                       01030067
        DO 100 J=0 , ND , 1                                             01040067
           H1=J                                                         01050067
           H3=0.D0                                                      01060067
           H3=H3 + AMPLI(0)                                             01070067
           H3=H3+AMPLI(2)*FNP2( DCOS(H1*PI/ND1) )                       01080067
           HI1(J)=DCOS(     PI*H1/ ND1)*H3                              01090067
           HI2(J)=DSIN(     PI*H1/ ND1)*H3                              01100067
  100   CONTINUE                                                        01110067
C                                                                       01120067
        HI3(0)=0.D0                                                     01130067
        DO 110 J=1, ND  ,1                                              01140067
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        01150067
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  01160067
  110   CONTINUE                                                        01170067
C                                                                       01180067
        STOT=HI3( ND  )                                                 01190067
        J=0                                                             01200067
        DO 120 K=1, N-1, 1                                              01210067
           H1=K                                                         01220067
  130      J=J+1                                                        01230067
           IF (J .GT. ND) GOTO 120                                      01240067
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         01250067
              H5=HI3(J)-HI3(J-1)                                        01260067
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             01270067
              H4=(HI3(J) - H1*STOT/N1)/H5                               01280067
              Z(K)=HI1(J)*H3 + HI1(J-1)*H4                              01290067
              R(K)=HI2(J)*H3 + HI2(J-1)*H4                              01300067
  120   CONTINUE                                                        01310067
        Z(0)=HI1(0)                                                     01320067
        R(0)=HI2(0)                                                     01330067
        Z(N)=HI1(ND)                                                    01340067
        R(N)=HI2(ND)                                                    01350067
        Z(-1)=Z(1)                                                      01360067
        R(-1)=-R(1)                                                     01370067
        Z(N+1)=Z(N-1)                                                   01380067
        R(N+1)=-R(N-1)                                                  01390067
C                                                                       01400067
C                                                                       01410067
C                                                                       01420067
C       **************************************************              01430067
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              01440067
C       **************************************************              01450067
C                                                                       01460067
        OPEN (22,FILE=DNW)                                              01470067
        REWIND 22                                                       01480067
   10   FORMAT (///' PROGRAMM-NAME :  NOCIRCLE(OBERD3)')                01490067
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                01500067
     *        ,F10.7,'    N =',I4)                                      01510067
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          01520067
   40   FORMAT (/,' EPS =',F10.5,' XXX =',F10.5,'   B =',F10.5)         01530067
        WRITE (6,10)                                                    01540067
        WRITE (6,20) DGAM,DELT,N                                        01550067
        WRITE (6,30) ZSEND,ZSPRT                                        01560067
        WRITE (6,40) EPS,1.D0,2.D0/K0                                   01570067
C                                                                       01580067
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01590067
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              01600067
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01610067
C                                                                       01620067
        T9=-1                                                           01630067
        T7=0                                                            01640067
        T8     = -DELT                                                  01650067
        APR=0                                                           01660067
        NI=N                                                            01670067
        NA=N                                                            01680067
        TT=0                                                            01690067
 1000   T9=T9+1                                                         01700067
        T8=T8+DELT                                                      01710067
C                                                                       01720067
C       ***************************************************             01730067
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             01740067
C       ***************************************************             01750067
C                                                                       01760067
 1300   T99=0                                                           01770067
C       STOT=TOTALE BOGENLAENGE (FUER HALBE KURVE)                      01780067
        STOT=0.0D0                                                      01790067
        DO 305 J=1, NI  ,1                                              01800067
            DS(J)=DSQRT(( Z(J)-Z(J-1) )*( Z(J)-Z(J-1) )+                01810067
     *            ( R(J)-R(J-1) )*( R(J)-R(J-1) ))                      01820067
        STOT=STOT+DS(J)                                                 01830067
  305   CONTINUE                                                        01840067
        DS(0)=DS(1)                                                     01850067
        DS(NI+1)=DS(NI)                                                 01860067
C                                                                       01870067
        GAM(0)=0.D0                                                     01880067
        DO 307 J=1,NI,1                                                 01890067
           GAM(J)=GAM(J-1)+DS(J)/STOT                                   01900067
  307   CONTINUE                                                        01910067
        GAM(-1)=-GAM(1)                                                 01920067
        GAM(NI+1)=2.D0-GAM(NI-1)                                        01930067
C                                                                       01940067
        H2=NI                                                           01950067
        DGAM=1.D0/H2                                                    01960067
        H2=DGAM/4.D0                                                    01970067
        DO 300 J=1, NI-1 , 1                                            01980067
           H1=J                                                         01990067
           H3=H1*DGAM                                                   02000067
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))        02010067
     *            T99=1                                                 02020067
  300   CONTINUE                                                        02030067
        MDIFF=0.0D0                                                     02040067
        DO 310 J=1, NI ,1                                               02050067
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))                     02060067
  310   CONTINUE                                                        02070067
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1                        02080067
        IF (TT .NE. 0) T99=1                                            02090067
        TT=0                                                            02100067
C                                                                       02110067
        IF (T99 .EQ. 0) GOTO 1500                                       02120067
C                                                                       02130067
C                                                                       02140067
C       ***************************************************             02150067
C       ****          NEUE DISKRETISIERUNG             ****             02160067
C       ***************************************************             02170067
C                                                                       02180067
        T7=T7+1                                                         02190067
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  02200067
     *          I11)                                                    02210067
        WRITE (6,90) T7,T9                                              02220067
        IF (T7 .GT. NDEND) GOTO 1200                                    02230067
C                                                                       02240067
C                                                                       02250067
        DO 401 J=-1,NI+1,1                                              02260067
           X1(J)=Z(J)                                                   02270067
           X2(J)=R(J)                                                   02280067
  401   CONTINUE                                                        02290067
C       *** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN            02300067
C       **  EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE         02310067
        DO 400 J=0,  NI  , 1                                            02320067
           H2=GAM(J+1)-GAM(J)                                           02330067
           H1=GAM(J)-GAM(J-1)                                           02340067
           H3=H1+H2                                                     02350067
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3              02360067
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3        02370067
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3                     02380067
           CKO1(J)=X1(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)                02390067
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3              02400067
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3        02410067
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3                     02420067
           CKO2(J)=X2(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)                02430067
  400   CONTINUE                                                        02440067
C                                                                       02450067
C       *** BESTIMMEN DERJENIGEN PUNKTE HI1(J),HI2(J) ,0<J<ND , FUER    02460067
C       **  DIE DIE BOGENLAENGE (VON HI1(0),HI2(0) AUS BERECHNET)       02470067
C       **  DEN WERT STOT*J/ND ANNIMMT.                                 02480067
        HI1(0)=X1(0)                                                    02490067
        HI2(0)=X2(0)                                                    02500067
        HI1(ND)=X1(NI)                                                  02510067
        HI2(ND)=X2(NI)                                                  02520067
        J=0                                                             02530067
        DO 410 K=1, NI,1                                                02540067
  420      J=J+1                                                        02550067
           IF (J .GT. ND  ) GOTO 410                                    02560067
           H1=J                                                         02570067
           H2=H1/ND1                                                    02580067
           IF (H2 .LT. GAM(K)) THEN                                     02590067
              H5=GAM(K)-GAM(K-1)                                        02600067
              H3=(GAM(K)-H2)/H5                                         02610067
              H4=(H2-GAM(K-1))/H5                                       02620067
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        02630067
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     02640067
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        02650067
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     02660067
              GOTO 420                                                  02670067
           ELSE                                                         02680067
              J=J-1                                                     02690067
              GOTO 410                                                  02700067
           ENDIF                                                        02710067
  410   CONTINUE                                                        02720067
C                                                                       02730067
C                                                                       02740067
C       *** BERECHNEN DER NEUEN KOORDINATENPUNKTE                       02750067
        HI3(0)=0.0D0                                                    02760067
        DO 450 J=1, ND  ,1                                              02770067
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        02780067
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  02790067
  450   CONTINUE                                                        02800067
        STOT=HI3( ND  )                                                 02810067
C                                                                       02820067
        X1(NA)=HI1(ND)                                                  02830067
        X2(NA)=HI2(ND)                                                  02840067
        N1=NA                                                           02850067
        J=0                                                             02860067
        DO 460 K=1, NA-1, 1                                             02870067
           H1=K                                                         02880067
  470      J=J+1                                                        02890067
           IF (J .GT. ND  ) GOTO 460                                    02900067
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         02910067
              H5=HI3(J)-HI3(J-1)                                        02920067
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             02930067
              H4=(HI3(J) - H1*STOT/N1  )/H5                             02940067
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             02950067
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             02960067
  460   CONTINUE                                                        02970067
        X1(-1)=X1(1)                                                    02980067
        X2(-1)=-X2(1)                                                   02990067
        X1(NA+1)=X1(NA-1)                                               03000067
        X2(NA+1)=-X2(NA-1)                                              03010067
        IF (TORUS .EQ. 'J') THEN                                        03020067
          X1(-1)=-X1(1)                                                 03030067
          X2(-1)= X2(1)                                                 03040067
          X1(NA+1)=-X1(NA-1)                                            03050067
          X2(NA+1)= X2(NA-1)                                            03060067
        ENDIF                                                           03070067
        DO 480 J=-1,NA+1,1                                              03080067
           Z(J)=X1(J)                                                   03090067
           R(J)=X2(J)                                                   03100067
  480   CONTINUE                                                        03110067
        NI=N                                                            03120067
        NA=N                                                            03130067
        IF (TORUS .EQ. 'J') THEN                                        03140067
           NI=NTORUS                                                    03150067
           NA=NTORUS                                                    03160067
        ENDIF                                                           03170067
        GOTO 1300                                                       03180067
C                                                                       03190067
C       **************************************************              03200067
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE     ****                03210067
C       *****                                         ****              03220067
C       **************************************************              03230067
C                                                                       03240067
 1500   CONTINUE                                                        03250067
        DO 220 J=0,  NA, 1                                              03260067
           H1=DS(J)                                                     03270067
           H2=DS(J+1)                                                   03280067
           H3=H1+H2                                                     03290067
           Z2(J)=(Z(J+1)/H2/H3-Z(J)/H1/H2+Z(J-1)/H1/H3)*2.D0            03300067
           R2(J)=(R(J+1)/H2/H3-R(J)/H1/H2+R(J-1)/H1/H3)*2.D0            03310067
           Z1(J)=(Z(J)-Z(J-1))/H1 + Z2(J)*H1/2.D0                       03320067
           R1(J)=(R(J)-R(J-1))/H1 + R2(J)*H1/2.D0                       03330067
           G(J)=Z1(J)*Z1(J)+R1(J)*R1(J)                                 03340067
           WG(J)=DSQRT(G(J))                                            03350067
  220   CONTINUE                                                        03360067
C                                                                       03370067
C       KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS              03380067
C       KM    = 2 * MITTLERE KRUEMMUNG                                  03390067
        DO 230 J=0,NA,1                                                 03400067
           GAMPKT(J)=0.D0                                               03410067
           KAPPA(J)=(-R1(J)*Z2(J)+Z1(J)*R2(J))/WG(J)/G(J)               03420067
  230   CONTINUE                                                        03430067
        DO 231 J=1,NA-1,1                                               03440067
           KM(J)=KAPPA(J)-Z1(J)/R(J)/WG(J)                              03450067
  231   CONTINUE                                                        03460067
        IF (TORUS .EQ. 'N') THEN                                        03470067
          KM(0)=KAPPA(0)-Z2(0)/R1(0)/WG(0)                              03480067
          KM(NA)=KAPPA(NA)-Z2(NA)/R1(NA)/WG(NA)                         03490067
        ENDIF                                                           03500067
        IF (TORUS .EQ. 'J') THEN                                        03510067
           KM(0)=KAPPA(0)-Z1(0)/R(0)/WG(0)                              03520067
           KM(NA)=KAPPA(NA)-Z1(NA)/R(NA)/WG(NA)                         03530067
        ENDIF                                                           03540067
        KM(NA+1)=KM(NA-1)                                               03550067
        KM(-1)=KM(1)                                                    03560067
C                                                                       03570067
C       LBKM = LAPLACE-BELTRAMI AUF 2*MITTLERE KRUEMMUNG ANGEWANDT      03580067
        DO 240 J=0,NA ,1                                                03590067
           H1=DS(J)                                                     03600067
           H2=DS(J+1)                                                   03610067
           H3=H1+H2                                                     03620067
           KM2(J)=( KM(J+1)/H2/H3-KM(J)/H1/H2+KM(J-1)/H1/H3 )*2.D0      03630067
           KM1(J)=( KM(J)-KM(J-1) )/H1 + KM2(J)*H1/2.D0                 03640067
  240   CONTINUE                                                        03650067
        DO 241 J=1,NA-1,1                                               03660067
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/        03670067
     *               G(J)/G(J)                                          03680067
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)                       03690067
  241   CONTINUE                                                        03700067
        LBKM(0)=2.D0*KM2(0)/G(0)                                        03710067
        LBKM(NA)=2.D0*KM2(NA)/G(NA)                                     03720067
        IF (TORUS .EQ. 'J') THEN                                        03730067
           J=0                                                          03740067
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/        03750067
     *               G(J)/G(J)                                          03760067
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)                       03770067
           J=NA                                                         03780067
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/        03790067
     *               G(J)/G(J)                                          03800067
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)                       03810067
        ENDIF                                                           03820067
C                                                                       03830067
        STPKT=0.0D0                                                     03840067
        DO 250 J=1,NA,1                                                 03850067
          H1=KAPPA(J-1)*(FNV(KM(J-1),K0)+EPS*LBKM(J-1))/WG(J-1)         03860067
          H2=KAPPA(J  )*(FNV(KM(J  ),K0)+EPS*LBKM(J  ))/WG(J  )         03870067
          DSPKT(J)= (H1+H2)*DS(J)/2.D0                                  03880067
          STPKT   = STPKT + DSPKT(J)                                    03890067
  250   CONTINUE                                                        03900067
C                                                                       03910067
C       *** GAMPKT IST DER KORREKTUR-TERM IN TANGENTIELLER              03920067
C       **  RICHTUNG.                                                   03930067
        IF (NURNR .EQ. 0) THEN                                          03940067
           H1=0.D0                                                      03950067
           DO 270 I=1, NA-1,1                                           03960067
              H1=H1+DSPKT(I)                                            03970067
              GAMPKT(I)=(H1-STPKT*GAM(I))/STOT                          03980067
  270      CONTINUE                                                     03990067
        ENDIF                                                           04000067
C                                                                       04010067
        DO 280 J= 0,NA,1                                                04020067
           H1=FNV(KM(J),K0)+EPS*LBKM(J)                                 04030067
           ZT(J)=  R1(J)*H1/WG(J)-Z1(J)*STOT*GAMPKT(J)                  04040067
           RT(J)= -Z1(J)*H1/WG(J)-R1(J)*STOT*GAMPKT(J)                  04050067
  280   CONTINUE                                                        04060067
C                                                                       04070067
C       *** VERGROESSERUNG BZW. VERKLEINERUNG VON  DELT , FALLS         04080067
C       **  DYNAMIK ZU LANGSAM BZW. ZU SCHNELL WIRD.                    04090067
        H1=0.D0                                                         04100067
        DO 291 J=0,NA,1                                                 04110067
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT + H1            04120067
  291   CONTINUE                                                        04130067
C       *** FUER DIE STEADY-STEATE-SUCHE SOLLTE  DELT  NICHT            04140067
C       **  VERGROESSERT WERDEN !                                       04150067
        IF (T9  .LT. VERGR) THEN                                        04160067
           IF (H1 .LT. STOT*.0002D0/N1 ) DELT=DELT*2.D0                 04170067
        ENDIF                                                           04180067
        N1=NA                                                           04190067
        DO 290 K=1,10,1                                                 04200067
        DO 290 J=0,NA,1                                                 04210067
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT                 04220067
           IF (H1 .GT. STOT*.001D0/N1) DELT=DELT*.5D0                   04230068
  290   CONTINUE                                                        04240067
C                                                                       04250067
C       ***************************************************             04260067
C       ****            PRINT PRINT PRINT              ****             04270067
C       ***************************************************             04280067
C                                                                       04290067
C                                                                       04300067
C       *** FALLS R(J) FUER EIN 0<J<N SEHR KLEIN WIRD (TEILUNG),        04310067
C       **  WIRD MIT DER RECHTEN HAELFTE WEITERGERECHNET.               04320067
C       **                (WIEDER MIT N PUNKTEN)                        04330067
C       *** FALLS Z(0)-Z(N) SEHR KLEIN WIRD (UEBERGANG ZUM              04340067
C       **  TORUS), WIRD MIT DER OBEREN HAELFTE WEITERGERECHNET.        04350067
C       **                (MIT N+N PUNKTEN)                             04360067
        IF (TORUS .EQ. 'N') THEN                                        04370067
          DO 510 J=NA-1,1,-1                                            04380067
             IF (R(J) .LT. .01D0) TT=J                                  04390067
  510     CONTINUE                                                      04400067
          IF (Z(0)-Z(NA) .LT. .005D0) TT=NA                             04410067
          IF (TT .NE. 0) GOTO 1700                                      04420067
        ENDIF                                                           04430067
        H2=T9                                                           04440067
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          04450067
C                                                                       04460067
C                                                                       04470067
 1700   CONTINUE                                                        04480067
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    04490067
     *     F12.6)                                                       04500067
   59   FORMAT (' DELT =',D14.7)                                        04510067
   60   FORMAT(/,T5,'J',T16,'X1',T28,'X2',T37,'  DGA',T52,'KM',         04520067
     *           T66,'  VN')                                            04530067
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.5,T56,D14.7) 04540067
   61   FORMAT(1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5)            04550067
   71   FORMAT(1X,'>>>>',I8,3X,I10)                                     04560067
        APR=APR+1                                                       04570067
        WRITE (6,50) T8,T9,STOT                                         04580067
        WRITE (6,59) DELT                                               04590067
        WRITE (6,60)                                                    04600067
        IF (TORUS .EQ. 'N') THEN                                        04610067
          WRITE (22,71) APR,N+N                                         04620067
        ENDIF                                                           04630067
        IF (TORUS .EQ. 'J') THEN                                        04640067
          WRITE (22,71) APR,NTORUS                                      04650067
        ENDIF                                                           04660067
        WRITE (22,61) T8,STOT                                           04670067
        IF (TORUS .EQ. 'N') THEN                                        04680067
          DO 550 J=0 ,N ,1                                              04690067
             WRITE (6,70) J,Z  (J),  R (J),DS(J)/STOT,KM(J),            04700067
     *                                 FNV(KM(J),K0)+EPS*LBKM(J)        04710067
             WRITE (22,61) Z (J),R (J),GAM(J),KM(J),                    04720067
     *                                 FNV(KM(J),K0)+EPS*LBKM(J)        04730067
  550     CONTINUE                                                      04740067
          DO 551 J=N+1,N+N,1                                            04750067
             K=N+N-J                                                    04760067
             WRITE (22,61) Z (K),-R(K),2.D0-GAM(K),KM(K),               04770067
     *                                 FNV(KM(K),K0)+EPS*LBKM(K)        04780067
  551     CONTINUE                                                      04790067
        ENDIF                                                           04800067
        IF (TORUS .EQ. 'J') THEN                                        04810067
          DO 555 J=0 ,NTORUS,1                                          04820067
             WRITE (6,70) J,Z  (J),  R (J),DS(J)/STOT,KM(J),            04830067
     *                                 FNV(KM(J),K0)+EPS*LBKM(J)        04840067
             WRITE (22,61) Z (J),R (J),GAM(J),KM(J),                    04850067
     *                                 FNV(KM(J),K0)+EPS*LBKM(J)        04860067
  555     CONTINUE                                                      04870067
        ENDIF                                                           04880067
        WRITE (22,71) 0,0                                               04890067
        BACKSPACE 22                                                    04900067
        IF (TT .NE. 0 .AND. TT .NE. N) THEN                             04910067
            NI=TT                                                       04920067
            NA=N                                                        04930067
            T9=0                                                        04940067
            R(NI)=0.D0                                                  04950067
            R(NI+1)=-R(NI-1)                                            04960067
            Z(NI+1)=Z(NI-1)                                             04970067
            T8=0.D0                                                     04980067
            GOTO 1300                                                   04990067
        ENDIF                                                           05000067
        IF (TT .EQ. N) THEN                                             05010067
            NI=N                                                        05020067
            NA=NTORUS                                                   05030067
            T9=0                                                        05040067
            R(0)=0.01D0                                                 05050067
            Z(0)=(Z(0) + Z(NI))*.5D0                                    05060067
            R(NI+1)= R(NI-1)                                            05070067
            Z(NI+1)=-Z(NI-1)                                            05080067
            R(NI)=R(0)                                                  05090067
            Z(NI)=Z(0)                                                  05100067
            Z(-1)=-Z(1)                                                 05110067
            R(-1)= R(1)                                                 05120067
            TORUS='J'                                                   05130067
            T8=0.D0                                                     05140067
            GOTO 1300                                                   05150067
        ENDIF                                                           05160067
C                                                                       05170067
C       **************************************************              05180067
C       ****      BERECHNEN DER NEUEN WERTE           ****              05190067
C       **************************************************              05200067
C                                                                       05210067
 1100   DO 600 J= 0,NA,1                                                05220067
           Z(J)=Z(J)+ZT(J)*DELT                                         05230067
           R(J)=R(J)+RT(J)*DELT                                         05240067
  600   CONTINUE                                                        05250067
        Z(-1)=Z(1)                                                      05260067
        Z(NA+1)=Z(NA-1)                                                 05270067
        R(-1)=-R(1)                                                     05280067
        R(NA+1)=-R(NA-1)                                                05290067
C                                                                       05300067
        IF (TORUS .EQ. 'J') THEN                                        05310067
          Z(-1)=-Z(1)                                                   05320067
          Z(NA+1)=-Z(NA-1)                                              05330067
          R(-1)= R(1)                                                   05340067
          R(NA+1)= R(NA-1)                                              05350067
        ENDIF                                                           05360067
C                                                                       05370067
C                                                                       05380067
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              05390067
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              05400067
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              05410067
C                                                                       05420067
        IF (T9 .GE. ZSEND) GOTO 1200                                    05430067
        GOTO 1000                                                       05440067
 1200   CONTINUE                                                        05450067
        CLOSE(22,STATUS='KEEP')                                         05460067
C        END                                                            05470067
C//GO.SYSIN    DD *                                                     05480067
C/*                                                                     05490067
C/*      EXEC DRUCKE                                                    05500067
C/*      END                                                            05510067
        END                                                             05520067
