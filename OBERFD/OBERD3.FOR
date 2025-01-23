//D14KF    JOB RACF,CLASS=F,                                            00010063
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020063
//            EXEC FORTCLG                                              00030063
//FORT.SYSIN  DD *                                                      00040063
C                                                                       00050002
C       #############################################################   00060002
C       ####   OBERFLAECHEN-DYNAMIK  (3-DIMENSIONAL)             ####   00070002
C       ####      (ROTATIONSSYMMETRISCH UM Z-ACHSE)              ####   00080002
C       ####                                                     ####   00090002
C       ####              (DX/DT)*NORMALE = U(KM)                ####   00100010
C       ####     U(KM) = FNV(KM)+EPS*(LAPLACE-BELTRAMI)(KM)      ####   00110010
C       ####        KM = 2 * MITTLERE KRUEMMUNG                  ####   00120010
C       ####       (NACH ABSCHNUERUNG WIRD MIT DER RECHTEN       ####   00130027
C       ####             HAELFTE WEITERGERECHNET.)                ####  00140027
C       ####                                                     ####   00150002
C       ####        NAME DES PROGRAMMES : NOCIRCLE(OBERD3)       ####   00160002
C       #############################################################   00170002
C                                                                       00180002
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00190002
C                                                                       00200002
C                                                                       00210000
        INTEGER           I,J,K,N,T7,T9,T99,ND,NDEND                    00220002
        INTEGER           NURNR,ZSEND,ZSPRT,APR,NI,NA,TT                00230022
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF                    00240002
        DOUBLE PRECISION  FNV,X,K0,EPS                                  00250054
        DOUBLE PRECISION  H1,H2,H3,H4,H5,N1,ND1,PI,T8                   00260002
        DOUBLE PRECISION  AMPLI(0:10)                                   00270002
        DOUBLE PRECISION  Z(-1:201),R(-1:201)                           00280002
        DOUBLE PRECISION  Z1(0:200),R1(0:200)                           00290032
        DOUBLE PRECISION  Z2(0:200),R2(0:200)                           00300042
        DOUBLE PRECISION  ZT(0:200),RT(0:200)                           00310042
        DOUBLE PRECISION  X1(-1:201),X2(-1:201)                         00320002
        DOUBLE PRECISION  DS(0:200),DSPKT(0:200)                        00330002
        DOUBLE PRECISION  GAM(-1:201),GAMPKT(0:200)                     00340007
        DOUBLE PRECISION  G(0:200),WG(0:200)                            00350002
        DOUBLE PRECISION  KAPPA(0:200),KM(-1:201),KM1(0:200)            00360010
        DOUBLE PRECISION  KM2(0:200),LBKM(0:200)                        00370010
        DOUBLE PRECISION  AKO1(0:200),BKO1(0:200),CKO1(0:200)           00380002
        DOUBLE PRECISION  AKO2(0:200),BKO2(0:200),CKO2(0:200)           00390002
        DOUBLE PRECISION  HI1(0: 4000),HI2(0: 4000),HI3(0: 4000)        00400000
        CHARACTER         DNW*30                                        00410002
        FNV(X,K0)=X-K0                                                  00420046
C       FNV(X,K0)=DTANH(    2.D0*(X-K0)  )/   2.D0                      00430049
C       FNV(X,K0)=X-K0-(X-K0)*(X-K0)*(X-K0)*.3D0                        00440045
C       FNV(X,K0)=X-K0+(X-K0)*(X-K0)                                    00450045
C       FNV(X,K0)=(X-K0)*2.D0 - DTANH(X-K0)*1.D0                        00460045
C       FNV(X,K0)= -(X-K0)*1.D0 + DTANH(X-K0)*2.D0                      00470046
C       FNV(X,K0)=.5D0*(-DEXP(-2.D0*(X-K0))+1.D0)                       00480015
C                                                                       00490002
C       **************************************************              00500002
C       ****      PARAMETER PARAMETER PARAMETER       ****              00510002
C       **************************************************              00520002
C                                                                       00530002
                 DNW   = 'D14K.OD3-01.DATA'                             00540065
                 DELT  =      0.000010000D0 * 64.D0                     00550063
                 N     =     36                                         00560047
                 NURNR =      0                                         00570042
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.          00580002
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00590002
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG            00600002
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.            00610002
C       N       : ANZ. DER STUETZPUNKTE (FUER HALBE KURVE)              00620002
C       DELT    : DELTA T                                               00630002
C       FUER N=20 UND NURNR=0 UND ZSEND=30000 IST DIE CPU-ZEIT          00640020
C                 CIRCA 1 MINUTE (CLASS C).                             00650000
C       FUER N=36 UND NURNR=0 UND ZSEND=50000 IST DIE CPU-ZEIT          00660051
C                 CIRCA 5 MINUTE (CLASS D).                             00670051
C                                                                       00680002
C                                                                       00690002
                 ZSEND = 360000                                         00700058
                 ZSPRT =  20000                                         00710063
C                                                                       00720002
                 K0    =      2.00000000D0                              00730010
                 EPS   =      1.D0/6.D0 - (2.D0/21.D0) * 0.1D0          00740064
C                                                                       00750002
                 ND    =   1000                                         00760039
                 NDEND =     50                                         00770042
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN     00780002
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN               00790002
                 ND1=ND                                                 00800002
                 N1=N                                                   00810002
                 DGAM=1.D0/N1                                           00820002
                 PI=4.D0*DATAN(1.D0)                                    00830002
C                                                                       00840002
C       *************************************************               00850002
C       ****    STARTKONZENTRATION UND START-RAND    ****               00860002
C       *************************************************               00870000
C                                                                       00880002
        AMPLI(0) = 2.D0/K0                                              00890010
        AMPLI(1) = 0.0000D0                                             00900002
        AMPLI(2) =-0.1400D0                                             00910065
        AMPLI(3) = 0.0000D0                                             00920002
        AMPLI(4) = 0.0000D0                                             00930002
        AMPLI(5) = 0.0000D0                                             00940002
        AMPLI(6) = 0.0000D0                                             00950002
        AMPLI(7) = 0.0000D0                                             00960002
        AMPLI(8) = 0.0000D0                                             00970000
        AMPLI(9) = 0.0000D0                                             00980002
        AMPLI(10)= 0.0000D0                                             00990002
C                                                                       01000002
        DO 100 J=0 , ND , 1                                             01010002
           H1=J                                                         01020002
           H3=0.D0                                                      01030002
           DO 105 I=0,10,1                                              01040052
              CALL LEGPOL( DCOS(H1*PI/ND1) ,I, H2)                      01050052
              H3=H3 + AMPLI(I)*H2                                       01060052
  105      CONTINUE                                                     01070052
           HI1(J)=DCOS(     PI*H1/ ND1)*H3                              01080000
           HI2(J)=DSIN(     PI*H1/ ND1)*H3                              01090002
  100   CONTINUE                                                        01100002
C                                                                       01110002
        HI3(0)=0.D0                                                     01120018
        DO 110 J=1, ND  ,1                                              01130002
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        01140002
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  01150002
  110   CONTINUE                                                        01160002
C                                                                       01170002
        STOT=HI3( ND  )                                                 01180000
        J=0                                                             01190002
        DO 120 K=1, N-1, 1                                              01200002
           H1=K                                                         01210002
  130      J=J+1                                                        01220002
           IF (J .GT. ND) GOTO 120                                      01230019
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         01240002
              H5=HI3(J)-HI3(J-1)                                        01250002
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             01260002
              H4=(HI3(J) - H1*STOT/N1)/H5                               01270002
              Z(K)=HI1(J)*H3 + HI1(J-1)*H4                              01280000
              R(K)=HI2(J)*H3 + HI2(J-1)*H4                              01290002
  120   CONTINUE                                                        01300002
        Z(0)=HI1(0)                                                     01310002
        R(0)=HI2(0)                                                     01320002
        Z(N)=HI1(ND)                                                    01330002
        R(N)=HI2(ND)                                                    01340002
        Z(-1)=Z(1)                                                      01350002
        R(-1)=-R(1)                                                     01360002
        Z(N+1)=Z(N-1)                                                   01370002
        R(N+1)=-R(N-1)                                                  01380000
C                                                                       01390002
C                                                                       01400002
C                                                                       01410002
C       **************************************************              01420002
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              01430002
C       **************************************************              01440002
C                                                                       01450002
        OPEN (22,FILE=DNW)                                              01460011
        REWIND 22                                                       01470011
   10   FORMAT (///' PROGRAMM-NAME :  NOCIRCLE(OBERD3)')                01480010
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                01490002
     *        ,F10.7,'    N =',I4)                                      01500002
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          01510002
   40   FORMAT (/,' EPS =',F10.5,' XXX =',F10.5,'   B =',F10.5)         01520002
        WRITE (6,10)                                                    01530002
        WRITE (6,20) DGAM,DELT,N                                        01540002
        WRITE (6,30) ZSEND,ZSPRT                                        01550002
        WRITE (6,40) EPS,1.D0,2.D0/K0                                   01560010
C                                                                       01570002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01580000
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              01590002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              01600002
C                                                                       01610002
        T9=-1                                                           01620002
        T7=0                                                            01630002
        T8     = -DELT                                                  01640002
        APR=0                                                           01650002
        NI=N                                                            01660032
        NA=N                                                            01670032
        TT=0                                                            01680032
 1000   T9=T9+1                                                         01690002
        T8=T8+DELT                                                      01700002
C                                                                       01710000
C       ***************************************************             01720000
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             01730002
C       ***************************************************             01740002
C                                                                       01750002
 1300   T99=0                                                           01760002
C       STOT=TOTALE BOGENLAENGE (FUER HALBE KURVE)                      01770051
        STOT=0.0D0                                                      01780024
        DO 305 J=1, NI  ,1                                              01790024
            DS(J)=DSQRT(( Z(J)-Z(J-1) )*( Z(J)-Z(J-1) )+                01800024
     *            ( R(J)-R(J-1) )*( R(J)-R(J-1) ))                      01810024
        STOT=STOT+DS(J)                                                 01820024
  305   CONTINUE                                                        01830024
        DS(0)=DS(1)                                                     01840024
        DS(NI+1)=DS(NI)                                                 01850024
C                                                                       01860024
        GAM(0)=0.D0                                                     01870024
        DO 307 J=1,NI,1                                                 01880024
           GAM(J)=GAM(J-1)+DS(J)/STOT                                   01890024
  307   CONTINUE                                                        01900024
        GAM(-1)=-GAM(1)                                                 01910024
        GAM(NI+1)=2.D0-GAM(NI-1)                                        01920024
C                                                                       01930024
        H2=NI                                                           01940024
        DGAM=1.D0/H2                                                    01950027
        H2=DGAM/4.D0                                                    01960002
        DO 300 J=1, NI-1 , 1                                            01970024
           H1=J                                                         01980002
           H3=H1*DGAM                                                   01990002
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))        02000002
     *            T99=1                                                 02010000
  300   CONTINUE                                                        02020002
        MDIFF=0.0D0                                                     02030002
        DO 310 J=1, NI ,1                                               02040033
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))                     02050002
  310   CONTINUE                                                        02060002
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1                        02070002
        IF (TT .NE. 0) T99=1                                            02080025
        TT=0                                                            02090026
C                                                                       02100002
        IF (T99 .EQ. 0) GOTO 1500                                       02110037
C                                                                       02120000
C                                                                       02130002
C       ***************************************************             02140002
C       ****          NEUE DISKRETISIERUNG             ****             02150002
C       ***************************************************             02160002
C                                                                       02170002
        T7=T7+1                                                         02180040
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  02190002
     *          I11)                                                    02200002
        WRITE (6,90) T7,T9                                              02210002
        IF (T7 .GT. NDEND) GOTO 1200                                    02220000
C                                                                       02230022
C                                                                       02240022
        DO 401 J=-1,NI+1,1                                              02250022
           X1(J)=Z(J)                                                   02260002
           X2(J)=R(J)                                                   02270002
  401   CONTINUE                                                        02280002
C       *** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN            02290051
C       **  EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE         02300051
        DO 400 J=0,  NI  , 1                                            02310022
           H2=GAM(J+1)-GAM(J)                                           02320002
           H1=GAM(J)-GAM(J-1)                                           02330002
           H3=H1+H2                                                     02340000
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3              02350002
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3        02360002
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3                     02370002
           CKO1(J)=X1(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)                02380002
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3              02390002
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3        02400002
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3                     02410002
           CKO2(J)=X2(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)                02420002
  400   CONTINUE                                                        02430002
C                                                                       02440002
C       *** BESTIMMEN DERJENIGEN PUNKTE HI1(J),HI2(J) ,0<J<ND , FUER    02450051
C       **  DIE DIE BOGENLAENGE (VON HI1(0),HI2(0) AUS BERECHNET)       02460051
C       **  DEN WERT STOT*J/ND ANNIMMT.                                 02470051
        HI1(0)=X1(0)                                                    02480002
        HI2(0)=X2(0)                                                    02490002
        HI1(ND)=X1(NI)                                                  02500039
        HI2(ND)=X2(NI)                                                  02510039
        J=0                                                             02520002
        DO 410 K=1, NI,1                                                02530022
  420      J=J+1                                                        02540002
           IF (J .GT. ND  ) GOTO 410                                    02550019
           H1=J                                                         02560002
           H2=H1/ND1                                                    02570002
           IF (H2 .LT. GAM(K)) THEN                                     02580000
              H5=GAM(K)-GAM(K-1)                                        02590002
              H3=(GAM(K)-H2)/H5                                         02600002
              H4=(H2-GAM(K-1))/H5                                       02610002
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        02620002
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     02630002
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        02640002
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     02650002
              GOTO 420                                                  02660002
           ELSE                                                         02670002
              J=J-1                                                     02680000
              GOTO 410                                                  02690002
           ENDIF                                                        02700002
  410   CONTINUE                                                        02710002
C                                                                       02720002
C                                                                       02730002
C       *** BERECHNEN DER NEUEN KOORDINATENPUNKTE                       02740051
        HI3(0)=0.0D0                                                    02750002
        DO 450 J=1, ND  ,1                                              02760002
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        02770002
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  02780000
  450   CONTINUE                                                        02790002
        STOT=HI3( ND  )                                                 02800027
C                                                                       02810002
        X1(NA)=HI1(ND)                                                  02820039
        X2(NA)=HI2(ND)                                                  02830039
        N1=NA                                                           02840027
        J=0                                                             02850002
        DO 460 K=1, NA-1, 1                                             02860022
           H1=K                                                         02870002
  470      J=J+1                                                        02880002
           IF (J .GT. ND  ) GOTO 460                                    02890019
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         02900002
              H5=HI3(J)-HI3(J-1)                                        02910000
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             02920002
              H4=(HI3(J) - H1*STOT/N1  )/H5                             02930002
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             02940019
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             02950019
  460   CONTINUE                                                        02960002
        X1(-1)=X1(1)                                                    02970019
        X2(-1)=-X2(1)                                                   02980019
        X1(NA+1)=X1(NA-1)                                               02990022
        X2(NA+1)=-X2(NA-1)                                              03000022
        DO 480 J=-1,NA+1,1                                              03010022
           Z(J)=X1(J)                                                   03020019
           R(J)=X2(J)                                                   03030019
  480   CONTINUE                                                        03040019
        NI=N                                                            03050022
        NA=N                                                            03060022
        GOTO 1300                                                       03070022
C                                                                       03080002
C       **************************************************              03090033
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****              03100033
C       *****                                         ****              03110033
C       **************************************************              03120033
C                                                                       03130033
 1500   CONTINUE                                                        03140032
        DO 220 J=0,  N, 1                                               03150032
           H1=DS(J)                                                     03160032
           H2=DS(J+1)                                                   03170032
           H3=H1+H2                                                     03180032
           Z2(J)=(Z(J+1)/H2/H3-Z(J)/H1/H2+Z(J-1)/H1/H3)*2.D0            03190032
           R2(J)=(R(J+1)/H2/H3-R(J)/H1/H2+R(J-1)/H1/H3)*2.D0            03200032
           Z1(J)=(Z(J)-Z(J-1))/H1 + Z2(J)*H1/2.D0                       03210032
           R1(J)=(R(J)-R(J-1))/H1 + R2(J)*H1/2.D0                       03220032
           G(J)=Z1(J)*Z1(J)+R1(J)*R1(J)                                 03230032
           WG(J)=DSQRT(G(J))                                            03240032
  220   CONTINUE                                                        03250032
C                                                                       03260032
C       KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS              03270032
C       KM    = 2 * MITTLERE KRUEMMUNG                                  03280032
        DO 230 J=0,N,1                                                  03290032
           GAMPKT(J)=0.D0                                               03300032
           KAPPA(J)=(-R1(J)*Z2(J)+Z1(J)*R2(J))/WG(J)/G(J)               03310032
  230   CONTINUE                                                        03320032
        DO 231 J=1,N-1,1                                                03330032
           KM(J)=KAPPA(J)-Z1(J)/R(J)/WG(J)                              03340032
  231   CONTINUE                                                        03350032
        KM(0)=KAPPA(0)-Z2(0)/R1(0)/WG(0)                                03360032
        KM(N)=KAPPA(N)-Z2(N)/R1(N)/WG(N)                                03370032
        KM(N+1)=KM(N-1)                                                 03380032
        KM(-1)=KM(1)                                                    03390032
C                                                                       03400032
C       LBKM = LAPLACE-BELTRAMI AUF 2*MITTLERE KRUEMMUNG ANGEWANDT      03410038
        DO 240 J=0,N  ,1                                                03420032
           H1=DS(J)                                                     03430032
           H2=DS(J+1)                                                   03440032
           H3=H1+H2                                                     03450032
           KM2(J)=( KM(J+1)/H2/H3-KM(J)/H1/H2+KM(J-1)/H1/H3 )*2.D0      03460032
           KM1(J)=( KM(J)-KM(J-1) )/H1 + KM2(J)*H1/2.D0                 03470032
  240   CONTINUE                                                        03480032
        DO 241 J=1,N-1,1                                                03490032
           LBKM(J)=KM2(J)/G(J)-KM1(J)*(Z2(J)*Z1(J)+R2(J)*R1(J))/        03500032
     *               G(J)/G(J)                                          03510032
           LBKM(J)=LBKM(J)+R1(J)/R(J)/G(J)*KM1(J)                       03520032
  241   CONTINUE                                                        03530032
        LBKM(0)=2.D0*KM2(0)/G(0)                                        03540032
        LBKM(N)=2.D0*KM2(N)/G(N)                                        03550032
C                                                                       03560032
        STPKT=0.0D0                                                     03570032
        DO 250 J=1,N,1                                                  03580032
          H1=KAPPA(J-1)*(FNV(KM(J-1),K0)+EPS*LBKM(J-1))/WG(J-1)         03590032
          H2=KAPPA(J  )*(FNV(KM(J  ),K0)+EPS*LBKM(J  ))/WG(J  )         03600032
          DSPKT(J)= (H1+H2)*DS(J)/2.D0                                  03610032
          STPKT   = STPKT + DSPKT(J)                                    03620032
  250   CONTINUE                                                        03630032
C                                                                       03640032
C       *** GAMPKT IST DER KORREKTUR-TERM IN TANGENTIELLER              03650051
C       **  RICHTUNG.                                                   03660051
        IF (NURNR .EQ. 0) THEN                                          03670032
           H1=0.D0                                                      03680032
           DO 270 I=1, N-1,1                                            03690032
              H1=H1+DSPKT(I)                                            03700032
              GAMPKT(I)=(H1-STPKT*GAM(I))/STOT                          03710032
  270      CONTINUE                                                     03720032
        ENDIF                                                           03730032
C                                                                       03740033
        DO 280 J= 0,N,1                                                 03750033
           H1=FNV(KM(J),K0)+EPS*LBKM(J)                                 03760032
           ZT(J)=  R1(J)*H1/WG(J)-Z1(J)*STOT*GAMPKT(J)                  03770034
           RT(J)= -Z1(J)*H1/WG(J)-R1(J)*STOT*GAMPKT(J)                  03780034
  280   CONTINUE                                                        03790033
C                                                                       03800049
C       *** VERGROESSERUNG BZW. VERKLEINERUNG VON  DELT , FALLS         03810051
C       **  DYNAMIK ZU LANGSAM BZW. ZU SCHNELL WIRD.                    03820051
        H1=0.D0                                                         03830045
        DO 291 J=0,N,1                                                  03840045
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT + H1            03850045
  291   CONTINUE                                                        03860045
C       *** FUER DIE STEADY-STEATE-SUCHE SOLLTE  DELT  NICHT            03870051
C       **  VERGROESSERT WERDEN !                                       03880051
C       IF (H1 .LT. STOT*.0010D0/N1 ) DELT=DELT*2.D0                    03890063
        DO 290 K=1,10,1                                                 03900045
        DO 290 J=0,N,1                                                  03910037
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT                 03920034
           IF (H1 .GT. STOT*.010D0/N1) DELT=DELT*.5D0                   03930049
  290   CONTINUE                                                        03940034
C                                                                       03950032
C       ***************************************************             03960002
C       ****            PRINT PRINT PRINT              ****             03970002
C       ***************************************************             03980002
C                                                                       03990002
C                                                                       04000002
C       *** FALLS R(J) FUER EIN 0<J<N SEHR KLEIN WIRD (TEILUNG),        04010051
C       **  WIRD MIT DER RECHTEN HAELFTE WEITERGERECHNET.               04020051
C       *** FALLS Z(0)-Z(N) SEHR KLEIN WIRD (UEBERGANG ZUM              04030051
C       **  TORUS), WIRD DAS PROGRAMM ABGEBROCHEN.                      04040051
        DO 510 J=N-1,1,-1                                               04050033
           IF (R(J) .LT. .01D0) TT=J                                    04060040
  510   CONTINUE                                                        04070016
        IF (Z(0)-Z(N) .LT. .005D0) TT=N                                 04080040
        IF (TT .NE. 0) GOTO 1700                                        04090022
        H2=T9                                                           04100016
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          04110002
C                                                                       04120000
C                                                                       04130002
 1700   CONTINUE                                                        04140022
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    04150002
     *     F12.6)                                                       04160002
   59   FORMAT (' DELT =',D14.7)                                        04170040
   60   FORMAT(/,T5,'J',T16,'X1',T28,'X2',T37,'  DGA',T52,'KM',         04180042
     *           T66,'LBKM')                                            04190042
   70   FORMAT(' ',I5,T8,F10.5,T20,F10.5,T32,F10.7,T44,F10.5,T56,D14.7) 04200042
   61   FORMAT(1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5)            04210002
   71   FORMAT(1X,'>>>>',I8,3X,I10)                                     04220002
        APR=APR+1                                                       04230002
        WRITE (6,50) T8,T9,STOT                                         04240000
        WRITE (6,59) DELT                                               04250038
        WRITE (6,60)                                                    04260002
        WRITE (22,71) APR,N+N                                           04270022
        WRITE (22,61) T8,STOT                                           04280011
        DO 550 J=0 ,N ,1                                                04290002
           WRITE (6,70) J,Z  (J),  R (J),DS(J)/STOT,KM(J),LBKM(J)       04300010
           WRITE (22,61) Z (J),R (J),GAM(J),KM(J),0.0                   04310044
  550   CONTINUE                                                        04320002
        DO 551 J=N+1,N+N,1                                              04330022
           K=N+N-J                                                      04340022
           WRITE (22,61) Z (K),-R(K),2.D0-GAM(K),KM(K),1.0              04350049
  551   CONTINUE                                                        04360022
        WRITE (22,71) 0,0                                               04370011
        BACKSPACE 22                                                    04380011
        IF (TT .NE. 0 .AND. TT .NE. N) THEN                             04390022
            NI=TT                                                       04400022
            NA=N                                                        04410022
            T9=0                                                        04420037
            R(NI)=0.D0                                                  04430022
            R(NI+1)=-R(NI-1)                                            04440022
            Z(NI+1)=Z(NI-1)                                             04450022
            T8=0.D0                                                     04460037
            GOTO 1300                                                   04470024
        ENDIF                                                           04480022
        IF (TT .EQ. N) GOTO 1200                                        04490022
C                                                                       04500002
C       **************************************************              04510002
C       ****      BERECHNEN DER NEUEN WERTE           ****              04520002
C       **************************************************              04530002
C                                                                       04540002
 1100   DO 600 J= 0,N,1                                                 04550002
           Z(J)=Z(J)+ZT(J)*DELT                                         04560034
           R(J)=R(J)+RT(J)*DELT                                         04570034
  600   CONTINUE                                                        04580002
        Z(-1)=Z(1)                                                      04590002
        Z(N+1)=Z(N-1)                                                   04600002
        R(-1)=-R(1)                                                     04610002
        R(N+1)=-R(N-1)                                                  04620002
C                                                                       04630002
C                                                                       04640002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              04650002
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              04660002
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              04670000
C                                                                       04680002
        IF (T9 .GE. ZSEND) GOTO 1200                                    04690002
        GOTO 1000                                                       04700002
 1200   CONTINUE                                                        04710002
        CLOSE(22,STATUS='KEEP')                                         04720011
        END                                                             04730048
C                                                                       04740052
C       >>>>>>>>>>>>>>>>>>>> SUBROUTINES <<<<<<<<<<<<<<<<<<<<<<<<<      04750052
C                                                                       04760052
        SUBROUTINE LEGPOL(X,L,RET)                                      04770052
        DOUBLE PRECISION X,X7,X8,X9,RET,DI                              04780054
        INTEGER L,I                                                     04790052
        IF (L .EQ. 0) RET=1.D0                                          04800052
        IF (L .EQ. 1) RET=X                                             04810052
        IF (L .LE. 1) RETURN                                            04820052
        X7=1.D0                                                         04830052
        X8=X                                                            04840052
        DO 10 I=2,L,1                                                   04850052
           DI=I                                                         04860052
           X9=( (DI+DI-1.D0)*X*X8 - (DI-1.D0)*X7 ) / DI                 04870052
           X7=X8                                                        04880052
           X8=X9                                                        04890052
   10   CONTINUE                                                        04900052
        RET=X9                                                          04910053
        RETURN                                                          04920052
        END                                                             04930063
//GO.SYSIN    DD *                                                      04940063
/*                                                                      04950063
/*      EXEC DRUCKE                                                     04960063
/*      END                                                             04970063
