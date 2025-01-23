C/D14KC    JOB RACF,CLASS=C,                                            00000100
C/    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00000200
C/            EXEC FORTCLG                                              00000300
C/FORT.SYSIN  DD *                                                      00000400
C       #############################################################   00000500
C       ####   OBERFLAECHEN-DYNAMIK  (3-DIMENSIONAL)             ####   00000600
C       ####    DES NAEHRSTOFFMODELLES    MA86                   ####   00000700
C       ####    HIER: FORMULIERUNG DES PROBLEMS ALS              ####   00000800
C       ####          REINES OBERFLAECHENPROBLEM MIT             ####   00000900
C       ####          GREENSCHEN FUNKTIONEN                      ####   00001000
C       ####     ( ROTATIONSSYMMETRISCH UM Z-ACHSE UND           ####   00001100
C       ####       SYMMETRIE BZGL. DER X-Y-EBENE )               ####   00001200
C       ####                                                     ####   00001300
C       ####        (DX/DT)*NORMALE = FUNKTION DER KURVE         ####   00001400
C       ####        KM = 2 * MITTLERE KRUEMMUNG                  ####   00001500
C       ####                                                     ####   00001600
C       ####        NAME DES PROGRAMMES : MA86OD3.FORT           ####   00001700
C       #############################################################   00001800
C       DIE DATEN WERDEN IN    DNW    AGGESPEICHERT.                    00001810
C       GRAPHIK-PROGRAMM:   OBERFD(PIC1)                                00001820
C                                                                       00001900
C       LITERATUR: EIGENE                                               00002000
C                                                                       00002100
C                                                                       00002200
        INTEGER           I,J,K,N,T7,T9,T99,ND,NDEND,NPHI,ISEC,SDP,N4   00002300
        INTEGER           NURNR,ZSEND,ZSPRT,APR,NI,NA,TT                00002400
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF,DELPHI             00002500
        DOUBLE PRECISION  EPS,TAU,DIFK,B0                               00002600
        DOUBLE PRECISION  H1,H2,H3,H4,H5,   H7,H8,H9,N1,ND1,PI,T8       00002700
        DOUBLE PRECISION  AMPLI(0:10)                                   00002800
        DOUBLE PRECISION  A1,B1,A2,B2,ABST,DPN,ELI1,ELI2                00002900
        DOUBLE PRECISION  QU,A6,A5,A4,A3                                00003000
C                                                                       00003100
        DOUBLE PRECISION  Z(-1:41),R(-1:41)                             00003200
        DOUBLE PRECISION  Z1(0:40),R1(0:40)                             00003300
        DOUBLE PRECISION  Z2(0:40),R2(0:40)                             00003400
        DOUBLE PRECISION  ZPZ(-1:161),ZPR(-1:161)                       00003500
        DOUBLE PRECISION  ZPZ1(0:160),ZPR1(0:160)                       00003600
        DOUBLE PRECISION  ZPZ2(0:160),ZPR2(0:160)                       00003700
        DOUBLE PRECISION  ZPDS(0:161),ZPWG(0:160),ZPG(0:160)            00003800
        DOUBLE PRECISION  ZT(0:40),RT(0:40)                             00003900
C                                                                       00004000
        DOUBLE PRECISION  DS(0:41),DSPKT(0:40)                          00004100
        DOUBLE PRECISION  GAM(-1:41),GAMPKT(0:40)                       00004200
        DOUBLE PRECISION  G(0:40),WG(0:40)                              00004300
        DOUBLE PRECISION  KAPPA(0:40),KM(0:40)                          00004400
C                                                                       00004500
        DOUBLE PRECISION  RHO(0:40),RHON(0:40)                          00004600
        DOUBLE PRECISION  GPOT(0:40),GPOTN(0:40)                        00004700
C                                                                       00004800
        DOUBLE PRECISION  INT1(0:40,1:80),INT2(0:40,1:80)               00004900
        DOUBLE PRECISION  INT3(0:40,1:80),INT4(0:40,1:80)               00005000
        DOUBLE PRECISION  INT2S(0:40),INT4S(0:40)                       00005100
        DOUBLE PRECISION  IS1KE(0:40),ISL1KE(1:40),ISR1KE(0:40)         00005200
        DOUBLE PRECISION  INT1CL(0:40),INT2CL(0:40)                     00005300
        DOUBLE PRECISION  INT3CL(0:40),INT4CL(0:40)                     00005400
        DOUBLE PRECISION  INT1KG,INT2KG,INT3KG,INT4KG                   00005500
        DOUBLE PRECISION  KORR1(0:40),KORR2(0:40)                       00005600
        DOUBLE PRECISION  KORR3(0:40),KORR4(0:40)                       00005700
C                                                                       00005800
        DOUBLE PRECISION  ARHO(82,82),AGPOT(82,82)                      00005900
        DOUBLE PRECISION  CRHO(82,82)    , DRHO(82), TH1,TH2,TH3,TH4    00006000
        DOUBLE PRECISION  BRHO(82),BGPOT(82)                            00006100
        DOUBLE PRECISION  VW(82),IP(82),EPSZ,IS                         00006200
        INTEGER           ICON,ISW                                      00006300
C                                                                       00006400
        DOUBLE PRECISION  X1(-1:41),X2(-1:41)                           00006500
        DOUBLE PRECISION  AKO1(0:40),BKO1(0:40),CKO1(0:40)              00006600
        DOUBLE PRECISION  AKO2(0:40),BKO2(0:40),CKO2(0:40)              00006700
        DOUBLE PRECISION  HI1(0: 4000),HI2(0: 4000),HI3(0: 4000)        00006800
        CHARACTER         DNW*30,PROB*1                                 00006900
C                                                                       00007000
C       **************************************************              00007100
C       ****      PARAMETER PARAMETER PARAMETER       ****              00007200
C       **************************************************              00007300
C                                                                       00007400
                 DNW   = 'D14K.TES1.DATA'                               00007500
                 DELT  =      0.00010000D0 * 256.D0                     00007600
                 N4    =     10                                         00007700
                 NPHI  =     16                                         00007800
                 NURNR =      0                                         00007900
                 T9    =     -2                                         00008000
                 SDP   =      1                                         00008100
C       T9      : =-2 KORREKTURTERME ANHAND DER KUGEL AUSGERECHNET      00008200
C               : =-1 OHNE KORREKTURTEME                                00008300
C       NPHI    : STUETZPUNKTE FUER HALBEN KREIS BEI INTEGRATION        00008400
C                 BZGL. DES WINKELS PHI  (NPHI >= 12)                   00008500
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.          00008600
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00008700
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG            00008800
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.            00008900
C       N4      : ANZ. DER STUETZPUNKTE (FUER VIERTEL KURVE)(N4<=40)    00009000
C       DELT    : DELTA T                                               00009100
C       SDP     :  =0 KEINE BILDSCHIRMAUSGABE                           00009200
C               :  =1 BILDSCHIRMAUSGABE                                 00009300
C               :  =2 VERMINDERTE BILDSCHIRMAUSGABE                     00009400
C       FUER N4=32 , NPHI=16   UND 1000 ZEIT-ITERATIONEN BETRAEGT DIE   00009500
C                 CPU-ZEIT  15 MINUTEN + 45 SEKUNDEN                    00009600
C       FUER N4=16 , NPHI=24   UND 1000 ZEIT-ITERATIONEN BETRAEGT DIE   00009700
C                 CPU-ZEIT  9 MINUTEN + 25 SEKUNDEN                     00009800
C                                                                       00009900
C                                                                       00010000
                 ZSEND =  16000                                         00010100
                 ZSPRT =     20                                         00010200
C                                                                       00010300
                 B0    =      1.00000000D0                              00010400
                 TAU   =      2.00000000D0                              00010500
                 DIFK  =      0.00000000D0                              00010600
                 EPS   =      2.00000000D0                              00010700
C        BEI DIFK=0.0 : B0=4.7333 EPS=2.0  (GERADE INSTABIL)            00010800
C                        RHO(AM KUGELRAND) = 2.0                        00010900
C        BEI DIFK=1.0 : B0=1.586 EPS=0.035 (GERADE INSTABIL)            00011000
C                        RHO(AM KUGELRAND) = 1.16                       00011100
C                                                                       00011200
        AMPLI(0) = B0                                                   00011300
        AMPLI(2) = 0.0000D0                                             00011400
        AMPLI(4) = 0.0000D0                                             00011500
        AMPLI(6) = 0.0000D0                                             00011600
        AMPLI(8) = 0.0000D0                                             00011700
        AMPLI(10)= 0.0000D0                                             00011800
C                                                                       00011900
                 ND    =   1000                                         00012000
                 NDEND =     50                                         00012100
                 EPSZ  =      1.D-30                                    00012200
                 ISW   =      1                                         00012300
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN     00012400
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN               00012500
                 N = N4+N4                                              00012600
                 ND1=ND                                                 00012700
                 N1=N                                                   00012800
                 DGAM=1.D0/N1                                           00012900
                 PI=4.D0*DATAN(1.D0)                                    00013000
                 DELPHI=NPHI                                            00013100
                 DELPHI=PI/DELPHI                                       00013200
C                                                                       00013300
C       *************************************************               00013400
C       ****    STARTKONZENTRATION UND START-RAND    ****               00013500
C       *************************************************               00013600
C                                                                       00013700
        INT1KG=B0                                                       00013800
        INT2KG=INT1KG - (1.D0-DEXP(-B0-B0))*.5D0                        00013900
        INT3KG=-.5D0                                                    00014000
        INT4KG=INT3KG - ( -.5D0+DEXP(-B0)*(DCOSH(B0)-DSINH(B0)/B0) )    00014100
        DO 104 I=0,N4                                                   00014200
           KORR1(I)=1.D0                                                00014300
           KORR2(I)=1.D0                                                00014400
           KORR3(I)=1.D0                                                00014500
           KORR4(I)=1.D0                                                00014600
  104   CONTINUE                                                        00014700
C                                                                       00014800
 1111   DO 100 J=0 , ND , 1                                             00014900
           H1=J                                                         00015000
           CALL LEGPOL( DCOS(H1*PI/ND1*.5D0) ,0, H2)                    00015100
           H3= AMPLI(0)*H2                                              00015200
           IF (T9 .NE. -2) THEN                                         00015300
             DO 105 I=2,10,2                                            00015400
                CALL LEGPOL( DCOS(H1*PI/ND1*.5D0) ,I, H2)               00015500
                H3=H3 + AMPLI(I)*H2                                     00015600
  105        CONTINUE                                                   00015700
           ENDIF                                                        00015800
C                                                                       00015900
           HI1(J)=DCOS(.5D0*PI*H1/ ND1)*H3                              00016000
           HI2(J)=DSIN(.5D0*PI*H1/ ND1)*H3                              00016100
  100   CONTINUE                                                        00016200
C                                                                       00016300
        HI3(0)=0.D0                                                     00016400
        DO 110 J=1, ND  ,1                                              00016500
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        00016600
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  00016700
  110   CONTINUE                                                        00016800
        STOT=HI3( ND  )                                                 00016900
        STOT=STOT*2.D0                                                  00017000
C                                                                       00017100
        J=0                                                             00017200
        DO 120 K=1, N4-1, 1                                             00017300
           H1=K                                                         00017400
  130      J=J+1                                                        00017500
           IF (J .GT. ND) GOTO 120                                      00017600
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         00017700
              H5=HI3(J)-HI3(J-1)                                        00017800
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             00017900
              H4=(HI3(J) - H1*STOT/N1)/H5                               00018000
              Z(K)=HI1(J)*H3 + HI1(J-1)*H4                              00018100
              R(K)=HI2(J)*H3 + HI2(J-1)*H4                              00018200
  120   CONTINUE                                                        00018300
        Z(0)=HI1(0)                                                     00018400
        R(0)=HI2(0)                                                     00018500
        Z(N4)=HI1(ND)                                                   00018600
        R(N4)=HI2(ND)                                                   00018700
        Z(-1)=Z(1)                                                      00018800
        R(-1)=-R(1)                                                     00018900
        Z(N4+1)=-Z(N4-1)                                                00019000
        R(N4+1)= R(N4-1)                                                00019100
        R(0)=0.D0                                                       00019200
        Z(N4)=0.D0                                                      00019300
C                                                                       00019400
C                                                                       00019500
C                                                                       00019600
C       **************************************************              00019700
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              00019800
C       **************************************************              00019900
C                                                                       00020000
      IF (T9 .NE. -2) THEN                                              00020100
        OPEN (22,FILE=DNW)                                              00020200
        REWIND 22                                                       00020300
   10   FORMAT (///' PROGRAMM-NAME :  MA86OD32.FORT    ')               00020400
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                00020500
     *        ,F10.7,'   N4 =',I4)                                      00020600
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          00020700
   40   FORMAT (/,' EPS =',F10.5,' TAU =',F10.5,' DIFK=',F10.5)         00020800
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                            00020900
          WRITE (6,10)                                                  00021000
          WRITE (6,20) DGAM,DELT,N4                                     00021100
          WRITE (6,30) ZSEND,ZSPRT                                      00021200
          WRITE (6,40) EPS,TAU,DIFK                                     00021300
        ENDIF                                                           00021400
      ENDIF                                                             00021500
C                                                                       00021600
C                                                                       00021700
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00021800
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              00021900
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00022000
C                                                                       00022100
        T7=0                                                            00022200
        T8     = -DELT                                                  00022300
        APR=0                                                           00022400
        NI=N4                                                           00022500
        NA=N4                                                           00022600
        TT=0                                                            00022700
 1000   T9=T9+1                                                         00022800
        T8=T8+DELT                                                      00022900
C                                                                       00023000
C       ***************************************************             00023100
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             00023200
C       ***************************************************             00023300
C                                                                       00023400
 1300   T99=0                                                           00023500
C       STOT=TOTALE BOGENLAENGE (FUER HALBE KURVE)                      00023600
        STOT=0.0D0                                                      00023700
        DO 305 J=1, NI  ,1                                              00023800
            DS(J)=DSQRT(( Z(J)-Z(J-1) )*( Z(J)-Z(J-1) )+                00023900
     *            ( R(J)-R(J-1) )*( R(J)-R(J-1) ))                      00024000
        STOT=STOT+DS(J)                                                 00024100
  305   CONTINUE                                                        00024200
        STOT=STOT+STOT                                                  00024300
        DS(0)=DS(1)                                                     00024400
        DS(NI+1)=DS(NI)                                                 00024500
C                                                                       00024600
        GAM(0)=0.D0                                                     00024700
        DO 307 J=1,NI,1                                                 00024800
           GAM(J)=GAM(J-1)+DS(J)/STOT                                   00024900
C          WRITE ( 6,111) J,GAM(J),DGAM                                 00025000
  307   CONTINUE                                                        00025100
        GAM(-1)=-GAM(1)                                                 00025200
        GAM(NI+1)=1.D0-GAM(NI-1)                                        00025300
C                                                                       00025400
        H2=NI                                                           00025500
        DGAM=1.D0/(H2+H2)                                               00025600
        H2=DGAM/4.D0                                                    00025700
        DO 300 J=1, NI-1 , 1                                            00025800
           H1=J                                                         00025900
           H3=H1*DGAM                                                   00026000
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))        00026100
     *            T99=1                                                 00026200
  300   CONTINUE                                                        00026300
        MDIFF=0.0D0                                                     00026400
        DO 310 J=1, NI ,1                                               00026500
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))                     00026600
  310   CONTINUE                                                        00026700
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1                        00026800
        IF (TT .NE. 0) T99=1                                            00026900
        TT=0                                                            00027000
C                                                                       00027100
        IF (T99 .EQ. 0) GOTO 1500                                       00027200
C                                                                       00027300
C                                                                       00027400
C       ***************************************************             00027500
C       ****          NEUE DISKRETISIERUNG             ****             00027600
C       ***************************************************             00027700
C                                                                       00027800
        T7=T7+1                                                         00027900
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  00028000
     *          I11)                                                    00028100
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) WRITE (6,90) T7,T9              00028200
        IF (T7 .GT. NDEND) GOTO 1200                                    00028300
C                                                                       00028400
C                                                                       00028500
        DO 401 J=-1,NI+1,1                                              00028600
           X1(J)=Z(J)                                                   00028700
           X2(J)=R(J)                                                   00028800
  401   CONTINUE                                                        00028900
C       *** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN            00029000
C       **  EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE         00029100
        DO 400 J=0,  NI  , 1                                            00029200
           H2=GAM(J+1)-GAM(J)                                           00029300
           H1=GAM(J)-GAM(J-1)                                           00029400
           H3=H1+H2                                                     00029500
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3              00029600
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3        00029700
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3                     00029800
           CKO1(J)=X1(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)                00029900
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3              00030000
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3        00030100
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3                     00030200
           CKO2(J)=X2(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)                00030300
  400   CONTINUE                                                        00030400
C                                                                       00030500
C       *** BESTIMMEN DERJENIGEN PUNKTE HI1(J),HI2(J) ,0<J<ND , FUER    00030600
C       **  DIE DIE BOGENLAENGE (VON HI1(0),HI2(0) AUS BERECHNET)       00030700
C       **  DEN WERT STOT*J/ND/2 ANNIMMT.                               00030800
C                                                                       00030900
        HI1(0)=X1(0)                                                    00031000
        HI2(0)=X2(0)                                                    00031100
        HI1(ND)=X1(NI)                                                  00031200
        HI2(ND)=X2(NI)                                                  00031300
        J=0                                                             00031400
        DO 410 K=1, NI,1                                                00031500
  420      J=J+1                                                        00031600
           IF (J .GT. ND  ) GOTO 410                                    00031700
           H1=J                                                         00031800
           H2=H1/ND1*.5D0                                               00031900
           IF (H2 .LT. GAM(K)) THEN                                     00032000
              H5=GAM(K)-GAM(K-1)                                        00032100
              H3=(GAM(K)-H2)/H5                                         00032200
              H4=(H2-GAM(K-1))/H5                                       00032300
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        00032400
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     00032500
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        00032600
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     00032700
              GOTO 420                                                  00032800
           ELSE                                                         00032900
              J=J-1                                                     00033000
              GOTO 410                                                  00033100
           ENDIF                                                        00033200
  410   CONTINUE                                                        00033300
C                                                                       00033400
C                                                                       00033500
C       *** BERECHNEN DER NEUEN KOORDINATENPUNKTE                       00033600
        HI3(0)=0.0D0                                                    00033700
        DO 450 J=1, ND  ,1                                              00033800
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        00033900
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  00034000
  450   CONTINUE                                                        00034100
        STOT=HI3( ND  )                                                 00034200
        STOT=STOT*2.D0                                                  00034300
C                                                                       00034400
        X1(NA)=HI1(ND)                                                  00034500
        X2(NA)=HI2(ND)                                                  00034600
        N1=NA+NA                                                        00034700
        J=0                                                             00034800
        DO 460 K=1, NA-1, 1                                             00034900
           H1=K                                                         00035000
  470      J=J+1                                                        00035100
           IF (J .GT. ND  ) GOTO 460                                    00035200
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         00035300
              H5=HI3(J)-HI3(J-1)                                        00035400
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             00035500
              H4=(HI3(J) - H1*STOT/N1  )/H5                             00035600
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             00035700
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             00035800
  460   CONTINUE                                                        00035900
        X1(-1)=X1(1)                                                    00036000
        X2(-1)=-X2(1)                                                   00036100
        X1(NA+1)=X1(NA-1)                                               00036200
        X2(NA+1)=-X2(NA-1)                                              00036300
        DO 480 J=-1,NA+1,1                                              00036400
           Z(J)=X1(J)                                                   00036500
           R(J)=X2(J)                                                   00036600
  480   CONTINUE                                                        00036700
        NI=N4                                                           00036800
        NA=N4                                                           00036900
        GOTO 1300                                                       00037000
C                                                                       00037100
C       **************************************************              00037200
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****              00037300
C       *****                                         ****              00037400
C       **************************************************              00037500
C                                                                       00037600
 1500   CONTINUE                                                        00037700
        DO 220 J=0, N4, 1                                               00037800
           H1=DS(J)                                                     00037900
           H2=DS(J+1)                                                   00038000
           H3=H1+H2                                                     00038100
           Z2(J)=(Z(J+1)/H2/H3-Z(J)/H1/H2+Z(J-1)/H1/H3)*2.D0            00038200
           R2(J)=(R(J+1)/H2/H3-R(J)/H1/H2+R(J-1)/H1/H3)*2.D0            00038300
           Z1(J)=(Z(J)-Z(J-1))/H1 + Z2(J)*H1/2.D0                       00038400
           R1(J)=(R(J)-R(J-1))/H1 + R2(J)*H1/2.D0                       00038500
           G(J)=Z1(J)*Z1(J)+R1(J)*R1(J)                                 00038600
           WG(J)=DSQRT(G(J))                                            00038700
  220   CONTINUE                                                        00038800
C                                                                       00038900
C       KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS              00039000
C       KM    = 2 * MITTLERE KRUEMMUNG                                  00039100
        DO 230 J=0,N4,1                                                 00039200
           GAMPKT(J)=0.D0                                               00039300
           KAPPA(J)=(-R1(J)*Z2(J)+Z1(J)*R2(J))/WG(J)/G(J)               00039400
  230   CONTINUE                                                        00039500
        DO 231 J=1,N4,1                                                 00039600
           KM(J)=KAPPA(J)-Z1(J)/R(J)/WG(J)                              00039700
  231   CONTINUE                                                        00039800
        KM(0)=KAPPA(0)-Z2(0)/R1(0)/WG(0)                                00039900
C                                                                       00040000
C       **************************************************              00040100
C       ***** BESTIMMEN DER ZWISCHENPUNKTE            ****              00040200
C       *****                                         ****              00040300
C       **************************************************              00040400
C                                                                       00040500
        DO 730 I=0,N4                                                   00040600
           ZPZ(2*I)=Z(I)                                                00040700
           ZPR(2*I)=R(I)                                                00040800
  730   CONTINUE                                                        00040900
C                                                                       00041000
        DO 740 I=0,N4-1                                                 00041100
           H1=DS(I+1)*.5D0                                              00041200
           H2=DS(I+1)-H1                                                00041300
           H8=(Z2(I  )*H1*H1*.5D0 + Z1(I  )*H1 + Z(I  )) * H2           00041400
           H9=(Z2(I+1)*H2*H2*.5D0 - Z1(I+1)*H2 + Z(I+1)) * H1           00041500
           ZPZ(1+2*I)=(H8+H9)/DS(I+1)                                   00041600
           H8=(R2(I  )*H1*H1*.5D0 + R1(I  )*H1 + R(I  )) * H2           00041700
           H9=(R2(I+1)*H2*H2*.5D0 - R1(I+1)*H2 + R(I+1)) * H1           00041800
           ZPR(1+2*I)=(H8+H9)/DS(I+1)                                   00041900
  740   CONTINUE                                                        00042000
        ZPR(-1)=-ZPR(1)                                                 00042100
        ZPZ(-1)= ZPZ(1)                                                 00042200
        ZPR(2*N4+1)= ZPR(2*N4-1)                                        00042300
        ZPZ(2*N4+1)=-ZPZ(2*N4-1)                                        00042400
C                                                                       00042500
        DO 745 K=1,2*N4                                                 00042600
           H1=ZPR(K)-ZPR(K-1)                                           00042700
           H2=ZPZ(K)-ZPZ(K-1)                                           00042800
           ZPDS(K)=DSQRT(H1*H1+H2*H2)                                   00042900
  745   CONTINUE                                                        00043000
        ZPDS(2*N4+1)=ZPDS(2*N4)                                         00043100
        ZPDS(0)=ZPDS(1)                                                 00043200
C                                                                       00043300
        DO 750 J=0,2*N4                                                 00043400
           H1=ZPDS(J)                                                   00043500
           H2=ZPDS(J+1)                                                 00043600
           H3=H1+H2                                                     00043700
           ZPZ2(J)=(ZPZ(J+1)/H2/H3-ZPZ(J)/H1/H2+ZPZ(J-1)/H1/H3)*2.D0    00043800
           ZPR2(J)=(ZPR(J+1)/H2/H3-ZPR(J)/H1/H2+ZPR(J-1)/H1/H3)*2.D0    00043900
           ZPZ1(J)=(ZPZ(J)-ZPZ(J-1))/H1 + ZPZ2(J)*H1/2.D0               00044000
           ZPR1(J)=(ZPR(J)-ZPR(J-1))/H1 + ZPR2(J)*H1/2.D0               00044100
           ZPG(J)=ZPZ1(J)*ZPZ1(J)+ZPR1(J)*ZPR1(J)                       00044200
           ZPWG(J)=DSQRT(ZPG(J))                                        00044300
  750   CONTINUE                                                        00044400
C                                                                       00044500
        DO 239 I=N4*2+1,2*N                                             00044600
          K=2*N - I                                                     00044700
          ZPZ(I)=-ZPZ(K)                                                00044800
          ZPR(I)= ZPR(K)                                                00044900
          ZPZ1(I)= ZPZ1(K)                                              00045000
          ZPR1(I)=-ZPR1(K)                                              00045100
          ZPG(I)=ZPG(K)                                                 00045200
          ZPWG(I)=ZPWG(K)                                               00045300
          ZPDS(I)=ZPDS(K+1)                                             00045400
  239   CONTINUE                                                        00045500
        ZPDS(N+N+1)=ZPDS(N+N)                                           00045600
        ZPZ1(0)   =0.D0                                                 00045700
        ZPR1(2*N4)=0.D0                                                 00045800
        ZPZ1(4*N4)=0.D0                                                 00045900
C                                                                       00046000
C       DO 999 I=0,4*N4+1                                               00046100
C         WRITE( 6,111) I, ZPZ1(I),ZPR1(I),ZPWG(I),ZPDS(I)              00046200
C 999   CONTINUE                                                        00046300
C                                                                       00046400
C       **************************************************              00046500
C       *****   G (R,R') := EXP(-K * |R-R'|) / |R-R'| ****              00046600
C       *****    K                                    ****              00046700
C       *****                                         ****              00046800
C       ***** (LAPLACE-OPERATOR  - K*K) G (R,R')      ****              00046900
C       *****                            K            ****              00047000
C       *****  =  - 4 * PI * DELTA( R-R')  , K>=0     ****              00047100
C       *****                                         ****              00047200
C       *****                                         ****              00047300
C       ***** BILDEN DER OBERFLAECHENINTEGRALE :      ****              00047400
C       *****                                         ****              00047500
C       ***** INT1 BZGL.        G (R,R')              ****              00047600
C       *****                    0                    ****              00047700
C       *****                                         ****              00047800
C       ***** INT2 BZGL.  G (R,R')  -  G (R,R')       ****              00047900
C       *****              0            1             ****              00048000
C       *****                                         ****              00048100
C       ***** INT3 BZGL.   (  GRAD' G (R,R')  ) * N'  ****              00048200
C       *****                        0                ****              00048300
C       *****                                         ****              00048400
C       ***** INT4 BZGL. GRAD'(G (R,R')-G (R,R')) * N'****              00048500
C       *****                   0        1            ****              00048600
C       **************************************************              00048700
C                                                                       00048800
C       >>>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKTE)              00048900
C       >>>>>       ABST = ] R - R' ]                                   00049000
C       >>>>>       DPN  = ( R - R' ) * N'                              00049100
        DO 244 I=0,N4                                                   00049200
          DO 242 J=1,N                                                  00049300
            H5= ZPWG(2*J-1)                                             00049400
            H7=  R(I)-ZPR(2*J-1)                                        00049500
            H8=  Z(I)-ZPZ(2*J-1)                                        00049600
C                                                                       00049700
            B1= R(I)*ZPZ1(2*J-1)                                        00049800
            A1= H8*ZPR1(2*J-1) -H7*ZPZ1(2*J-1) +B1                      00049900
            B2=  2.D0 * R(I) * ZPR(2*J-1)                               00050000
            A2= H7*H7 + H8*H8 + B2                                      00050100
C                                                                       00050200
            INT2(I,J)=0.D0                                              00050300
            INT4(I,J)=0.D0                                              00050400
C                                                                       00050500
            DO 240 M=0,NPHI-1                                           00050600
              H1=M                                                      00050700
              H1= DCOS( (H1+ .5D0) * DELPHI )                           00050800
              ABST = DSQRT( A2 - B2 * H1 )                              00050900
              DPN = A1 - B1 * H1                                        00051000
              INT2(I,J)=INT2(I,J) + (1.D0 - DEXP(-ABST)) / ABST         00051100
              INT4(I,J)=INT4(I,J) +                                     00051200
     *          (1.D0 - DEXP(-ABST)*(ABST+1.D0) ) * DPN /ABST/ABST/ABST 00051300
  240       CONTINUE                                                    00051400
C                                                                       00051500
            INT2(I,J)=INT2(I,J) * DELPHI * ZPR(2*J-1) *2.D0 *H5 /4.D0/PI00051600
            INT4(I,J)=INT4(I,J) * DELPHI * ZPR(2*J-1) *2.D0 /4.D0/PI    00051700
C           WRITE ( 6, 112) I,J,INT2(I,J),INT4(I,J)                     00051800
C                                                                       00051900
C           >>>> BERECHNEN DER INTEGRALE INT1 UND INT3 MIT DEN          00052000
C           >>>> (VOLLSTAENDIGEN) ELLIPTISCHEN INTEGRALEN 1. UND 2. ART 00052100
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00052200
            CALL DCELI1(H1,ELI1,ICON)                                   00052300
            CALL DCELI2(H1,ELI2,ICON)                                   00052400
C                                                                       00052500
            H1 = DSQRT( A2 + B2 )                                       00052600
            INT1(I,J)=2.D0 * ELI1 / H1                                  00052700
C           >>>> H2= B1/B2 ( WIRD BENOETIGT FUER R(I)=0.D0 )            00052800
            H2= ZPZ1(2*J-1) / 2.D0 / ZPR(2*J-1)                         00052900
            INT3(I,J)= ( ELI1*H2 + ELI2 * (A1-A2*H2) / (A2-B2) )*2.D0/H100053000
C                                                                       00053100
            INT1(I,J)=INT1(I,J)  * ZPR(2*J-1) *2.D0 *H5 /4.D0/PI        00053200
            INT3(I,J)=INT3(I,J)  * ZPR(2*J-1) *2.D0 /4.D0/PI            00053300
C                                                                       00053400
C           WRITE( 6,112) I,J,INT1(I,J)/B0,INT3(I,J)*(-2.D0)            00053500
  242     CONTINUE                                                      00053600
  244   CONTINUE                                                        00053700
C                                                                       00053800
C       >>>>> INTEGRATION IN PHI-RICHTUNG (UEBER DEN BEZUGSPUNKT)       00053900
C       >>>>> NUR FUER INT2 UND INT4 , DA KEINE SINGULARITAETEN         00054000
        DO 248 I=1,N4                                                   00054100
            H5= ZPWG(2*I)                                               00054200
C                                                                       00054300
            B1= R(I)*ZPZ1(2*I)                                          00054400
            A1= B1                                                      00054500
            B2=  2.D0 * R(I) * ZPR(2*I)                                 00054600
            A2= B2                                                      00054700
C                                                                       00054800
            INT2S(I) =0.D0                                              00054900
            INT4S(I) =0.D0                                              00055000
C                                                                       00055100
            DO 247 M=0,NPHI-1                                           00055200
              H1=M                                                      00055300
              H1= DCOS( (H1+ .5D0) * DELPHI )                           00055400
              ABST = DSQRT( A2 - B2 * H1 )                              00055500
              DPN = A1 - B1 * H1                                        00055600
              INT2S(I) =INT2S(I)  + (1.D0 - DEXP(-ABST)) / ABST         00055700
              INT4S(I) =INT4S(I)  +                                     00055800
     *          (1.D0 - DEXP(-ABST)*(ABST+1.D0) ) * DPN /ABST/ABST/ABST 00055900
  247       CONTINUE                                                    00056000
C                                                                       00056100
            INT2S(I) =INT2S(I)  * DELPHI * ZPR(2*I) *2.D0 *H5 /4.D0/PI  00056200
            INT4S(I) =INT4S(I)  * DELPHI * ZPR(2*I) *2.D0 /4.D0/PI      00056300
C           WRITE ( 6, 111) I,INT2S(I) ,INT4S(I)                        00056400
  248   CONTINUE                                                        00056500
C                                                                       00056600
C       >>>> FUER INTEGRATION VON INT1 UEBER DER SINGULARITAET:         00056700
C       >>>>       VOM URSPRUENGLICHEN INTEGRAL WIRD INTEGRAL           00056800
C       >>>>       UEBER KEGEL ( R'=R(S=0) + R1(S=0) * S  ,             00056900
C       >>>>       Z'=Z(S=0) + Z1(S=0) * S ) SUBTRAHIERT UND            00057000
C       >>>>       INTEGRAL UEBER KEGEL GESONDERT AUSGERECHNET.         00057100
C                                                                       00057200
C       >>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKT RECHTS VON      00057300
C       >>>> DER SINGULARITAET) (VOLLST. ELLIPT. INTGRAL 1. ART)        00057400
        DO 292 I=0,N4                                                   00057500
            J=I*2                                                       00057600
            H5= R(I) + ZPR1(J) * ZPDS(J+1)                              00057700
            B2=2.D0*R(I)* H5                                            00057800
            A2=ZPWG(J)*ZPWG(J)*ZPDS(J+1)*ZPDS(J+1) + B2                 00057900
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00058000
            CALL DCELI1(H1,ELI1,ICON)                                   00058100
            ISR1KE(I)=2.D0 * ELI1 / DSQRT( A2 + B2 )                    00058200
            ISR1KE(I)=ISR1KE(I) * H5 * ZPWG(J) * 2.D0  /4.D0 /PI        00058300
C           WRITE( 6,111) I,ISR1KE(I), INT1(I,I+1)                      00058400
  292   CONTINUE                                                        00058500
C                                                                       00058600
C       >>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKT LINKS  VON      00058700
C       >>>> DER SINGULARITAET) (VOLLST. ELLIPT. INTGRAL 1. ART)        00058800
        DO 294 I=1,N4                                                   00058900
            J=I*2                                                       00059000
            H5= R(I) - ZPR1(J) * ZPDS(J)                                00059100
            B2=2.D0*R(I)* H5                                            00059200
            A2=ZPWG(J)*ZPWG(J)*ZPDS(J)*ZPDS(J) + B2                     00059300
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00059400
            CALL DCELI1(H1,ELI1,ICON)                                   00059500
            ISL1KE(I)=2.D0 * ELI1 / DSQRT( A2 + B2 )                    00059600
            ISL1KE(I)=ISL1KE(I) * H5 * ZPWG(J) * 2.D0  /4.D0 /PI        00059700
C           WRITE( 6,111) I,ISL1KE(I), INT1(I,I  )                      00059800
  294   CONTINUE                                                        00059900
C                                                                       00060000
C                                                                       00060100
C       >>>>> INTEGRATION IN PHI-RICHTUNG UND IN S-RICHTUNG UEBER       00060200
C       >>>>> DEN BEZUGSPUNKT                                           00060300
        DO 296 I=1,N4                                                   00060400
            J=I*2                                                       00060500
            H2=ZPDS(J+1)                                                00060600
            H1=ZPDS(J  )                                                00060700
C                                                                       00060800
            IS1KE(I)=0.D0                                               00060900
C                                                                       00061000
            DO 298 M=0,NPHI-1                                           00061100
              H3=M                                                      00061200
              H4= DCOS( (H3+ .5D0) * DELPHI * .5D0)                     00061300
              H5= DSIN( (H3+ .5D0) * DELPHI * .5D0)                     00061400
              A2=4.D0*R(I)*R(I)*H5*H5                                   00061500
              B2=4.D0*R(I)*ZPR1(J)*H5*H5                                00061600
              B1=DSQRT(A2 + B2 * H2 + ZPG(J) * H2*H2)                   00061700
              QU=DLOG( ( 2.D0*ZPWG(J)* B1 + 2.D0*ZPG(J)*H2 + B2)        00061800
     *                   /  ( 2.D0 *DSQRT(A2) * ZPWG(J) + B2) )         00061900
              A3=(1.D0 - H4) * (R(I) - B2 * ZPR1(J) * .5D0 /ZPG(J) )*QU 00062000
              A4= H4 * B2 * ZPR1(J) / ZPG(J) * QU                       00062100
              A1= ZPR1(J) / ZPWG(J)* ( B1 - DSQRT(A2) )                 00062200
              A5= ( DSQRT(A2)*ZPWG(J) + B2 )/(2.D0*DSQRT(A2)*ZPWG(J)+B2)00062300
              A6= ( ZPWG(J)*B1 + B2 - ZPG(J)*ZPWG(J)*H2*H2/B1 )         00062400
     *                   / ( 2.D0 * ZPWG(J) * B1 + B2 + 2.D0 *ZPG(J)*H2)00062500
              A5= 2.D0 * H4 * ( R(I) - B2 *ZPR1(J) *.5D0 /ZPG(J) ) * A5 00062600
              A6=-2.D0 * H4 * ( R(I) - B2 *ZPR1(J) *.5D0 /ZPG(J) ) * A6 00062700
              IS1KE(I)=IS1KE(I) + A1 + A3 + A4 + A5 + A6                00062800
C                                                                       00062900
              B1=DSQRT(A2 - B2 * H1 + ZPG(J) * H1*H1)                   00063000
              QU=DLOG( ( 2.D0*ZPWG(J)* B1 + 2.D0*ZPG(J)*H1 - B2)        00063100
     *                   /  ( 2.D0 *DSQRT(A2) * ZPWG(J) - B2) )         00063200
              A3=(1.D0 - H4) * (R(I) - B2 * ZPR1(J) * .5D0 / ZPG(J) )*QU00063300
              A4= H4 * B2 * ZPR1(J) / ZPG(J) * QU                       00063400
              A1= - ZPR1(J) / ZPWG(J)* ( B1 - DSQRT(A2) )               00063500
              A5= ( DSQRT(A2)*ZPWG(J) - B2 )/(2.D0*DSQRT(A2)*ZPWG(J)-B2)00063600
              A6= ( ZPWG(J)*B1 - B2 - ZPG(J)*ZPWG(J)*H1*H1/B1 )         00063700
     *                   / ( 2.D0 * ZPWG(J) * B1 - B2 + 2.D0 *ZPG(J)*H1)00063800
              A5= 2.D0 * H4 * ( R(I) - B2 *ZPR1(J) *.5D0 /ZPG(J) ) * A5 00063900
              A6=-2.D0 * H4 * ( R(I) - B2 *ZPR1(J) *.5D0 /ZPG(J) ) * A6 00064000
C                                                                       00064100
              IS1KE(I)=IS1KE(I) + A1 + A3 + A4 + A5 + A6                00064200
  298       CONTINUE                                                    00064300
C                                                                       00064400
            IS1KE(I)=IS1KE(I) * DELPHI  *2.D0                           00064500
              A2=4.D0*R(I)*R(I)                                         00064600
              B2=4.D0*R(I)*ZPR1(J)                                      00064700
              B1=DSQRT(A2 + B2 * H2 + ZPG(J) * H2*H2)                   00064800
              QU=DLOG( ( 2.D0*ZPWG(J)* B1 + 2.D0*ZPG(J)*H2 + B2)        00064900
     *                   /  ( 2.D0 *DSQRT(A2) * ZPWG(J) + B2) )         00065000
           IS1KE(I)= IS1KE(I) + 4.D0*(R(I)-B2*ZPR1(J)*.5D0/ZPG(J))*QU   00065100
              B1=DSQRT(A2 - B2 * H1 + ZPG(J) * H1*H1)                   00065200
              QU=DLOG( ( 2.D0*ZPWG(J)* B1 + 2.D0*ZPG(J)*H1 - B2)        00065300
     *                   /  ( 2.D0 *DSQRT(A2) * ZPWG(J) - B2) )         00065400
           IS1KE(I)= IS1KE(I) + 4.D0*(R(I)-B2*ZPR1(J)*.5D0/ZPG(J))*QU   00065500
           IS1KE(I)= IS1KE(I) / 4.D0 / PI                               00065600
  296   CONTINUE                                                        00065700
        IS1KE(0)= ZPWG(0)  * ZPDS(1) / 2.D0                             00065800
C                                                                       00065900
C       DO 299 I=0,N4                                                   00066000
C          WRITE (6,111) I, ISL1KE(I),ISR1KE(I),IS1KE(I)                00066100
C          WRITE (6,111) I, INT1(I,I),INT1(I,I+1)                       00066200
C 299   CONTINUE                                                        00066300
C                                                                       00066400
C       **************************************************              00066500
C       ***** AUFSTELLEN DES LIN. GLEICHUNGSSYSTEMS   ****              00066600
C       *****     FUER RHO UND RHON = GRAD(RHO) * N   ****              00066700
C       *****              AM RANDE                   ****              00066800
C       **************************************************              00066900
C                                                                       00067000
        DO 250 J=1,N4-1                                                 00067100
          H2=( ZPDS(2*J+1) + ZPDS(2*J) ) / 2.D0                         00067200
          DO 252 I=0,N4                                                 00067300
            K=N-J                                                       00067400
            ARHO(    I+1,    J+1)=  (INT1(I,J+1)+INT1(I,J))*H2          00067500
     *                           +  (INT1(I,K+1)+INT1(I,K))*H2          00067600
            ARHO(    I+1,N4+1+J+1)=  (INT3(I,J+1)+INT3(I,J))*H2         00067700
     *                           +  (INT3(I,K+1)+INT3(I,K))*H2          00067800
            ARHO(N4+1+I+1,    J+1)=  (INT2(I,J+1)+INT2(I,J))*H2         00067900
     *                           +  (INT2(I,K+1)+INT2(I,K))*H2          00068000
            ARHO(N4+1+I+1,N4+1+J+1)=  (INT4(I,J+1)+INT4(I,J))*H2        00068100
     *                           +  (INT4(I,K+1)+INT4(I,K))*H2          00068200
  252     CONTINUE                                                      00068300
  250   CONTINUE                                                        00068400
C                                                                       00068500
        J=0                                                             00068600
          K=N-J                                                         00068700
          H2= ZPDS(1) /2.D0                                             00068800
          DO 254 I=1,N4                                                 00068900
            ARHO(    I+1,    J+1)=      (INT1(I,J+1)+0.D0     )*H2      00069000
     *                           +      ( 0.D0      +INT1(I,K))*H2      00069100
            ARHO(    I+1,N4+1+J+1)=      (INT3(I,J+1)+0.D0     )*H2     00069200
     *                           +      ( 0.D0      +INT3(I,K))*H2      00069300
            ARHO(N4+1+I+1,    J+1)=      (INT2(I,J+1)+0.D0     )*H2     00069400
     *                           +      ( 0.D0      +INT2(I,K))*H2      00069500
            ARHO(N4+1+I+1,N4+1+J+1)=      (INT4(I,J+1)+0.D0     )*H2    00069600
     *                           +      ( 0.D0      +INT4(I,K))*H2      00069700
  254     CONTINUE                                                      00069800
C                                                                       00069900
        J=N4                                                            00070000
          K=N-J                                                         00070100
          H2= ZPDS(N4+N4)                                               00070200
          DO 256 I=0,N4-1                                               00070300
            ARHO(    I+1,    J+1)=  (INT1(I,J+1)+INT1(I,J))*H2          00070400
            ARHO(    I+1,N4+1+J+1)=  (INT3(I,J+1)+INT3(I,J))*H2         00070500
            ARHO(N4+1+I+1,    J+1)=  (INT2(I,J+1)+INT2(I,J))*H2         00070600
            ARHO(N4+1+I+1,N4+1+J+1)=  (INT4(I,J+1)+INT4(I,J))*H2        00070700
  256     CONTINUE                                                      00070800
C                                                                       00070900
C       >>>>>   I=J    (FUER INT2 UND INT4)                             00071000
C                                                                       00071100
        DO 669 J=1,N4-1                                                 00071200
            I=J                                                         00071300
            K=N-J                                                       00071400
            H1= ZPDS(2*J+1)/2.D0                                        00071500
            H2=( ZPDS(2*J+1) + ZPDS(2*J) ) / 2.D0                       00071600
            H3= ZPDS(2*J)/2.D0                                          00071700
            ARHO(N4+1+I+1,    J+1)=  INT2(I,J+1)*H1+INT2(I,J)*H3        00071800
     *                               + INT2S(I)*H2                      00071900
     *                            + (INT2(I,K+1)+INT2(I,K))*H2          00072000
            ARHO(N4+1+I+1,    J+1)=  INT2(I,J+1)*H2+INT2(I,J)*H2        00072100
     *                            + (INT2(I,K+1)+INT2(I,K))*H2          00072200
            ARHO(N4+1+I+1,N4+1+J+1)= INT4(I,J+1)*H1+INT4(I,J)*H3        00072300
     *                               + INT4S(I)*H2                      00072400
     *                            + (INT4(I,K+1)+INT4(I,K))*H2          00072500
            ARHO(N4+1+I+1,N4+1+J+1)= INT4(I,J+1)*H2+INT4(I,J)*H2        00072600
     *                            + (INT4(I,K+1)+INT4(I,K))*H2          00072700
  669   CONTINUE                                                        00072800
C                                                                       00072900
        J=N4                                                            00073000
        I=J                                                             00073100
        H2=  ZPDS(N4+N4)                                                00073200
        ARHO(N4+1+I+1,    J+1)=  INT2(I,J)*H2+INT2S(I)*H2               00073300
        ARHO(N4+1+I+1,    J+1)=  INT2(I,J)*(H2+H2)                      00073400
        ARHO(N4+1+I+1,N4+1+J+1)= INT4(I,J)*H2+INT4S(I)*H2               00073500
        ARHO(N4+1+I+1,N4+1+J+1)= INT4(I,J)*(H2+H2)                      00073600
C                                                                       00073700
        I=0                                                             00073800
        J=0                                                             00073900
            K=N-J                                                       00074000
            H2=  ZPDS(1) / 2.D0                                         00074100
            ARHO(N4+1+I+1,     J+1)=      (INT2(I,J+1)+0.D0     )*H2    00074200
     *                           +      ( 0.D0      +INT2(I,K))*H2      00074300
            ARHO(N4+1+I+1,N4+1+J+1)=      (INT4(I,J+1)+0.D0     )*H2    00074400
     *                           +      ( 0.D0      +INT4(I,K))*H2      00074500
C                                                                       00074600
C       >>>>  I=J   (FUER INT1)                                         00074700
C                                                                       00074800
        DO 673 J=1,N4-1                                                 00074900
          I=J                                                           00075000
          K=N-J                                                         00075100
          H2=ZPDS(2*J+1)                                                00075200
          H1=ZPDS(2*J)                                                  00075300
          H3= (INT1(I,J+1) - ISR1KE(I)) * H2 * .5D0                     00075400
     *      + (INT1(I,J  ) - ISL1KE(I)) * H1 * .5D0                     00075500
     *      + (INT1(I,K+1) + INT1(I,K)) * (H1+H2) *.5D0                 00075600
          ARHO(    I+1,    J+1)= IS1KE(I) + H3                          00075700
  673   CONTINUE                                                        00075800
C                                                                       00075900
        J=N4                                                            00076000
          I=J                                                           00076100
          K=N-J                                                         00076200
          H2=ZPDS(2*J)                                                  00076300
          H1=ZPDS(2*J)                                                  00076400
          H3= (INT1(I,J+1) - ISR1KE(I)) * H2 * .5D0                     00076500
     *      + (INT1(I,J  ) - ISL1KE(I)) * H1 * .5D0                     00076600
          ARHO(    I+1,    J+1)= IS1KE(I) + H3                          00076700
C                                                                       00076800
        J=0                                                             00076900
          I=J                                                           00077000
          K=N-J                                                         00077100
          H2=ZPDS(1)                                                    00077200
          H3= (INT1(I,J+1) - ISR1KE(I)) * H2 * .5D0                     00077300
     *      + (0.D0        + INT1(I,K)) * H2 * .5D0                     00077400
          ARHO(    I+1,    J+1)= IS1KE(I) + H3                          00077500
C                                                                       00077600
C                                                                       00077700
C       >>>>  I=J   (FUER INT3)                                         00077800
C                                                                       00077900
        DO 677 I=0,N4                                                   00078000
          H3=0.D0                                                       00078100
          DO 678 J=0,N4                                                 00078200
             IF (J .NE. I) THEN                                         00078300
                H3=H3+ARHO(    I+1,N4+1+J+1)                            00078400
             ENDIF                                                      00078500
  678     CONTINUE                                                      00078600
          ARHO(   I+1,N4+1+I+1)= -.5D0 -H3                              00078700
  677   CONTINUE                                                        00078800
C                                                                       00078900
C                                                                       00079000
C                                                                       00079100
C       >>>>>>>> AUSRECHEN DER KONTROLLGROESSEN !!!!!!                  00079200
C       >>>>>>>> INT3CL(I) = -1/2        (FUER ALLE I UND ALLE FORMEN)  00079300
C       >>>>>>>> UND FUER KUGEL MIT RADIUS R MUSS GELTEN:               00079400
C       >>>>>>>> INT1CL(I) =   R         (FUER ALLE I)                  00079500
C       >>>>>>>> INT2CL(I) = INT2CL(J)   (FUER ALLE I,J)                00079600
C       >>>>>>>> INN4CL(I) = INT4CL(J)   (FUER ALLE I,J)                00079700
        DO 258 I=0,N4                                                   00079800
          INT1CL(I)=0.D0                                                00079900
          INT2CL(I)=0.D0                                                00080000
          INT3CL(I)=0.D0                                                00080100
          INT4CL(I)=0.D0                                                00080200
          DO 259 J=0,N4                                                 00080300
            INT1CL(I)=INT1CL(I)+ARHO(     I+1,     J+1)                 00080400
            INT2CL(I)=INT2CL(I)+ARHO(N4+1+I+1,     J+1)                 00080500
            INT3CL(I)=INT3CL(I)+ARHO(     I+1,N4+1+J+1)                 00080600
            INT4CL(I)=INT4CL(I)+ARHO(N4+1+I+1,N4+1+J+1)                 00080700
C           WRITE (6,112) I,J,ARHO(N4+1+I+1,     J+1)                   00080800
  259     CONTINUE                                                      00080900
  258   CONTINUE                                                        00081000
  111   FORMAT(1X,I2,4(2X,D14.7))                                       00081100
  112   FORMAT(1X,I2,2X,I2,4(2X,D14.7))                                 00081200
C                                                                       00081300
C       >>>>>>> KORREKTURTEME NUR FUER KUGEL UND T9 = -1                00081400
        IF (T9. EQ. -1) THEN                                            00081500
        DO 777 I=0,N4                                                   00081600
          KORR1(I) = INT1KG/INT1CL(I)                                   00081700
          KORR2(I) = INT2KG/INT2CL(I)                                   00081800
          KORR3(I) = INT3KG/INT3CL(I)                                   00081900
          KORR4(I) = INT4KG/INT4CL(I)                                   00082000
          IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                          00082100
            WRITE ( 6,111) I,KORR1(I),KORR2(I),KORR3(I),KORR4(I)        00082200
          ENDIF                                                         00082300
  777   CONTINUE                                                        00082400
        GOTO 1111                                                       00082500
        ENDIF                                                           00082600
C                                                                       00082700
C                                                                       00082800
        PROB='J'                                                        00082900
        DO 260 I=0,N4                                                   00083000
          DO 260 J=0,N4                                                 00083100
            ARHO(     I+1,     J+1)= ARHO(     I+1,     J+1) * KORR1(I) 00083200
            ARHO(     I+1,N4+1+J+1)= ARHO(     I+1,N4+1+J+1) * KORR3(I) 00083300
            ARHO(N4+1+I+1,     J+1)= ARHO(N4+1+I+1,     J+1) * KORR2(I) 00083400
            ARHO(N4+1+I+1,N4+1+J+1)= ARHO(N4+1+I+1,N4+1+J+1) * KORR4(I) 00083500
C           >>>>>> MERKEN DER FUER DAS GESCHW.POT. BENOETIGTEN WERTE    00083600
            AGPOT(     I+1,     J+1)=   ARHO(     I+1,     J+1)         00083700
            AGPOT(     I+1,N4+1+J+1)=   ARHO(     I+1,N4+1+J+1)         00083800
C           >>>>>> FAKTOREN UND VORZEICHEN FUER DIE RHO-GLEICHUNGEN     00083900
            H1 = ARHO(     I+1,     J+1)                                00084000
            H3 = ARHO(     I+1,N4+1+J+1)                                00084100
            H2 = ARHO(N4+1+I+1,     J+1)                                00084200
            H4 = ARHO(N4+1+I+1,N4+1+J+1)                                00084300
            IF (PROB .NE. 'J') THEN                                     00084400
              ARHO(     I+1,     J+1)=-DIFK * H1                        00084500
              ARHO(     I+1,N4+1+J+1)= H3                               00084600
              ARHO(N4+1+I+1,     J+1)= -H2 + (1.D0 - DIFK) * H1         00084700
              ARHO(N4+1+I+1,N4+1+J+1)= H4                               00084800
            ENDIF                                                       00084900
  260   CONTINUE                                                        00085000
C                                                                       00085100
        IF (PROB .EQ. 'J') THEN                                         00085200
C                                                                       00085300
        DO 1150 I=0,N4                                                  00085400
          DO 1152 J=0,N4                                                00085500
            TH1=DATAN2(ZPR(2*I),ZPZ(2*I))                               00085600
            TH2=DATAN2(ZPR(2*J-1),ZPZ(2*J-1))                           00085700
            TH3=DATAN2(ZPR(2*J+1),ZPZ(2*J+1))                           00085800
            IF (J .EQ. 0) TH2=0.D0                                      00085900
C           WRITE(6,112) I,J,TH1,TH2,TH3                                00086000
C                                                                       00086100
            H9=0.D0                                                     00086200
            H8=0.D0                                                     00086300
            H7=0.D0                                                     00086400
            H5=0.D0                                                     00086500
            DELTH= (TH3 -TH2)/ 20.D0                                    00086600
            DO 1154 L=0, 20-1                                           00086700
              TH4=L                                                     00086800
              TH4=TH2 + ( TH4+.5D0) * DELTH                             00086900
              DO 1156 M=0,NPHI-1                                        00087000
                H3=M                                                    00087100
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00087200
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00087300
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00087400
                H9= H9 + DSIN(TH4)/ABST                                 00087500
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))          00087600
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST           00087700
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00087800
     *                      * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) )       00087900
 1156         CONTINUE                                                  00088000
              TH4=PI-TH4                                                00088100
              DO 1158 M=0,NPHI-1                                        00088200
                H3=M                                                    00088300
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00088400
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00088500
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00088600
                H9= H9 + DSIN(TH4)/ABST                                 00088700
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))          00088800
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST           00088900
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00089000
     *                      * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) )       00089100
 1158         CONTINUE                                                  00089200
 1154       CONTINUE                                                    00089300
            H9=H9     *B0*B0*DELPHI*DELTH  / 2.D0 / PI                  00089400
            H8=H8     *B0*B0*DELPHI*DELTH  / 2.D0 / PI                  00089500
            H7=H7     *B0*B0*DELPHI*DELTH  / 2.D0 / PI                  00089600
            H5=H5     *B0*B0*DELPHI*DELTH  / 2.D0 / PI                  00089700
            CRHO(     I+1,     J+1)=H9                                  00089800
            CRHO(N4+1+I+1,     J+1)=H8                                  00089900
            CRHO(     I+1,N4+1+J+1)=H7                                  00090000
            CRHO(N4+1+I+1,N4+1+J+1)=H5                                  00090100
            IF (J .EQ. N4) CRHO(     I+1,     J+1) = H9*.5D0            00090200
            IF (J .EQ. N4) CRHO(N4+1+I+1,     J+1) = H8*.5D0            00090300
            IF (J .EQ. N4) CRHO(     I+1,N4+1+J+1) = H7*.5D0            00090400
            IF (J .EQ. N4) CRHO(N4+1+I+1,N4+1+J+1) = H5*.5D0            00090500
C           WRITE (6,112) I,J, ARHO(     I+1,J+1), CRHO(     I+1,J+1)   00090600
            WRITE (6,112) I,J, ARHO(N4+1+I+1,J+1), CRHO(N4+1+I+1,J+1)   00090700
C           WRITE (6,112) I,J, ARHO(I+1,N4+1+J+1), CRHO(I+1,N4+1+J+1)   00090800
C           L=N4+2                                                      00090900
C           WRITE (6,112) I,J, ARHO( I + L , J+L), CRHO( I + L , J+L)   00091000
 1152     CONTINUE                                                      00091100
 1150   CONTINUE                                                        00091200
C                                                                       00091300
C                                                                       00091400
C                                                                       00091500
C                                                                       00091600
C                                                                       00091700
          DO 1201 I=0,N4                                                00091800
            RHO(I)=-INT1KG                                              00091900
            BRHO(I+1)= RHO(I) * (.5D0 - INT3KG)                         00092000
            DRHO(I+1)=BRHO(I+1)                                         00092100
          DO 1201 J=0,N4                                                00092200
            ARHO(     I+1,     J+1)=-ARHO(    I+1,    J+1)              00092300
            CRHO(     I+1,     J+1)=-CRHO(    I+1,    J+1)              00092400
 1201     CONTINUE                                                      00092500
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00092600
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00092700
          WRITE(6,*) 'KONTROLLE FUER INT1:'                             00092800
          DO 1202 I=0,N4                                                00092900
            RHON(I)=BRHO(I+1)                                           00093000
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00093100
 1202     CONTINUE                                                      00093200
          DO 1203 I=0,N4                                                00093300
            RHO(I)=-INT2KG/(1.D0-INT4KG)                                00093400
            BRHO(I+1)= RHO(I) * (1.D0 - INT4KG)                         00093500
            DRHO(I+1)=BRHO(I+1)                                         00093600
          DO 1203 J=0,N4                                                00093700
            ARHO(     I+1,     J+1)=-ARHO(N4+1+I+1,     J+1)            00093800
            CRHO(     I+1,     J+1)=-CRHO(N4+1+I+1,    J+1)             00093900
 1203     CONTINUE                                                      00094000
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00094100
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00094200
          WRITE(6,*) 'KONTROLLE FUER INT2:'                             00094300
          DO 1204 I=0,N4                                                00094400
            RHON(I)=BRHO(I+1)                                           00094500
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00094600
 1204     CONTINUE                                                      00094700
          DO 1205 I=0,N4                                                00094800
            RHON(I)=-1.D0/INT1KG                                        00094900
            BRHO(I+1)= RHON(I)* INT1KG                                  00095000
            DRHO(I+1)= BRHO(I+1)                                        00095100
          DO 1205 J=0,N4                                                00095200
            ARHO(     I+1,     J+1)= ARHO(     I+1,N4+1+J+1)            00095300
            CRHO(     I+1,     J+1)= CRHO(     I+1,N4+1+J+1)            00095400
            IF (I .EQ. J)   ARHO(I+1,I+1)=ARHO(I+1,I+1) - .5D0          00095500
            IF (I .EQ. J)   CRHO(I+1,I+1)=CRHO(I+1,I+1) - .5D0          00095600
 1205     CONTINUE                                                      00095700
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00095800
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00095900
          WRITE(6,*) 'KONTROLLE FUER INT3:'                             00096000
          DO 1206 I=0,N4                                                00096100
            RHO(I)=BRHO(I+1)                                            00096200
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00096300
 1206     CONTINUE                                                      00096400
          DO 1207 I=0,N4                                                00096500
            RHON(I)=-(1.D0-INT4KG)/INT2KG                               00096600
            BRHO(I+1)= RHON(I) *INT2KG                                  00096700
            DRHO(I+1)= BRHO(I+1)                                        00096800
          DO 1207 J=0,N4                                                00096900
            ARHO(     I+1,     J+1)= ARHO(N4+1+I+1,N4+1+J+1)            00097000
            CRHO(     I+1,     J+1)= CRHO(N4+1+I+1,N4+1+J+1)            00097100
            IF (I .EQ. J)   ARHO(I+1,I+1)=ARHO(I+1,I+1) - 1.D0          00097200
            IF (I .EQ. J)   CRHO(I+1,I+1)=CRHO(I+1,I+1) - 1.D0          00097300
 1207     CONTINUE                                                      00097400
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00097500
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00097600
          WRITE(6,*) 'KONTROLLE FUER INT4:'                             00097700
          DO 1208 I=0,N4                                                00097800
            RHO(I)=BRHO(I+1)                                            00097900
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00098000
 1208     CONTINUE                                                      00098100
        ENDIF                                                           00098200
C                                                                       00098300
        DO 262 I=0,N4                                                   00098400
          ARHO(     I+1,N4+1+I+1)=ARHO(     I+1,N4+1+I+1) - 0.5D0       00098500
          ARHO(N4+1+I+1,N4+1+I+1)=ARHO(N4+1+I+1,N4+1+I+1) - 1.0D0       00098600
C          >>>>>> BESTIMMEN DER INHOMOGENITAET IM LIN. GLEICH.SYST.     00098700
          BRHO(     I+1)=-TAU                                           00098800
          BRHO(N4+1+I+1)=-TAU                                           00098900
  262   CONTINUE                                                        00099000
C                                                                       00099100
        CALL DLAX(ARHO,82,N4+N4+2,BRHO,EPSZ,ISW,IS,VW,IP,ICON)          00099200
C                                                                       00099300
C       WRITE( 6,*) 'ICON = ',ICON                                      00099400
        DO 264 I=0,N4                                                   00099500
C          >>>> RHO(I) = NAEHRSTOFFKONZENTRATION AM RANDE               00099600
C          >>>> RHON(I) = GRADIENT DER NAEHRST.KONZ. IN NORMALENRICHT.  00099700
           RHO(I) =BRHO(N4+1+I+1)                                       00099800
           RHON(I)=BRHO(     I+1)                                       00099900
  264   CONTINUE                                                        00100000
C                                                                       00100100
C       **************************************************              00100200
C       ***** AUFSTELLEN DES LIN. GLEICHUNGSSYSTEMS   ****              00100300
C       *****     FUER GESCHWIND. IN NORMALENRICHT.   ****              00100400
C       *****              AM RANDE                   ****              00100500
C       **************************************************              00100600
C                                                                       00100700
C                                                                       00100800
C       >>>>> HIER: GPOT = GESCHW.POTENTIAL - RHO + 1/6 * (ORTSV.)**2   00100900
        DO 266 I=0,N4                                                   00101000
          GPOT(I)=-EPS*KM(I)-RHO(I)+(R(I)*R(I)+Z(I)*Z(I))/6.D0          00101100
  266   CONTINUE                                                        00101200
C                                                                       00101300
        DO 267 I=0,N4                                                   00101400
          BGPOT(I+1) = GPOT(I)/2.D0                                     00101500
          DO 267 J=0,N4                                                 00101600
C          >>>>>> BESTIMMEN DER INHOMOGENITAET IM LIN. GLEICH.SYST.     00101700
            BGPOT(I+1) = BGPOT(I+1) + AGPOT(    I+1,N4+1+J+1)* GPOT(J)  00101800
  267     CONTINUE                                                      00101900
C                                                                       00102000
        CALL DLAX(AGPOT,82,N4+1,BGPOT,EPSZ,ISW,IS,VW,IP,ICON)           00102100
C                                                                       00102200
C       WRITE( 6,*) 'ICON = ',ICON                                      00102300
        DO 270 I=0,N4                                                   00102400
C          >>>> GPOT(I) = GESCHWINDIGKEITSPOTENTIAL AM RANDE            00102500
C          >>>> GPOTN(I) = GESCHWINDIGKEIT IN NORMALENRICHTUNG          00102600
           GPOT(I) = -EPS*KM(I)                                         00102700
           GPOTN(I)=BGPOT(I+1) + RHON(I)                                00102800
     *                     - (Z(I)*R1(I)-R(I)*Z1(I)) / WG(I) / 3.D0     00102900
C           WRITE ( 6,111) I,RHO(I),RHON(I),GPOT(I),GPOTN(I)            00103000
  270   CONTINUE                                                        00103100
C                                                                       00103200
C                                                                       00103300
C       **************************************************              00103400
C       ***** BERECHNUNG DES TANGENTIELLEN            ****              00103500
C       *****     AUSGLEICHSTERMES                    ****              00103600
C       **************************************************              00103700
C                                                                       00103800
C       >>>> STPKT IST DIE ZEITLICHE ENTWICKLUNG DER BOGENLAENGE        00103900
C       >>>> (FUER HALBE KURVE)                                         00104000
        STPKT=0.0D0                                                     00104100
        DO 280 J=1,N4,1                                                 00104200
          H1=KAPPA(J-1)*GPOTN(J-1)                                      00104300
          H2=KAPPA(J  )*GPOTN(J)                                        00104400
          DSPKT(J)= (H1+H2)*DS(J)/2.D0                                  00104500
          STPKT   = STPKT + DSPKT(J)                                    00104600
  280   CONTINUE                                                        00104700
        STPKT=STPKT+STPKT                                               00104800
C                                                                       00104900
C       *** GAMPKT IST DER KORREKTUR-TERM IN TANGENTIELLER              00105000
C       **  RICHTUNG.                                                   00105100
        IF (NURNR .EQ. 0) THEN                                          00105200
           H1=0.D0                                                      00105300
           DO 282 I=1, N4-1,1                                           00105400
              H1=H1+DSPKT(I)                                            00105500
              GAMPKT(I)=(H1-STPKT*GAM(I))/STOT                          00105600
  282      CONTINUE                                                     00105700
        ENDIF                                                           00105800
C                                                                       00105900
        DO 284 J= 0,N4,1                                                00106000
           H1=GPOTN(J)                                                  00106100
           ZT(J)=  R1(J)*H1/WG(J)-Z1(J)*STOT*GAMPKT(J)                  00106200
           RT(J)= -Z1(J)*H1/WG(J)-R1(J)*STOT*GAMPKT(J)                  00106300
  284   CONTINUE                                                        00106400
C                                                                       00106500
C       **************************************************              00106600
C       *** VERGROESSERUNG BZW. VERKLEINERUNG VON     ****              00106700
C       *** DELT , FALLS DIE DYNAMIK ZU LANGSAM       ****              00106800
C       *** BZW. ZU SCHNELL WIRD.                     ****              00106900
C       **************************************************              00107000
C                                                                       00107100
        H1=0.D0                                                         00107200
        DO 291 J=0,N4,1                                                 00107300
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT + H1            00107400
  291   CONTINUE                                                        00107500
C       *** FUER DIE STEADY-STEATE-SUCHE SOLLTE  DELT  NICHT            00107600
C       **  VERGROESSERT WERDEN !                                       00107700
C                                                                       00107800
        IF (H1 .LT. STOT*.0000D0/N1 ) DELT=DELT*2.D0                    00107900
        DO 290 K=1,10,1                                                 00108000
        DO 290 J=0,N4,1                                                 00108100
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT                 00108200
           IF (H1 .GT. STOT*.010D0/N1) DELT=DELT*.5D0                   00108300
C          WRITE ( 6, 111) J,H1,STOT*.01D0/N1,DELT                      00108400
  290   CONTINUE                                                        00108500
C                                                                       00108600
C       ***************************************************             00108700
C       ****            PRINT PRINT PRINT              ****             00108800
C       ***************************************************             00108900
C                                                                       00109000
C                                                                       00109100
C       *** FALLS R(J) FUER EIN 0<J<N SEHR KLEIN WIRD (TEILUNG),        00109200
C       **  WIRD DAS PROGRAMM ABGEBOCHEN.                               00109300
C       *** FALLS Z(0)      SEHR KLEIN WIRD (UEBERGANG ZUM              00109400
C       **  TORUS), WIRD DAS PROGRAMM ABGEBROCHEN.                      00109500
        DO 510 J=N4-1,1,-1                                              00109600
           IF (R(J) .LT. .01D0) TT=J                                    00109700
  510   CONTINUE                                                        00109800
        IF (Z(0)      .LT. .005D0) TT=N4                                00109900
        IF (TT .NE. 0) GOTO 1700                                        00110000
        H2=T9                                                           00110100
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          00110200
C                                                                       00110300
C                                                                       00110400
 1700   CONTINUE                                                        00110500
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    00110600
     *     F12.6)                                                       00110700
   59   FORMAT (' DELT =',D14.7,'     CPU-ZEIT(IN SEC.) =',I8)          00110800
   60   FORMAT(1X,' I       Z       R     DGA     RHO    RHON',         00110900
     *          '    GPOT      VN      KM')                             00111000
   70   FORMAT(1X,I2,1X,F7.2,1X,F7.2,1X,F7.4,1X,F7.2,1X,F7.2,1X,F7.2,   00111100
     *               1X,F7.3,1X,F7.2)                                   00111200
   61   FORMAT(8(1X,F12.5))                                             00111300
   71   FORMAT(1X,'>>>>',I8,3X,I10)                                     00111400
        APR=APR+1                                                       00111500
C                                                                       00111600
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                            00111700
        CALL CLOCK(ISEC)                                                00111800
        WRITE (6,50) T8,T9,STOT                                         00111900
        WRITE (6,59) DELT,ISEC                                          00112000
        WRITE (6,60)                                                    00112100
        ENDIF                                                           00112200
C                                                                       00112300
        WRITE (22,71) APR,N+N                                           00112400
        WRITE (22,61) T8,STOT                                           00112500
        DO 550 J=0 ,N4,1                                                00112600
           WRITE (22,61) Z (J),R (J),GAM(J),RHO(J),RHON(J),             00112700
     *              GPOT(J),GPOTN(J),KM(J)                              00112800
  550   CONTINUE                                                        00112900
        DO 551 J=N4+1,N4+N4,1                                           00113000
           K=N4+N4-J                                                    00113100
           WRITE (22,61) -Z (K), R(K),1.D0-GAM(K),RHO(K),RHON(K),       00113200
     *              GPOT(K),GPOTN(K),KM(K)                              00113300
  551   CONTINUE                                                        00113400
        DO 552 J=N4+N4+1,N4+N4+N4,1                                     00113500
           K=J-N4-N4                                                    00113600
           WRITE (22,61) -Z (K),-R(K),1.D0+GAM(K),RHO(K),RHON(K),       00113700
     *              GPOT(K),GPOTN(K),KM(K)                              00113800
  552   CONTINUE                                                        00113900
        DO 553 J=N4+N4+N4+1,N4+N4+N4+N4,1                               00114000
           K=N4+N4+N4+N4-J                                              00114100
           WRITE (22,61)  Z (K),-R(K),2.D0-GAM(K),RHO(K),RHON(K),       00114200
     *              GPOT(K),GPOTN(K),KM(K)                              00114300
  553   CONTINUE                                                        00114400
C                                                                       00114500
        IF (SDP .EQ. 1) THEN                                            00114600
        DO 560 J=0 ,N4,1                                                00114700
           WRITE (6,70) J,Z(J),R(J),DS(J)/STOT,RHO(J),RHON(J),          00114800
     *              GPOT(J),GPOTN(J),KM(J)                              00114900
  560   CONTINUE                                                        00115000
        DO 561 I=0,N4                                                   00115100
           WRITE ( 6,111) I,INT1CL(I),INT2CL(I),INT3CL(I),INT4CL(I)     00115200
  561   CONTINUE                                                        00115300
        ENDIF                                                           00115400
C                                                                       00115500
        WRITE (22,71) 0,0                                               00115600
        BACKSPACE 22                                                    00115700
        IF (TT .NE. 0 .AND. TT .NE. N4) THEN                            00115800
            NI=TT                                                       00115900
            NA=N4                                                       00116000
            T9=0                                                        00116100
            R(NI)=0.D0                                                  00116200
            R(NI+1)=-R(NI-1)                                            00116300
            Z(NI+1)=Z(NI-1)                                             00116400
            T8=0.D0                                                     00116500
            GOTO 1300                                                   00116600
        ENDIF                                                           00116700
        IF (TT .NE. 0) GOTO 1200                                        00116800
C                                                                       00116900
C       **************************************************              00117000
C       ****      BERECHNEN DER NEUEN WERTE           ****              00117100
C       **************************************************              00117200
C                                                                       00117300
 1100   DO 600 J= 0,N4,1                                                00117400
           Z(J)=Z(J)+ZT(J)*DELT                                         00117500
           R(J)=R(J)+RT(J)*DELT                                         00117600
  600   CONTINUE                                                        00117700
        Z(-1)=Z(1)                                                      00117800
        R(-1)=-R(1)                                                     00117900
        R(0)=0.D0                                                       00118000
        Z(N4)=0.D0                                                      00118100
        R(N4+1)= R(N4-1)                                                00118200
        Z(N4+1)=-Z(N4-1)                                                00118300
C                                                                       00118400
C                                                                       00118500
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00118600
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              00118700
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00118800
C                                                                       00118900
        IF (T9 .GE. ZSEND) GOTO 1200                                    00119000
        GOTO 1000                                                       00119100
 1200   CONTINUE                                                        00119200
        CLOSE(22,STATUS='KEEP')                                         00119300
        END                                                             00119400
C                                                                       00119500
C       >>>>>>>>>>>>>>>>>>>> SUBROUTINES <<<<<<<<<<<<<<<<<<<<<<<<<      00119600
C                                                                       00119700
        SUBROUTINE LEGPOL(X,L,RET)                                      00119800
        DOUBLE PRECISION X,X7,X8,X9,RET,DI                              00119900
        INTEGER L,I                                                     00120000
        IF (L .EQ. 0) RET=1.D0                                          00120100
        IF (L .EQ. 1) RET=X                                             00120200
        IF (L .LE. 1) RETURN                                            00120300
        X7=1.D0                                                         00120400
        X8=X                                                            00120500
        DO 10 I=2,L,1                                                   00120600
           DI=I                                                         00120700
           X9=( (DI+DI-1.D0)*X*X8 - (DI-1.D0)*X7 ) / DI                 00120800
           X7=X8                                                        00120900
           X8=X9                                                        00121000
   10   CONTINUE                                                        00121100
        RET=X9                                                          00121200
        RETURN                                                          00121300
C       END                                                             00121400
C/LKED.SYSLIB     DD   DSN=SYS1.SSL2,DISP=SHR                           00121500
C/                DD   DSN=SYS1.FORTLIB,DISP=SHR                        00121600
C/GO.SYSIN    DD *                                                      00121700
C*                                                                      00121800
C*      EXEC DRUCKE                                                     00121900
C*      END                                                             00122000
        END                                                             00122100
