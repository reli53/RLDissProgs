C/D14KG    JOB RACF,CLASS=G,                                            00000100
C/    MSGCLASS=X,MSGLEVEL=(1,1),REGION=2048K,NOTIFY=D14K                00000200
C/            EXEC FORTCLG,REGION.FORT=1024K                            00000300
C/            EXEC FORTCLG                                              00000310
C/FORT.SYSIN  DD *                                                      00000400
C       #############################################################   00000500
C       ####   OBERFLAECHEN-DYNAMIK  (3-DIMENSIONAL)             ####   00000600
C       ####    DES NAEHRSTOFFMODELLES    MA86                   ####   00000700
C       ####    HIER: FORMULIERUNG DES PROBLEMS ALS              ####   00000800
C       ####          REINES OBERFLAECHEN-DYNAMIK-PROBLEM        ####   00000900
C       ####          MIT GREENSCHEN FUNKTIONEN.                 ####   00001000
C       ####     ( ROTATIONSSYMMETRISCH UM Z-ACHSE UND           ####   00001100
C       ####       SYMMETRIE BZGL. DER X-Y-EBENE )               ####   00001200
C       ####                                                     ####   00001300
C       ####        (DX/DT)*NORMALE = FUNKTION DER KURVE         ####   00001400
C       ####        KM = 2 * MITTLERE KRUEMMUNG                  ####   00001500
C       ####                                                     ####   00001600
C       ####        NAME DES PROGRAMMES :  MA86OD3V.FORT         ####   00001700
C       #############################################################   00001800
C                                                                       00001900
C       LITERATUR: EIGENE                                               00002000
C                                                                       00002100
        INTEGER           I,J,K,N,T7,T9,T99,ND,NDEND,NPHI,ISEC,SDP,N4   00002200
        INTEGER           NURNR,ZSEND,ZSPRT,APR,NI,NA,TT,NSIG           00002300
        DOUBLE PRECISION  DGAM,DELT,STOT,STPKT,MDIFF,DELPHI,N1SIG,DSIG  00002400
        DOUBLE PRECISION  EPS,TAU,DIFK,B0                               00002500
        DOUBLE PRECISION  H1,H2,H3,H4,H5,H6,H7,H8,H9,N1,ND1,PI,T8       00002600
        DOUBLE PRECISION  AMPLI(0:10)                                   00002700
        DOUBLE PRECISION  A1,B1,A2,B2,ABST,DPN,ELI1,ELI2                00002800
C                                                                       00002900
        DOUBLE PRECISION  Z(-1:41),R(-1:41)                             00003000
        DOUBLE PRECISION  Z1(0:40),R1(0:40)                             00003100
        DOUBLE PRECISION  Z2(0:40),R2(0:40)                             00003200
        DOUBLE PRECISION  ZPZ(-1:321),ZPR(-1:321)                       00003300
        DOUBLE PRECISION  ZPZ1(0:320),ZPR1(0:320)                       00003400
        DOUBLE PRECISION  ZPZ2(0:320),ZPR2(0:320)                       00003500
        DOUBLE PRECISION  ZPDS(0:321),ZPWG(0:320),ZPG(0:320)            00003600
        DOUBLE PRECISION  ZT(0:40),RT(0:40)                             00003700
C                                                                       00003800
        DOUBLE PRECISION  DS(0:41),DSPKT(0:40)                          00003900
        DOUBLE PRECISION  GAM(-1:41),GAMPKT(0:40)                       00004000
        DOUBLE PRECISION  G(0:40),WG(0:40)                              00004100
        DOUBLE PRECISION  KAPPA(0:40),KM(0:40)                          00004200
C                                                                       00004300
        DOUBLE PRECISION  RHO(0:40),RHON(0:40)                          00004400
        DOUBLE PRECISION  GPOT(0:40),GPOTN(0:40)                        00004500
C                                                                       00004600
        DOUBLE PRECISION  INT1(0:40,0:160),INT2(0:40,0:160)             00004700
        DOUBLE PRECISION  INT3(0:40,0:160),INT4(0:40,0:160)             00004800
        DOUBLE PRECISION  IS1SP(0:40),ISL1SP(0:40),ISR1SP(0:40)         00004900
        DOUBLE PRECISION  IS3SP(0:40),ISL3SP(0:40),ISR3SP(0:40)         00005000
        DOUBLE PRECISION  HINT1(0:100),HINT3(0:100)                     00005100
        DOUBLE PRECISION  INT1CL(0:40),INT2CL(0:40)                     00005200
        DOUBLE PRECISION  INT3CL(0:40),INT4CL(0:40)                     00005300
        DOUBLE PRECISION  INT1KG,INT2KG,INT3KG,INT4KG                   00005400
        DOUBLE PRECISION  KORR1(0:40),KORR2(0:40)                       00005500
        DOUBLE PRECISION  KORR3(0:40),KORR4(0:40)                       00005600
C                                                                       00005700
        DOUBLE PRECISION  RARHO(80,80),RBRHO(80)                        00005800
        DOUBLE PRECISION  ARHO(82,82),AGPOT(82,82)                      00005900
        DOUBLE PRECISION  BRHO(82),BGPOT(82)                            00006000
        DOUBLE PRECISION  CRHO(82,82)    , DRHO(82), TH1,TH2,TH3,TH4    00006100
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
                 DNW   = 'D14K.D05T2.DATA'                              00007500
                 DELT  =      0.00010000D0 *   64.D0                    00007600
C                DELT  =      0.00001000D0 *    8.D0                    00007610
                 N4    =     16                                         00007700
                 NPHI  =     16                                         00007800
                 NSIG  =     16                                         00007900
                 NURNR =      0                                         00008000
                 T9    =     -1                                         00008100
                 SDP   =      1                                         00008200
                 PROB  = 'N'                                            00008300
C       PROB    : 'N' = PRODUKTIONSBETRIEB                              00008400
C               : 'J' = PROBEBETRIEB (NUR FUER KUGEL)                   00008500
C               : 'R' = REDUZ. DER MATR. F. RHO UND RHON (SONST WIE 'N')00008600
C               :  (EMPFEHLENSWERT, DA FUER I=0 UND I=1 BESSERE WERTE)  00008700
C       T9      : =-2 KORREKTURTERME ANHAND DER KUGEL AUSGERECHNET      00008800
C               : =-1 OHNE KORREKTURTEME                                00008900
C       NPHI    : STUETZPUNKTE FUER HALBEN KREIS BEI INTEGRATION        00009000
C                 BZGL. DES WINKELS PHI  (NPHI >= 12)                   00009100
C       NSIG    : STUETZPUNKTE UM DIE SINGULARITAETEN BEI INT1 + INT3   00009200
C                 BZGL. DES KURVENPARAMETERS SIGMA (100 >= NSIG >=  6)  00009300
C       NURNR=1 : BEWEGUNG DES RANDES NUR IN NORMALENRICHTUNG.          00009400
C       NURNR=0 : BEWEGUNG DES RANDES MIT TANGENT. AUSGLEICHSTERM.      00009500
C                 DER ANFANGSPUNKT J=0 DER DURCHNUMMERIERUNG            00009600
C                 BEWEGT SICH DABEI NUR IN NORMALENRICHTUNG.            00009700
C       N4      : ANZ. DER STUETZPUNKTE (FUER VIERTEL KURVE)(N4<=40)    00009800
C       DELT    : DELTA T                                               00009900
C               : FUER N4=20 UND B0=4.7333 IST 0.0032 EIN GUTER WERT!   00010000
C               : FUER N4=16 UND B0=4.7333 IST 0.0064 EIN GUTER WERT!   00010100
C               : FUER N4=16 UND B0=2.0000 IST 0.0064 EIN GUTER WERT!   00010200
C       SDP     :  =0 KEINE BILDSCHIRMAUSGABE                           00010300
C               :  =1 BILDSCHIRMAUSGABE                                 00010400
C               :  =2 VERMINDERTE BILDSCHIRMAUSGABE                     00010500
C       FUER N4=12 , NPHI=16 , NSIG=16 UND 1000 ZEITITERATIONEN         00010600
C               BETRAEGT DIE CPU-ZEIT  8 MINUTEN +  0 SEKUNDEN          00010700
C       FUER N4=16 , NPHI=16 , NSIG=16 UND 1000 ZEITITERATIONEN         00010800
C               BETRAEGT DIE CPU-ZEIT 13 MINUTEN + 30 SEKUNDEN          00010900
C       FUER N4=20 , NPHI=16 , NSIG=16 UND 1000 ZEITITERATIONEN         00011000
C               BETRAEGT DIE CPU-ZEIT 20 MINUTEN + 20 SEKUNDEN          00011100
C       >>>>>   1200 SEKUNDEN = 20 MINUTEN                              00011200
C       >>>>>   1020 SEKUNDEN = 17 MINUTEN                              00011300
C       >>>>>    900 SEKUNDEN = 15 MINUTEN                              00011400
C       >>>>>    780 SEKUNDEN = 13 MINUTEN                              00011500
C                                                                       00011600
C                                                                       00011700
                 ZSEND =  40000                                         00011800
                 ZSPRT =   1000                                         00011900
C                ZSEND =   1000                                         00011910
C                ZSPRT =    100                                         00012000
C                                                                       00012100
                 B0    =      2.10220000D0                              00012200
C                B0    =      0.38538400D0                              00012210
                 TAU   =      2.00000000D0                              00012300
                 DIFK  =      0.50000000D0                              00012400
                 EPS   =      0.12500000D0                              00012500
C                                                                       00012600
C        ********************** PARAMETER-TABELLE **********************00012700
C        B0 IST DER STATIONAERE KUGELRADIUS ZU VORGEGEBENEM TAU + DIFK !00012800
C        EPS(I) IST DER KRITISCHE OBERFLAECHENSPANNUNGSKOEFFIZIENT BZGL.00012900
C        DER MODE P (COS(THETA)), I=2,3,4,...... .                      00013000
C                  I                                                    00013100
C       >DIFK<   >TAU<      >B0<       >EPS(2)<   >EPS(3)<   >EPS4(4)<  00013200
C        0.0      2.0        4.7333     2.3502     1.0869     0.59696   00013300
C        0.5      2.0        2.1022     0.12496    0.04859    0.02375   00013400
C        1.0      2.0        1.58977    0.04135    0.01526    0.00721   00013500
C        1.5      2.0        1.33211    0.019935   0.007142   0.0033123 00013600
C        2.0      2.0        1.16971    0.0147866  0.004032   0.0018475 00013700
C        3.0      2.0        0.96903    0.0050440  0.0017282  0.0007801 00013800
C        5.0      2.0        0.75979    0.0016787  0.0005606  0.0002375 00013900
C       10.0      2.0        0.542370   0.0003453  0.0001125  --------- 00013910
C       20.0      2.0        0.385384   0.00006637 0.00002132 --------- 00013920
C                                                                       00014000
C        0.0      1.240738   2.0        0.061228   0.0261854  0.0136400 00014100
C        0.5      1.907404   2.0        0.099413   0.0385106  0.0187832 00014200
C        1.0      2.574071   2.0        0.120097   0.0451481  0.0215323 00014300
C        2.0      3.907404   2.0        0.141962   0.0521355  0.0244111 00014400
C        5.0      7.907404   2.0        0.165113   0.0595014  0.0274288 00014500
C       10.0     14.574071   2.0        0.175113   0.0629831  0.0288491 00014600
C       20.0     27.907404   2.0        0.182501   0.0650118  0.0296750 00014700
C                                                                       00014800
C        ********************** PARAMETER-TABELLE **********************00014900
C        B0 IST DER STATION. ZYLINDER-RADIUS ZU VORGEGEBEM TAU (DIFK=0).00015000
C        EPS IST DER KRITISCHE OBERFLAECHENSPANNUNGSKOEFFIZIENT.        00015100
C                                                                       00015200
C       >DIFK<     >TAU<      >B0<       >EPS<                          00015300
C        0.0        2.0        3.3258     2.053                         00015400
C                                                                       00015500
        AMPLI(0) = B0                                                   00015600
        AMPLI(2) = 0.3000D0                                             00015700
        AMPLI(4) = 0.0000D0                                             00015800
        AMPLI(6) = 0.0000D0                                             00015900
        AMPLI(8) = 0.0000D0                                             00016000
        AMPLI(10)= 0.0000D0                                             00016100
C                                                                       00016200
                 ND    =   1000                                         00016300
                 NDEND =     50                                         00016400
                 EPSZ  =      1.D-40                                    00016500
                 ISW   =      1                                         00016600
C       ND :ANZ. DER STUETZPUNKTE F. NEU-DISKRETISIERUNGS-VERFAHREN     00016700
C       NDEND : MAXIMALE ANZAHL DER NEU-DISKRETISIERUNGEN               00016800
                 N = N4+N4                                              00016900
                 ND1=ND                                                 00017000
                 N1=N                                                   00017100
                 DGAM=1.D0/N1                                           00017200
                 PI=4.D0*DATAN(1.D0)                                    00017300
                 DELPHI=NPHI                                            00017400
                 DELPHI=PI/DELPHI                                       00017500
                 N1SIG=NSIG                                             00017600
C                                                                       00017700
C       *************************************************               00017800
C       ****    STARTKONZENTRATION UND START-RAND    ****               00017900
C       *************************************************               00018000
C                                                                       00018100
        INT1KG=B0                                                       00018200
        INT2KG=INT1KG - (1.D0-DEXP(-B0-B0))*.5D0                        00018300
        INT3KG=-.5D0                                                    00018400
        INT4KG=INT3KG - ( -.5D0+DEXP(-B0)*(DCOSH(B0)-DSINH(B0)/B0) )    00018500
        DO 104 I=0,N4                                                   00018600
           KORR1(I)=1.D0                                                00018700
           KORR2(I)=1.D0                                                00018800
           KORR3(I)=1.D0                                                00018900
           KORR4(I)=1.D0                                                00019000
  104   CONTINUE                                                        00019100
C                                                                       00019200
 1111   DO 100 J=0 , ND , 1                                             00019300
           H1=J                                                         00019400
           CALL LEGPOL( DCOS(H1*PI/ND1*.5D0) ,0, H2)                    00019500
           H3= AMPLI(0)*H2                                              00019600
           IF (T9 .NE. -2) THEN                                         00019700
             DO 105 I=2,10,2                                            00019800
                CALL LEGPOL( DCOS(H1*PI/ND1*.5D0) ,I, H2)               00019900
                H3=H3 + AMPLI(I)*H2                                     00020000
  105        CONTINUE                                                   00020100
           ENDIF                                                        00020200
C                                                                       00020300
           HI1(J)=DCOS(.5D0*PI*H1/ ND1)*H3                              00020400
           HI2(J)=DSIN(.5D0*PI*H1/ ND1)*H3                              00020500
  100   CONTINUE                                                        00020600
C                                                                       00020700
        HI3(0)=0.D0                                                     00020800
        DO 110 J=1, ND  ,1                                              00020900
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        00021000
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  00021100
  110   CONTINUE                                                        00021200
        STOT=HI3( ND  )                                                 00021300
        STOT=STOT*2.D0                                                  00021400
C                                                                       00021500
        J=0                                                             00021600
        DO 120 K=1, N4-1, 1                                             00021700
           H1=K                                                         00021800
  130      J=J+1                                                        00021900
           IF (J .GT. ND) GOTO 120                                      00022000
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 130                         00022100
              H5=HI3(J)-HI3(J-1)                                        00022200
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             00022300
              H4=(HI3(J) - H1*STOT/N1)/H5                               00022400
              Z(K)=HI1(J)*H3 + HI1(J-1)*H4                              00022500
              R(K)=HI2(J)*H3 + HI2(J-1)*H4                              00022600
  120   CONTINUE                                                        00022700
        Z(0)=HI1(0)                                                     00022800
        R(0)=HI2(0)                                                     00022900
        Z(N4)=HI1(ND)                                                   00023000
        R(N4)=HI2(ND)                                                   00023100
        Z(-1)=Z(1)                                                      00023200
        R(-1)=-R(1)                                                     00023300
        Z(N4+1)=-Z(N4-1)                                                00023400
        R(N4+1)= R(N4-1)                                                00023500
        R(0)=0.D0                                                       00023600
        Z(N4)=0.D0                                                      00023700
C                                                                       00023800
C                                                                       00023900
C                                                                       00024000
C       **************************************************              00024100
C       ****      PRINTEN DER PARAMETER-TABELLE       ****              00024200
C       **************************************************              00024300
C                                                                       00024400
      IF (T9 .NE. -2) THEN                                              00024500
        OPEN (22,FILE=DNW)                                              00024600
        REWIND 22                                                       00024700
   10   FORMAT (///' PROGRAMM-NAME :  MA86OD3V.FORT    ')               00024800
   20   FORMAT (/,' DELTA GAMMA =',F10.7,'    DELTA T ='                00024900
     *        ,F10.7,'   N4 =',I4)                                      00025000
   30   FORMAT (' ZSEND =',I10,'  ZSPRT =',I8)                          00025100
   40   FORMAT (/,' EPS =',F10.5,' TAU =',F10.5,' DIFK=',F10.5)         00025200
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                            00025300
          WRITE (6,10)                                                  00025400
          WRITE (6,20) DGAM,DELT,N4                                     00025500
          WRITE (6,30) ZSEND,ZSPRT                                      00025600
          WRITE (6,40) EPS,TAU,DIFK                                     00025700
        ENDIF                                                           00025800
      ENDIF                                                             00025900
C                                                                       00026000
C                                                                       00026100
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00026200
C       >>>>   ANFANG DER ZEITSCHLEIFE                <<<<              00026300
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00026400
C                                                                       00026500
        T7=0                                                            00026600
        T8     = -DELT                                                  00026700
        APR=0                                                           00026800
        NI=N4                                                           00026900
        NA=N4                                                           00027000
        TT=0                                                            00027100
 1000   T9=T9+1                                                         00027200
        T8=T8+DELT                                                      00027300
C                                                                       00027400
C       ***************************************************             00027500
C       ****    BEDINGUNG FUER NEUE DISKRETISIERUNG    ****             00027600
C       ***************************************************             00027700
C                                                                       00027800
 1300   T99=0                                                           00027900
C       STOT=TOTALE BOGENLAENGE (FUER HALBE KURVE)                      00028000
        STOT=0.0D0                                                      00028100
        DO 305 J=1, NI  ,1                                              00028200
            DS(J)=DSQRT(( Z(J)-Z(J-1) )*( Z(J)-Z(J-1) )+                00028300
     *            ( R(J)-R(J-1) )*( R(J)-R(J-1) ))                      00028400
        STOT=STOT+DS(J)                                                 00028500
  305   CONTINUE                                                        00028600
        STOT=STOT+STOT                                                  00028700
        DS(0)=DS(1)                                                     00028800
        DS(NI+1)=DS(NI)                                                 00028900
C                                                                       00029000
        GAM(0)=0.D0                                                     00029100
        DO 307 J=1,NI,1                                                 00029200
           GAM(J)=GAM(J-1)+DS(J)/STOT                                   00029300
C          WRITE ( 6,111) J,GAM(J),DGAM                                 00029400
  307   CONTINUE                                                        00029500
        GAM(-1)=-GAM(1)                                                 00029600
        GAM(NI+1)=1.D0-GAM(NI-1)                                        00029700
C                                                                       00029800
        H2=NI                                                           00029900
        DGAM=1.D0/(H2+H2)                                               00030000
        H2=DGAM/4.D0                                                    00030100
        DO 300 J=1, NI-1 , 1                                            00030200
           H1=J                                                         00030300
           H3=H1*DGAM                                                   00030400
           IF ((GAM(J-1)+H2 .GT. H3) .OR. (H3 .GT. GAM(J+1)-H2))        00030500
     *            T99=1                                                 00030600
  300   CONTINUE                                                        00030700
        MDIFF=0.0D0                                                     00030800
        DO 310 J=1, NI ,1                                               00030900
           MDIFF=DMAX1(MDIFF,DABS(DS(J)/STOT-DGAM))                     00031000
  310   CONTINUE                                                        00031100
        IF (MDIFF .GT. 1.D0* DGAM/4.D0)    T99=1                        00031200
        IF (TT .NE. 0) T99=1                                            00031300
        TT=0                                                            00031400
C                                                                       00031500
        IF (T99 .EQ. 0) GOTO 1500                                       00031600
C                                                                       00031700
C                                                                       00031800
C       ***************************************************             00031900
C       ****          NEUE DISKRETISIERUNG             ****             00032000
C       ***************************************************             00032100
C                                                                       00032200
        T7=T7+1                                                         00032300
   90   FORMAT ('  DIE',I4,' .TE NEU-DISKRETISIERUNG BEI ZEITSCHRITT',  00032400
     *          I11)                                                    00032500
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) WRITE (6,90) T7,T9              00032600
        IF (T7 .GT. NDEND) GOTO 1200                                    00032700
C                                                                       00032800
C                                                                       00032900
        DO 401 J=-1,NI+1,1                                              00033000
           X1(J)=Z(J)                                                   00033100
           X2(J)=R(J)                                                   00033200
  401   CONTINUE                                                        00033300
C       *** APPROXIMATION DER ZWISCHENPUNKTE DURCH DAS LEGEN            00033400
C       **  EINER PARABEL DURCH JEWEILS DREI BENACHBARTE PUNKTE         00033500
        DO 400 J=0,  NI  , 1                                            00033600
           H2=GAM(J+1)-GAM(J)                                           00033700
           H1=GAM(J)-GAM(J-1)                                           00033800
           H3=H1+H2                                                     00033900
           AKO1(J)=X1(J-1)/H1/H3-X1(J)/H1/H2+X1(J+1)/H2/H3              00034000
           BKO1(J)=-2.D0*GAM(J)*AKO1(J)+(X1(J)-X1(J-1))*H2/H1/H3        00034100
           BKO1(J)=BKO1(J)+(X1(J+1)-X1(J))*H1/H2/H3                     00034200
           CKO1(J)=X1(J)-(AKO1(J)*GAM(J)+BKO1(J))*GAM(J)                00034300
           AKO2(J)=X2(J-1)/H1/H3-X2(J)/H1/H2+X2(J+1)/H2/H3              00034400
           BKO2(J)=-2.D0*GAM(J)*AKO2(J)+(X2(J)-X2(J-1))*H2/H1/H3        00034500
           BKO2(J)=BKO2(J)+(X2(J+1)-X2(J))*H1/H2/H3                     00034600
           CKO2(J)=X2(J)-(AKO2(J)*GAM(J)+BKO2(J))*GAM(J)                00034700
  400   CONTINUE                                                        00034800
C                                                                       00034900
C       *** BESTIMMEN DERJENIGEN PUNKTE HI1(J),HI2(J) ,0<J<ND , FUER    00035000
C       **  DIE DIE BOGENLAENGE (VON HI1(0),HI2(0) AUS BERECHNET)       00035100
C       **  DEN WERT STOT*J/ND/2 ANNIMMT.                               00035200
C                                                                       00035300
        HI1(0)=X1(0)                                                    00035400
        HI2(0)=X2(0)                                                    00035500
        HI1(ND)=X1(NI)                                                  00035600
        HI2(ND)=X2(NI)                                                  00035700
        J=0                                                             00035800
        DO 410 K=1, NI,1                                                00035900
  420      J=J+1                                                        00036000
           IF (J .GT. ND  ) GOTO 410                                    00036100
           H1=J                                                         00036200
           H2=H1/ND1*.5D0                                               00036300
           IF (H2 .LT. GAM(K)) THEN                                     00036400
              H5=GAM(K)-GAM(K-1)                                        00036500
              H3=(GAM(K)-H2)/H5                                         00036600
              H4=(H2-GAM(K-1))/H5                                       00036700
              HI1(J)=(AKO1(K-1)*H2*H2+BKO1(K-1)*H2+CKO1(K-1))*H3        00036800
              HI1(J)=(AKO1(K)*H2*H2+BKO1(K)*H2+CKO1(K))*H4 + HI1(J)     00036900
              HI2(J)=(AKO2(K-1)*H2*H2+BKO2(K-1)*H2+CKO2(K-1))*H3        00037000
              HI2(J)=(AKO2(K)*H2*H2+BKO2(K)*H2+CKO2(K))*H4 + HI2(J)     00037100
              GOTO 420                                                  00037200
           ELSE                                                         00037300
              J=J-1                                                     00037400
              GOTO 410                                                  00037500
           ENDIF                                                        00037600
  410   CONTINUE                                                        00037700
C                                                                       00037800
C                                                                       00037900
C       *** BERECHNEN DER NEUEN KOORDINATENPUNKTE                       00038000
        HI3(0)=0.0D0                                                    00038100
        DO 450 J=1, ND  ,1                                              00038200
           HI3(J)=DSQRT((HI1(J  )-HI1(J-1))*(HI1(J  )-HI1(J-1))+        00038300
     *               (HI2(J  )-HI2(J-1))*(HI2(J  )-HI2(J-1)))+HI3(J-1)  00038400
  450   CONTINUE                                                        00038500
        STOT=HI3( ND  )                                                 00038600
        STOT=STOT*2.D0                                                  00038700
C                                                                       00038800
        X1(NA)=HI1(ND)                                                  00038900
        X2(NA)=HI2(ND)                                                  00039000
        N1=NA+NA                                                        00039100
        J=0                                                             00039200
        DO 460 K=1, NA-1, 1                                             00039300
           H1=K                                                         00039400
  470      J=J+1                                                        00039500
           IF (J .GT. ND  ) GOTO 460                                    00039600
           IF (HI3(J) .LT. H1*STOT/N1) GOTO 470                         00039700
              H5=HI3(J)-HI3(J-1)                                        00039800
              H3=(H1*STOT/N1 - HI3(J-1))/H5                             00039900
              H4=(HI3(J) - H1*STOT/N1  )/H5                             00040000
              X1(K)=HI1(J)*H3 + HI1(J-1)*H4                             00040100
              X2(K)=HI2(J)*H3 + HI2(J-1)*H4                             00040200
  460   CONTINUE                                                        00040300
        X1(-1)=X1(1)                                                    00040400
        X2(-1)=-X2(1)                                                   00040500
        X1(NA+1)=X1(NA-1)                                               00040600
        X2(NA+1)=-X2(NA-1)                                              00040700
        DO 480 J=-1,NA+1,1                                              00040800
           Z(J)=X1(J)                                                   00040900
           R(J)=X2(J)                                                   00041000
  480   CONTINUE                                                        00041100
        NI=N4                                                           00041200
        NA=N4                                                           00041300
        GOTO 1300                                                       00041400
C                                                                       00041500
C       **************************************************              00041600
C       ***** ABLEITUNGEN BZGL. DER OBERFLAECHE       ****              00041700
C       *****                                         ****              00041800
C       **************************************************              00041900
C                                                                       00042000
 1500   CONTINUE                                                        00042100
        DO 220 J=0, N4, 1                                               00042200
           H1=DS(J)                                                     00042300
           H2=DS(J+1)                                                   00042400
           H3=H1+H2                                                     00042500
           Z2(J)=(Z(J+1)/H2/H3-Z(J)/H1/H2+Z(J-1)/H1/H3)*2.D0            00042600
           R2(J)=(R(J+1)/H2/H3-R(J)/H1/H2+R(J-1)/H1/H3)*2.D0            00042700
           Z1(J)=(Z(J)-Z(J-1))/H1 + Z2(J)*H1/2.D0                       00042800
           R1(J)=(R(J)-R(J-1))/H1 + R2(J)*H1/2.D0                       00042900
           G(J)=Z1(J)*Z1(J)+R1(J)*R1(J)                                 00043000
           WG(J)=DSQRT(G(J))                                            00043100
  220   CONTINUE                                                        00043200
C                                                                       00043300
C       KAPPA = KRUEMMUNG IN RICHTUNG DES KURVENPARAMETERS              00043400
C       KM    = 2 * MITTLERE KRUEMMUNG                                  00043500
        DO 230 J=0,N4,1                                                 00043600
           GAMPKT(J)=0.D0                                               00043700
           KAPPA(J)=(-R1(J)*Z2(J)+Z1(J)*R2(J))/WG(J)/G(J)               00043800
  230   CONTINUE                                                        00043900
        DO 231 J=1,N4,1                                                 00044000
           KM(J)=KAPPA(J)-Z1(J)/R(J)/WG(J)                              00044100
  231   CONTINUE                                                        00044200
        KM(0)=KAPPA(0)-Z2(0)/R1(0)/WG(0)                                00044300
C                                                                       00044400
C       **************************************************              00044500
C       ***** BESTIMMEN DER ZWISCHENPUNKTE            ****              00044600
C       ***** (ZWISCHEN 2 BENACHBARTEN PUNKTEN WERDEN ****              00044700
C       *****  3 WEITERE PUNKTE GELEGT)               ****              00044800
C       **************************************************              00044900
C                                                                       00045000
        DO 730 I=0,N4                                                   00045100
           ZPZ(4*I)=Z(I)                                                00045200
           ZPR(4*I)=R(I)                                                00045300
  730   CONTINUE                                                        00045400
C                                                                       00045500
        DO 740 I=0,N4-1                                                 00045600
           H1=DS(I+1)*.25D0                                             00045700
           H2=DS(I+1)-H1                                                00045800
           H8=(Z2(I  )*H1*H1*.5D0 + Z1(I  )*H1 + Z(I  )) * H2           00045900
           H9=(Z2(I+1)*H2*H2*.5D0 - Z1(I+1)*H2 + Z(I+1)) * H1           00046000
           ZPZ(1+4*I)=(H8+H9)/DS(I+1)                                   00046100
           H8=(R2(I  )*H1*H1*.5D0 + R1(I  )*H1 + R(I  )) * H2           00046200
           H9=(R2(I+1)*H2*H2*.5D0 - R1(I+1)*H2 + R(I+1)) * H1           00046300
           ZPR(1+4*I)=(H8+H9)/DS(I+1)                                   00046400
C                                                                       00046500
           H1=DS(I+1)*.5D0                                              00046600
           H2=DS(I+1)-H1                                                00046700
           H8=(Z2(I  )*H1*H1*.5D0 + Z1(I  )*H1 + Z(I  )) * H2           00046800
           H9=(Z2(I+1)*H2*H2*.5D0 - Z1(I+1)*H2 + Z(I+1)) * H1           00046900
           ZPZ(2+4*I)=(H8+H9)/DS(I+1)                                   00047000
           H8=(R2(I  )*H1*H1*.5D0 + R1(I  )*H1 + R(I  )) * H2           00047100
           H9=(R2(I+1)*H2*H2*.5D0 - R1(I+1)*H2 + R(I+1)) * H1           00047200
           ZPR(2+4*I)=(H8+H9)/DS(I+1)                                   00047300
C                                                                       00047400
           H1=DS(I+1)*.75D0                                             00047500
           H2=DS(I+1)-H1                                                00047600
           H8=(Z2(I  )*H1*H1*.5D0 + Z1(I  )*H1 + Z(I  )) * H2           00047700
           H9=(Z2(I+1)*H2*H2*.5D0 - Z1(I+1)*H2 + Z(I+1)) * H1           00047800
           ZPZ(3+4*I)=(H8+H9)/DS(I+1)                                   00047900
           H8=(R2(I  )*H1*H1*.5D0 + R1(I  )*H1 + R(I  )) * H2           00048000
           H9=(R2(I+1)*H2*H2*.5D0 - R1(I+1)*H2 + R(I+1)) * H1           00048100
           ZPR(3+4*I)=(H8+H9)/DS(I+1)                                   00048200
  740   CONTINUE                                                        00048300
        ZPR(-1)=-ZPR(1)                                                 00048400
        ZPZ(-1)= ZPZ(1)                                                 00048500
        ZPR(4*N4+1)= ZPR(4*N4-1)                                        00048600
        ZPZ(4*N4+1)=-ZPZ(4*N4-1)                                        00048700
C                                                                       00048800
        DO 745 K=1,4*N4                                                 00048900
           H1=ZPR(K)-ZPR(K-1)                                           00049000
           H2=ZPZ(K)-ZPZ(K-1)                                           00049100
           ZPDS(K)=DSQRT(H1*H1+H2*H2)                                   00049200
  745   CONTINUE                                                        00049300
        ZPDS(4*N4+1)=ZPDS(4*N4)                                         00049400
        ZPDS(0)=ZPDS(1)                                                 00049500
C                                                                       00049600
        DO 750 J=0,4*N4                                                 00049700
           H1=ZPDS(J)                                                   00049800
           H2=ZPDS(J+1)                                                 00049900
           H3=H1+H2                                                     00050000
           ZPZ2(J)=(ZPZ(J+1)/H2/H3-ZPZ(J)/H1/H2+ZPZ(J-1)/H1/H3)*2.D0    00050100
           ZPR2(J)=(ZPR(J+1)/H2/H3-ZPR(J)/H1/H2+ZPR(J-1)/H1/H3)*2.D0    00050200
           ZPZ1(J)=(ZPZ(J)-ZPZ(J-1))/H1 + ZPZ2(J)*H1/2.D0               00050300
           ZPR1(J)=(ZPR(J)-ZPR(J-1))/H1 + ZPR2(J)*H1/2.D0               00050400
           ZPG(J)=ZPZ1(J)*ZPZ1(J)+ZPR1(J)*ZPR1(J)                       00050500
           ZPWG(J)=DSQRT(ZPG(J))                                        00050600
  750   CONTINUE                                                        00050700
C                                                                       00050800
        DO 239 I=N4*4+1,8*N4                                            00050900
          K=8*N4 - I                                                    00051000
          ZPZ(I)=-ZPZ(K)                                                00051100
          ZPR(I)= ZPR(K)                                                00051200
          ZPZ1(I)= ZPZ1(K)                                              00051300
          ZPR1(I)=-ZPR1(K)                                              00051400
          ZPG(I)=ZPG(K)                                                 00051500
          ZPWG(I)=ZPWG(K)                                               00051600
          ZPDS(I)=ZPDS(K+1)                                             00051700
  239   CONTINUE                                                        00051800
        ZPDS(8*N4+1)=ZPDS(8*N4)                                         00051900
        ZPZ1(0)   =0.D0                                                 00052000
        ZPR1(4*N4)=0.D0                                                 00052100
        ZPZ1(8*N4)=0.D0                                                 00052200
C                                                                       00052300
C       DO 999 I=0,8*N4+1                                               00052400
C         WRITE( 6,111) I, ZPZ(I),ZPR(I),ZPG(I),ZPDS(I)                 00052500
C         WRITE( 6,111) I, ZPZ1(I),ZPR1(I),ZPWG(I),ZPDS(I)              00052600
C 999   CONTINUE                                                        00052700
C                                                                       00052800
C       **************************************************              00052900
C       *****   G (R,R') := EXP(-K * |R-R'|) / |R-R'| ****              00053000
C       *****    K                                    ****              00053100
C       *****                                         ****              00053200
C       ***** (LAPLACE-OPERATOR  - K*K) G (R,R')      ****              00053300
C       *****                            K            ****              00053400
C       *****  =  - 4 * PI * DELTA( R-R')  , K>=0     ****              00053500
C       *****                                         ****              00053600
C       *****                                         ****              00053700
C       ***** BILDEN DER OBERFLAECHENINTEGRALE :      ****              00053800
C       *****                                         ****              00053900
C       ***** INT1 BZGL.        G (R,R')              ****              00054000
C       *****                    0                    ****              00054100
C       *****                                         ****              00054200
C       ***** INT2 BZGL.  G (R,R')  -  G (R,R')       ****              00054300
C       *****              0            1             ****              00054400
C       *****                                         ****              00054500
C       ***** INT3 BZGL.   (  GRAD' G (R,R')  ) * N'  ****              00054600
C       *****                        0                ****              00054700
C       *****                                         ****              00054800
C       ***** INT4 BZGL. GRAD'(G (R,R')-G (R,R')) * N'****              00054900
C       *****                   0        1            ****              00055000
C       **************************************************              00055100
C                                                                       00055200
C       >>>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKTE)              00055300
C       >>>>>       ABST = ] R - R' ]                                   00055400
C       >>>>>       DPN  = ( R - R' ) * N'                              00055500
        DO 244 I=0,N4                                                   00055600
          DO 242 L=0,4*N4                                               00055700
            J=2*L                                                       00055800
            IF (L .EQ. 0   ) J=2*L+1                                    00055900
            IF (L .EQ. 4*N4) J=2*L-1                                    00056000
            H5= ZPWG(J)                                                 00056100
            H7=  R(I)-ZPR(J)                                            00056200
            H8=  Z(I)-ZPZ(J)                                            00056300
C                                                                       00056400
            B1= R(I)*ZPZ1(J)                                            00056500
            A1= H8*ZPR1(J) -H7*ZPZ1(J) +B1                              00056600
            B2=  2.D0 * R(I) * ZPR(J)                                   00056700
            A2= H7*H7 + H8*H8 + B2                                      00056800
C                                                                       00056900
            INT2(I,L)=0.D0                                              00057000
            INT4(I,L)=0.D0                                              00057100
C                                                                       00057200
            DO 240 M=0,NPHI-1                                           00057300
              H1=M                                                      00057400
              H1= DCOS( (H1+ .5D0) * DELPHI )                           00057500
              ABST = DSQRT( A2 - B2 * H1 )                              00057600
              DPN = A1 - B1 * H1                                        00057700
              INT2(I,L)=INT2(I,L) + (1.D0 - DEXP(-ABST)) / ABST         00057800
              INT4(I,L)=INT4(I,L) +                                     00057900
     *          (1.D0 - DEXP(-ABST)*(ABST+1.D0) ) * DPN /ABST/ABST/ABST 00058000
  240       CONTINUE                                                    00058100
C                                                                       00058200
            INT2(I,L)=INT2(I,L) * DELPHI * ZPR(J) *2.D0 *H5 /4.D0/PI    00058300
            INT4(I,L)=INT4(I,L) * DELPHI * ZPR(J) *2.D0 /4.D0/PI        00058400
C           WRITE ( 6, 112) I,J,INT2(I,L),INT4(I,L)                     00058500
C                                                                       00058600
C           >>>> BERECHNEN DER INTEGRALE INT1 UND INT3 MIT DEN          00058700
C           >>>> (VOLLSTAENDIGEN) ELLIPTISCHEN INTEGRALEN 1. UND 2. ART 00058800
            IF (J .NE. 4*I) THEN                                        00058900
              H1=DABS( (B2 + B2) / ( A2 + B2) )                         00059000
              CALL DCELI1(H1,ELI1,ICON)                                 00059100
              CALL DCELI2(H1,ELI2,ICON)                                 00059200
C                                                                       00059300
              H1 = DSQRT( A2 + B2 )                                     00059400
              INT1(I,L)=2.D0 * ELI1 / H1                                00059500
C             >>>> H2= B1/B2 ( WIRD BENOETIGT FUER R(I)=0.D0 )          00059600
              H2= ZPZ1(J) / 2.D0 / ZPR(J)                               00059700
              INT3(I,L)= ( ELI1*H2 + ELI2*(A1-A2*H2) / (A2-B2) )*2.D0/H100059800
C                                                                       00059900
              INT1(I,L)=INT1(I,L)  * ZPR(J) *2.D0 *H5 /4.D0/PI          00060000
              INT3(I,L)=INT3(I,L)  * ZPR(J) *2.D0 /4.D0/PI              00060100
C                                                                       00060200
            ENDIF                                                       00060300
  242     CONTINUE                                                      00060400
  244   CONTINUE                                                        00060500
C                                                                       00060600
C                                                                       00060700
C       >>>> FUER INTEGRATION VON INT1 + INT3 UEBER DER SINGULARITAET:  00060800
C       >>>>       VOM URSPRUENGLICHEN INTEGRAL WIRD INTEGRAL           00060900
C       >>>>       UEBER SCHMIEGEPARABEL                                00061000
C       >>>>       R'=R(S=0) + R1(S=0)*S + (Z1*KAPPA*WG)(S=0)*S*S/2     00061100
C       >>>>       Z'=R(S=0) + Z1(S=0)*S - (R1*KAPPA*WG)(S=0)*S*S/2     00061200
C       >>>>       SUBTRAHIERT UND INTEGRAL UEBER SCHMIEGEPARABEL       00061300
C       >>>>       GESONDERT AUSGERECHNET.                              00061400
C                                                                       00061500
C       >>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKT RECHTS VON      00061600
C       >>>> DER SINGULARITAET) (VOLLST. ELLIPT. INTEGRAL 1.+2.ART)     00061700
        DO 292 I=0 ,N4                                                  00061800
            J=I*4                                                       00061900
            H2=ZPDS(J+1)+ZPDS(J+2)                                      00062000
            H3=KAPPA(I)*ZPWG(J)                                         00062100
            H5= ( ZPR(J)+ZPR1(J)*H2+ZPZ1(J)*H3*H2*H2*.5D0 ) *2.D0       00062200
            B2=ZPR(J)* H5                                               00062300
            A2=ZPG(J)*H2*H2*(1.D0 + H3*H3*H2*H2*.25D0) + B2             00062400
            H6=-ZPZ1(J)+ZPR1(J)*H3*H2                                   00062500
            B1= H6*ZPR(J)                                               00062600
            A1=ZPG(J)*H3*H2*H2*.5D0 + B1                                00062700
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00062800
            CALL DCELI1(H1,ELI1,ICON)                                   00062900
            CALL DCELI2(H1,ELI2,ICON)                                   00063000
            ISR1SP(I)=2.D0 * ELI1 / DSQRT( A2 + B2 )                    00063100
            ISR1SP(I)=ISR1SP(I) * H5 * ZPWG(J)   /4.D0 /PI              00063200
            ISR1SP(I)=ISR1SP(I) * DSQRT(1.D0 + H3*H3*H2*H2)             00063300
            ISR3SP(I)=ELI1 *H6 + (A1*H5 - A2*H6) * ELI2 / (A2 - B2)     00063400
            ISR3SP(I)= -ISR3SP(I) *2.D0 /DSQRT( A2 + B2 ) /4.D0 /PI     00063500
C           WRITE( 6,111) I,ISR1SP(I), INT1(I,I+1)                      00063600
  292   CONTINUE                                                        00063700
C                                                                       00063800
C       >>>> INTEGRATION IN PHI-RICHTUNG (ZWISCHENPUNKT LINKS  VON      00063900
C       >>>> DER SINGULARITAET) (VOLLST. ELLIPT. INTGRAL 1.+2.ART)      00064000
        DO 294 I=0,N4                                                   00064100
            J=I*4                                                       00064200
            H2=ZPDS(1)                                                  00064300
            IF (I .NE. 0) H2=-ZPDS(J  )-ZPDS(J-1)                       00064400
            H3=KAPPA(I)*ZPWG(J)                                         00064500
            H5= ( ZPR(J)+ZPR1(J)*H2+ZPZ1(J)*H3*H2*H2*.5D0 ) *2.D0       00064600
            B2=ZPR(J)* H5                                               00064700
            A2=ZPG(J)*H2*H2*(1.D0 + H3*H3*H2*H2*.25D0) + B2             00064800
            H6=-ZPZ1(J)+ZPR1(J)*H3*H2                                   00064900
            B1= H6*ZPR(J)                                               00065000
            A1=ZPG(J)*H3*H2*H2*.5D0 + B1                                00065100
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00065200
            CALL DCELI1(H1,ELI1,ICON)                                   00065300
            CALL DCELI2(H1,ELI2,ICON)                                   00065400
            ISL1SP(I)=2.D0 * ELI1 / DSQRT( A2 + B2 )                    00065500
            ISL1SP(I)=ISL1SP(I) * H5 * ZPWG(J)   /4.D0 /PI              00065600
            ISL1SP(I)=ISL1SP(I) * DSQRT(1.D0 + H3*H3*H2*H2)             00065700
            ISL3SP(I)=ELI1 *H6 + (A1*H5 - A2*H6) * ELI2 / (A2 - B2)     00065800
            ISL3SP(I)= -ISL3SP(I) *2.D0 /DSQRT( A2 + B2 ) /4.D0 /PI     00065900
C           WRITE( 6,111) I,ISL1SP(I), INT1(I,I  )                      00066000
  294   CONTINUE                                                        00066100
C                                                                       00066200
C                                                                       00066300
C       >>>>> INTEGRATION IN PHI-RICHTUNG UND IN S-RICHTUNG UEBER       00066400
C       >>>>> DEN BEZUGSPUNKT                                           00066500
        DO 296 I=1,N4                                                   00066600
          J=I*4                                                         00066700
C                                                                       00066800
          DSIG=(ZPDS(J+1)+ZPDS(J+2))/N1SIG                              00066900
          DO 298 M=1,NSIG                                               00067000
            H3=M                                                        00067100
            H2=(H3     )*DSIG                                           00067200
            H3=KAPPA(I)*ZPWG(J)                                         00067300
            H5= ( ZPR(J)+ZPR1(J)*H2+ZPZ1(J)*H3*H2*H2*.5D0 ) *2.D0       00067400
            B2=ZPR(J)* H5                                               00067500
            A2=ZPG(J)*H2*H2*(1.D0 + H3*H3*H2*H2*.25D0) + B2             00067600
            H6=-ZPZ1(J)+ZPR1(J)*H3*H2                                   00067700
            B1= H6*ZPR(J)                                               00067800
            A1=ZPG(J)*H3*H2*H2*.5D0 + B1                                00067900
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00068000
            CALL DCELI1(H1,ELI1,ICON)                                   00068100
            CALL DCELI2(H1,ELI2,ICON)                                   00068200
            HINT1(M)= 2.D0 * ELI1 / DSQRT( A2 + B2 )                    00068300
     *                          * DSQRT(1.D0 + H3*H3*H2*H2) *H5*ZPWG(J) 00068400
            HINT3(M)= -(ELI1 *H6 + (A1*H5 - A2*H6) * ELI2 / (A2 - B2))  00068500
     *                            *2.D0 /DSQRT( A2 + B2 )               00068600
  298     CONTINUE                                                      00068700
          IS1SP(I)=0.D0                                                 00068800
          IS3SP(I)=0.D0                                                 00068900
          DO 327 M=1,NSIG                                               00069000
            H9=1.D0                                                     00069100
            IF (M .EQ. 1     ) H9= 9.D0/24.D0 + 1.D0 + 1.D0/DLOG(2.D0)  00069200
            IF (M .EQ. 2     ) H9=28.D0/24.D0        - 1.D0/DLOG(2.D0)  00069300
            IF (M .EQ. 3     ) H9=23.D0/24.D0                           00069400
            IF (M .EQ. NSIG  ) H9=23.D0/24.D0                           00069500
            IF (M .EQ. NSIG-1) H9=28.D0/24.D0                           00069600
            IF (M .EQ. NSIG-2) H9= 9.D0/24.D0                           00069700
            IS1SP(I) = IS1SP(I) + HINT1(M)* H9 *DSIG /4.D0 /PI          00069800
            IS3SP(I) = IS3SP(I) + HINT3(M)* H9 *DSIG /4.D0 /PI          00069900
  327     CONTINUE                                                      00070000
C                                                                       00070100
          DSIG=(ZPDS(J  )+ZPDS(J-1))/N1SIG                              00070200
          DO 299 M=1,NSIG                                               00070300
            H3=M                                                        00070400
            H2=-(H3     )*DSIG                                          00070500
            H3=KAPPA(I)*ZPWG(J)                                         00070600
            H5= ( ZPR(J)+ZPR1(J)*H2+ZPZ1(J)*H3*H2*H2*.5D0 ) *2.D0       00070700
            B2=ZPR(J)* H5                                               00070800
            A2=ZPG(J)*H2*H2*(1.D0 + H3*H3*H2*H2*.25D0) + B2             00070900
            H6=-ZPZ1(J)+ZPR1(J)*H3*H2                                   00071000
            B1= H6*ZPR(J)                                               00071100
            A1=ZPG(J)*H3*H2*H2*.5D0 + B1                                00071200
            H1=DABS( (B2 + B2) / ( A2 + B2) )                           00071300
            CALL DCELI1(H1,ELI1,ICON)                                   00071400
            CALL DCELI2(H1,ELI2,ICON)                                   00071500
            HINT1(M)= 2.D0 * ELI1 / DSQRT( A2 + B2 )                    00071600
     *                          * DSQRT(1.D0 + H3*H3*H2*H2) *H5*ZPWG(J) 00071700
            HINT3(M)= -(ELI1 *H6 + (A1*H5 - A2*H6) * ELI2 / (A2 - B2))  00071800
     *                            *2.D0 /DSQRT( A2 + B2 )               00071900
  299     CONTINUE                                                      00072000
          DO 309 M=1,NSIG                                               00072100
            H9=1.D0                                                     00072200
            IF (M .EQ. 1     ) H9= 9.D0/24.D0 + 1.D0 + 1.D0/DLOG(2.D0)  00072300
            IF (M .EQ. 2     ) H9=28.D0/24.D0        - 1.D0/DLOG(2.D0)  00072400
            IF (M .EQ. 3     ) H9=23.D0/24.D0                           00072500
            IF (M .EQ. NSIG  ) H9=23.D0/24.D0                           00072600
            IF (M .EQ. NSIG-1) H9=28.D0/24.D0                           00072700
            IF (M .EQ. NSIG-2) H9= 9.D0/24.D0                           00072800
            IS1SP(I) = IS1SP(I) + HINT1(M)* H9 *DSIG /4.D0 /PI          00072900
            IS3SP(I) = IS3SP(I) + HINT3(M)* H9 *DSIG /4.D0 /PI          00073000
  309     CONTINUE                                                      00073100
C                                                                       00073200
  296   CONTINUE                                                        00073300
C                                                                       00073400
        I=0                                                             00073500
          J=I*4                                                         00073600
C                                                                       00073700
          H8=0.D0                                                       00073800
          H9=0.D0                                                       00073900
          DSIG=(ZPDS(J+1)+ZPDS(J+2))/N1SIG                              00074000
          DO 303 M=1,NSIG                                               00074100
            H3=M                                                        00074200
            H2=(H3     )*DSIG                                           00074300
            H3=KAPPA(I)*ZPWG(J)                                         00074400
            H1= ZPR1(J) + ZPZ1(J)*H3*H2*.5D0                            00074500
            H4=DSQRT( 1.D0 + H3*H3*H2*H2*.25D0 )                        00074600
            H5=DSQRT( 1.D0 + H3*H3*H2*H2 )                              00074700
            HINT1(M)= 2.D0 * PI * H1 * H5 / H4                          00074800
            HINT3(M)= -PI * H1 * KAPPA(I) /H4/H4/H4                     00074900
  303     CONTINUE                                                      00075000
          HINT1(0)= 2.D0 * PI * ZPR1(J)                                 00075100
          HINT3(0)= -PI * ZPR1(J) * KAPPA(I)                            00075200
          IS1SP(I)=0.D0                                                 00075300
          IS3SP(I)=0.D0                                                 00075400
          DO 311 M=0,NSIG                                               00075500
            H9=1.D0                                                     00075600
            IF (M .EQ. 0     ) H9=12.D0/24.D0                           00075700
            IF (M .EQ. NSIG  ) H9=23.D0/24.D0                           00075800
            IF (M .EQ. NSIG-1) H9=28.D0/24.D0                           00075900
            IF (M .EQ. NSIG-2) H9= 9.D0/24.D0                           00076000
            IS1SP(I) = IS1SP(I) + HINT1(M)* H9 *DSIG /4.D0 /PI          00076100
            IS3SP(I) = IS3SP(I) + HINT3(M)* H9 *DSIG /4.D0 /PI          00076200
  311     CONTINUE                                                      00076300
C                                                                       00076400
C                                                                       00076500
C                                                                       00076600
C       DO 789 I=0,N4                                                   00076700
C          WRITE (6,111) I, ISL1KE(I),ISR1KE(I),IS1KE(I)                00076800
C          WRITE (6,111) I, INT1(I,I),INT1(I,I+1)                       00076900
C 789   CONTINUE                                                        00077000
C                                                                       00077100
C       **************************************************              00077200
C       ***** AUFSTELLEN DES LIN. GLEICHUNGSSYSTEMS   ****              00077300
C       *****     FUER RHO UND RHON = GRAD(RHO) * N   ****              00077400
C       *****              AM RANDE                   ****              00077500
C       **************************************************              00077600
C                                                                       00077700
        DO 250 J=1,N4                                                   00077800
          L=2*J                                                         00077900
          H5=ZPDS(2*L  )+ZPDS(2*L-1)                                    00078000
          H6=ZPDS(2*L+1)+ZPDS(2*L+2)                                    00078100
C         >>> ABSCHAETZUNG MIT A + B*X + C*X*X (NICHT-AEQUIDISTANT)     00078200
          H1=( 2.D0*H5*H5 + H5*H6 - H6*H6 ) /H5 /6.D0                   00078300
          H3=( 2.D0*H6*H6 + H5*H6 - H5*H5 ) /H6 /6.D0                   00078400
          H2=  H5 + H6 - H1 - H3                                        00078500
          DO 252 I=0,N4                                                 00078600
            K=4*N4-L                                                    00078700
            ARHO(     I+1,     J+1)=  (INT1(I,L) + INT1(I,K))*H2        00078800
     *                           +  (INT1(I,L+1)+INT1(I,K-1))*H3        00078900
     *                           +  (INT1(I,L-1)+INT1(I,K+1))*H1        00079000
            ARHO(     I+1,N4+1+J+1)=  (INT3(I,L) + INT3(I,K))*H2        00079100
     *                           +  (INT3(I,L+1)+INT3(I,K-1))*H3        00079200
     *                           +  (INT3(I,L-1)+INT3(I,K+1))*H1        00079300
            ARHO(N4+1+I+1,     J+1)=  (INT2(I,L) + INT2(I,K))*H2        00079400
     *                           +  (INT2(I,L+1)+INT2(I,K-1))*H3        00079500
     *                           +  (INT2(I,L-1)+INT2(I,K+1))*H1        00079600
            ARHO(N4+1+I+1,N4+1+J+1)=  (INT4(I,L) + INT4(I,K))*H2        00079700
     *                           +  (INT4(I,L+1)+INT4(I,K-1))*H3        00079800
     *                           +  (INT4(I,L-1)+INT4(I,K+1))*H1        00079900
  252     CONTINUE                                                      00080000
  250   CONTINUE                                                        00080100
C                                                                       00080200
        J=N4                                                            00080300
          DO 256 I=0,N4                                                 00080400
            ARHO(     I+1,     J+1)=ARHO(     I+1,     J+1)*.5D0        00080500
            ARHO(     I+1,N4+1+J+1)=ARHO(     I+1,N4+1+J+1)*.5D0        00080600
            ARHO(N4+1+I+1,     J+1)=ARHO(N4+1+I+1,     J+1)*.5D0        00080700
            ARHO(N4+1+I+1,N4+1+J+1)=ARHO(N4+1+I+1,N4+1+J+1)*.5D0        00080800
  256   CONTINUE                                                        00080900
C                                                                       00081000
C                                                                       00081100
        J=0                                                             00081200
          K=4*N4                                                        00081300
C         >>>> ABSCHAETZUNG MIT A + B*X + C*X*X*X (AEQUIDISTANT)        00081400
          H1=  ZPDS(1) *1.D0/3.D0                                       00081500
          H2=  ZPDS(1) *4.D0/3.D0                                       00081600
          H3=  ZPDS(1) *1.D0/3.D0                                       00081700
          DO 258 I=0,N4                                                 00081800
            ARHO(     I+1,     J+1)=    (INT1(I,0  )+INT1(I,K  ))*H2    00081900
     *                           +      (INT1(I,1  )+INT1(I,K-1))*H3    00082000
            ARHO(     I+1,N4+1+J+1)=    (INT3(I,0  )+INT3(I,K  ))*H2    00082100
     *                           +      (INT3(I,1  )+INT3(I,K-1))*H3    00082200
            ARHO(N4+1+I+1,     J+1)=    (INT2(I,0  )+INT2(I,K  ))*H2    00082300
     *                           +      (INT2(I,1  )+INT2(I,K-1))*H3    00082400
            ARHO(N4+1+I+1,N4+1+J+1)=    (INT4(I,0  )+INT4(I,K  ))*H2    00082500
     *                           +      (INT4(I,1  )+INT4(I,K-1))*H3    00082600
  258     CONTINUE                                                      00082700
C                                                                       00082800
C                                                                       00082900
C       >>>>  I=J   (FUER INT1 + INT3)                                  00083000
C                                                                       00083100
        DO 673 J=1,N4-1                                                 00083200
          I=J                                                           00083300
          L=2*J                                                         00083400
          K=4*N4-L                                                      00083500
          H5=ZPDS(2*L  )+ZPDS(2*L-1)                                    00083600
          H6=ZPDS(2*L+1)+ZPDS(2*L+2)                                    00083700
C         >>> ABSCHAETZUNG MIT A + B*X + C*X*X (NICHT-AEQUIDISTANT)     00083800
          H1=( 2.D0*H5*H5 + H5*H6 - H6*H6 ) /H5 /6.D0                   00083900
          H3=( 2.D0*H6*H6 + H5*H6 - H5*H5 ) /H6 /6.D0                   00084000
          H2=  H5 + H6 - H1 - H3                                        00084100
            H9=  (INT1(I,L+1) - ISR1SP(I))*H3                           00084200
     *        +  (INT1(I,L-1) - ISL1SP(I))*H1   +  IS1SP(I)             00084300
     *        +  INT1(I,K-1)*H3 + INT1(I,K)*H2 + INT1(I,K+1)*H1         00084400
          ARHO(     I+1,     J+1)= H9                                   00084500
            H9=  (INT3(I,L+1) - ISR3SP(I))*H3                           00084600
     *        +  (INT3(I,L-1) - ISL3SP(I))*H1   +  IS3SP(I)             00084700
     *        +  INT3(I,K-1)*H3 + INT3(I,K)*H2 + INT3(I,K+1)*H1         00084800
          ARHO(     I+1,N4+1+J+1)= H9                                   00084900
  673   CONTINUE                                                        00085000
C                                                                       00085100
        J=N4                                                            00085200
          I=J                                                           00085300
          L=2*J                                                         00085400
          K=4*N4-L                                                      00085500
          H5=ZPDS(2*L  )+ZPDS(2*L-1)                                    00085600
          H6=ZPDS(2*L+1)+ZPDS(2*L+2)                                    00085700
C         >>> ABSCHAETZUNG MIT A + B*X + C*X*X (NICHT-AEQUIDISTANT)     00085800
          H1=( 2.D0*H5*H5 + H5*H6 - H6*H6 ) /H5 /6.D0                   00085900
          H3=( 2.D0*H6*H6 + H5*H6 - H5*H5 ) /H6 /6.D0                   00086000
          H2=  H5 + H6 - H1 - H3                                        00086100
            H9=  (INT1(I,L+1) - ISR1SP(I))*H3                           00086200
     *        +  (INT1(I,L-1) - ISL1SP(I))*H1   +  IS1SP(I)             00086300
          ARHO(     I+1,     J+1)= H9                                   00086400
            H9=  (INT3(I,L+1) - ISR3SP(I))*H3                           00086500
     *        +  (INT3(I,L-1) - ISL3SP(I))*H1   +  IS3SP(I)             00086600
          ARHO(     I+1,N4+1+J+1)= H9                                   00086700
C                                                                       00086800
C                                                                       00086900
C       >>>>ABSCHAETZUNG FUER INT1 UND INT3 UND INT4 BEI I=J=0          00087000
C       >>>>ABSCH. MIT A + B*X*X + C*X*X*X*X    (AEQUIDISTANT)          00087100
C       >>>>ABSCHAETZUNG FUER INT1 UND INT3 UND INT4 BEI I=0 , J=N4+N4  00087200
C       >>>>ABSCH. MIT A + B*X + C*X*X*X    (AEQUIDISTANT)              00087300
        J=0                                                             00087400
          I=J                                                           00087500
          K=4*N4                                                        00087600
          H1=  ZPDS(1  ) *12.D0/45.D0                                   00087700
          H2=  ZPDS(1  ) *64.D0/45.D0                                   00087800
          H3=  ZPDS(1  ) *14.D0/45.D0                                   00087900
          H5=  ZPDS(1  ) *1.D0/3.D0                                     00088000
          H6=  ZPDS(1  ) *4.D0/3.D0                                     00088100
          H7=  ZPDS(1  ) *1.D0/3.D0                                     00088200
            H9=  (INT1(I,0  ) - ISL1SP(0))*H2  +  INT1(I,K  )*H6        00088300
     *        +  (INT1(I,1  ) - ISR1SP(0))*H3  +  INT1(I,K-1)*H7        00088400
     *        +   IS1SP(I)                                              00088500
          ARHO(     I+1,     J+1)= H9                                   00088600
            H9=  (INT3(I,0  ) - ISL3SP(0))*H2  +  INT3(I,K  )*H6        00088700
     *        +  (INT3(I,1  ) - ISR3SP(0))*H3  +  INT3(I,K-1)*H7        00088800
     *        +   IS3SP(I)                                              00088900
          ARHO(     I+1,N4+1+J+1)= H9                                   00089000
            H9=   INT4(I,0  )*H2  +  INT4(I,1  )*H3                     00089100
     *        +   INT4(I,K  )*H6  +  INT4(I,K-1)*H7                     00089200
          ARHO(N4+1+I+1,N4+1+J+1)= H9                                   00089300
C                                                                       00089400
C       >>>>ABSCHAETZUNG FUER INT4     BEI I=J=N4                       00089500
C       >>>>ABSCH. MIT A + B*X*X*X*X    (AEQUIDISTANT)                  00089600
        I=N4                                                            00089700
          J=N4                                                          00089800
          L=2*J                                                         00089900
          H1=  (ZPDS(2*L)+ZPDS(2*L-1)) *1.D0/5.D0                       00090000
          H2=  (ZPDS(2*L)+ZPDS(2*L-1)) *8.D0/5.D0                       00090100
          H3=  (ZPDS(2*L)+ZPDS(2*L-1)) *1.D0/5.D0                       00090200
            H9=   INT4(I,L-1)*H1  +  INT4(I,L  )*H2  +  INT4(I,L+1)*H3  00090300
          ARHO(N4+1+I+1,N4+1+J+1)= H9                                   00090400
C                                                                       00090500
C                                                                       00090600
C                                                                       00090700
C       >>>>>>>> AUSRECHEN DER KONTROLLGROESSEN !!!!!!                  00090800
C       >>>>>>>> INT3CL(I) = -1/2        (FUER ALLE I UND ALLE FORMEN)  00090900
C       >>>>>>>> UND FUER KUGEL MIT RADIUS R MUSS GELTEN:               00091000
C       >>>>>>>> INT1CL(I) =   R         (FUER ALLE I)                  00091100
C       >>>>>>>> INT2CL(I) = INT2CL(J)   (FUER ALLE I,J)                00091200
C       >>>>>>>> INN4CL(I) = INT4CL(J)   (FUER ALLE I,J)                00091300
        DO 602 I=0,N4                                                   00091400
          INT1CL(I)=0.D0                                                00091500
          INT2CL(I)=0.D0                                                00091600
          INT3CL(I)=0.D0                                                00091700
          INT4CL(I)=0.D0                                                00091800
          DO 601 J=0,N4                                                 00091900
            INT1CL(I)=INT1CL(I)+ARHO(     I+1,     J+1)                 00092000
            INT2CL(I)=INT2CL(I)+ARHO(N4+1+I+1,     J+1)                 00092100
            INT3CL(I)=INT3CL(I)+ARHO(     I+1,N4+1+J+1)                 00092200
            INT4CL(I)=INT4CL(I)+ARHO(N4+1+I+1,N4+1+J+1)                 00092300
C           WRITE (6,112) I,J,ARHO(N4+1+I+1,     J+1)                   00092400
  601     CONTINUE                                                      00092500
  602   CONTINUE                                                        00092600
  111   FORMAT(1X,I2,4(2X,D14.7))                                       00092700
  112   FORMAT(1X,I2,2X,I2,4(2X,D14.7))                                 00092800
C                                                                       00092900
C                                                                       00093000
C                                                                       00093100
C       >>>>>>> KORREKTURTEME NUR FUER KUGEL UND T9 = -1                00093200
C       IF (T9. EQ. -1) THEN                                            00093300
C       DO 777 I=0,N4                                                   00093400
C         KORR1(I) = INT1KG/INT1CL(I)                                   00093500
C         KORR2(I) = INT2KG/INT2CL(I)                                   00093600
C         KORR3(I) = INT3KG/INT3CL(I)                                   00093700
C         KORR4(I) = INT4KG/INT4CL(I)                                   00093800
C         IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                          00093900
C           WRITE ( 6,111) I,KORR1(I),KORR2(I),KORR3(I),KORR4(I)        00094000
C         ENDIF                                                         00094100
C 777   CONTINUE                                                        00094200
C       GOTO 1111                                                       00094300
C       ENDIF                                                           00094400
C                                                                       00094500
C       >>>>>>> KORREKTURTEME (ALS KONTROLLGROESSE DIENT INT3)          00094600
        DO 777 I=0,N4                                                   00094700
          KORR3(I) = -.5D0 /INT3CL(I)                                   00094800
          KORR4(I) = KORR3(I)                                           00094900
          KORR2(I) = 1.D0                                               00095000
          KORR1(I) = 1.D0                                               00095100
  777   CONTINUE                                                        00095200
C                                                                       00095300
C                                                                       00095400
        DO 260 I=0,N4                                                   00095500
          DO 260 J=0,N4                                                 00095600
            ARHO(     I+1,     J+1)= ARHO(     I+1,     J+1) * KORR1(I) 00095700
            ARHO(     I+1,N4+1+J+1)= ARHO(     I+1,N4+1+J+1) * KORR3(I) 00095800
            ARHO(N4+1+I+1,     J+1)= ARHO(N4+1+I+1,     J+1) * KORR2(I) 00095900
            ARHO(N4+1+I+1,N4+1+J+1)= ARHO(N4+1+I+1,N4+1+J+1) * KORR4(I) 00096000
C           >>>>>> MERKEN DER FUER DAS GESCHW.POT. BENOETIGTEN WERTE    00096100
            AGPOT(     I+1,     J+1)=   ARHO(     I+1,     J+1)         00096200
            AGPOT(     I+1,N4+1+J+1)=   ARHO(     I+1,N4+1+J+1)         00096300
C           >>>>>> FAKTOREN UND VORZEICHEN FUER DIE RHO-GLEICHUNGEN     00096400
            H1 = ARHO(     I+1,     J+1)                                00096500
            H3 = ARHO(     I+1,N4+1+J+1)                                00096600
            H2 = ARHO(N4+1+I+1,     J+1)                                00096700
            H4 = ARHO(N4+1+I+1,N4+1+J+1)                                00096800
            IF (PROB .NE. 'J') THEN                                     00096900
              ARHO(     I+1,     J+1)=-DIFK * H1                        00097000
              ARHO(     I+1,N4+1+J+1)= H3                               00097100
              ARHO(N4+1+I+1,     J+1)= -H2 + (1.D0 - DIFK) * H1         00097200
              ARHO(N4+1+I+1,N4+1+J+1)= H4                               00097300
            ENDIF                                                       00097400
  260   CONTINUE                                                        00097500
C                                                                       00097600
        IF (PROB .EQ. 'J') THEN                                         00097700
C                                                                       00097800
        DO 1150 I=0,N4                                                  00097900
          DO 1152 J=0,N4                                                00098000
            TH1=DATAN2(ZPR(4*I  ),ZPZ(4*I  ))                           00098100
            TH2=DATAN2(ZPR(4*J  ),ZPZ(4*J  ))                           00098200
            TH3=DATAN2(ZPR(4*J+2),ZPZ(4*J+2))                           00098300
C           WRITE(6,112) I,J,TH1,TH2,TH3                                00098400
            H9=0.D0                                                     00098500
            H8=0.D0                                                     00098600
            H7=0.D0                                                     00098700
            H5=0.D0                                                     00098800
            DELTH= (TH3 -TH2)/ 20.D0                                    00098900
            DO 1154 L=0, 20-1                                           00099000
              TH4=L                                                     00099100
              TH4=TH2 + ( TH4+.5D0) * DELTH                             00099200
              DO 1156 M=0,NPHI-1                                        00099300
                H3=M                                                    00099400
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00099500
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00099600
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00099700
                DPN=DPN * B0                                            00099800
                H9= H9 + DSIN(TH4)/ABST *DELTH                          00099900
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))*DELTH    00100000
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST *DELTH    00100100
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00100200
     *                 * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) ) *DELTH     00100300
 1156         CONTINUE                                                  00100400
              TH4=PI-TH4                                                00100500
              DO 1158 M=0,NPHI-1                                        00100600
                H3=M                                                    00100700
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00100800
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00100900
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00101000
                DPN=DPN * B0                                            00101100
                H9= H9 + DSIN(TH4)/ABST *DELTH                          00101200
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))*DELTH    00101300
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST *DELTH    00101400
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00101500
     *                 * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) ) *DELTH     00101600
 1158         CONTINUE                                                  00101700
 1154       CONTINUE                                                    00101800
            IF (J .EQ. 0) GOTO 1160                                     00101900
            TH1=DATAN2(ZPR(4*I  ),ZPZ(4*I  ))                           00102000
            TH2=DATAN2(ZPR(4*J-2),ZPZ(4*J-2))                           00102100
            TH3=DATAN2(ZPR(4*J  ),ZPZ(4*J  ))                           00102200
C           WRITE(6,112) I,J,TH1,TH2,TH3                                00102300
            DELTH= (TH3 -TH2)/ 20.D0                                    00102400
            DO 1164 L=0, 20-1                                           00102500
              TH4=L                                                     00102600
              TH4=TH2 + ( TH4+.5D0) * DELTH                             00102700
              DO 1166 M=0,NPHI-1                                        00102800
                H3=M                                                    00102900
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00103000
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00103100
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00103200
                DPN=DPN * B0                                            00103300
                H9= H9 + DSIN(TH4)/ABST *DELTH                          00103400
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))*DELTH    00103500
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST *DELTH    00103600
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00103700
     *                 * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) ) *DELTH     00103800
 1166         CONTINUE                                                  00103900
              TH4=PI-TH4                                                00104000
              DO 1168 M=0,NPHI-1                                        00104100
                H3=M                                                    00104200
                H4= DCOS( (H3+ .5D0) * DELPHI )                         00104300
                DPN=DSIN(TH1)*DSIN(TH4)*H4+DCOS(TH1)*DCOS(TH4)-1.D0     00104400
                ABST=DSQRT( -2.D0 * B0 * B0 * DPN )                     00104500
                DPN=DPN * B0                                            00104600
                H9= H9 + DSIN(TH4)/ABST *DELTH                          00104700
                H8= H8 + DSIN(TH4)/ABST * (1.D0 - DEXP(-ABST))*DELTH    00104800
                H7= H7 + DSIN(TH4) * DPN / ABST / ABST / ABST *DELTH    00104900
                H5= H5 + DSIN(TH4) * DPN / ABST / ABST / ABST           00105000
     *                 * ( 1.D0 - DEXP(-ABST) *(ABST+1.D0) ) *DELTH     00105100
 1168         CONTINUE                                                  00105200
 1164       CONTINUE                                                    00105300
 1160       CONTINUE                                                    00105400
            H9=H9     *B0*B0*DELPHI    / 2.D0 / PI                      00105500
            H8=H8     *B0*B0*DELPHI    / 2.D0 / PI                      00105600
            H7=H7     *B0*B0*DELPHI    / 2.D0 / PI                      00105700
            H5=H5     *B0*B0*DELPHI    / 2.D0 / PI                      00105800
            CRHO(     I+1,     J+1)=H9                                  00105900
            CRHO(N4+1+I+1,     J+1)=H8                                  00106000
            CRHO(     I+1,N4+1+J+1)=H7                                  00106100
            CRHO(N4+1+I+1,N4+1+J+1)=H5                                  00106200
            IF (J .EQ. N4) CRHO(     I+1,     J+1) = H9*.5D0            00106300
            IF (J .EQ. N4) CRHO(N4+1+I+1,     J+1) = H8*.5D0            00106400
            IF (J .EQ. N4) CRHO(     I+1,N4+1+J+1) = H7*.5D0            00106500
            IF (J .EQ. N4) CRHO(N4+1+I+1,N4+1+J+1) = H5*.5D0            00106600
  113       FORMAT(1X,A5,1X,I4,1X,I4,4(2X,D12.5))                       00106700
C     WRITE (6,113) 'INT1:',I,J, ARHO(     I+1,J+1), CRHO(     I+1,J+1) 00106800
C     WRITE (6,113) 'INT2:',I,J, ARHO(N4+1+I+1,J+1), CRHO(N4+1+I+1,J+1) 00106900
C     WRITE (6,113) 'INT3:',I,J, ARHO(I+1,N4+1+J+1), CRHO(I+1,N4+1+J+1) 00107000
C           L=N4+2                                                      00107100
C     WRITE (6,113) 'INT4:',I,J, ARHO( I + L , J+L), CRHO( I + L , J+L) 00107200
 1152     CONTINUE                                                      00107300
 1150   CONTINUE                                                        00107400
C                                                                       00107500
C                                                                       00107600
C                                                                       00107700
C                                                                       00107800
C                                                                       00107900
          DO 1201 I=0,N4                                                00108000
            RHO(I)=-INT1KG                                              00108100
            BRHO(I+1)= RHO(I) * (.5D0 - INT3KG)                         00108200
            DRHO(I+1)=BRHO(I+1)                                         00108300
          DO 1201 J=0,N4                                                00108400
            ARHO(     I+1,     J+1)=-ARHO(    I+1,    J+1)              00108500
            CRHO(     I+1,     J+1)=-CRHO(    I+1,    J+1)              00108600
 1201     CONTINUE                                                      00108700
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00108800
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT1 =', ICON               00108900
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00109000
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT1 (KUGELKOOR.) =', ICON  00109100
          WRITE(6,*) 'KONTROLLE FUER INT1:'                             00109200
          DO 1202 I=0,N4                                                00109300
            RHON(I)=BRHO(I+1)                                           00109400
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00109500
 1202     CONTINUE                                                      00109600
          DO 1203 I=0,N4                                                00109700
            RHO(I)=-INT2KG/(1.D0-INT4KG)                                00109800
            BRHO(I+1)= RHO(I) * (1.D0 - INT4KG)                         00109900
            DRHO(I+1)=BRHO(I+1)                                         00110000
          DO 1203 J=0,N4                                                00110100
            ARHO(     I+1,     J+1)=-ARHO(N4+1+I+1,     J+1)            00110200
            CRHO(     I+1,     J+1)=-CRHO(N4+1+I+1,    J+1)             00110300
 1203     CONTINUE                                                      00110400
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00110500
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT2 =', ICON               00110600
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00110700
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT2 (KUGELKOOR.) =', ICON  00110800
          WRITE(6,*) 'KONTROLLE FUER INT2:'                             00110900
          DO 1204 I=0,N4                                                00111000
            RHON(I)=BRHO(I+1)                                           00111100
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00111200
 1204     CONTINUE                                                      00111300
          DO 1205 I=0,N4                                                00111400
            RHON(I)=-1.D0/INT1KG                                        00111500
            BRHO(I+1)= RHON(I)* INT1KG                                  00111600
            DRHO(I+1)= BRHO(I+1)                                        00111700
          DO 1205 J=0,N4                                                00111800
            ARHO(     I+1,     J+1)= ARHO(     I+1,N4+1+J+1)            00111900
            CRHO(     I+1,     J+1)= CRHO(     I+1,N4+1+J+1)            00112000
            IF (I .EQ. J)   ARHO(I+1,I+1)=ARHO(I+1,I+1) - .5D0          00112100
            IF (I .EQ. J)   CRHO(I+1,I+1)=CRHO(I+1,I+1) - .5D0          00112200
 1205     CONTINUE                                                      00112300
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00112400
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT3 =', ICON               00112500
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00112600
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT3 (KUGELKOOR.) =', ICON  00112700
          WRITE(6,*) 'KONTROLLE FUER INT3:'                             00112800
          DO 1206 I=0,N4                                                00112900
            RHO(I)=BRHO(I+1)                                            00113000
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00113100
 1206     CONTINUE                                                      00113200
          DO 1207 I=0,N4                                                00113300
            RHON(I)=-(1.D0-INT4KG)/INT2KG                               00113400
            BRHO(I+1)= RHON(I) *INT2KG                                  00113500
            DRHO(I+1)= BRHO(I+1)                                        00113600
          DO 1207 J=0,N4                                                00113700
            ARHO(     I+1,     J+1)= ARHO(N4+1+I+1,N4+1+J+1)            00113800
            CRHO(     I+1,     J+1)= CRHO(N4+1+I+1,N4+1+J+1)            00113900
            IF (I .EQ. J)   ARHO(I+1,I+1)=ARHO(I+1,I+1) - 1.D0          00114000
            IF (I .EQ. J)   CRHO(I+1,I+1)=CRHO(I+1,I+1) - 1.D0          00114100
 1207     CONTINUE                                                      00114200
          CALL DLAX(ARHO,82,N4+1,BRHO,EPSZ,ISW,IS,VW,IP,ICON)           00114300
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT4 =', ICON               00114400
          CALL DLAX(CRHO,82,N4+1,DRHO,EPSZ,ISW,IS,VW,IP,ICON)           00114500
          IF (ICON .NE. 0) WRITE(6,*) 'ICON INT4 (KUGELKOOR.) =', ICON  00114600
          WRITE(6,*) 'KONTROLLE FUER INT4:'                             00114700
          DO 1208 I=0,N4                                                00114800
            RHO(I)=BRHO(I+1)                                            00114900
            WRITE (6,111) I, RHO(I),RHON(I),DRHO(I+1)                   00115000
 1208     CONTINUE                                                      00115100
        ENDIF                                                           00115200
C                                                                       00115300
        DO 262 I=0,N4                                                   00115400
          ARHO(     I+1,N4+1+I+1)=ARHO(     I+1,N4+1+I+1) - 0.5D0       00115500
          ARHO(N4+1+I+1,N4+1+I+1)=ARHO(N4+1+I+1,N4+1+I+1) - 1.0D0       00115600
C          >>>>>> BESTIMMEN DER INHOMOGENITAET IM LIN. GLEICH.SYST.     00115700
          BRHO(     I+1)=-TAU                                           00115800
          BRHO(N4+1+I+1)=-TAU                                           00115900
  262   CONTINUE                                                        00116000
C                                                                       00116100
C                                                                       00116200
      IF (PROB .EQ. 'R') THEN                                           00116300
C       >>>>> REDUKTION VON A <<<<<<< (EIN VERSUCH)                     00116400
C       >>>>> RHO UND RHON BEI I=1 WIRD DURCH DAS LEGEN EINER PARABEL   00116500
C       >>>>> DURCH RHO UND RHON BEI I=0 UND BEI I=2 APPROXIMIERT.      00116600
C       >>>>> ES EMPFIEHT SICH, DAS LINEARE GLEICHUNGSSYSTEM DURCH      00116700
C       >>>>> STREICHEN DER I=1-ZEILE (K=2) ZU REDUZIEREN.              00116800
C       >>>>> (FUER DIE GESCHWINDIGKEIT DES RANDES IN NORMALENRICHTUNG  00116900
C       >>>>>  NICHT GEEIGNET!!!!!!)                                    00117000
        H1=ZPDS(1)+ZPDS(2)                                              00117100
        H2=ZPDS(3)+ZPDS(4)+H1                                           00117200
        H3=(H2*H2-H1*H1)/H2/H2                                          00117300
        H4=H1*H1/H2/H2                                                  00117400
        K=2                                                             00117500
C       >>>>> DIE K.-TE ZEILE WIRD GESTRICHEN                           00117600
        DO 555 I=1,N4                                                   00117700
          L=I                                                           00117800
          IF (I .GE. K) L=I+1                                           00117900
          RARHO(   I,   1)=ARHO(   L  ,   1)+ARHO(   L  ,   2)*H3       00118000
          RARHO(   I,N4+1)=ARHO(   L  ,N4+2)+ARHO(   L  ,N4+3)*H3       00118100
          RARHO(N4+I,   1)=ARHO(N4+L+1,   1)+ARHO(N4+L+1,   2)*H3       00118200
          RARHO(N4+I,N4+1)=ARHO(N4+L+1,N4+2)+ARHO(N4+L+1,N4+3)*H3       00118300
C                                                                       00118400
          RARHO(   I,   2)=ARHO(   L  ,   3)+ARHO(   L  ,   2)*H4       00118500
          RARHO(   I,N4+2)=ARHO(   L  ,N4+4)+ARHO(   L  ,N4+3)*H4       00118600
          RARHO(N4+I,   2)=ARHO(N4+L+1,   3)+ARHO(N4+L+1,   2)*H4       00118700
          RARHO(N4+I,N4+2)=ARHO(N4+L+1,N4+4)+ARHO(N4+L+1,N4+3)*H4       00118800
C                                                                       00118900
          RBRHO(   I)=BRHO(   L  )                                      00119000
          RBRHO(N4+I)=BRHO(N4+L+1)                                      00119100
          DO 555 J=3,N4                                                 00119200
            RARHO(   I,   J)=ARHO(   L  ,   J+1)                        00119300
            RARHO(   I,N4+J)=ARHO(   L  ,N4+J+2)                        00119400
            RARHO(N4+I,   J)=ARHO(N4+L+1,   J+1)                        00119500
            RARHO(N4+I,N4+J)=ARHO(N4+L+1,N4+J+2)                        00119600
  555   CONTINUE                                                        00119700
        CALL DLAX(RARHO,80,N4+N4,RBRHO,EPSZ,ISW,IS,VW,IP,ICON)          00119800
        DO 264 I=1,N4                                                   00119900
C          >>>> RHO(I) = NAEHRSTOFFKONZENTRATION AM RANDE               00120000
C          >>>> RHON(I) = GRADIENT DER NAEHRST.KONZ. IN NORMALENRICHT.  00120100
           RHO(I) =RBRHO(N4+I)                                          00120200
           RHON(I)=RBRHO(   I)                                          00120300
  264   CONTINUE                                                        00120400
        RHO(0)=RHO(1)                                                   00120500
        RHON(0)=RHON(1)                                                 00120600
        RHO(1)=H3*RHO(0)+H4*RHO(2)                                      00120700
        RHON(1)=H3*RHON(0)+H4*RHON(2)                                   00120800
C                                                                       00120900
      ELSE                                                              00121000
C                                                                       00121100
        CALL DLAX(ARHO,82,N4+N4+2,BRHO,EPSZ,ISW,IS,VW,IP,ICON)          00121200
        DO 269 I=0,N4                                                   00121300
C          >>>> RHO(I) = NAEHRSTOFFKONZENTRATION AM RANDE               00121400
C          >>>> RHON(I) = GRADIENT DER NAEHRST.KONZ. IN NORMALENRICHT.  00121500
           RHO(I) =BRHO(N4+1+I+1)                                       00121600
           RHON(I)=BRHO(     I+1)                                       00121700
  269   CONTINUE                                                        00121800
C                                                                       00121900
      ENDIF                                                             00122000
C                                                                       00122100
C       **************************************************              00122200
C       ***** AUFSTELLEN DES LIN. GLEICHUNGSSYSTEMS   ****              00122300
C       *****     FUER GESCHWIND. IN NORMALENRICHT.   ****              00122400
C       *****              AM RANDE                   ****              00122500
C       **************************************************              00122600
C                                                                       00122700
C                                                                       00122800
C       >>>>> HIER: GPOT = GESCHW.POTENTIAL - RHO + 1/6 * (ORTSV.)**2   00122900
        DO 266 I=0,N4                                                   00123000
          GPOT(I)=-EPS*KM(I)-RHO(I)+(R(I)*R(I)+Z(I)*Z(I))/6.D0          00123100
  266   CONTINUE                                                        00123200
C                                                                       00123300
        DO 267 I=0,N4                                                   00123400
          BGPOT(I+1) = GPOT(I)/2.D0                                     00123500
          DO 267 J=0,N4                                                 00123600
C          >>>>>> BESTIMMEN DER INHOMOGENITAET IM LIN. GLEICH.SYST.     00123700
            BGPOT(I+1) = BGPOT(I+1) + AGPOT(    I+1,N4+1+J+1)* GPOT(J)  00123800
  267     CONTINUE                                                      00123900
C                                                                       00124000
C                                                                       00124100
        CALL DLAX(AGPOT,82,N4+1,BGPOT,EPSZ,ISW,IS,VW,IP,ICON)           00124200
C                                                                       00124300
C       WRITE( 6,*) 'ICON = ',ICON                                      00124400
        DO 270 I=0,N4                                                   00124500
C          >>>> GPOT(I) = GESCHWINDIGKEITSPOTENTIAL AM RANDE            00124600
C          >>>> GPOTN(I) = GESCHWINDIGKEIT IN NORMALENRICHTUNG          00124700
           GPOT(I) = -EPS*KM(I)                                         00124800
           GPOTN(I)=BGPOT(I+1) + RHON(I)                                00124900
     *                     - (Z(I)*R1(I)-R(I)*Z1(I)) / WG(I) / 3.D0     00125000
C           WRITE ( 6,111) I,RHO(I),RHON(I),GPOT(I),GPOTN(I)            00125100
  270   CONTINUE                                                        00125200
C                                                                       00125300
C                                                                       00125400
C       **************************************************              00125500
C       ***** BERECHNUNG DES TANGENTIELLEN            ****              00125600
C       *****     AUSGLEICHSTERMES                    ****              00125700
C       **************************************************              00125800
C                                                                       00125900
C       >>>> STPKT IST DIE ZEITLICHE ENTWICKLUNG DER BOGENLAENGE        00126000
C       >>>> (FUER HALBE KURVE)                                         00126100
        STPKT=0.0D0                                                     00126200
        DO 280 J=1,N4,1                                                 00126300
          H1=KAPPA(J-1)*GPOTN(J-1)                                      00126400
          H2=KAPPA(J  )*GPOTN(J)                                        00126500
          DSPKT(J)= (H1+H2)*DS(J)/2.D0                                  00126600
          STPKT   = STPKT + DSPKT(J)                                    00126700
  280   CONTINUE                                                        00126800
        STPKT=STPKT+STPKT                                               00126900
C                                                                       00127000
C       *** GAMPKT IST DER KORREKTUR-TERM IN TANGENTIELLER              00127100
C       **  RICHTUNG.                                                   00127200
        IF (NURNR .EQ. 0) THEN                                          00127300
           H1=0.D0                                                      00127400
           DO 282 I=1, N4-1,1                                           00127500
              H1=H1+DSPKT(I)                                            00127600
              GAMPKT(I)=(H1-STPKT*GAM(I))/STOT                          00127700
  282      CONTINUE                                                     00127800
        ENDIF                                                           00127900
C                                                                       00128000
        DO 284 J= 0,N4,1                                                00128100
           H1=GPOTN(J)                                                  00128200
           ZT(J)=  R1(J)*H1/WG(J)-Z1(J)*STOT*GAMPKT(J)                  00128300
           RT(J)= -Z1(J)*H1/WG(J)-R1(J)*STOT*GAMPKT(J)                  00128400
  284   CONTINUE                                                        00128500
C                                                                       00128600
C       **************************************************              00128700
C       *** VERGROESSERUNG BZW. VERKLEINERUNG VON     ****              00128800
C       *** DELT , FALLS DIE DYNAMIK ZU LANGSAM       ****              00128900
C       *** BZW. ZU SCHNELL WIRD.                     ****              00129000
C       **************************************************              00129100
C                                                                       00129200
        H1=0.D0                                                         00129300
        DO 291 J=0,N4,1                                                 00129400
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT + H1            00129500
  291   CONTINUE                                                        00129600
C       *** FUER DIE STEADY-STEATE-SUCHE SOLLTE  DELT  NICHT            00129700
C       **  VERGROESSERT WERDEN !                                       00129800
        IF (H1 .LT. STOT*.0000D0/N1 ) DELT=DELT*2.D0                    00129900
C                                                                       00130000
        DO 290 K=1,10,1                                                 00130100
           H1=DSQRT( ZT(N4)*ZT(N4) + RT(N4)*RT(N4) ) * DELT             00130200
C          >>>> FUER B0 = 2.0                                           00130210
           IF (H1 .GT. R(N4)*0.005000D0) DELT=DELT*.5D0                 00130300
C          >>>> FUER B0 = 0.5                                           00130301
C          IF (H1 .GT. R(N4)*0.000002D0) DELT=DELT*.5D0                 00130310
        DO 290 J=0,N4,1                                                 00130400
           H1=DSQRT( ZT(J)*ZT(J) + RT(J)*RT(J) ) * DELT                 00130500
C          >>>> FUER B0 = 2.0                                           00130510
           IF (H1 .GT. STOT*0.002500D0/N1) DELT=DELT*.5D0               00130600
C          >>>> FUER B0 = 0.5                                           00130601
C          IF (H1 .GT. STOT*0.000001D0/N1) DELT=DELT*.5D0               00130610
C          WRITE ( 6, 111) J,H1,STOT*.01D0/N1,DELT                      00130700
  290   CONTINUE                                                        00130800
C                                                                       00130900
C       ***************************************************             00131000
C       ****            PRINT PRINT PRINT              ****             00131100
C       ***************************************************             00131200
C                                                                       00131300
C                                                                       00131400
C       *** FALLS R(J) FUER EIN 0<J<=N4 SEHR KLEIN WIRD (TEILUNG),      00131500
C       **  WIRD DAS PROGRAMM ABGEBOCHEN.                               00131600
C       *** FALLS Z(0)      SEHR KLEIN WIRD (UEBERGANG ZUM              00131700
C       **  TORUS), WIRD DAS PROGRAMM ABGEBROCHEN.                      00131800
        DO 510 J=N4  ,1,-1                                              00131900
           IF (R(J) .LT. B0*.0050D0) TT=J                               00132000
  510   CONTINUE                                                        00132100
        IF (Z(0)      .LT. B0*.0025D0) TT=N4                            00132200
        IF (TT .NE. 0) GOTO 1700                                        00132300
        H2=T9                                                           00132400
        IF (T9 .NE. INT(H2/ZSPRT + .0000001)* ZSPRT) GOTO 1100          00132500
C                                                                       00132600
C                                                                       00132700
 1700   CONTINUE                                                        00132800
   50   FORMAT (/' T =',F13.5,'    ZEITSCHRITT =',I11,'   STOTAL =',    00132900
     *     F12.6)                                                       00133000
   59   FORMAT (' DELT =',D14.7,'     CPU-ZEIT(IN SEC.) =',I8)          00133100
   60   FORMAT(1X,' I        Z        R      DGA      RHO     RHON',    00133200
     *          '     GPOT       VN       KM')                          00133300
   64   FORMAT(1X,' I        INT1           INT2           INT3',       00133400
     *          '           INT4           KORR  ')                     00133500
   65   FORMAT(1X,I2,5(1X,D14.7))                                       00133600
   70   FORMAT(1X,I2,1X,F8.3,1X,F8.3,1X,F8.5,1X,F8.3,1X,F8.3,1X,F8.3,   00133700
     *               1X,F8.4,1X,F8.3)                                   00133800
   61   FORMAT(8(1X,F12.5))                                             00133900
   71   FORMAT(1X,'>>>>',I8,3X,I10)                                     00134000
        APR=APR+1                                                       00134100
C                                                                       00134200
        IF (SDP .EQ. 1 .OR. SDP .EQ. 2) THEN                            00134300
        CALL CLOCK(ISEC)                                                00134400
        WRITE (6,50) T8,T9,STOT                                         00134500
        WRITE (6,59) DELT,ISEC                                          00134600
        WRITE (6,60)                                                    00134700
        ENDIF                                                           00134800
C                                                                       00134900
        WRITE (22,71) APR,N+N                                           00135000
        WRITE (22,61) T8,STOT                                           00135100
        DO 550 J=0 ,N4,1                                                00135200
           WRITE (22,61) Z (J),R (J),GAM(J),RHO(J),RHON(J),             00135300
     *              GPOT(J),GPOTN(J),KM(J)                              00135400
  550   CONTINUE                                                        00135500
        DO 551 J=N4+1,N4+N4,1                                           00135600
           K=N4+N4-J                                                    00135700
           WRITE (22,61) -Z (K), R(K),1.D0-GAM(K),RHO(K),RHON(K),       00135800
     *              GPOT(K),GPOTN(K),KM(K)                              00135900
  551   CONTINUE                                                        00136000
        DO 552 J=N4+N4+1,N4+N4+N4,1                                     00136100
           K=J-N4-N4                                                    00136200
           WRITE (22,61) -Z (K),-R(K),1.D0+GAM(K),RHO(K),RHON(K),       00136300
     *              GPOT(K),GPOTN(K),KM(K)                              00136400
  552   CONTINUE                                                        00136500
        DO 553 J=N4+N4+N4+1,N4+N4+N4+N4,1                               00136600
           K=N4+N4+N4+N4-J                                              00136700
           WRITE (22,61)  Z (K),-R(K),2.D0-GAM(K),RHO(K),RHON(K),       00136800
     *              GPOT(K),GPOTN(K),KM(K)                              00136900
  553   CONTINUE                                                        00137000
C                                                                       00137100
        IF (SDP .EQ. 1) THEN                                            00137200
        DO 560 J=0 ,N4,1                                                00137300
           WRITE (6,70) J,Z(J),R(J),DS(J)/STOT,RHO(J),RHON(J),          00137400
     *              GPOT(J),GPOTN(J),KM(J)                              00137500
  560   CONTINUE                                                        00137600
        WRITE ( 6,64)                                                   00137700
        DO 561 I=0,N4                                                   00137800
           WRITE ( 6,65) I,INT1CL(I),INT2CL(I),INT3CL(I),INT4CL(I)      00137900
     *                    ,KORR3(I)                                     00138000
  561   CONTINUE                                                        00138100
        ENDIF                                                           00138200
C                                                                       00138300
        WRITE (22,71) 0,0                                               00138400
        BACKSPACE 22                                                    00138500
        IF (TT .NE. 0 .AND. TT .NE. N4) THEN                            00138600
            NI=TT                                                       00138700
            NA=N4                                                       00138800
            T9=0                                                        00138900
            R(NI)=0.D0                                                  00139000
            R(NI+1)=-R(NI-1)                                            00139100
            Z(NI+1)=Z(NI-1)                                             00139200
            T8=0.D0                                                     00139300
            GOTO 1300                                                   00139400
        ENDIF                                                           00139500
        IF (TT .NE. 0) GOTO 1200                                        00139600
C                                                                       00139700
C       **************************************************              00139800
C       ****      BERECHNEN DER NEUEN WERTE           ****              00139900
C       **************************************************              00140000
C                                                                       00140100
 1100   DO 600 J= 0,N4,1                                                00140200
           Z(J)=Z(J)+ZT(J)*DELT                                         00140300
           R(J)=R(J)+RT(J)*DELT                                         00140400
  600   CONTINUE                                                        00140500
        Z(-1)=Z(1)                                                      00140600
        R(-1)=-R(1)                                                     00140700
        R(0)=0.D0                                                       00140800
        Z(N4)=0.D0                                                      00140900
        R(N4+1)= R(N4-1)                                                00141000
        Z(N4+1)=-Z(N4-1)                                                00141100
C                                                                       00141200
C                                                                       00141300
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00141400
C       >>>>         ENDE DER ZEITSCHLEIFE            <<<<              00141500
C       >>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<              00141600
C                                                                       00141700
        IF (T9 .GE. ZSEND) GOTO 1200                                    00141800
        GOTO 1000                                                       00141900
 1200   CONTINUE                                                        00142000
        CLOSE(22,STATUS='KEEP')                                         00142100
        END                                                             00142200
C                                                                       00142300
C       >>>>>>>>>>>>>>>>>>>> SUBROUTINES <<<<<<<<<<<<<<<<<<<<<<<<<      00142400
C                                                                       00142500
        SUBROUTINE LEGPOL(X,L,RET)                                      00142600
        DOUBLE PRECISION X,X7,X8,X9,RET,DI                              00142700
        INTEGER L,I                                                     00142800
        IF (L .EQ. 0) RET=1.D0                                          00142900
        IF (L .EQ. 1) RET=X                                             00143000
        IF (L .LE. 1) RETURN                                            00143100
        X7=1.D0                                                         00143200
        X8=X                                                            00143300
        DO 10 I=2,L,1                                                   00143400
           DI=I                                                         00143500
           X9=( (DI+DI-1.D0)*X*X8 - (DI-1.D0)*X7 ) / DI                 00143600
           X7=X8                                                        00143700
           X8=X9                                                        00143800
   10   CONTINUE                                                        00143900
        RET=X9                                                          00144000
        RETURN                                                          00144100
C       END                                                             00144200
C/LKED.SYSLIB     DD   DSN=SYS1.SSL2,DISP=SHR                           00144300
C/                DD   DSN=SYS1.FORTLIB,DISP=SHR                        00144400
C/GO.SYSIN    DD *                                                      00144500
C*                                                                      00144600
C*      EXEC DRUCKE                                                     00144700
C*      END                                                             00144800
        END                                                             00144900
