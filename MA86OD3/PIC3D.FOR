C        VERSUCH VON 3D BILDERN                                         00000100
C                                                                       00000200
C ---- FESTLEGEN DES KOORDINATENSYSTEMS (HIER: 2D + KARTESISCH)         00000300
      EXTERNAL PL2CA                                                    00000400
C                                                                       00000500
      REAL*4 A(0: 50,0: 50,1:3)                                         00000600
      REAL*4 AD(0: 50,0: 50,1:3)                                        00000700
      REAL*4 HX,HY,HZ , HX1,HY1,HZ1 , HX2,HY2,HZ2 , HX3,HY3,HZ3         00000800
      REAL*4 HX1P,HY1P, HX2P,HY2P   , HX8,HY8,HZ8 , HX9,HY9,HZ9         00000900
      REAL*4 XTP(0: 50,0: 50) , YTP(0: 50,0: 50)                        00001000
      REAL*4 KX,KY,KZ,KB     , YPRO,   TH,THGRAD , PI                   00001100
      REAL*4 XMIN,XMAX,YMIN,YMAX                                        00001200
      REAL*4 D11,D12,D13,D21,D22,D23,D31,D32,D33                        00001300
      REAL*4 A11,A12,A13,A21,A22,A23,A31,A32,A33 ,DETA                  00001400
      REAL*4 B1,B2,B3, Q1,Q2,Q3  , Q1A ,Q1E , Q1S                       00001500
      REAL*4 CBG ,CBGQU ,    AB1,AB2,AB3 ,ABMIN,ABMAX   ,SW             00001600
      REAL*4 EPS1                                                       00001700
      INTEGER DEKOOR(1:5000,1:6), VD(0: 50,0: 50)                       00001800
      INTEGER I,N1,N2,NL1,NL2    , CONTR1,CONTR2,CONTR3                 00001900
      INTEGER COUNDA,COUNDE   ,K1,K2,  RICHT                            00002000
      CHARACTER DSW*30                                                  00002100
      PI=4.*ATAN(1.)                                                    00002200
C                                                                       00002300
C                                                                       00002400
C                                                                       00002500
C ---- EINGABE DER KOORDINATEN                                          00002600
C                                                                       00002700
 9554 WRITE ( 6 , *) ' NAME DER DATEN-DATEI <D14K.   .   > '            00002800
      READ  ( 6,  *) DSW                                                00002900
 9555 WRITE ( 6 , *) ' 1 = 1 BILD , 4 = 4 BILDER '                      00003000
      READ  ( 6 , *) CONTR2                                             00003100
 9556 WRITE ( 6 , *) ' FENSTERGROESSE (HORIZONTAL=X-WERTE, VERTIKAL=Y-WE00003200
     *RTE)'                                                             00003300
      WRITE ( 6 , *) ' XMIN , XMAX , YMIN , YMAX (4 WERTE, DURCH KOMMATA00003400
     * GETRENNT)'                                                       00003500
      READ  ( 6 , *) XMIN,XMAX,YMIN,YMAX                                00003600
 9557 WRITE( 6 , *) ' PUNKTPROJEKTION AUF DIE X-Z-EBENE BZGL(0,YPRO,0)' 00003700
      WRITE ( 6 , *) ' YPRO ( SOLLTE NEGATIV SEIN ) '                   00003800
      READ  ( 6 , *) YPRO                                               00003900
      WRITE ( 6 , *) ' DREHACHSE UND DREHWINKEL (IN GRAD) '             00004000
      WRITE ( 6 , *) ' KX,KY,KZ , THGRAD'                               00004100
      READ  ( 6 , *) KX,KY,KZ,THGRAD                                    00004200
 9558 WRITE ( 6 , *) ' 0 = ALLES / 1 = VERDECKTE LINIEN '               00004300
      READ  ( 6 , *) CONTR3                                             00004400
C                                                                       00004500
C ---- EINLESEN DER DATEN                                               00004600
        OPEN(22,FILE=DSW)                                               00004700
        REWIND 22                                                       00004800
        READ(22, *)  N1,N2                                              00004900
        DO 980 NL1=0,N1,1                                               00005000
        DO 980 NL2=0,N2,1                                               00005100
            READ(22, *)  A(NL1,NL2,1) , A(NL1,NL2,2) , A(NL1,NL2,3)     00005200
  980   CONTINUE                                                        00005300
        CLOSE(22,STATUS='KEEP')                                         00005400
C                                                                       00005500
C ---- BEMERKUNG: ZWISCHEN CALL PLT00 UND CALL PLTEJ KANN               00005600
C ----            NICHT INTERAKTIV GEARBEITET WERDEN !                  00005700
C                                                                       00005800
C ---- INITIALISIEREN                                                   00005900
      CALL PLT00                                                        00006000
C                                                                       00006100
      DO 9444 I=1,4,1                                                   00006200
      IF (I .GT. 1 .AND. CONTR2 .EQ. 1) GOTO 9444                       00006300
C ---- QUADRATISCHES FENSTER EROEFFNEN                                  00006400
      IF (I .EQ. 1 .AND. CONTR2 .EQ. 1)                                 00006500
     #              CALL SETVP2 ( 0.0/1.4035 , 1.0/1.4035 , 0.0 , 1.0)  00006600
      IF (I .EQ. 1 .AND. CONTR2 .EQ. 4)                                 00006700
     #              CALL SETVP2 ( 0.0/1.4035 , 0.5/1.4035 , 0.5 , 1.0)  00006800
      IF (I .EQ. 2) CALL SETVP2 ( 0.5/1.4035 , 1.0/1.4035 , 0.5 , 1.0)  00006900
      IF (I .EQ. 3) CALL SETVP2 ( 0.0/1.4035 , 0.5/1.4035 , 0.0 , 0.5)  00007000
      IF (I .EQ. 4) CALL SETVP2 ( 0.5/1.4035 , 1.0/1.4035 , 0.0 , 0.5)  00007100
C ---- RAHMEN UM DAS DEFINIERTE FENSTER ZEICHNEN                        00007200
      CALL PLTBX                                                        00007300
C ---- WERTEBEREICH DEFINIEREN                                          00007400
      CALL SETWW (XMIN,XMAX,YMIN,YMAX)                                  00007500
C                                                                       00007600
C                                                                       00007700
C ----- PROJECTIONEN                                                    00007800
C       I = 1 : DREHUNG UM WINKEL TH UM DIE (KX,KY,KZ)-ACHSE            00007900
C       I = 2 : VON VORNE   (RECHTS= X , OBEN = Z , VORNE =-Y)          00008000
C       I = 3 : VON RECHTS  (RECHTS= Y , OBEN = Z , VORNE = X)          00008100
C       I = 4 : VON OBEN    (RECHTS= X , OBEN = Y , VORNE = Z)          00008200
      IF (I .EQ. 2) THEN                                                00008300
         KX=0.                                                          00008400
         KY=0.                                                          00008500
         KZ=1.                                                          00008600
         THGRAD=0.                                                      00008700
      ENDIF                                                             00008800
      IF (I .EQ. 3) THEN                                                00008900
         KX=0.                                                          00009000
         KY=0.                                                          00009100
         KZ=1.                                                          00009200
         THGRAD=-90.                                                    00009300
      ENDIF                                                             00009400
      IF (I .EQ. 4) THEN                                                00009500
         KX=1.                                                          00009600
         KY=0.                                                          00009700
         KZ=0.                                                          00009800
         THGRAD= 90.                                                    00009900
      ENDIF                                                             00010000
      CBG=AMAX1( ABS(XMAX-XMIN) , ABS(YMAX-YMIN) ) / 4000.              00010100
      CBGQU=CBG*CBG                                                     00010200
      KB=SQRT(KX*KX + KY*KY + KZ*KZ)                                    00010300
      TH=THGRAD/180. * PI                                               00010400
      KX=KX/KB                                                          00010500
      KY=KY/KB                                                          00010600
      KZ=KZ/KB                                                          00010700
C ---- DREHMATRIX (DREHUNG UM WINKEL TH UM ACHSE (KX,KY,KZ) )           00010800
      D11=1. +  0. * SIN(TH)  -  (KZ**2+KY**2) * (1. - COS(TH))         00010900
      D12=0. -  KZ * SIN(TH)  +  ( KX * KY   ) * (1. - COS(TH))         00011000
      D13=0. +  KY * SIN(TH)  +  ( KX * KZ   ) * (1. - COS(TH))         00011100
      D21=0. +  KZ * SIN(TH)  +  ( KX * KY   ) * (1. - COS(TH))         00011200
      D22=1. +  0. * SIN(TH)  -  (KZ**2+KX**2) * (1. - COS(TH))         00011300
      D23=0. -  KX * SIN(TH)  +  ( KY * KZ   ) * (1. - COS(TH))         00011400
      D31=0. -  KY * SIN(TH)  +  ( KX * KZ   ) * (1. - COS(TH))         00011500
      D32=0. +  KX * SIN(TH)  +  ( KY * KZ   ) * (1. - COS(TH))         00011600
      D33=1. +  0. * SIN(TH)  -  (KY**2+KX**2) * (1. - COS(TH))         00011700
C ---- DREHUNG DER X,Y,Z - KOORDINATEN   UND                            00011800
C ---- PROJEKTIONEN AUF BILDEBENE AUSRECHNEN                            00011900
      ABMIN=1.E40                                                       00012000
      ABMAX=1.E-40                                                      00012100
      DO 1070 NL1=0,N1,1                                                00012200
        DO 1080 NL2=0,N2,1                                              00012300
          VD(NL1,NL2)=0                                                 00012400
          HX= D11*A(NL1,NL2,1) + D12*A(NL1,NL2,2) + D13*A(NL1,NL2,3)    00012500
          HY= D21*A(NL1,NL2,1) + D22*A(NL1,NL2,2) + D23*A(NL1,NL2,3)    00012600
          HZ= D31*A(NL1,NL2,1) + D32*A(NL1,NL2,2) + D33*A(NL1,NL2,3)    00012700
          IF ( (HY - YPRO)**2  .GT. 1.E-40) THEN                        00012800
             XTP(NL1,NL2)= HX * YPRO / (YPRO - HY)                      00012900
             YTP(NL1,NL2)= HZ * YPRO / (YPRO - HY)                      00013000
          ENDIF                                                         00013100
          AD(NL1,NL2,1) = HX                                            00013200
          AD(NL1,NL2,2) = HY                                            00013300
          AD(NL1,NL2,3) = HZ                                            00013400
C ---- MINIMALER + MAXIMALER ABSTAND VOM PROJEKTIONSPUNKT BERECHNEN     00013500
          IF (HY .GT. YPRO) THEN                                        00013600
            AB1=(HX-0.   )**2 + (HY-YPRO)**2 + (HZ-0.   )**2            00013700
            ABMIN=AMIN1( AB1 , ABMIN )                                  00013800
            ABMAX=AMAX1( AB1 , ABMAX )                                  00013900
          ENDIF                                                         00014000
 1080   CONTINUE                                                        00014100
 1070 CONTINUE                                                          00014200
      ABMIN=SQRT(ABMIN)                                                 00014300
      ABMAX=SQRT(ABMAX)                                                 00014400
C                                                                       00014500
      IF (CONTR3 .EQ. 1) THEN                                           00014600
C                                                                       00014700
C ---- DREIECKSZERLEGUNG DER OBERFLAECHE                                00014800
C ---- (ES WERDEN NUR DIEJENIGEN DREIECKE, DIE VON BENACHBARTEN         00014900
C ----  PUNKTEN AUFGESPANNT WERDEN UND NICHT ENTARTET SIND,             00015000
C ----  DURCHNUMMERIERT) ( COUNDA:   1,.....,COUNDE )                   00015100
C ----  DIE INDIZES DER DREIECKSKOORDINATEN WERDEN IN                   00015200
C ----  DEKOOR(COUNDA,I) I=1,...,6 ABGESPEICHERT.                       00015300
        COUNDA= 0                                                       00015400
        DO 1200 RICHT=1,2,1                                             00015500
           IF (RICHT .EQ. 1) K2=0                                       00015600
           IF (RICHT .EQ. 2) K2=1                                       00015700
        DO 1201 NL1=0,N1-1,1                                            00015800
        DO 1201 NL2=0,N2-1,1                                            00015900
           HX1=AD(NL1   ,NL2+1 ,1)                                      00016000
           HY1=AD(NL1   ,NL2+1 ,2)                                      00016100
           HZ1=AD(NL1   ,NL2+1 ,3)                                      00016200
           HX2=AD(NL1+K2,NL2+K2,1)                                      00016300
           HY2=AD(NL1+K2,NL2+K2,2)                                      00016400
           HZ2=AD(NL1+K2,NL2+K2,3)                                      00016500
           HX3=AD(NL1+1 ,NL2   ,1)                                      00016600
           HY3=AD(NL1+1 ,NL2   ,2)                                      00016700
           HZ3=AD(NL1+1 ,NL2   ,3)                                      00016800
           AB1=(HX1-HX2)**2 +(HY1-HY2)**2 +(HZ1-HZ2)**2                 00016900
           AB2=(HX2-HX3)**2 +(HY2-HY3)**2 +(HZ2-HZ3)**2                 00017000
           AB3=(HX3-HX1)**2 +(HY3-HY1)**2 +(HZ3-HZ1)**2                 00017100
           IF ( AB1 .GE. CBGQU .AND. AB2 .GE. CBGQU .AND.               00017200
     &          AB3 .GE. CBGQU )   THEN                                 00017300
              COUNDA=COUNDA+1                                           00017400
              DEKOOR(COUNDA,1)=NL1                                      00017500
              DEKOOR(COUNDA,2)=NL2+1                                    00017600
              DEKOOR(COUNDA,3)=NL1+K2                                   00017700
              DEKOOR(COUNDA,4)=NL2+K2                                   00017800
              DEKOOR(COUNDA,5)=NL1+1                                    00017900
              DEKOOR(COUNDA,6)=NL2                                      00018000
           ENDIF                                                        00018100
 1201   CONTINUE                                                        00018200
 1200   CONTINUE                                                        00018300
        COUNDE=COUNDA                                                   00018400
C       WRITE ( 6, *) NL1*NL2,'   ANZ. DER DREIECKE:',COUNDA            00018500
      ENDIF                                                             00018600
C                                                                       00018700
C                                                                       00018800
C                                                                       00018900
C ---- ZU JEDEM PUNKTE (HX,HY,HZ) WIRD AUSGERECHNET, WO DER SCHNITT-    00019000
C ---- PUNKT DES STRAHLES (0,YPRO,0) + Q1 * ( (HX,HY,HZ)-(0,YPRO,0) )   00019100
C ---- MIT DER DURCH JEDES DREIECK AUFGESPANNTE EBENE LIEGT.            00019200
C ---- ( VA + Q2*(VB-VA) + Q3*(VC-VA) IST DIE GLEICHUNG FUER DIE        00019300
C ----  AUFGESPANNTE FLAECHE. DIE INNEREN PUNKTE (INCL. RANDPUNKTE)     00019400
C ----  SIND DURCH Q2>=0 , Q3>=0 , Q2+Q3 <=1 CHARAKTERISIERT. )         00019500
        IF (CONTR3 .EQ. 1) THEN                                         00019600
         DO 1250 NL1=0,N1,1                                             00019700
         DO 1250 NL2=0,N2,1                                             00019800
         HX = AD(NL1,NL2,1)                                             00019900
         HY = AD(NL1,NL2,2)                                             00020000
         HZ = AD(NL1,NL2,3)                                             00020100
         DO 1252 COUNDA=1,COUNDE,1                                      00020200
           IF (VD(NL1,NL2) .EQ. 1) GOTO 1252                            00020300
           HX1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 1)           00020400
           HY1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 2)           00020500
           HZ1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 3)           00020600
           HX2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 1)           00020700
           HY2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 2)           00020800
           HZ2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 3)           00020900
           HX3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 1)           00021000
           HY3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 2)           00021100
           HZ3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 3)           00021200
           AB1=(HX -HX1)**2 +(HY -HY1)**2 +(HZ -HZ1)**2                 00021300
           AB2=(HX -HX2)**2 +(HY -HY2)**2 +(HZ -HZ2)**2                 00021400
           AB3=(HX -HX3)**2 +(HY -HY3)**2 +(HZ -HZ3)**2                 00021500
C ---- FALLS DER BETRACHTETE PUNKT EIN RANDPUNKT DES DREIECKES IST,     00021600
C ---- WIRD ER NICHT VERDECKT.                                          00021700
           IF ( AB1 .LE. CBGQU  .OR. AB2 .LE. CBGQU  .OR.               00021800
     &          AB3 .LE. CBGQU )   GOTO 1252                            00021900
           A11 = HX  - 0.                                               00022000
           A12 = HX1 - HX2                                              00022100
           A13 = HX1 - HX3                                              00022200
           A21 = HY  - YPRO                                             00022300
           A22 = HY1 - HY2                                              00022400
           A23 = HY1 - HY3                                              00022500
           A31 = HZ  - 0.                                               00022600
           A32 = HZ1 - HZ2                                              00022700
           A33 = HZ1 - HZ3                                              00022800
           DETA=A33*(A22*A11-A21*A12) - A32*(A23*A11-A21*A13)           00022900
     &                  + A31*(A23*A12-A13*A22)                         00023000
           IF (ABS(DETA) .LT. 1E-40) GOTO 1252                          00023100
           B1 = HX1  - 0.                                               00023200
           B2 = HY1  - YPRO                                             00023300
           B3 = HZ1  - 0.                                               00023400
           Q1 = (  (A33*A22 -A32*A23)*B1 - (A33*A12-A32*A13)*B2         00023500
     &              + (A23*A12-A22*A13)*B3  )                           00023600
           Q2 = ( -(A33*A21 -A31*A23)*B1 + (A33*A11-A31*A13)*B2         00023700
     &              - (A23*A11-A21*A13)*B3  )                           00023800
           Q3 = (  (A32*A21 -A31*A22)*B1 - (A32*A11-A31*A12)*B2         00023900
     &              + (A22*A11-A21*A12)*B3  )                           00024000
           Q1=Q1/DETA                                                   00024100
           Q2=Q2/DETA                                                   00024200
           Q3=Q3/DETA                                                   00024300
           AB1=SQRT( (HX -0.  )**2 +(HY -YPRO)**2 +(HZ -0.  )**2 )      00024400
C ---- FALLS Q1<1 , Q2>=0 ,Q3>=0 , Q2+Q3<=1 : VERDECKTER PUNKT          00024500
           EPS1=.001                                                    00024600
           IF (      AB1*Q1 .LT. AB1 - (ABMAX-ABMIN)*EPS1               00024700
     &      .AND.     Q2 .GE. -EPS1                                     00024800
     &      .AND.     Q3 .GE. -EPS1  .AND.  Q2+Q3 .LE. 1.+EPS1)         00024900
     &    THEN                                                          00025000
              VD(NL1,NL2)=1                                             00025100
           ENDIF                                                        00025200
 1252   CONTINUE                                                        00025300
 1250   CONTINUE                                                        00025400
      ENDIF                                                             00025500
C                                                                       00025600
C                                                                       00025700
        DO 1010 NL1=0,N1,1                                              00025800
          DO 1010 NL2=0,N2-1,1                                          00025900
            IF (VD(NL1,NL2)+VD(NL1,NL2+1) .EQ. 0) THEN                  00026000
              CALL MOVA2( XTP(NL1,NL2  ) , YTP(NL1,NL2  ) )             00026100
              CALL LINA2( XTP(NL1,NL2+1) , YTP(NL1,NL2+1) )             00026200
            ENDIF                                                       00026300
 1010   CONTINUE                                                        00026400
C                                                                       00026500
        DO 1020 NL2=0,N2,1                                              00026600
          DO 1020 NL1=0,N1-1,1                                          00026700
            IF (VD(NL1,NL2)+VD(NL1+1,NL2) .EQ. 0) THEN                  00026800
              CALL MOVA2( XTP(NL1  ,NL2  ) , YTP(NL1  ,NL2  ) )         00026900
              CALL LINA2( XTP(NL1+1,NL2  ) , YTP(NL1+1,NL2  ) )         00027000
            ENDIF                                                       00027100
 1020   CONTINUE                                                        00027200
C                                                                       00027300
C                                                                       00027400
      IF (CONTR3 .EQ. 1) THEN                                           00027500
C                                                                       00027600
        SW=.1                                                           00027700
C                                                                       00027800
        DO 1700 RICHT=1,2,1                                             00027900
          IF (RICHT .EQ. 1) THEN                                        00028000
            K1=1                                                        00028100
            K2=0                                                        00028200
          ENDIF                                                         00028300
          IF (RICHT .EQ. 2) THEN                                        00028400
            K1=0                                                        00028500
            K2=1                                                        00028600
          ENDIF                                                         00028700
        DO 1610 NL1=0,N1-K1,1                                           00028800
        DO 1610 NL2=0,N2-K2,1                                           00028900
           IF (VD(NL1,NL2)+VD(NL1+K1,NL2+K2) .EQ. 0) GOTO 1610          00029000
           IF (VD(NL1,NL2)+VD(NL1+K1,NL2+K2) .EQ. 2) GOTO 1610          00029100
C          WRITE ( 6,*)  'NL1=',NL1,'   NL2=',NL2                       00029200
C ---- ZU JEDEM PUNKTE (HX,HY,HZ) WIRD AUSGERECHNET, WO DER SCHNITT-    00029300
C ---- PUNKT DES STRAHLES (0,YPRO,0) + Q1 * ( (HX,HY,HZ)-(0,YPRO,0) )   00029400
C ---- MIT DER DURCH JEDES DREIECK AUFGESPANNTE EBENE LIEGT.            00029500
C ---- ( VA + Q2*(VB-VA) + Q3*(VC-VA) IST DIE GLEICHUNG FUER DIE        00029600
C ----  AUFGESPANNTE FLAECHE. DIE INNEREN PUNKTE (INCL. RANDPUNKTE)     00029700
C ----  SIND DURCH Q2>=0 , Q3>=0 , Q2+Q3 <=1 CHARAKTERISIERT. )         00029800
C                                                                       00029900
        Q1A=0.                                                          00030000
 1630   IF (Q1A .GT. 0.999) GOTO 1610                                   00030100
        Q1E=Q1A+SW                                                      00030200
        Q1S=Q1A+SW*.5                                                   00030300
        HX8= AD(NL1,NL2,1)                                              00030400
        HY8= AD(NL1,NL2,2)                                              00030500
        HZ8= AD(NL1,NL2,3)                                              00030600
        HX9= AD(NL1+K1,NL2+K2,1)                                        00030700
        HY9= AD(NL1+K1,NL2+K2,2)                                        00030800
        HZ9= AD(NL1+K1,NL2+K2,3)                                        00030900
        HX=HX8+Q1S*(HX9-HX8)                                            00031000
        HY=HY8+Q1S*(HY9-HY8)                                            00031100
        HZ=HZ8+Q1S*(HZ9-HZ8)                                            00031200
C ---- FALLS DER PUNKT (HX,HY,HZ) HINTER DEM PROJEKTIONSPUNKT           00031300
C ---- (0,YPRO,0) LIEGT, WIRD ER NICHT GEPLOTTET.                       00031400
        IF (HY .LE. YPRO) GOTO 1630                                     00031500
C ---- IST DER PUNKT (HX,HY,HZ) UEBERDECKT VON EINEM DER DREIECKE?      00031600
        COUNDA=0                                                        00031700
 1650   COUNDA=COUNDA+1                                                 00031800
           IF (COUNDA .GT. COUNDE) GOTO 1690                            00031900
           HX1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 1)           00032000
           HY1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 2)           00032100
           HZ1 = AD( DEKOOR(COUNDA,1) , DEKOOR(COUNDA,2) , 3)           00032200
           HX2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 1)           00032300
           HY2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 2)           00032400
           HZ2 = AD( DEKOOR(COUNDA,3) , DEKOOR(COUNDA,4) , 3)           00032500
           HX3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 1)           00032600
           HY3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 2)           00032700
           HZ3 = AD( DEKOOR(COUNDA,5) , DEKOOR(COUNDA,6) , 3)           00032800
           AB1=(HX -HX1)**2 +(HY -HY1)**2 +(HZ -HZ1)**2                 00032900
           AB2=(HX -HX2)**2 +(HY -HY2)**2 +(HZ -HZ2)**2                 00033000
           AB3=(HX -HX3)**2 +(HY -HY3)**2 +(HZ -HZ3)**2                 00033100
C ---- FALLS DER BETRACHTETE PUNKT EIN RANDPUNKT DES DREIECKES IST,     00033200
C ---- WIRD ER NICHT VERDECKT. (ES WIRD DAS NAECHSTE DREIECK GENOMMEN)  00033300
           IF ( AB1 .LE. CBGQU  .OR. AB2 .LE. CBGQU  .OR.               00033400
     &          AB3 .LE. CBGQU )   GOTO 1650                            00033500
           A11 = HX  - 0.                                               00033600
           A12 = HX1 - HX2                                              00033700
           A13 = HX1 - HX3                                              00033800
           A21 = HY  - YPRO                                             00033900
           A22 = HY1 - HY2                                              00034000
           A23 = HY1 - HY3                                              00034100
           A31 = HZ  - 0.                                               00034200
           A32 = HZ1 - HZ2                                              00034300
           A33 = HZ1 - HZ3                                              00034400
           DETA=A33*(A22*A11-A21*A12) - A32*(A23*A11-A21*A13)           00034500
     &                  + A31*(A23*A12-A13*A22)                         00034600
           IF (ABS(DETA) .LE. 1.E-30       ) GOTO 1650                  00034700
           B1 = HX1  - 0.                                               00034800
           B2 = HY1  - YPRO                                             00034900
           B3 = HZ1  - 0.                                               00035000
           Q1 = (  (A33*A22 -A32*A23)*B1 - (A33*A12-A32*A13)*B2         00035100
     &              + (A23*A12-A22*A13)*B3  )                           00035200
           Q2 = ( -(A33*A21 -A31*A23)*B1 + (A33*A11-A31*A13)*B2         00035300
     &              - (A23*A11-A21*A13)*B3  )                           00035400
           Q3 = (  (A32*A21 -A31*A22)*B1 - (A32*A11-A31*A12)*B2         00035500
     &              + (A22*A11-A21*A12)*B3  )                           00035600
           Q1=Q1/DETA                                                   00035700
           Q2=Q2/DETA                                                   00035800
           Q3=Q3/DETA                                                   00035900
           AB1=SQRT( (HX -0.  )**2 +(HY -YPRO)**2 +(HZ -0.  )**2 )      00036000
C                                                                       00036100
C ---- FALLS Q1<1 , Q2>=0 ,Q3>=0 , Q2+Q3<=1 : VERDECKTER PUNKT          00036200
        EPS1=.001                                                       00036300
        IF (      AB1*Q1 .LT. AB1 - (ABMAX-ABMIN)*EPS1                  00036400
     &      .AND.     Q2 .GE. -EPS1                                     00036500
     &      .AND.     Q3 .GE. -EPS1  .AND.  Q2+Q3 .LE. 1.+EPS1)         00036600
     &    THEN                                                          00036700
             Q1A=Q1E                                                    00036800
             GOTO 1630                                                  00036900
          ELSE                                                          00037000
             GOTO 1650                                                  00037100
          ENDIF                                                         00037200
C                                                                       00037300
C  ---- FALLS DER PUNKT NICHT UEBERDECKT IST, WIRD GEPLOTTET            00037400
 1690   IF (Q1A .LT. Q1E ) THEN                                         00037500
           Q1E=AMIN1(Q1E,1.)                                            00037600
           HX1P=XTP(NL1,NL2) + Q1A*(XTP(NL1+K1,NL2+K2) - XTP(NL1,NL2))  00037700
           HY1P=YTP(NL1,NL2) + Q1A*(YTP(NL1+K1,NL2+K2) - YTP(NL1,NL2))  00037800
           HX2P=XTP(NL1,NL2) + Q1E*(XTP(NL1+K1,NL2+K2) - XTP(NL1,NL2))  00037900
           HY2P=YTP(NL1,NL2) + Q1E*(YTP(NL1+K1,NL2+K2) - YTP(NL1,NL2))  00038000
           CALL MOVA2(HX1P,HY1P)                                        00038100
           CALL LINA2(HX2P,HY2P)                                        00038200
C          WRITE(6,*) ' PLOT VON ' ,HX1P,HY1P,' NACH ' ,HX2P,HY2P       00038300
C          WRITE(6,*) ' PLOT VON     ' ,Q1A,'    NACH    ' ,Q1E         00038400
C          WRITE(6,*) 'Q1,Q2,Q3 ',Q1,Q2,Q3,'   AB1 ',AB1                00038500
           Q1A=Q1E                                                      00038600
           GOTO 1630                                                    00038700
        ENDIF                                                           00038800
 1610 CONTINUE                                                          00038900
 1700 CONTINUE                                                          00039000
      ENDIF                                                             00039100
C                                                                       00039200
C                                                                       00039300
C                                                                       00039400
C                                                                       00039500
 9444 CONTINUE                                                          00039600
C                                                                       00039700
C ---- PLOTENDE                                                         00039800
      CALL PLTEJ                                                        00039900
C                                                                       00040000
C                                                                       00040100
      WRITE ( 6 , * ) ' 0 = TEMINATE THE PROGRAM '                      00040200
      WRITE ( 6 , * ) ' 1 = NOCHMALS VON VORNE '                        00040300
      WRITE ( 6 , * ) ' 2 = 1 + DIESELBE DATEI '                        00040400
      WRITE ( 6 , * ) ' 3 = 2 + DIESELBE ANZAHL DER BILDER '            00040500
      WRITE ( 6 , * ) ' 4 = 3 + DIESELBEN ACHSEN '                      00040600
      WRITE ( 6 , * ) ' 5 = 4 + VEDECKT / NICHT VERDECKT '              00040700
      READ  ( 6 , * ) CONTR1                                            00040800
      IF (CONTR1 .EQ. 1 ) GOTO 9554                                     00040900
      IF (CONTR1 .EQ. 2 ) GOTO 9555                                     00041000
      IF (CONTR1 .EQ. 3 ) GOTO 9556                                     00041100
      IF (CONTR1 .EQ. 4 ) GOTO 9557                                     00041200
      IF (CONTR1 .EQ. 5 ) GOTO 9558                                     00041300
      END                                                               00041400
