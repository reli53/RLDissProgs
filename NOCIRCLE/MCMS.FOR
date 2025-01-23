//D14KM    JOB RACF,CLASS=D,                                            00010053
//    MSGCLASS=X,MSGLEVEL=(2,0),NOTIFY=D14K                             00020023
//            EXEC FORTCLG,                                             00030038
//    REGION.FORT=2048K,REGION.GO=2048K                                 00040036
//FORT.SYSIN    DD *                                                    00050002
C                                                                       00060000
C                                                                       00070000
C                                                                       00080000
C       ##################################################              00090000
C       ####     BERECHNUNG DER MATRIZEN  BZGL.       ####              00100007
C       ####     DER COSINUS UND SINUS-MODEN          ####              00110007
C       ####     (UM STEADY-STATE R0(FI))             ####              00120007
C       ####                                          ####              00130000
C       ####   NAME DES PROGRAMMES : NOCIRCLE(MCMS)   ####              00140007
C       ##################################################              00150000
C                                                                       00160000
C       LITERATUR: BROWER, KESSLER, KOPLIK AND LEVINE                   00170000
C                                                                       00180000
C                                                                       00190000
        INTEGER           I,J,K,POT                                     00200056
        INTEGER           L,N,N9,N8                                     00210008
        DOUBLE PRECISION  PI,H1,H2,H3,H4,H5                             00220008
        DOUBLE PRECISION  GAMMA,B,K0MIN                                 00230047
        DOUBLE PRECISION  B0,B1,B2,B3,B4                                00240008
        DOUBLE PRECISION  G0,G1,G2,      A0,A1,A2                       00250008
        DOUBLE PRECISION  WG0,  G02,G03,G05                             00260008
        DOUBLE PRECISION  C1,C2,C3,C4,C5,C6                             00270008
        DOUBLE PRECISION  D0,D1,D2,D3,D4,   E0,E1,E2                    00280008
        DOUBLE PRECISION  VS,K0                                         00290008
C                                                                       00300003
        DOUBLE PRECISION  FI(0:2000),R0(0:2000)                         00310013
        DOUBLE PRECISION  R1(0:2000),R2(0:2000)                         00320013
        DOUBLE PRECISION  R3(0:2000),R4(0:2000)                         00330013
        DOUBLE PRECISION  F0(0:2000),F1(0:2000)                         00340013
        DOUBLE PRECISION  F2(0:2000),F3(0:2000)                         00350013
        DOUBLE PRECISION  F4(0:2000),DFI(1:2000)                        00360013
        DOUBLE PRECISION  MC(0:100,0:100),MS(0:100,0:100)               00370030
        CHARACTER         FNR*30 , FNW*30                               00380050
C                                                                       00390008
        DOUBLE PRECISION  B9,X9,FNVS1,FNVS2                             00400056
        FNVS1(B9,X9)=-3.0D0*(X9-1.0D0/B9)*(X9-1.0D0/B9)+1.0D0           00410056
        FNVS2(B9,X9)=+3.0D0*(X9-1.0D0/B9)*(X9-1.0D0/B9)+1.0D0           00420056
C                                                                       00430008
C       **************************************************              00440008
C       ****      PARAMETER                           ****              00450008
C       **************************************************              00460008
C                                                                       00470002
        POT =  1                                                        00480059
        N8  = 50                                                        00490056
        FNR ='D14K.R022.DATA'                                           00500058
        FNW ='D14K.M022.DATA'                                           00510058
C                                                                       00520050
C       N8 = MATRIZENGROESSE                                            00530008
C       MC(0:N8,0:N8)                                                   00540008
C       MS(0:N8,0:N8)                                                   00550030
C                                                                       00560000
C       **************************************************              00570007
C       ****      LESEN DER DATEN                     ****              00580007
C       **************************************************              00590007
C                                                                       00600007
        OPEN (21,FILE=FNR)                                              00610050
        OPEN (22,FILE=FNW)                                              00620050
        REWIND 21                                                       00630007
        REWIND 22                                                       00640050
   40   FORMAT(1X,7X       ,I6)                                         00650047
   41   FORMAT(1X,7X       ,D23.16)                                     00660055
   49   FORMAT(1X,I6,1X,D23.16,1X,D23.16)                               00670055
        READ  (21,40) L                                                 00680047
        READ  (21,41) GAMMA                                             00690047
        READ  (21,41) B                                                 00700047
        READ  (21,41) K0MIN                                             00710047
        READ  (21,40) N                                                 00720047
        N9=L*N*2                                                        00730007
        DO 100 J=0 ,N9                                                  00740007
           READ  (21,49) I,R0(J),FI(J)                                  00750047
  100   CONTINUE                                                        00760007
        CLOSE (21,STATUS='KEEP')                                        00770012
C                                                                       00780000
C                                                                       00790000
C                L = MODE                                               00800002
C                N = ANZAHL DER STUETZPUNKTE FUER 1/2-PERIODE           00810002
C                N9 = L*N*2 = ANZAHL DER STUETZPUNKTE FUER L-PERIODEN   00820002
C                GAMMA = KOEFFIZIENT DER OBERFLAECHENSPANNUBG           00830002
C                B = STEADY-STATE RAIUS                                 00840007
C                K0MIN= MINIMALES KAPPA                                 00850055
C                                                                       00860002
                 H1=1.0D+00                                             00870054
                 PI=4.0D+00*DATAN(H1)                                   00880054
C                                                                       00890000
C       *************************************************               00900000
C       **** BERECHNEN DER 1. BIS 4. ABL. VON R0(FI) ****               00910008
C       *************************************************               00920000
C                                                                       00930000
        DO 200 I=1,N9                                                   00940008
           DFI(I)=FI(I)-FI(I-1)                                         00950008
  200   CONTINUE                                                        00960008
C                                                                       00970008
        CALL ABLEI(R0,DFI,N9,R1)                                        00980008
        CALL ABLEI(R1,DFI,N9,R2)                                        00990008
        CALL ABLEI(R2,DFI,N9,R3)                                        01000008
        CALL ABLEI(R3,DFI,N9,R4)                                        01010008
C                                                                       01020008
C       **************************************************              01030003
C       **** BERECHNEN DER FUNKT. F0(FI) BIS F4(FI)   ****              01040008
C       **************************************************              01050003
C                                                                       01060003
        DO 300 I=0,N9                                                   01070008
           B0=R0(I)                                                     01080008
           B1=R1(I)                                                     01090008
           B2=R2(I)                                                     01100008
           B3=R3(I)                                                     01110008
           B4=R4(I)                                                     01120008
           G0=B0*B0+B1*B1                                               01130008
           G1=2.D0*B1*(B0+B2)                                           01140055
           G2=2.D0*B2*(B0+B2)+2.D0*B1*(B1+B3)                           01150055
           A0=B0*B0+2.D0*B1*B1-B0*B2                                    01160055
           A1=2.D0*B0*B1+3.D0*B1*B2-B0*B3                               01170055
           A2=B2*(2.D0*B0+3.D0*B2)+2.D0*B1*(B1+B3)-B0*B4                01180055
           WG0=DSQRT(G0)                                                01190008
           G02=G0*G0                                                    01200008
           G03=G0*G02                                                   01210008
           G05=G02*G03                                                  01220008
           C1=(49.*G0*G1*A1+21.*G0*G2*A0-10.*G0*G0*A2-81.*G1*G1*A0)/    01230008
     *          4./G05                                                  01240008
           C2=(9.*G0*G1*A0-3.5D+00*G0*G0*A1)/G05                        01250054
           C3=-1.5D+00*A0/G03                                           01260054
           C4=(9.*G0*G1*G1-3.*G0*G0*G2)/G05/2.                          01270008
           C5=-3.5D+00*G1/G03                                           01280054
           C6=1.0D+00/G02                                               01290054
           K0=A0/(WG0*WG0*WG0)                                          01300008
           D0=2.*C1*B0+2.*C2*B1+2.*C3*B2+C4*(2.*B0-B2)+C5*(2.*B1-B3)    01310008
     *          +C6*(2.*B2-B4)                                          01320008
           D1=C1*2.*B1+C2*2.*(B0+B2)+C3*2.*(2.*B1+B3)+C4*4.*B1          01330008
     *          +C5*(2.*B0+3.*B2)+C6*(4.*B1+2.*B3)                      01340008
           D2=C2*2.*B1+C3*2.*(B0+2.*B2)-C4*B0+C5*3.*B1+C6*(2.*B0+6.*B2) 01350009
           D3=C3*2.*B1-C5*B0+C6*2.*B1                                   01360008
           D4=-C6*B0                                                    01370008
           IF (POT .EQ. 1) THEN                                         01380056
              VS=FNVS1(B,K0)/G02                                        01390056
           ENDIF                                                        01400056
           IF (POT .EQ. 2) THEN                                         01410056
              VS=FNVS2(B,K0)/G02                                        01420056
           ENDIF                                                        01430056
           E0=VS*(G0*(2.*B0-B2)-3.*A0*B0)                               01440008
           E1=VS*(G0*4.*B1-3.*A0*B1)                                    01450008
           E2=-VS*G0*B0                                                 01460008
           F0(I)=(E0+GAMMA*D0)/B0                                       01470008
           F1(I)=(E1+GAMMA*D1)/B0                                       01480008
           F2(I)=(E2+GAMMA*D2)/B0                                       01490008
           F3(I)=    GAMMA*D3 /B0                                       01500008
           F4(I)=    GAMMA*D4 /B0                                       01510008
  300   CONTINUE                                                        01520008
C                                                                       01530008
C       **************************************************              01540008
C       **** BERECHNEN DER MATR. MC FUER COSINUS-MODE ****              01550008
C       **************************************************              01560008
C                                                                       01570008
        DO 400 I=0,N8                                                   01580008
        H5=I                                                            01590055
        DO 400 J=0,N8                                                   01600008
           H3=J                                                         01610055
           H4=H3*H3                                                     01620008
           MC(I,J)=0.0D0                                                01630055
           H2=( (F0(0)-H4*F2(0)+H4*H4*F4(0))*DCOS(H3*FI(0))             01640008
     *         +H3*(H4*F3(0)-F1(0))*DSIN(H3*FI(0)) )* DCOS(H5*FI(0))    01650008
           DO 410 K=1,N9                                                01660008
              H1=H2                                                     01670008
              H2=( (F0(K)-H4*F2(K)+H4*H4*F4(K))*DCOS(H3*FI(K))          01680008
     *            +H3*(H4*F3(K)-F1(K))*DSIN(H3*FI(K)) )* DCOS(H5*FI(K)) 01690008
              MC(I,J)=MC(I,J)+(H1+H2)*DFI(K)*.5D0                       01700055
  410      CONTINUE                                                     01710008
           MC(I,J)=MC(I,J)/PI                                           01720008
           IF (I .EQ. 0) MC(I,J)=MC(I,J)*.5D0                           01730055
  400   CONTINUE                                                        01740008
C                                                                       01750008
C                                                                       01760008
C       **************************************************              01770008
C       **** BERECHNEN DER MATR. MS FUER SINUS-MODEN  ****              01780008
C       **************************************************              01790008
C                                                                       01800008
        DO 500 I=1,N8                                                   01810008
        H5=I                                                            01820055
        DO 500 J=1,N8                                                   01830008
           H3=J                                                         01840055
           H4=H3*H3                                                     01850008
           MS(I,J)=0.0D+00                                              01860054
           H2=( (F0(0)-H4*F2(0)+H4*H4*F4(0))*DSIN(H3*FI(0))             01870008
     *         -H3*(H4*F3(0)-F1(0))*DCOS(H3*FI(0)) )* DSIN(H5*FI(0))    01880008
           DO 510 K=1,N9                                                01890008
              H1=H2                                                     01900008
              H2=( (F0(K)-H4*F2(K)+H4*H4*F4(K))*DSIN(H3*FI(K))          01910008
     *            -H3*(H4*F3(K)-F1(K))*DCOS(H3*FI(K)) )* DSIN(H5*FI(K)) 01920008
              MS(I,J)=MS(I,J)+(H1+H2)*DFI(K)*.5D+00                     01930054
  510      CONTINUE                                                     01940008
           MS(I,J)=MS(I,J)/PI                                           01950008
  500   CONTINUE                                                        01960008
C                                                                       01970030
        DO 520 I=0,N8                                                   01980030
           MS(I,0)=.0D+00                                               01990054
           MS(0,I)=.0D+00                                               02000054
  520   CONTINUE                                                        02010030
C                                                                       02020030
C                                                                       02030030
C       **************************************************              02040002
C       ****      PRINTEN DER MATRIZEN                ****              02050007
C       **************************************************              02060000
C                                                                       02070000
C       REWIND 22                                                       02080008
   50   FORMAT(1X,'L     =',I6)                                         02090047
   51   FORMAT(1X,'GAMMA =',D23.16)                                     02100055
   52   FORMAT(1X,'B     =',D23.16)                                     02110055
   53   FORMAT(1X,'K0MIN =',D23.16)                                     02120055
   54   FORMAT(1X,'N     =',I6)                                         02130047
   55   FORMAT(1X,'N8    =',I6)                                         02140047
   59   FORMAT(1X,'MC(',I3,',',I3,') =',D22.15,                         02150055
     *         1X,'MS(',I3,',',I3,') =',D22.15)                         02160055
        WRITE (22,50) L                                                 02170047
        WRITE (22,51) GAMMA                                             02180047
        WRITE (22,52) B                                                 02190047
        WRITE (22,53) K0MIN                                             02200047
        WRITE (22,54) N                                                 02210047
        WRITE (22,55) N8                                                02220047
C                                                                       02230009
        DO 1000 J=0,N8                                                  02240008
           DO 1000 I=0,N8                                               02250008
              WRITE (22,59) I,J,MC(I,J),I,J,MS(I,J)                     02260047
 1000   CONTINUE                                                        02270008
        CLOSE (22,STATUS='KEEP')                                        02280050
C       BACKSPACE 22                                                    02290050
        END                                                             02300008
C                                                                       02310002
C       **************************************************              02320008
C       ****      SUBROUTINES                         ****              02330008
C       **************************************************              02340008
C                                                                       02350008
        SUBROUTINE ABLEI(FU,DELX,N7,FUA)                                02360008
        DOUBLE PRECISION FU(0:2000),DELX(1:2000),FUA(0:2000)            02370014
        DOUBLE PRECISION D1,D2                                          02380008
        INTEGER N7,I                                                    02390008
        DO 2000 I=1,N7-1                                                02400008
           D1=DELX(I)                                                   02410008
           D2=DELX(I+1)                                                 02420008
           FUA(I)=((FU(I+1)-FU(I))*D1/D2+(FU(I)-FU(I-1))*D2/D1)/(D1+D2) 02430008
 2000   CONTINUE                                                        02440008
        D1=DELX(N7)                                                     02450008
        D2=DELX(1)                                                      02460008
        FUA(N7)=((FU(1)-FU(N7))*D1/D2+(FU(N7)-FU(N7-1))*D2/D1)/(D1+D2)  02470008
        FUA(0) =FUA(N7)                                                 02480008
        RETURN                                                          02490008
C                                                                       02500002
        END                                                             02510000
//GO.SYSIN DD *                                                         02520024
/*                                                                      02530050
/*      EXEC DRUCKE                                                     02540000
/*      END                                                             02550000
