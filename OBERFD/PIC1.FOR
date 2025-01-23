C                                                                       00010001
C                                                                       00020001
C     ******************************************************************00030001
C     ******************************************************************00040001
C     ****                                                          ****00050001
C     ****            P I C  1         (GRAPHIC-PROGRAMM)           ****00060001
C     ****                                                          ****00070001
C     ******************************************************************00080001
C     ******************************************************************00090001
C                                                                       00100000
C                                                                       00110001
      INTEGER I,J,K1,K,AMPN,   AB,M,APR,N(1:30)                         00120001
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX                              00130001
      DOUBLE PRECISION KMIN,KMAX,AMIN,AMAX,AMPMAX,AMPMIN,RMAX,RMIN      00140001
      DOUBLE PRECISION MMIN,MMAX,THMIN,THMAX,RSMAX,RSMIN                00150001
      DOUBLE PRECISION H1,H2,H3,H4,PI    ,T8,STOT                       00160001
      DOUBLE PRECISION XPMAX,YPMAX,XPMIN,YPMIN                          00170001
      DOUBLE PRECISION X(1:21,0:160 ),Y(1:21,0:160 )                    00180001
      DOUBLE PRECISION GAMMA(1:21,0:160 ),KAPPA(1:21,0:160 )            00190001
      DOUBLE PRECISION R(1:21,0:160 ),THETA(1:21,0:160 )                00200000
      DOUBLE PRECISION AMP(1:21,0:160 )                                 00210001
      DOUBLE PRECISION A(1:21,0:160 )                                   00220001
      DOUBLE PRECISION XP(1:21,0:160 ),YP(1:21,0:160 )                  00230001
      CHARACTER PROG*1,FN*30,SCR*2                                      00240001
C                                                                       00250001
C                                                                       00260001
C                                                                       00270001
      AMPN=25                                                           00280001
      PI=4.D0*DATAN(1.D0)                                               00290004
      FN= 'D14K.TEST.DATA'                                              00300003
C     ***********        INPUT OF THE TERMINAL-NUMBER        ***********00310001
      CALL INITT (960)                                                  00320001
      CALL WRITEK (1,' ')                                               00330001
      CALL NEWLIN                                                       00340001
      CALL WRITEK (34,'WHICH TEKTRONIX-TERMINAL IS USED ?')             00350001
      CALL NEWLIN                                                       00360001
      CALL WRITEK (34,'**********************************')             00370001
      CALL NEWLIN                                                       00380001
      CALL WRITEK (1,' ')                                               00390001
      CALL NEWLIN                                                       00400000
      CALL WRITEK (34,'ENTER: DEFAULT  IF TEKTRONIX 4012 ')             00410001
      CALL NEWLIN                                                       00420001
      CALL WRITEK (34,'            T2  IF TEKTRONIX 4014 ')             00430001
      CALL NEWLIN                                                       00440001
      SCR='T1'                                                          00450001
      READ (5,'(A2)') SCR                                               00460001
      IF (SCR .NE. 'T2') THEN                                           00470001
         K1=1                                                           00480001
         CALL SETBUF (3)                                                00490001
         CALL TERM (1,1024)                                             00500000
         CALL ERASE                                                     00510001
         CALL WRITEK (1,' ')                                            00520001
         CALL WRITEK (33,'*********************************')           00530001
         CALL WRITEK (33,'*                               *')           00540001
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4012 *')           00550001
         CALL WRITEK (33,'*                               *')           00560001
         CALL WRITEK (33,'*********************************')           00570001
      ELSE                                                              00580001
         K1=4                                                           00590001
         CALL SETBUF (3)                                                00600000
         CALL TERM (3,4096)                                             00610001
         CALL ERASE                                                     00620001
         CALL WRITEK (1,' ')                                            00630001
         CALL WRITEK (33,'*********************************')           00640001
         CALL WRITEK (33,'*                               *')           00650001
         CALL WRITEK (33,'* WELCOME TO THE TEKTRONIX 4014 *')           00660001
         CALL WRITEK (33,'*                               *')           00670001
         CALL WRITEK (33,'*********************************')           00680001
      END IF                                                            00690001
  555 CALL MOVABS (0,660*K1)                                            00700005
      CALL ANMODE                                                       00710004
      WRITE (6,*) ' '                                                   00720003
      WRITE (6,*) ' '                                                   00730001
      WRITE (6,*) ' '                                                   00740001
      WRITE (6,*) '       EINGABE DER DATEN-DATEI'                      00750001
      WRITE (6,*) '=========================================='          00760001
      WRITE (6,*) ' '                                                   00770001
  137 FORMAT (' YOU CAN CHOOSE: DEFAULT   ',A30)                        00780001
      WRITE (6,137) FN                                                  00790001
      WRITE (6,*) ' '                                                   00800001
      WRITE (6,*) '                    (1)   D14K.TES1.DATA'            00810002
      WRITE (6,*) '                    (2)   D14K.TES2.DATA'            00820002
      WRITE (6,*) '                    (3)   D14K.TES3.DATA'            00830002
      WRITE (6,*) '                    (4)   D14K.TES4.DATA'            00840002
      WRITE (6,*) '                    (5)   D14K.TES5.DATA'            00850002
      WRITE (6,*) '                    (6)   D14K.TES6.DATA'            00860002
      WRITE (6,*) '                    (7)   D14K.TES7.DATA'            00870002
      WRITE (6,*) '                    (8)   D14K.TES8.DATA'            00880002
      WRITE (6,*) '                    (0)       .    .    '            00890002
      WRITE (6,*) ' '                                                   00900001
      PROG =' '                                                         00910001
      READ (5,'(A1)') PROG                                              00920001
      IF (PROG .EQ. '1') FN= 'D14K.TES1.DATA'                           00930002
      IF (PROG .EQ. '2') FN= 'D14K.TES2.DATA'                           00940002
      IF (PROG .EQ. '3') FN= 'D14K.TES3.DATA'                           00950002
      IF (PROG .EQ. '4') FN= 'D14K.TES4.DATA'                           00960002
      IF (PROG .EQ. '5') FN= 'D14K.TES5.DATA'                           00970002
      IF (PROG .EQ. '6') FN= 'D14K.TES6.DATA'                           00980002
      IF (PROG .EQ. '7') FN= 'D14K.TES7.DATA'                           00990002
      IF (PROG .EQ. '8') FN= 'D14K.TES8.DATA'                           01000002
      IF (PROG .EQ. '0') GOTO 853                                       01010001
      GOTO 666                                                          01020001
  853 WRITE (6,*) ' '                                                   01030001
      WRITE (6,*) ' NAME DER DATEN-DATEI :'                             01040000
      READ (5,'(A20)') FN                                               01050001
C                                                                       01060001
C     ***********  INPUT OF THE FILENAME INCLUDING THE DATA  ***********01070001
  666 CONTINUE                                                          01080001
C     CALL ERASE                                                        01090001
C     WRITE (6,*) ' '                                                   01100001
C     WRITE (6,*) 'WHEN THE CURSOR APPEARS:'                            01110001
C     WRITE (6,*) '========================'                            01120001
C     WRITE (6,*) 'ENTER: BLANK     TO KEEP  THE SCREEN'                01130001
C     WRITE (6,*) '       C         TO CLEAR THE SCREEN'                01140000
C     WRITE (6,*) '       E         TO TERMINATE THE PROGRAM'           01150001
C     WRITE (6,*) '       S         TO START THE PROGRAM AGAIN'         01160001
C     WRITE (6,*) '       A         TO PLOT ANOTHER CURVE-PARAMETER'    01170001
C     WRITE (6,*) '       P         TO CHANGE THE SCALE-PARAMETER'      01180001
C     WRITE (6,*) '       1,...,9   TO ADD THE CORRESPONDING CURVE'     01190001
C     WRITE (6,*) '       *         TO ADD THE LAST CURVE'              01200001
C     WRITE (6,*) ' '                                                   01210001
C     WRITE (6,*) 'WHEN THE BELL SOUNDS: YOU CAN DECIDE TO GENERATE A HA01220001
C    *RDCOPY'                                                           01230001
C     WRITE (6,*) '====================='                               01240000
C                                                                       01250001
C                                                                       01260001
C                                                                       01270001
C     ***********  INPUT OF THE DATA ***********                        01280001
  777 OPEN(21,FILE=FN)                                                  01290001
      REWIND 21                                                         01300001
  200 FORMAT (5X,I8,3X,I10)                                             01310001
  201 FORMAT (1X,F12.5,1X,F12.5)                                        01320001
  202 FORMAT (1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5,1X,F12.5)             01330001
      AB=0                                                              01340000
      DO 1100 I=1,30,1                                                  01350001
         READ (21,200) APR,N(I)                                         01360001
         IF (APR .EQ. 0) GOTO 1101                                      01370001
         READ (21,201) T8,STOT                                          01380001
         AB=AB+1                                                        01390001
         DO 1000 J=0,N(I),1                                             01400001
            READ (21,202) X(I,J),Y(I,J),GAMMA(I,J),KAPPA(I,J),A(I,J)    01410001
 1000    CONTINUE                                                       01420001
 1100 CONTINUE                                                          01430001
 1101 CLOSE(21,STATUS='KEEP')                                           01440000
C                                                                       01450001
      XMIN=X(1,0)                                                       01460001
      XMAX=X(1,0)                                                       01470001
      YMIN=Y(1,0)                                                       01480001
      YMAX=Y(1,0)                                                       01490001
      KMAX=KAPPA(1,0)                                                   01500001
      KMIN=KAPPA(1,0)                                                   01510001
      RMIN=DSQRT(X(1,0)*X(1,0)+Y(1,0)*Y(1,0))                           01520001
      RMAX=RMIN                                                         01530001
      AMAX=A    (1,0)                                                   01540000
      AMIN=A    (1,0)                                                   01550001
      DO 1010 I=1, AB ,1                                                01560001
         DO 1020 J=0, N(I), 1                                           01570001
            XMIN=DMIN1(XMIN,X(I,J))                                     01580001
            XMAX=DMAX1(XMAX,X(I,J))                                     01590001
            YMIN=DMIN1(YMIN,Y(I,J))                                     01600001
            YMAX=DMAX1(YMAX,Y(I,J))                                     01610001
            KMAX=DMAX1(KMAX,KAPPA(I,J))                                 01620001
            KMIN=DMIN1(KMIN,KAPPA(I,J))                                 01630001
            AMAX=DMAX1(AMAX,A    (I,J))                                 01640000
            AMIN=DMIN1(AMIN,A    (I,J))                                 01650001
            R(I,J)=DSQRT(X(I,J)*X(I,J)+Y(I,J)*Y(I,J))                   01660001
            THETA(I,J)=DATAN2(Y(I,J),X(I,J))                            01670001
            IF (THETA(I,J) .LT. 0.) THETA(I,J)=PI+PI+THETA(I,J)         01680001
            RMIN=DMIN1(RMIN,R(I,J))                                     01690001
            RMAX=DMAX1(RMAX,R(I,J))                                     01700001
 1020    CONTINUE                                                       01710001
         IF (THETA(I,N(I)) .LT. PI) THETA(I,N(I))=THETA(I,N(I))+PI+PI   01720001
 1010 CONTINUE                                                          01730001
      DO 1030 I=1,AB,1                                                  01740000
         DO 1040 K=0,AMPN,1                                             01750001
           AMP(I,K)=0.                                                  01760001
           H1=K                                                         01770001
           DO 1050 J=0,N(I)-1,1                                         01780001
              H2= R(I,J)*DCOS(H1*THETA(I,J))*.5                         01790001
              H3= R(I,J+1)*DCOS(H1*THETA(I,J+1))*.5                     01800001
              AMP(I,K)=AMP(I,K)+(H2+H3)*(THETA(I,J+1)-THETA(I,J))       01810001
 1050      CONTINUE                                                     01820001
           AMP(I,K)=AMP(I,K)/PI                                         01830001
 1040    CONTINUE                                                       01840000
         AMP(I,0)=AMP(I,0)*.5                                           01850001
 1030 CONTINUE                                                          01860001
      AMPMIN=AMP(1,0)                                                   01870001
      AMPMAX=AMP(1,0)                                                   01880001
      DO 1060 I=1,AB,1                                                  01890001
         DO 1070 K=0,AMPN,1                                             01900001
            AMPMIN=DMIN1(AMPMIN,AMP(I,K))                               01910001
            AMPMAX=DMAX1(AMPMAX,AMP(I,K))                               01920001
 1070    CONTINUE                                                       01930001
 1060 CONTINUE                                                          01940000
C                                                                       01950001
      XMAX=DMAX1(XMAX,YMAX)                                             01960001
      XMIN=DMIN1(XMIN,YMIN)                                             01970001
      YMAX=XMAX                                                         01980001
      YMIN=XMIN                                                         01990001
      H1=XMAX-XMIN                                                      02000001
      H2=YMAX-YMIN                                                      02010001
      H3=KMAX-KMIN                                                      02020001
      H4=AMAX-AMIN                                                      02030001
      XMAX= XMAX+H1/10.                                                 02040000
      YMAX= YMAX+H2/10.                                                 02050001
      YMIN= YMIN-H2/10.                                                 02060001
      XMIN= XMIN-H1/10.                                                 02070001
      KMAX= KMAX+H3/10.                                                 02080001
      KMIN= KMIN-H3/10.                                                 02090001
      AMAX= AMAX+H4/10.                                                 02100001
      AMIN= AMIN-H4/10.                                                 02110001
      H1=RMAX-RMIN                                                      02120001
      H2=AMPMAX-AMPMIN                                                  02130001
      RMAX=RMAX+H1/10.                                                  02140000
      RMIN=RMIN-H1/10.                                                  02150001
      AMPMAX=AMPMAX+H2/10.                                              02160001
      AMPMIN=AMPMIN-H2/10.                                              02170001
      RSMIN=0.                                                          02180001
      RSMAX=1.                                                          02190001
      THMIN=0.                                                          02200001
      THMAX=2.*PI                                                       02210001
      MMIN=-.5                                                          02220001
      MMAX=16.5                                                         02230001
C                                                                       02240000
  988 CALL ERASE                                                        02250003
      CALL MOVABS (0,730*K1)                                            02260004
      CALL ANMODE                                                       02270003
      WRITE (6,*) '         MAX. AND MIN. VALUES      '                 02280001
      WRITE (6,*) '=========================================='          02290001
      WRITE (6,*) ' '                                                   02300001
  251 FORMAT ('  (B)  BOUNDARY      :   XMIN=',F10.5,'    XMAX='F10.5)  02310001
  252 FORMAT ('                     :   YMIN=',F10.5,'    YMAX='F10.5)  02320001
  253 FORMAT ('  (K)  CURVATURE     :  RSMIN=',F10.5,'   RSMAX='F10.5)  02330000
  254 FORMAT ('                     :   KMIN=',F10.5,'    KMAX='F10.5)  02340001
  255 FORMAT ('  (A)  CONCENTRATION :  RSMIN=',F10.5,'   RSMAX='F10.5)  02350001
  256 FORMAT ('                     :   AMIN=',F10.5,'    AMAX='F10.5)  02360001
  257 FORMAT ('  (R)  RADIUS(WINKEL):  THMIN=',F10.5,'   THMAX='F10.5)  02370001
  258 FORMAT ('                     :   RMIN=',F10.5,'    RMAX='F10.5)  02380001
  259 FORMAT ('  (M)  MODEN-AMPLIT. :   MMIN=',F10.5,'    MMAX='F10.5)  02390001
  261 FORMAT ('                     : AMPMIN=',F10.5,'  AMPMAX='F10.5)  02400001
      WRITE (6,251) XMIN,XMAX                                           02410001
      WRITE (6,252) YMIN,YMAX                                           02420001
      WRITE (6,253) RSMIN,RSMAX                                         02430000
      WRITE (6,254) KMIN,KMAX                                           02440001
      WRITE (6,255) RSMIN,RSMAX                                         02450001
      WRITE (6,256) AMIN,AMAX                                           02460001
      WRITE (6,257) THMIN,THMAX                                         02470001
      WRITE (6,258) RMIN,RMAX                                           02480001
      WRITE (6,259) MMIN,MMAX                                           02490001
      WRITE (6,261) AMPMIN,AMPMAX                                       02500001
      WRITE (6,*) ' '                                                   02510001
      WRITE (6,*) ' '                                                   02520001
      WRITE (6,*) 'CHANGE SCALE-PARAMETER ?  (DEFAULT = NO CHANGE)'     02530000
      WRITE (6,*) ' '                                                   02540001
      PROG=' '                                                          02550001
      READ (5,'(A1)') PROG                                              02560001
      IF (PROG .EQ. ' ') GOTO 999                                       02570001
      WRITE (6,*) 'VIER WERTE (DURCH KOMMATA GETRENNT)'                 02580001
      READ (5,*) H1,H2,H3,H4                                            02590001
      IF (PROG .EQ. 'B') THEN                                           02600001
            XMIN=H1                                                     02610001
            XMAX=H2                                                     02620001
            YMIN=H3                                                     02630000
            YMAX=H4                                                     02640001
      ENDIF                                                             02650001
      IF (PROG .EQ. 'K') THEN                                           02660001
            KMIN=H3                                                     02670001
            KMAX=H4                                                     02680001
           RSMIN=H1                                                     02690001
           RSMAX=H2                                                     02700001
      ENDIF                                                             02710001
      IF (PROG .EQ. 'A') THEN                                           02720001
            AMIN=H3                                                     02730000
            AMAX=H4                                                     02740001
           RSMIN=H1                                                     02750001
           RSMAX=H2                                                     02760001
      ENDIF                                                             02770001
      IF (PROG .EQ. 'R') THEN                                           02780001
            RMIN=H3                                                     02790001
            RMAX=H4                                                     02800001
           RSMIN=H1                                                     02810001
           RSMAX=H2                                                     02820001
      ENDIF                                                             02830000
      IF (PROG .EQ. 'M') THEN                                           02840001
          AMPMIN=H3                                                     02850001
          AMPMAX=H4                                                     02860001
            MMIN=H1                                                     02870001
            MMAX=H2                                                     02880001
      ENDIF                                                             02890001
      GOTO 988                                                          02900001
C                                                                       02910001
  999 CALL ERASE                                                        02920001
      CALL MOVABS (0,730*K1)                                            02930004
      CALL ANMODE                                                       02940004
      PROG=' '                                                          02950000
      WRITE (6,*) '           AUSWAHL DER KURVEN      '                 02960001
      WRITE (6,*) '=========================================='          02970001
      WRITE (6,*) ' '                                                   02980001
      WRITE (6,*) ' '                                                   02990001
      WRITE (6,*) ' '                                                   03000001
      WRITE (6,*) 'YOU CAN CHOOSE: DEFAULT   BOUNDARY     '             03010000
      WRITE (6,*) '                    (K)   CURVATURE    '             03020001
      WRITE (6,*) '                    (A)   CONCENTRATION'             03030001
      WRITE (6,*) '                    (R)   RADIUS(WINKEL)'            03040001
      WRITE (6,*) '                    (M)   MODEN-SELEKTION'           03050001
      WRITE (6,*) '                                       '             03060001
      WRITE (6,*) ' '                                                   03070001
      WRITE (6,*) ' '                                                   03080001
      READ (5,'(A1)') PROG                                              03090001
      DO 742 I=1,AB,1                                                   03100001
         DO 744 J=0, N(I), 1                                            03110000
            XP(I,J)=X(I,J)                                              03120001
            YP(I,J)=Y(I,J)                                              03130001
            IF (PROG .EQ. 'K')      XP(I,J)=GAMMA(I,J)                  03140001
            IF (PROG .EQ. 'K')      YP(I,J)=KAPPA(I,J)                  03150001
            IF (PROG .EQ. 'A')      XP(I,J)=GAMMA(I,J)                  03160001
            IF (PROG .EQ. 'A')      YP(I,J)=A    (I,J)                  03170001
            IF (PROG .EQ. 'R')      YP(I,J)=R    (I,J)                  03180001
            IF (PROG .EQ. 'R')      XP(I,J)=THETA(I,J)                  03190001
            IF (PROG .EQ. 'M')      XP(I,J)=J                           03200001
            IF (PROG .EQ. 'M')      YP(I,J)=AMP(I,J)                    03210000
  744    CONTINUE                                                       03220001
  742 CONTINUE                                                          03230001
      XPMAX=XMAX                                                        03240001
      YPMAX=YMAX                                                        03250001
      XPMIN=XMIN                                                        03260001
      YPMIN=YMIN                                                        03270001
      IF (PROG .EQ. 'K') THEN                                           03280001
         XPMAX=RSMAX                                                    03290001
         XPMIN=RSMIN                                                    03300001
         YPMAX=KMAX                                                     03310000
         YPMIN=KMIN                                                     03320001
      ENDIF                                                             03330001
      IF (PROG .EQ. 'A') THEN                                           03340001
         XPMAX=RSMAX                                                    03350001
         XPMIN=RSMIN                                                    03360001
         YPMAX=AMAX                                                     03370001
         YPMIN=AMIN                                                     03380001
      ENDIF                                                             03390001
      IF (PROG .EQ. 'R') THEN                                           03400001
         XPMAX=THMAX                                                    03410000
         XPMIN=THMIN                                                    03420001
         YPMAX=RMAX                                                     03430001
         YPMIN=RMIN                                                     03440001
      ENDIF                                                             03450001
      IF (PROG .EQ. 'M') THEN                                           03460001
         XPMAX=MMAX                                                     03470001
         XPMIN=MMIN                                                     03480001
         YPMAX=AMPMAX                                                   03490001
         YPMIN=AMPMIN                                                   03500001
      ENDIF                                                             03510000
C                                                                       03520001
      CALL TWINDO (250*K1, 950*K1, 50*K1,750*K1)                        03530001
      CALL DWINDO (XPMIN,XPMAX,YPMIN,YPMAX)                             03540001
      CALL ERASE                                                        03550001
      CALL MOVEA (XPMIN,YPMIN)                                          03560001
      CALL DRAWA (XPMAX,YPMIN)                                          03570001
      CALL DRAWA (XPMAX,YPMAX)                                          03580001
      CALL DRAWA (XPMIN,YPMAX)                                          03590001
      CALL DRAWA (XPMIN,YPMIN)                                          03600001
      CALL MOVABS (0, 730*K1)                                           03610000
      CALL ANMODE                                                       03620001
  210 FORMAT (' XMIN =',F8.2)                                           03630001
  220 FORMAT (' XMAX =',F8.2)                                           03640001
  230 FORMAT (' YMIN =',F8.2)                                           03650001
  240 FORMAT (' YMAX =',F8.2)                                           03660001
      WRITE (6,*) 'ABSCISSA:'                                           03670001
      WRITE (6,210) XPMIN                                               03680001
      WRITE (6,220) XPMAX                                               03690001
      WRITE (6,*) ' '                                                   03700001
      WRITE (6,*) 'ORDINATE:'                                           03710000
      WRITE (6,230) YPMIN                                               03720001
      WRITE (6,240) YPMAX                                               03730001
      DO 2100 M=1,AB,1                                                  03740001
         J=M                                                            03750001
 1900    CALL MOVEA (XP(J,0),YP(J,0))                                   03760001
         DO 2000 I=1,N(J)  ,1                                           03770001
            CALL DRAWA (XP(J,I),YP(J,I))                                03780001
 2000    CONTINUE                                                       03790001
         IF (M .EQ. AB) CALL BELL                                       03800001
         CALL VCURSR (ICH,XDUMMY,YDUMMY)                                03810000
         IF (ICH .GT. 48 .AND. ICH .LT. 58 .OR. ICH .EQ. 42) THEN       03820001
            IF (ICH .EQ. 42) THEN                                       03830001
               J=AB                                                     03840001
            ELSE                                                        03850001
               J=ICH-48                                                 03860001
            END IF                                                      03870001
            GOTO 1900                                                   03880001
         END IF                                                         03890001
         IF (ICH .EQ. 67 .OR. ICH .EQ. 99) THEN                         03900001
            CALL ERASE                                                  03910000
            CALL MOVEA (XPMIN,YPMIN)                                    03920001
            CALL DRAWA (XPMAX,YPMIN)                                    03930001
            CALL DRAWA (XPMAX,YPMAX)                                    03940001
            CALL DRAWA (XPMIN,YPMAX)                                    03950001
            CALL DRAWA (XPMIN,YPMIN)                                    03960001
            CALL MOVABS (0, 730*K1)                                     03970001
            CALL ANMODE                                                 03980001
            WRITE (6,*) 'ABSCISSA:'                                     03990001
            WRITE (6,210) XPMIN                                         04000001
            WRITE (6,220) XPMAX                                         04010000
            WRITE (6,*) ' '                                             04020001
            WRITE (6,*) 'ORDINATE:'                                     04030001
            WRITE (6,230) YPMIN                                         04040001
            WRITE (6,240) YPMAX                                         04050001
         END IF                                                         04060001
         IF (ICH .EQ. 69 .OR. ICH .EQ. 101) THEN                        04070001
            CALL ERASE                                                  04080001
            GOTO 9999                                                   04090001
         END IF                                                         04100001
         IF (ICH .EQ. 65 .OR. ICH .EQ. 97) GOTO 999                     04110000
         IF (ICH .EQ. 80 .OR. ICH .EQ. 112) GOTO 988                    04120001
         IF (ICH .EQ. 83 .OR. ICH .EQ. 115) THEN                        04130001
            CALL ERASE                                                  04140001
            GOTO 555                                                    04150001
         END IF                                                         04160001
 2100 CONTINUE                                                          04170001
C                                                                       04180001
C                                                                       04190001
 9999 STOP                                                              04200001
      END                                                               04210000
