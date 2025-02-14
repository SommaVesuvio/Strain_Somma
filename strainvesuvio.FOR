      PROGRAM STRAINVesuvio       !  Rev. 27 ottobre 2012
C
c  
c  Differisce dalla ver.1 per
c       1)il formato 1030 (a4,2f12.3,4f10.3,1x,a1)
c       2)per dimensione CS a 4 caratteri e non 6	(vedi anche CALSTR sub)
c       3) quindi la sequenza di fine serie è '****' 
c  
c  NON effettua calcolo degli errori
c
c
C Programma per il calcolo dei parametri principali di strain per
C zone comprese tra vertici selezionati della rete.
C Sono utilizzate le formule valide per lo strain infinitesimo bidi-
C mensionale.
C Per ogni coppia di vertici sono costruite due equazioni con inco-
C gnite i tre parametri dello strain (Ex,Ey,Gxy) e la componente di
C rotazione. Dai valori dei parametri dello strain si ricavano, poi,
C i valori degli stessi nel sistema principale (G nullo), i loro
C azimuths (dal Nord in senso orario) e i valori della dilatazione
C e del Gmax.
C Occorrono, pertanto, almeno tre vertici per risolvere, abbondante-
C mente, il problema.
C Il programma chiede interattivamente i nomi dei files d'ingresso
C (deve essere un disk-file) e d'uscita (6=video).
C Il file d'input inizia con una stringa di commento (60 caratteri)
C che viene riportata nell'output; ad essa seguono gruppi di records
C con i dati dei vertici da utilizzare per il calcolo; il numero di
C tali vertici deve essere minimo 3 e massimo 10.
C Ogni record deve contenere (formato: A6,2F11.4,4F8.4,9X,VE):        
C
C      a- Sigla del vertice (A6); 
C      b- Coordinate X ed Y (F11.4);
C      c- Componente Dx dello spostamento e suo errore Sx (F8.4);
C      d- Componente Dy dello spostamento e suo errore Sy (F8.4);
C      e- Un carattere di controllo in 70.ma colonna; se esso Š
C            uguale al carattere '*', il vertice viene ignorato.
C 
C I parametri alle lettere b),c) e d) devono essere nella stessa
C unit… di misura. Se Sx e/o Sy sono nulli, non si procede al
C calcolo degli errori.
C I gruppi di dati riferentesi a zone diverse sono divise da tra 
C dal sequenza '******' nelle prime sei colonne. Esempio d'input:
C
C   ESEMPIO DI FILE D'INPUT CON CALCOLO IN DUE AREE
C pukoae 08316.7580 01802.7860  0.0155  0.0100  0.0179  0.0180
C conepk 10127.9810 05725.3944  0.0790  0.0130 -0.0428  0.0230
C hvo114 14345.3200 04161.4695 -0.0220  0.0150  0.0044  0.0200
C ******
C pukoae 08316.7580 01802.7860  0.0155  0.0000  0.0179  0.0000
C conepk 10127.9810 05725.3944  0.0790  0.0000 -0.0428  0.0000
C hvo114 14345.3200 04161.4695 -0.0220  0.0000  0.0044  0.0000
C trkoae 09316.7590 09802.7860  0.0955  0.0000  0.0679  0.0000
C gruepk 10127.9810 05725.3944  0.0790  0.0000 -0.0528  0.0000
C hvo224 17345.3200 03161.4695 -0.0230  0.0000  0.0144  0.0000
C ******
C
C                                
C
C
      IMPLICIT REAL*8 (A-H,O-Z) 
      REAL*8 MUDX,MUDY
      PARAMETER (MXST=10,MXEQ=MXST*(MXST-1),MXIN=4,MXCL=5)  
      CHARACTER*1 VE 
      CHARACTER*60 SCR
c      CHARACTER*6 CS
      CHARACTER*4 CS
      CHARACTER*20 INFILE,OUFILE
      COMMON/BL1/B(MXEQ,MXCL),BN(MXIN,MXCL) 
      COMMON/BL2/CS(MXST),X(MXST),Y(MXST),DX(MXST),DY(MXST), 
     1           MUDX(MXST),MUDY(MXST)  
      COMMON/BL3/SCR,PIG,GAR,LU 
      PIG=3.141592653
c      GAR=PIG/200.
	gar=pig/180.
      WRITE(*,1520) 
      READ(*,1590) OUFILE 
      WRITE(*,1580) 
      READ(*,1590) INFILE 
      OPEN(70,FILE=INFILE,status='old',action='read')
      READ(70,1000) SCR
      IF(OUFILE.NE.'6') THEN
	LU=90
	OPEN(LU,FILE=OUFILE,action='write')
      ELSE
	LU=6
      END IF
      WRITE(LU,1060) SCR
    3 do i=1,mxst+1
    1   READ(70,1030,end=99) CS(I),X(I),Y(I),dx(i),mudx(i),dy(i),
     1                       mudy(i),ve
	IF(VE.EQ.'*') GO TO 1
c	IF(cs(i).eq.'******') go to 2
	IF(cs(i).eq.'****') go to 2 
      end do
      write(*,*) ' Troppi vertici !!'
      stop
    2 np=i-1 
      call calstr(np)
      go to 3
   99 if(i.ne.1) then 
	write(*,*) ' L''ultima serie non ha il marker di fine'
      end if
      stop 
 1000 FORMAT(A60)
c 1030 FORMAT(A6,2F11.2,4F8.4,9X,A1) 
 1030 FORMAT(A4,2F12.3,4F10.4,1X,A1) 
 1050 FORMAT(' INTESTAZIONE - (A60)')
 1060 FORMAT(1H1,T12,A60//)
 1520 FORMAT($' NOME FILE D''USCITA (6=CRT)  : ')  
 1580 FORMAT($' ENTER: Nome file d`input    : ') 
 1590 FORMAT(A20) 
      END 
      SUBROUTINE MANOR(N1,N2,N3)
      IMPLICIT REAL*8 (A-H,O-Z) 
      PARAMETER (MXST=10,MXEQ=MXST*(MXST-1),MXIN=4,MXCL=5)  
      COMMON/BL1/B(MXEQ,MXCL),BN(MXIN,MXCL) 
      DO 1 I=1,N2 
      DO 1 J=1,N3 
    1   BN(I,J)=0 
      DO 2 I=1,N2 
      DO 2 J=I,N3 
	DO 3 K=1,N1 
    3   BN(I,J)=BN(I,J)+B(K,I)*B(K,J) 
	IF(I.NE.J.AND.J.NE.N3) BN(J,I)=BN(I,J)
    2 CONTINUE
      RETURN
      END 
      SUBROUTINE SOSI(A,MA,NA,NR,INDIC,DETER,*) 
      IMPLICIT REAL*8 (A-H,O-Z) 
      PARAMETER (MXST=10,MXEQ=MXST*(MXST-1)/2.,MXIN=4,MXCL=5)  
      DIMENSION IR(MXIN),IC(MXIN),JV(MXIN),YT(MXIN),XT(MXIN)  
      DIMENSION A(MA,NA)
      EPS=1D-11 
      NC=NR 
      IF(INDIC.GE.0) NC=NR+1
    1 DETER=1 
      DO 2 K=1,NR 
      KM1=K-1 
      PIVOT=0.
      DO 3 I=1,NR 
      DO 3 J=1,NR 
      IF(K.EQ.1) GO TO 4
      DO 5 IQ=1,KM1 
      DO 5 IT=1,KM1 
      IF(I.EQ.IR(IQ)) GO TO 3 
      IF(J.EQ.IC(IT)) GO TO 3 
    5 CONTINUE
    4 IF(DABS(A(I,J)).LE.DABS(PIVOT)) GO TO 3 
      PIVOT=A(I,J)
      IR(K)=I 
      IC(K)=J 
    3 CONTINUE
      IF(DABS(PIVOT).GT.EPS) GO TO 6
      RETURN 1
    6 IRK=IR(K) 
      ICK=IC(K) 
      DETER=DETER*PIVOT 
      DO 7 J=1,NC 
    7 A(IRK,J)=A(IRK,J)/PIVOT 
      A(IRK,ICK)=1/PIVOT
      DO 2 I=1,NR 
      IF(I.EQ.IRK) GO TO 2
      AICK=A(I,ICK) 
      A(I,ICK)=-AICK/PIVOT
      DO 8 J=1,NC 
    8 IF(J.NE.ICK) A(I,J)=A(I,J)-AICK*A(IRK,J)
    2 CONTINUE
      DO 9 I=1,NR 
      IRI=IR(I) 
      ICI=IC(I) 
      JV(IRI)=ICI 
    9 IF(INDIC.GE.0) XT(ICI)=A(IRI,NC)
      IF(INDIC.LT.0) GO TO 17 
      DO 16 I=1,NR
   16 A(I,NC)=XT(I) 
   17 ICAST=0 
      NRM1=NR-1 
      DO 10 I=1,NRM1
      IP1=I+1 
      DO 10 J=IP1,NR
      IF(JV(J).GE.JV(I)) GO TO 10 
      JVT=JV(J) 
      JV(J)=JV(I) 
      JV(I)=JVT 
      ICAST=ICAST+1 
   10 CONTINUE
      IF(ICAST/2*2.NE.ICAST) DETER=-DETER 
      IF(INDIC.LE.0) GO TO 11 
      RETURN
   11 DO 12 J=1,NR
      DO 13 I=1,NR
      IRI=IR(I) 
      ICI=IC(I) 
   13 YT(ICI)=A(IRI,J)
      DO 12 I=1,NR
   12 A(I,J)=YT(I)
      DO 15 I=1,NR
      DO 14 J=1,NR
      IRJ=IR(J) 
      ICJ=IC(J) 
   14 YT(IRJ)=A(I,ICJ)
      DO 15 J=1,NR
   15 A(I,J)=YT(J)
      RETURN
      END 
      SUBROUTINE ROTMC(SX,SY,SXY,AL,SXP,SYP,SXYP) 
      IMPLICIT REAL*8 (A-H,O-Z) 
      SXQ=SX*SX 
      SYQ=SY*SY 
      SXY2=2.*SXY 
      COS=DCOS(AL)
      SIN=DSIN(AL)
      SXP=DSQRT(SXQ*COS*COS+SYQ*SIN*SIN+SXY2*SIN*COS) 
      SYP=DSQRT(SXQ*SIN*SIN+SYQ*COS*COS-SXY2*SIN*COS) 
      SXYP=SIN*COS*(SYQ-SXQ)+.5*SXY2*(COS*COS-SIN*SIN)
      RETURN
      END 
      SUBROUTINE CALSTR (N)                                          
      IMPLICIT REAL*8 (A-H,O-Z)                                       
      REAL*8 MUDX,MUDY
      PARAMETER (MXST=10,MXEQ=MXST*(MXST-1),MXIN=4,MXCL=5)  
c      CHARACTER*6 CS
      CHARACTER*4 CS
      CHARACTER*60 SCR
      COMMON/BL1/B(MXEQ,MXCL),BN(MXIN,MXCL) 
      COMMON/BL2/CS(MXST),X(MXST),Y(MXST),DX(MXST),DY(MXST), 
     1           MUDX(MXST),MUDY(MXST)  
      COMMON/BL3/SCR,PIG,GAR,LU 
      DIMENSION STR(MXEQ/2)                                
      CSTR(A,B1,C,D)=DSQRT((B1**2+D**2)/(A**2+C**2))-1 
	xm=0
	ym=0 
	do i=1,n
	  xm=xm+x(i)
	  ym=ym+y(i)
	end do
	xm=xm/n
	ym=ym/n                  
      DO 7 I=1,N*(N-1)                                                      
      DO 7 J=1,5                                                      
    7 B(I,J)=0                                                        
    1 NT=NT+1                                                         
      IF(MOD(NT,5).EQ.1.AND.NT.NE.1) WRITE(lu,'(1H1//)')              
      nr=0
      DO I=1,n-1                                                      
	do j=i+1,n
	  XP=X(J)-X(I)                    
	  XPP=X(J)+dx(j)-X(I)-dx(i)                    
	  YP=Y(J)-Y(I)                    
	  YPP=Y(J)+dy(j)-Y(I)-dy(i)                    
	  UP=XPP-XP
	  VP=YPP-YP
	  nr=nr+1
	  B(2*nr-1,1)=XP
	  B(2*nr-1,3)=YP/2
	  B(2*nr-1,4)=-YP                
	  B(2*nr-1,5)=UP
	  B(2*nr,2)=YP
	  B(2*nr,3)=XP/2
	  B(2*nr,4)=XP       
	  B(2*nr,5)=VP                                                     
	  if(xp.eq.0.and.yp.eq.0) then
	    str(nr)=0.
	    write(*,*)' N.B. : Vertice ripetuto nella serie # ',nr
	  else
	    STR(nr)=CSTR(XP,XPP,YP,YPP)              
	  end if  
	end do  
      end do  
      CALL MANOR(n*(n-1),4,5)                                               
      CALL SOSI(BN,MXIN,MXCL,4,1,DETER,*2)                                
      EX=BN(1,5)
      EY=BN(2,5)
      GXY=BN(3,5)
      OMEGA=BN(4,5)           
      CALL ORASP(EX,EY,GXY,EPS1,TETA1,EPS2,TETA2)                     
      DELTA=EPS1+EPS2
      GMAX=EPS1-EPS2                                
      write(lu,403) (cs(i),i=1,n)
	write(lu,407) xm,ym
  407 format(10x,'EST medio = ',f12.4,' NORD medio = ',f12.4//)
      nr=0
      do i=1,n-1
	do j=i+1,n
	  nr=nr+1
	  write(lu,404) cs(i),cs(j),str(nr)
	end do
      end do
      write(lu,405) ex,ey,gxy,eps1,teta1,eps2,teta2,gmax,delta,omega
      return
    2 write(lu,406) nt  
      return
  402 FORMAT(6X,I2,2(6X,I2),45X,A1)                                   
  403 FORMAT(1X,T2,'*** VERTICI ',<n>(1X,A6),' ***'/)                      
  404 FORMAT(T2,'LATO ',2(1X,A6),4X,'ALLUNGAMENTO=',1PD10.3)              
  405 FORMAT(//T2,'EX=',1PD11.4,5X,'EY=',D11.4,9X,'GXY=',D11.4/T2,
     1       'E1=',D11.4,5X,'AZIM1(gr)=',0PF8.4,5X,'E2 =',1PD11.4,                  
     2       3X,'AZIM2(gr)=',0PF8.4/T2,'GMAX=',1PD11.4,3X,             
     3       'DILATAZ.=',D11.4,3X,'ROTAZ.=',D11.4//)                                    
  406 FORMAT(////T10,'LA MATRICE E'' SINGOLARE PER LA',
     1       ' SERIE  # ',I3//////)
      END                                                             
      SUBROUTINE ORASP(EX,EY,GXY,X1,TH1,X2,TH2)                       
      IMPLICIT REAL*8 (A-H,O-Z)                                       
      CHARACTER*60 SCR
      COMMON/BL3/SCR,PIG,GAR,LU 
      EPS(A,B,C,D)=B*DCOS(D)**2+A*DSIN(D)**2+C*DSIN(D)*DCOS(D)        
      TH=DATAN(GXY/(EY-EX))/2                                
      TH1=TH                                
      EE=EPS(EX,EY,GXY,TH)
      X1=EE                                    
      IF(TH)1,1,2                                                     
    2 TH=TH-PIG/2                                                     
      GO TO 3                                                         
    1 TH=TH+PIG/2                                                     
    3 EE=EPS(EX,EY,GXY,TH)                                            
      IF(EE.GE.X1) GO TO 4                                            
      X2=EE                                         
      TH2=TH                                        
      GO TO 5                                        
    4 X2=X1                                                           
      TH2=TH1                                        
      X1=EE                                        
      TH1=TH                                        
    5 TH1=TH1/GAR                                                     
      TH2=TH2/GAR
      RETURN                                            
      END                                                             
