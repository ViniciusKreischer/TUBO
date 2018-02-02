                                                                                
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      PARA A ANALISE DE ESTRUTURAS PELO                            .         
C .      METODO DOS ELEMENTOS FINITOS                                 .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      INTEGER ARQ1,ARQ2,ARQ3,ARQ4,ARQ5,ARQ6,RIGROT,FORCAS,FNODEQ,ESFMOD         
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,            
     *             NLCDIN,LCASE,NLCOMB,NOMASS,NOREDI,ISAVE,NIVMAX               
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     *          ,NEMSE,NEQTV1,NEQTV2                                            
      COMMON /VAR/ NG,MODEX,CONV,GRAVID,GX,GY,GZ                                
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
     *              ,KSTIFF,ESFMOD,MODOS,INFLU,IWORK                            
      COMMON /CARGAS/ ICARGA,N302,N303,N304,N305,N306,N307,N308,                
     *                N309,N310                                                 
      COMMON /MATRIZ/IMTV,MAXTNV                                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/ARQUIV/ARQ1,ARQ2,ARQ3,ARQ4,ARQ5,ARQ6                               
      COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                   
      COMMON/EXTRA/ITIP,IMASSA                                                  
      COMMON /UM/ LOCAL,LOCAL1,NGTV,NUMINT,NGRUPO,NUMAX,KODTV,NTCICL,           
     1            CONVER                                                        
      COMMON /DOIS/ N601,N602,N603,N6021,INDV,NDEQ,N600,N604,N605,N606          
      COMMON/TRES/NLMA,NLMB,NLMCD,NHOOP,NPMAX                                   
      COMMON /PROPRI/ N5A,N5B,N5C                                               
      COMMON A(500000)                                                          
      DIMENSION TIM(9),LABEL(5),ICARGA(10)                                      
      DIMENSION IA(1)                                                           
      EQUIVALENCE (A(1),IA(1))   
C     Codigo era assim:
C     DATA LABEL/'PROG','RAMA',' PRI','NCIP','AL  '/                            
      CHARACTER*4 LABEL
      LABEL(1)='PROG'
      LABEL(2)='RAMA'
      LABEL(3)=' PRI'
      LABEL(4)='NCIP'
      LABEL(5)='AL  '
C
C  ---------- Para dar um "flush" no arquivo de saida e obter o ponto
C             exato em q a execução parou:
C     Call flush(IOUT)  by Krempser 05/04/2017
C
C--------------------- INICIALIZACAO DOS ARQUIVOS                               
      CALL TFILES                                                               
C--------------------- INICIALIZACAO DA CONTAGEM DE TEMPO ( MTS )               
      CALL TIME(0)                                                              
C--------------------- DIMENSAO DO VETOR DE TRABALHO                            
      MTOT=500000                                                               
C                                                                               
      NPAG=0                                                                    
      LINHA=0                                                                   
      LPPAG=60                                                                  
C ............................................................                  
C     CARTAO DE DUPLA PRECISAO                                                  
C        ITWO = 1 PRECISAO SIMPLES                                              
C        ITWO = 2 PRECISAO DUPLA                                                
C ............................................................                  
C                                                                               
      ITWO=2                                                                    
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C      OS SEGUINTES ARQUIVOS SAO USADOS                                         
C       IELMNT = ARMAZENA DADOS DOS ELEMENTOS                                   
C       ILOAD  = ARMAZENA VETORES DE CARGA/DESLOCAMENTOS                        
C       IIN    = LEITURA                                                        
C       IOUT   = SAIDA                                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      MODOS=3                                                                   
      IIN=4                                                                     
      INFILE=5                                                                  
      IOUT=6                                                                    
      FORCAS=7                                                                  
      FNODEQ=8                                                                  
      RIGROT=9                                                                  
      ESFMOD=10                                                                 
      IELMNT=11                                                                 
      ILOAD=12                                                                  
      IMTV=13                                                                   
C----------------- ARQUIVO PROPRIO DA ROTINA SSPACE                             
      NSTIF=14                                                                  
C---------------------------------------------------                            
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .                   
C        ARQUIVOS USADOS NA MODAL                                               
C        ARQ1,ARQ2,ARQ3,ARQ4,ARQ5,ARQ6                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .                   
       ARQ1=15                                                                  
       ARQ2=16                                                                  
       ARQ3=17                                                                  
       ARQ4=18                                                                  
       ARQ5=19                                                                  
C------------------- ARQUIVOS COMPARTILHADOS ------                             
      ARQ6=10                                                                   
      KSTIFF=14                                                                 
      INFLU=15                                                                  
      IWORK=16                                                                  
C--------------------------------------------------                             
      CALL IMAGEM(INFILE,IIN,IOUT)                                              
  200 NUMEST=0                                                                  
      MAXEST=0                                                                  
      MAXTNV=0                                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C        E  N  T  R  A  D  A                                                    
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      CALL SECOND (TIM(1))                                                      
C                                                                               
C   LEITURA DAS INFORMACOES DE CONTROLE                                         
C                                                                               
      READ (IIN,1000)   HED,                                                    
     1      NUMNP,NUMEG,NPMAT,NPSEC,NLCEST,NLCDIN,NLCOMB,NOMASS,                
     2      NOREDI,NIVMAX,MODEX,ITIP,ISAVE,                                     
     3      CONV,GRAVID,GX,GY,GZ                                                
C---------------------------------- CONTROLE DE PARADA                          
C      IF ( NUMNP .EQ. 0 ) STOP                                                 
C     todos os "STOP" foram substituidos por CALL CLOSEFILES
      IF ( NUMNP .EQ. 0 ) CALL CLOSEFILES
C---------------------------------- VALORES 'DEFAULT'                           
      IF ( CONV .LE. 0. ) CONV=1.0                                              
      IF ( GRAVID .LE. 0. ) GRAVID=1.0                                          
      IF ( GX .EQ. 0. .AND. GY .EQ. 0. .AND. GZ .EQ. 0. ) GY=-1.0               
C                                                                               
      WRITE (IOUT,2000) HED,ITIP,NUMNP,NUMEG,NPMAT,NPSEC,NLCEST,                
     1       NLCDIN,NLCOMB,NOMASS,NOREDI,NIVMAX                                 
      WRITE (IOUT,2001) MODEX,ISAVE,CONV,GRAVID,GX,GY,GZ                        
C---------------------------------- CONSISTENCIA DE 'ITIP' E 'ISAVE'            
      IF(ITIP .GE. 0  .AND.  ITIP .LE. 5)GO TO 220                              
      WRITE(IOUT,1100)ITIP                                                      
 1100 FORMAT(//,10X,' ITIP =  ',I3,'  OPCAO NAO IMPLEMENTADA')                  
      CALL CLOSEFILES
  220 IF(ISAVE .GE. 0  .AND.  ISAVE .LE. 2)GO TO 230                            
      WRITE(IOUT,1105)ISAVE                                                     
 1105 FORMAT(//,10X,' ISAVE =  ',I3,'  OPCAO NAO IMPLEMENTADA')                 
      CALL CLOSEFILES                                                                      
C-------------------------------------------------------------------            
  230 CONTINUE                                                                  
C                                                                               
C        LEITURA DOS INDICADORES DE SAIDAS INTERMEDIARIAS                       
C                       E DE DEPURACAO                                          
C                                                                               
      READ(IIN,2035)IDEPUR,INTSAI  
	  write(iout,5555) idepur,intsai
 5555 format('idepur',15i1,' intsai ',15i1)
C                                                                               
C      LEITURA DOS DADOS DOS PONTOS NODAIS                                      
C                                                                               
      N1=1                                                                      
      N2=N1+6*NUMNP                                                             
      N3=N2+NUMNP*ITWO                                                          
      N4=N3+NUMNP*ITWO                                                          
    1 N5=N4+NUMNP*ITWO                                                          
      IF (N5.GT.MTOT) CALL ERROR (N5-MTOT,1,LABEL,1)                            
C                                
      write(iout,6666)nomass, numnp, neq
 6666 format('lendo nomass, numnp, neq',3i10)	                                               
         CALL INPUT(A(N1),A(N2),A(N3),A(N4),NUMNP,NEQ)                          
C                                                                               
C        LE GERA E ARMAZENA DADOS DOS ELEMENTOS                                 
C                                                                               
C        LIMPA A MEMORIA                                                        
C                                                                               
      N5A=N5+NEQ                                                                
      N5B=N5A+8*NPMAT*ITWO                                                      
      N5C=N5B+8*NPSEC*ITWO                                                      
   2  N6=N5C+NPMAT                                                              
      IF(N6.GT.MTOT) CALL ERROR(N6-MTOT,2,LABEL,2)                              
      DO 10 I=N5,N6                                                             
   10 IA(I)=0                                                                   
C                                                                               
C     LEITURA DE PROPRIEDADES DE MATERIAIS E DE SECOES                          
C                                                                               
      CALL LERPRO(A(N5A),A(N5B),A(N5C),NPMAT,NPSEC)                             
C                                                                               
      IND=1                                                                     
      KODT=1                                                                    
      NEQT=0                                                                    
      NEQTV1=0                                                                  
      NEQTV2=0                                                                  
      CALL ELCAL                                                                
      CALL SECOND (TIM(2))                                                      
C                                                                               
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C        S O L U C A O                                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      CALL ADDRES (A(N2),A(N5))                                                 
      MM=NWK/NEQ                                                                
      WRITE(IOUT,2025) NEQ,NWK,MK,MM      
C                                                                               
C        MONTAGEM E SOLUCAO DO PROBLEMA                                         
C                                                                               
      IDIN=ITIP+1                                                               
      GO TO (610,620,620,620,630,680),IDIN                                      
C                                                                               
C        ANALISE ESTATICA                                                       
C                                                                               
  610 IF(MODEX.EQ.1) GO TO 640                                                  
      DO 615 I=3,7                                                              
  615 TIM(I)=TIM(2)                                                             
      GO TO 650                                                                 
  640 CALL ESTAT                                                                
      CALL SECOND(TIM(3))                                                       
      DO 645 I=4,7                                                              
  645 TIM(I)=TIM(3)                                                             
      GO TO 650                                                                 
C                                                                               
C        RESOLUCAO DO PROBLEMA DE AUTOVALOR                                     
C                                                                               
  620 TIM(3)=TIM(2)                                                             
      CALL AUTOV                                                                
      CALL SECOND(TIM(4))                                                       
      DO 625 I=5,7                                                              
  625 TIM(I)=TIM(4)                                                             
      GO TO (650,660,670),ITIP                                                  
C                                                                               
C        RESPOSTA - SUPERPOSICAO MODAL                                          
C                                                                               
  660 CALL MODAL                                                                
      CALL SECOND(TIM(5))                                                       
      TIM(6)=TIM(5)                                                             
      TIM(7)=TIM(5)                                                             
      GO TO 650                                                                 
C                                                                               
C        RESPOSTA - ANALISE ESPECTRAL                                           
C                                                                               
C                                                                               
C     CALL RESPEC                                                               
C                                                                               
  670 CONTINUE                                                                  
      CALL SECOND(TIM(6))                                                       
      TIM(7)=TIM(6)                                                             
      GO TO 650                                                                 
C                                                                               
C        RESPOSTA - INTEGRACAO PASSO-A-PASSO                                    
C                                                                               
  630 DO 635 I=3,6                                                              
  635 TIM(I)=TIM(2)                                                             
      CALL STEPBY                                                               
      CALL SECOND(TIM(7))                                                       
      GO TO 650                                                                 
C                                                                               
C       VERIFICACAO DE TENSOES SEGUNDO A ASME SEC. III                          
C                       	                                                          
 680  CALL TUBNUC                                                               
  650 CONTINUE                                                                  
C                                                                               
C        CALCULO E IMPRESSAO DOS TEMPOS DE SOLUCAO                              
C                                                                               
      CALL TEMPO(TIM,7)                                                         
      WRITE(IOUT,2030) HED,(TIM(I),I=1,7)                                       
C                                                                               
C        LE O PROXIMO CASO                                                      
C                                                                               
      GO TO 200                                                                 
C                                                                               
C        FORMATOS                                                               
C                                                                               
 1000 FORMAT(20A4/13I5/5F10.0)                                                  
 2000 FORMAT(1H1,20A4///                                                        
     110X,'I N F O R M A C O E S  D E  C O N T R O L E',//5X,                   
     A'TIPO DE ANALISE                           (ITIP)   =',I5,//7X,           
     B'EQ.2 - RESPOSTA/ANALISE MODAL',/7X,'EQ.4 - RESPOSTA/INTEG. PASSO-        
     CA-PASSO',/7X,'EQ.5 - VERIFICACAO SEGUNDO A ASME-III',                     
     D //5X,'NUMERO DE PONTOS NODAIS             (NUMNP)  =',I5//5X,            
     F'NUMERO DE GRUPOS DE ELEMENTOS             (NUMEG)  =',I5//5X,            
     2'NUMERO DE TABELAS DE MATERIAIS            (NTMAT)  =',I5//5X,            
     3'NUMERO DE TABELAS DE SECOES               (NTSEC)  =',I5//5X,            
     4'NUMERO DE CASOS DE CARREGAMENTO ESTATICO  (NLCEST) =',I5//5X,            
     5'NUMERO DE CASOS DE CARREGAMENTO DINAMICO  (NLCDIN) =',I5//5X,            
     6'NUMERO DE COMBINACOES DE CARREGAMENTO     (NLCOMB) =',I5//5X,            
     7'NUMERO DE MASSAS CONCENTRADAS             (NOMASS) =',I5//5X,            
     8'NUMERO DE NOS C/ RESTRICOES DINAMICAS     (NOREDI) =',I5//5X,            
     9'NUMERO MAX. DE NIVEIS P/ EXCIT. MULTIPLA  (NIVMAX) =',I5//)              
 2001 FORMAT(5X,                                                                
     Z'OPCAO VERIFICACAO OU RESOLUCAO            (MODEX)  =',I5/5X,             
     A5X,'SE MODEX = 0 IMPLICA VERIFICACAO DOS DADOS          '/5X,             
     B5X,'SE MODEX = 1 IMPLICA RESOLUCAO                      '//5X,            
     1'ARMAZENAMENTO/USO DE AUTOVETORES(VALORES) (ISAVE)  =',I5//5X,            
     8'FATOR DE CONVERSAO DAS COORDENADAS NODAIS   (CONV) =',F9.4//5X,          
     9'ACELERACAO DA GRAVIDADE                   (GRAVID) =',F9.4//5X,          
     Z'COMPONENTES DO VETOR ACEL. DA GRAVIDADE : (GX)     =',F9.4//5X,          
     A'                                          (GY)     =',F9.4//5X,          
     B'                                          (GZ)     =',F9.4)              
 2025 FORMAT(1H1,10X,'DADOS TOTAIS DO SISTEMA',              ///5X,             
     1'NUMERO DE EQUACOES                        (NEQ)    =',I5//5X,            
     2'ELEMENTOS DA MATRIZ                       (NWK)    =',I5//5X,            
     3'MAXIMA LARGURA DA SEMI-BANDA              (MK)     =',I5//5X,            
     4'LARGURA MEDIA DA SEMI-BANDA               (MM)     =',I5)                
 2030 FORMAT(1H1,10X,'E S T A T I S T I C A  D E  T E M P O',//                 
     110X,'PARA O PROBLEMA  :',20A4,////5X,                                     
     2'LEITURA DE DADOS GERAIS                            =',E12.5//5X,         
     3'SOLUCAO DO PROBLEMA ESTATICO                       =',E12.5//5X,         
     4'SOLUCAO DO PROBLEMA DE AUTOVALOR                   =',E12.5//5X,         
     5'RESPOSTA - ANALISE MODAL                           =',E12.5//5X,         
     A'RESPOSTA - ANALISE ESPECTRAL                       =',E12.5//5X,         
     6'RESPOSTA - INTEGRACAO PASSO-A-PASSO                =',E12.5//5X,         
     7'T E M P O  T O T A L  D E  E X E C U C A O         =',E12.5)             
2035  FORMAT(30I1)                                                              
      END                                                                       
         SUBROUTINE KABECA                                                      
         COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                
         COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                             
         LINHA = 2                                                              
         NPAG = NPAG + 1                                                        
         WRITE(IOUT,1000) HED,NPAG                                              
1000     FORMAT(14H1*** TUBO2 ***,9X,20A4,9X,3HPAG,I5 )                         
         RETURN                                                                 
         END                                                                    
                                                                                
      SUBROUTINE INPUT(ID,X,Y,Z,NUMNP,NEQ)                                      
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      .PARA LEITURA ,GERACAO ,E ARMAZENAMENTO DOS DADOS NODAIS     .         
C .      .PARA CALCULO DO NUMERO DE EQUACOES                          .         
C .       E ARMAZENAMENTO NA MATRIZ  ID                               .         
C .                                                                   .         
C .      N     =NUMERO DO ELEMENTO                                    .         
C .      ID    =CONDICAO DE CONTORNO(0=LIVRE,1=PRESCRITO)             .         
C .      X,Y,Z =COORDENADAS                                           .         
C .      KN    =INCREMENTO PARA GERACAO AUTOMATICA DOS NOS            .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      REAL CONV                                                                 
      COMMON /VAR/ NG,MODEX,CONV                                                
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION X(1),Y(1),Z(1),ID(6,NUMNP),NOREFE(4)                            
C      Codigo era assim:
C      DATA NOREFE/'NORE','F   ','    ','    '/                                  
      CHARACTER*4 NOREFE
      NOREFE(1)='NORE'
      NOREFE(2)='F   '
      NOREFE(3)='    '
      NOREFE(4)='    '
C                                                                               
C     LE E GERA DADOS RELATIVOS AOS PONTOS NODAIS                               
C                                                                               
      IF(INTSAI(1) .EQ. 0) GO TO 5                                              
      WRITE(IOUT,2000)                                                          
      WRITE(IOUT,2010)                                                          
      WRITE(IOUT,2020)NOREFE(1),NOREFE(2)                                       
5     CONTINUE                                                                  
      KNOLD=0                                                                   
      NOLD=0                                                                    
C                                                                               
   10 READ(IIN,1000)N,(ID(I,N),I=1,6),X(N),Y(N),Z(N),KN,NOREF                   
      IF(INTSAI(1) .EQ. 1)                                                      
     *WRITE(IOUT,2030)N,(ID(I,N),I=1,6),X(N),Y(N),Z(N),KN,NOREF                 
      IF( NOREF .LE. 0 ) GO TO 25                                               
      IF( NOREF .LT. N ) GO TO 20                                               
      WRITE(IOUT,2050) N,NOREF                                                  
      CALL CLOSEFILES                                                                      
 20   X(N)=X(N) + X(NOREF)                                                      
      Y(N)=Y(N) + Y(NOREF)                                                      
      Z(N)=Z(N) + Z(NOREF)                                                      
 25   CONTINUE                                                                  
      IF(KNOLD.EQ.0)GO TO 50                                                    
      NUM=(N-NOLD)/KNOLD                                                        
      NUMN=NUM-1                                                                
      IF(NUMN.LT.1)GO TO 50                                                     
      XNUM=NUM                                                                  
      DX=(X(N)-X(NOLD))/XNUM                                                    
      DY=(Y(N)-Y(NOLD))/XNUM                                                    
      DZ=(Z(N)-Z(NOLD))/XNUM                                                    
      K=NOLD                                                                    
      DO 30 J=1,NUMN                                                            
      KK=K                                                                      
      K=K+KNOLD                                                                 
      X(K)=X(KK)+DX                                                             
      Y(K)=Y(KK)+DY                                                             
      Z(K)=Z(KK)+DZ                                                             
      DO 30 I=1,6                                                               
      ID(I,K)=ID(I,KK)                                                          
   30 CONTINUE                                                                  
C                                                                               
   50 NOLD=N                                                                    
      KNOLD=KN                                                                  
      IF(N.NE.NUMNP)GO TO 10                                                    
C                                                                               
C        CONVERSAO DAS COORDENADAS                                              
C                                                                               
      DO 60 I=1,NUMNP                                                           
      X(I)=X(I)*CONV                                                            
      Y(I)=Y(I)*CONV                                                            
 60   Z(I)=Z(I)*CONV                                                            
C                                                                               
C        ESCREVE DADOS NODAIS COMPLETOS                                         
       IF(INTSAI(1) .EQ. 0) GO TO 55                                            
      WRITE(IOUT,2015)                                                          
      WRITE(IOUT,2020)NOREFE(3),NOREFE(4)                                       
      DO 200 N=1,NUMNP                                                          
  200 WRITE(IOUT,2030) N,(ID(I,N),I=1,6),X(N),Y(N),Z(N),KN                      
55     CONTINUE                                                                 
C                                                                               
C     NUMERO DE INCOGNITAS                                                      
C                                                                               
      NEQ=0                                                                     
      DO 100 N=1,NUMNP                                                          
      DO 100 I=1,6                                                              
      IF(ID(I,N))110,120,110                                                    
  120 NEQ=NEQ+1                                                                 
      ID(I,N)=NEQ                                                               
      GO TO 100                                                                 
  110 ID(I,N)=0                                                                 
  100 CONTINUE                                                                  
C                                                                               
C     ESCREVE NUMERO DE EQUACOES                                                
C                                                                               
      IF(INTSAI(1) .EQ. 1)                                                      
     *WRITE(IOUT,2040)(N,(ID(I,N),I=1,6),N=1,NUMNP)                             
      RETURN                                                                    
C                                                                               
 1000 FORMAT(7I5,3F10.0,2I5)                                                    
 2000 FORMAT(1H1,10X,'D A D O S  R E L A T I V O S  A O S  N O S'//)            
 2010 FORMAT(10X,'DADOS NODAIS LIDOS'//)                                        
 2015 FORMAT(///10X,'DADOS NODAIS GERADOS'//)                                   
 2020 FORMAT('  NUMERO',11X,'CONDICOES DE CONTORNO',20X,'COORDENADAS',          
     1        16X,'CODIGO',/'  DO  NO',18X,'(CODIGO)',29X,'NODAIS',             
     2        21X,'GERACAO',/85X,'MALHA',/15X,'X',4X,'Y',4X,'Z',3X,             
     3'RX,',3X,'RY',3X,'RZ',11X,'X',11X,'Y',12X,'Z',8X,'KN',4X,2A4)             
 2030 FORMAT(I5,6X,6I5,4X,3F12.4,I5,3X,I5)                                      
 2040 FORMAT(//10X,'NUMERO DE EQUACOES',//2X,'NUMERO',10X,'GRAUS DE',           
     1        ' LIBERDADE',/2X,'DO  NO',6X,'X',4X,'Y',4X,'Z',3X,'RX',           
     2        3X,'RY',3X,'RZ',//(I5,5X,6I5))                                    
 2050 FORMAT(//,10X,'COORDENADAS DO NO',I3,' DADAS EM RELACAO AO NO',I3         
     1       ,' NAO FORNECIDO ANTERIORMENTE.',//,10X,'EXECUCAO INTERROM'        
     2       ,'PIDA.')                                                          
C                                                                               
      END                                                                       
      SUBROUTINE LERPRO(TABMAT,TABSEC,KCURVA,NPMAT,NPSEC)                       
C                                                                               
C     SUBROTINA PARA LEITURA DAS TABELAS DE PROPRIEDADES DE MATERIAIS           
C     E DE SECOES                                                               
C     TABMAT: TABELAS DE MATERIAIS; TABSEC: TABELAS DE SECOES                   
C     KCURVA: NUMEROS DAS CURVAS DE FADIGA (ASSOCIADAS AOS MATERIAIS)           
C                                                                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION TABMAT(8,1),TABSEC(8,1),KCURVA(1)                               
      NLINHA=0                                                                  
      DO 100 MAT=1,NPMAT                                                        
      READ(IIN,1000) N,(TABMAT(L,N),L=1,8),KCURVA(N)                            
      IF(INTSAI(1) .NE. 1)GO TO 100                                             
      NLINHA=NLINHA+2                                                           
      IF(NLINHA.GT.44) NLINHA=2                                                 
      IF(NLINHA.EQ.2) WRITE(IOUT,2000)                                          
      WRITE(IOUT,2010) N,(TABMAT(L,N),L=1,8),KCURVA(N)                          
  100 CONTINUE                                                                  
      IF(INTSAI(1) .NE. 1)GO TO 140                                             
      IF(NLINHA.GT.30) NLINHA=0                                                 
      IF(NLINHA.LT.30)WRITE(IOUT,2020)                                          
 140  DO 200 NSEC=1,NPSEC                                                       
      READ(IIN,1010) N,(TABSEC(L,N),L=1,8)                                      
      IF (INTSAI(1) .NE. 1)GO TO 200                                            
      NLINHA=NLINHA+2                                                           
      IF(NLINHA.GT.44) NLINHA=2                                                 
      IF(NLINHA.GT.2) GO TO 150                                                 
      WRITE(IOUT,2025)                                                          
      WRITE(IOUT,2020)                                                          
 150  WRITE(IOUT,2030) N,(TABSEC(L,N),L=1,8)                                    
  200 CONTINUE                                                                  
C                                                                               
C                                                                               
 1000 FORMAT(I5,F10.0,F5.0,5F10.0,F5.0,I5)                                      
 1010 FORMAT(I5,F20.0,6F10.0,F5.0)                                                    
 2000 FORMAT(1H1,/,10X,'TABELAS DE PROPRIEDADES DOS MATERIAIS',//,              
     14X,'N MD. YOUNG     POISSON   EXP TERMICA    SM/SC',8X,'NN',10X,'M        
     2M',10X,'SH',5X,'FAT REDUCAO',2X,'CURVA DE FADIGA',/)                      
 2010 FORMAT(/,I5,8G12.4,6X,I3)                                                 
C                                                                               
 2020 FORMAT(///,10X,'TABELAS DE PROPRIEDADES DAS SECOES',//,4X,                
     1'N TUBULARES :   DIAM. EXT.   ESPESSURA  PESO/COMPR. ESP. REVEST P        
     2ESO REVEST PESO CONT. PRESS P/FLEX',/,6X,                                 
     3'VIGAS RETAS :    AREA      INERCIA X   INERCIA Y   INERCIA Z  MAS        
     4S ESPEC  CORTANTE Y  CORTANTE Z  ORIENT SECAO',/)                         
 2025 FORMAT(1H1)                                                               
 2030 FORMAT(/,I5,13X,8G12.4)                                                   
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE COLHT(MHT,ND,LM)                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA CALCULAR ALTURA DAS COLUNAS                           .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION LM(1),MHT(1)                                                    
C                                                                               
      LS=100000                                                                 
      DO 100 I=1,ND                                                             
      IF (LM(I)) 110,100,110                                                    
  110 IF (LM(I)-LS) 120,100,100                                                 
  120 LS=LM(I)                                                                  
  100 CONTINUE                                                                  
C                                                                               
      DO 200 I=1,ND                                                             
      II=LM(I)                                                                  
      IF (II.EQ.0) GO TO 200                                                    
      ME=II-LS                                                                  
      IF (ME.GT.MHT(II)) MHT(II)=ME                                             
  200 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ASSEM(AA)                                                      
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA CHAMAR SUBROTINA DOS ELEMENTOS PARA MONTAR            .         
C .        A MATRIZ DE RIGIDEZ DA ESTRUTURA                           .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      INTEGER RIGROT                                                            
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC                                       
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT                         
      COMMON/MATRIZ/IMTV                                                        
      COMMON/EXTRA/ITIP                                                         
      DIMENSION AA(1)                                                           
      REWIND IELMNT                                                             
      REWIND RIGROT                                                             
      REWIND IMTV                                                               
      DO 200 N=1,NUMEG                                                          
      READ(IELMNT)NUMEST,NPAR,(AA(I),I=1,NUMEST)                                
      IF(ITIP.EQ.1) NPAR(10)=1                                                  
C                                                                               
      CALL ELEMNT                                                               
C                                                                               
  200 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ELCAL                                                          
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA VARRER TODOS OS GRUPOS DE ELEMENTOS LENDO,            .         
C .        GERANDO E ARMAZENANDO DADOS DE ELEMENTOS                   .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /VAR/ NG                                                           
      COMMON A(1)                                                               
      COMMON /MATRIZ/IMTV,MAXTNV                                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /EXTRA/ ITIP,IMASSA                                                
C                                                                               
      REWIND IELMNT                                                             
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2000)                                   
C                                                                               
C        LOOP SOBRE TODOS OS GRUPOS DE ELEMENTOS                                
C                                                                               
C     # NO. DE GRUPOS COM TENSOES CALCULADAS              
      NGTC=0                
C     # MASSA DIAGONAL =>  NWM = NEQ                      
      IMASSA=0              
      DO 100 NG=1,NUMEG                                                         
      IF( INTSAI(1) .EQ. 1 .AND. NG .NE. 1 ) WRITE(IOUT,2010)                   
C                                                                               
      READ (IIN,1000)(NPAR(I),I=1,5),(NPAR(J),J=10,20)                          
C     # STIF6 N/ CALC. TENSOES               
      IF( NPAR(1) .EQ. 6 ) NPAR(10)=-1   
C     # MOLA  N/ TEM MAT. MASSA              
      IF( NPAR(1) .EQ. 4 ) NPAR(4)=-1    
C     # STIF6 N/ TEM MAT. MASSA              
      IF( NPAR(1) .EQ. 6 ) NPAR(4)=-1    
C     # MASSA CONSIST.=>NWM=NWK              
      IF( NPAR(4) .EQ. 0 ) IMASSA=1      
C     # GRUPO COM TENSOES CALC.              
      IF( NPAR(10).GE. 0 ) NGTC=NGTC+1   
      DO 50 I=6,9                                                               
   50 NPAR(I)=0                                                                 
      NPAR(20)=0                                                                
C                                                                               
      CALL ELEMNT                                                               
      IF( MIDEST.GT.MAXEST ) MAXEST=MIDEST                                      
      IF(NPAR(20).GT.MAXTNV) MAXTNV=NPAR(20)                                    
C                                                                               
      WRITE(IELMNT)MIDEST,NPAR,(A(I),I=NFIRST,NLAST)                            
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
C                                                                               
 1000 FORMAT(16I5)                                                              
 2000 FORMAT(1H1,10X,'D A D O S  D O S  G R U P O S  D E  E L E M E N T         
     1O S ',///)                                                                
 2010 FORMAT (1H1)                                                              
C                                                                               
      END                                                                       
                                                                                
      SUBROUTINE ELEMNT                                                         
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA CHAMAR A SUBROTINA DO ELEMENTO APROPIADO              .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /EL/ IND,NPAR(20)                                                  
C                                                                               
      NPAR1=NPAR(1)                                                             
C                                                                               
      GO TO(1,2,3,4,5,6,7),NPAR1                                                
C                                                                               
    1 CALL TRUSS                                                                
      RETURN                                                                    
C                                                                               
    2 CALL PVIGA                                                                
      RETURN                                                                    
C                                                                               
 3    CALL TUBOR                                                                
      RETURN                                                                    
C                                                                               
    4 CALL MOLAS                                                                
      RETURN                                                                    
C                                                                               
 5    CALL TUBOC                                                                
      RETURN                                                                    
C                                                                               
 6    CALL STIF6                                                                
      RETURN                                                                    
C                                                                               
 7    CALL TUBOCP                                                               
      RETURN                                                                    
C                                                                               
C      OUTROS TIPOS DE ELEMENTOS SERAO CHAMADOS AQUI,                           
C      IDENTIFICANDO CADA TIPO DE ELEMENTO POR DIFERENTES                       
C      PARAMETROS NPAR(1)                                                       
C                                                                               
      END                                                                       
      SUBROUTINE ADDRES(MAXA,MHT)                                               
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA CALCULAR ENDERECO DOS ELEMENTOS DA DIAGONAL NA        .         
C .        NA MATRIZ BANDA CUJA ALTURA DAS COLUNAS SAO CONHECIDAS     .         
C .                                                                   .         
C .        MHT  = ALTURA DA COLUNA ATIVA                              .         
C .        MAXA = ENDERECO DO ELEMENTO DA DIAGONAL                    .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION MAXA(2),MHT(1)                                                  
C                                                                               
C      ZERAR VETOR MAXA                                                         
C                                                                               
      NN=NEQ+1                                                                  
      DO 20 I=1,NN                                                              
   20 MAXA(I)=0                                                                 
C                                                                               
      MAXA(1)=1                                                                 
      MAXA(2)=2                                                                 
      MK=0                                                                      
      IF (NEQ.EQ.1) GO TO 100                                                   
      DO 10 I=2,NEQ                                                             
      IF (MHT(I).GT.MK) MK=MHT(I)                                               
   10 MAXA(I+1)=MAXA(I)+MHT(I)+1                                                
  100 MK=MK+1                                                                   
      NWK=MAXA(NEQ+1)-MAXA(1)                                                   
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ROTA(CX,CY,CZ,SAL,CAL,XP,YP,ZP,ROT,IDEF)                       
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .      SUBROTINA PARA MONTAR A MATRIZ DE ROTACAO                    .         
C .      DO SISTEMA LOCAL PARA O GLOBAL                               .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      DIMENSION ROT(3,3)                                                        
      SQRT(X)=DSQRT(X)                                                          
      TOL=0.001                                                                 
      IF( IDEF .GT. 0 ) GO TO 10                                                
      Q=SQRT(CX*CX + CZ*CZ)                                                     
      IF( Q .GT. TOL ) GO TO 5                                                  
      ROT(1,1)=0.                                                               
      ROT(1,2)= CY                                                              
      ROT(1,3)=0.                                                               
      ROT(2,1)=-CY*CAL                                                          
      ROT(2,2)=0.                                                               
      ROT(2,3)=SAL                                                              
      ROT(3,1)=CY*SAL                                                           
      ROT(3,2)=0.                                                               
      ROT(3,3)=CAL                                                              
      RETURN                                                                    
 5    ROT(2,1)=-(CX*CY*CAL + CZ*SAL)/Q                                          
      ROT(2,2)= Q*CAL                                                           
      ROT(2,3)=(-CY*CZ*CAL + CX*SAL)/Q                                          
      ROT(3,1)= (CX*CY*SAL - CZ*CAL)/Q                                          
      ROT(3,2)= Q*SAL                                                           
      ROT(3,3)= (CY*CZ*SAL + CX*CAL)/Q                                          
      GO TO 40                                                                  
 10   ZX= CY*ZP - CZ*YP                                                         
      ZY= CZ*XP - CX*ZP                                                         
      ZZ= CX*YP - CY*XP                                                         
      Q=SQRT( ZX*ZX + ZY*ZY + ZZ*ZZ)                                            
      ZX=ZX/Q                                                                   
      ZY=ZY/Q                                                                   
      ZZ=ZZ/Q                                                                   
      ROT(2,1)= ZY*CZ - ZZ*CY                                                   
      ROT(2,2)= ZZ*CX - ZX*CZ                                                   
      ROT(2,3)= ZX*CY - ZY*CX                                                   
      ROT(3,1)= ZX                                                              
      ROT(3,2)= ZY                                                              
      ROT(3,3)= ZZ                                                              
 40   ROT(1,1)= CX                                                              
      ROT(1,2)= CY                                                              
      ROT(1,3)= CZ                                                              
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PROD(RE,FI,V,M,N,L,K,KK)                                       
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION RE(L),V(K),FI(M,N)                                              
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      PARA EFETUAR O PRODUTO DE UMA MATRIZ RETANGULAR 'FI'         .         
C .      POR UM VETOR 'V' COLOCANDO O RESULTADO NO VETOR 'RE'         .         
C .      KK.EQ.1 - PRODUTO SEM TRANSPOR                               .         
C .      KK.NE.1 - PRODUTO PELA MATRIZ TRANSPOSTA                     .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      DO 10 I=1,L                                                               
      RE(I)=0.                                                                  
      DO 10 J=1,K                                                               
      IF(KK.EQ.1) GO TO 11                                                      
      RE(I)=RE(I)+FI(J,I)*V(J)                                                  
      GO TO 10                                                                  
   11 RE(I)=RE(I)+FI(I,J)*V(J)                                                  
   10 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE MULTI(  ITR,A,B,C)                                             
      IMPLICIT REAL*8(A-H,O-Z)                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .      SUBROTINA PARA CALCULO DO PRODUTO MATRICIAL                  .         
C .      C*B  GUARDANDO O RESULTADO EM A                              .         
C .                                                                   .         
C .      ITR  =VARIAVEL INDICATRIZ DE  C                              .         
C .            SE = 1  A = C * B  (OBS.AS MATRIZES A,C PODEM SER      .         
C .                                      COINCIDENTES                 .         
C .            SE = 2 A = CT * B  (OBS. AS MATRIZES A,B PODEM SER     .         
C                                        COINCIDENTES )               .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION B(3,3),C(3,3),AUX(3),A(3,3)                                     
      N=3                                                                       
      GO TO(1,2),ITR                                                            
    1 CONTINUE                                                                  
      DO 10 I=1,N                                                               
      DO 20 J=1,N                                                               
      AUX(J)=0.                                                                 
      DO 20 K=1,N                                                               
      AUX(J)=C(I,K)*B(K,J)+AUX(J)                                               
   20 CONTINUE                                                                  
      DO 10 L=1,N                                                               
      A(I,L)=AUX(L)                                                             
   10 CONTINUE                                                                  
      RETURN                                                                    
    2 CONTINUE                                                                  
      DO 30 I=1,N                                                               
      DO 40 J=1,N                                                               
      AUX(J)=0.                                                                 
      DO 40 K=1,N                                                               
      AUX(J)=C(K,J)*B(K,I) + AUX(J)                                             
   40 CONTINUE                                                                  
      DO 30 L=1,N                                                               
      A(L,I)=AUX(L)                                                             
   30 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
         SUBROUTINE MULT (TT,B,RR,MAXA,NN,NWM)                                  
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA PARA AVALIAR O PRODUTO DE B POR RR E ARMAZENAR O     *        
C*       RESULTADO EM TT.                                              *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         DIMENSION TT(1),B(1),RR(1),MAXA(1)                                     
C                                                                               
         IF (NWM.GT.NN) GO TO 20                                                
         DO 10 I=1,NN                                                           
   10    TT(I)=B(I)*RR(I)                                                       
         RETURN                                                                 
C                                                                               
   20    DO 40 I=1,NN                                                           
   40    TT(I)=0.                                                               
         DO 100 I=1,NN                                                          
         KL = MAXA(I)                                                           
         KU=MAXA(I+1)-1                                                         
         II=I+1                                                                 
         CC=RR(I)                                                               
         DO 100 KK=KL,KU                                                        
         II=II-1                                                                
  100    TT(II)=TT(II)+B(KK)*CC                                                 
         IF (NN.EQ.1) RETURN                                                    
         DO 200 I=2,NN                                                          
         KL=MAXA(I)+1                                                           
         KU=MAXA(I+1)-1                                                         
         IF (KU-KL) 200,210,210                                                 
  210    II=I                                                                   
         AA = 0.                                                                
         DO 220 KK=KL,KU                                                        
         II=II-1                                                                
  220    AA=AA+B(KK)*RR(II)                                                     
         TT(I)=TT(I)+AA                                                         
  200    CONTINUE                                                               
         RETURN                                                                 
C                                                                               
         END                                                                    
                                                                                
      SUBROUTINE PRODUT(TT,B,RR,MAXA,NN,NWM)                                    
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA PARA AVALIAR O PRODUTO DE B POR RR E ARMAZENAR O     *        
C*       RESULTADO EM TT. ( TT NAO E INICIALIZADO)                     *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         DIMENSION TT(1),B(1),RR(1),MAXA(1)                                     
C                                                                               
         IF (NWM.GT.NN) GO TO 20                                                
         DO 10 I=1,NN                                                           
   10    TT(I)=B(I)*RR(I) +TT(I)                                                
         RETURN                                                                 
C                                                                               
   20 CONTINUE                                                                  
         DO 100 I=1,NN                                                          
         KL = MAXA(I)                                                           
         KU=MAXA(I+1)-1                                                         
         II=I+1                                                                 
         CC=RR(I)                                                               
         DO 100 KK=KL,KU                                                        
         II=II-1                                                                
  100    TT(II)=TT(II)+B(KK)*CC                                                 
         IF (NN.EQ.1) RETURN                                                    
         DO 200 I=2,NN                                                          
         KL=MAXA(I)+1                                                           
         KU=MAXA(I+1)-1                                                         
         IF (KU-KL) 200,210,210                                                 
  210    II=I                                                                   
         AA = 0.                                                                
         DO 220 KK=KL,KU                                                        
         II=II-1                                                                
  220    AA=AA+B(KK)*RR(II)                                                     
         TT(I)=TT(I)+AA                                                         
  200    CONTINUE                                                               
         RETURN                                                                 
C                                                                               
         END                                                                    
                                                                                
                                                                                
      SUBROUTINE COMPAC(IA,IB,N1,N2)                                            
      DIMENSION IA(1),IB(1)                                                     
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .        PARA COMPACTAR O VETOR 'A'.                                .         
C .        SE IA(1)=A(NI) E IB(1)=A(N1), OS (N2-N1) ELEMENTOS DE      .         
C .        A(N1) SAO TRANSFERIDOS PARA O ENDERECO A(NI).              .         
C .                                                                   .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      N=N2-N1                                                                   
      DO 5 I=1,N                                                                
    5 IA(I)=IB(I)                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE CLEAR(A,N)                                                     
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA LIMPAR O ARRAY A                                      .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      DIMENSION A(1)                                                            
      DO 10 I=1,N                                                               
   10 A(I)=0.                                                                   
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE ADDBAN (A,MAXA,S,LM,ND,XM,SM,N)                                
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA MONTAGEM DA MATRIZ DE RIGIDEZ TRIANGULAR SUPERIOR     .         
C .        DO ELEMENTO NA MATRIZ GLOBAL COMPACTADA                    .         
C .                                                                   .         
C .        A  = MATRIZ DE RIGIDEZ GLOBAL                              .         
C .        S  = MATRIZ DE RIGIDEZ DO ELEMENTO                         .         
C .        SM = MATRIZ DE MASSA DO ELEMENTO                           .         
C .        XM = MATRIZ DE MASSA GLOBAL                                .         
C .        ND = GRAUS DE LIBERDADE NO ELEMENTO                        .         
C .   VERIFICAR USO DE NPAR(4) !!!!!!                                 .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      IMPLICIT REAL*8(A-H,O-Z)                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
C                                                                               
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                      
      COMMON/EL/IND,NPAR(20)                                                    
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION A(1),MAXA(1),S(1),LM(1),XM(1),SM(1)                             
      NDI=0                                                                     
      DO 200 I=1,ND                                                             
      II=LM(I)                                                                  
      IF(II) 200,200,100                                                        
  100 MI=MAXA(II)                                                               
      KS=I                                                                      
      DO 220 J=1,ND                                                             
      JJ=LM(J)                                                                  
      IF (JJ) 220,220,110                                                       
  110 IJ=II-JJ                                                                  
      IF (IJ) 220,210,210                                                       
  210 KK=MI + IJ                                                                
      KSS= KS                                                                   
      IF (J.GE.I) KSS=J + NDI                                                   
      A(KK)=A(KK) + S(KSS)                                                      
C     # MASSA CONSIST.               
      IF( NPAR(4) .EQ. 0 ) XM(KK)=XM(KK)+SM(KSS) 
  220 KS=KS +ND-J                                                               
  200 NDI=NDI +ND-I                                                             
C                                                                               
      IF( N .NE. NPAR(2) ) RETURN                                               
      IF(INTSAI(3) .EQ. 0) RETURN                                               
      WRITE(IOUT,1000)                                                          
 1000 FORMAT(//120('*'),//10X,'MATRIZ DE RIGIDEZ GLOBAL',                       
     1       //120('*'))                                                        
      DO 1111 I=1,NEQ                                                           
         J=MAXA(I)                                                              
        J1=MAXA(I+1)-1                                                          
C       Era assim:
C        WRITE(IOUT,1010)(I,(A(K),K=J,J1))                                       
        WRITE(IOUT,1010)I,(A(K),K=J,J1)
1010    FORMAT(I5,/,(8G15.5))                                                   
1111  CONTINUE                                                                  
C     # MAT. N/ CONSISTENTE                         
      IF( NPAR(4) .NE. 0 ) RETURN 
      WRITE(IOUT,1020)                                                          
 1020 FORMAT(//120('*'),//10X,'MATRIZ DE MASSA GLOBAL',                         
     1       //120('*'))                                                        
      IF(NWM .EQ. NEQ) GO TO 1115                                               
      DO 1112 I=1,NEQ                                                           
      J=MAXA(I)                                                                 
      J1=MAXA(I+1)-1                                                            
C     Era assim:
C      WRITE(IOUT,1010)(I,(XM(K),K=J,J1))                                        
      WRITE(IOUT,1010)I,(XM(K),K=J,J1)
1112  CONTINUE                                                                  
      GO TO 300                                                                 
1115  WRITE(IOUT,1030)(K,XM(K),K=1,NEQ)                                         
 1030 FORMAT(I5,G20.7)                                                          
  300 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C                                                                               
      SUBROUTINE ERROR(N,L,LABEL,M)                                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .      P R O G R A M A                                              .         
C .          IMPRIME MENSAGENS DE ERRO QUANDO                         .         
C .          DIMENSAO DO VETOR 'A' E EXCEDIDA                                   
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF                                  
      COMMON/EL/MUDA(23),MTOT,MMUDA(3)                                          
      DIMENSION LABEL(4)                                                        
      K=MTOT+N                                                                  
      WRITE(IOUT,2000) N,MTOT,K                                                 
      WRITE(IOUT,3000) L                                                        
      WRITE(IOUT,4000) LABEL                                                    
      WRITE(IOUT,5000) M                                                        
      CALL CLOSEFILES                                                                      
C                                                                               
C        FORMATOS                                                               
C                                                                               
 2000 FORMAT(//5X,'DIMENSAO DO VETOR  A  FOI EXCEDIDA DE',I10//                 
     15X,'DIMENSAO ATUAL             - COMMON A (',I10,')',//                   
     25X,'DIMENSAO MINIMA NECESSARIA - COMMON A (MTOT=',I10,                    
     3')')                                                                      
3000  FORMAT(///,5X,'ERRO NUMERO    =',I5)                                      
 4000 FORMAT(///5X,'ERRO NA(O)      =',5A4)                                     
 5000 FORMAT(///,5X,'DECLARACAO NUM.=',I5)                                      
      END                                                                       
                                                                                
      SUBROUTINE LOADV (V,N,ITAPE,KOD,KODIMP)                                   
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA LER OU ESCREVER UM VETOR V DE DIMENSAO N NO ARQUIVO   .         
C .        ITAPE                                                      .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C                                                                               
      INTEGER RIGROT,FORCAS                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS                  
      DIMENSION V(N)                                                            
      GO TO (1,2),KOD                                                           
 1    WRITE(ITAPE)(V(I),I=1,N)                                                  
      GO TO 3                                                                   
 2    READ (ITAPE)(V(I),I=1,N)                                                  
 3    IF( KODIMP .EQ. 1 ) WRITE(IOUT,2000)(V(I),I=1,N)                          
 2000 FORMAT(//,1X,120('-')//10X,'V E T O R   D E   C A R G A S '//             
     *  10(8G15.5/))                                                            
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE CARGA(AA)                                                      
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      PARA CALCULO DAS CARGAS NODAIS                               .         
C .      EQUIVALENTES EM TODOS OS ELEMENTOS                           .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      INTEGER RIGROT,FORCAS,FNODEQ                                              
      COMMON/EL/IND,NPAR(20),NUMEG                                              
      COMMON/VAR/NG,MODEX                                                       
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ             
      DIMENSION AA(1)                                                           
      REWIND IELMNT                                                             
      REWIND RIGROT                                                             
      REWIND FNODEQ                                                             
      DO 100 N=1,NUMEG                                                          
      NG=N                                                                      
C                                                                               
      READ(IELMNT)NUMEST,NPAR,(AA(I),I=1,NUMEST)                                
C                                                                               
      CALL ELEMNT                                                               
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE COLSOL(A,V,MAXA,NN,NWK,NNM,KKK)                                
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA RESOLVER AS EQUACOES DE EQUILIBRIO ESTATICO DOS       .         
C .        ELEMENTOS FINITOS NA MEMORIA, USANDO ARMANEZAMENTO         .         
C .        COMPACTO E ESQUEMA DE REDUCAO DAS ALTURAS DAS COLUNAS      .         
C .                                                                   .         
C .      . VARIAVEIS DE ENTRADA .                                     .         
C .        A(NWK)    = MATRIZ DE RIGIDEZ NA FORMA COMPACTA            .         
C .        V(NN)     = VECTOR DE CARGAS                               .         
C .        MAXA(NNM) = VECTOR CONTENDO ENDERECOS DOS ELEMENTOS DA     .         
C .                    DIAGONAL DA MATRIZ A                           .         
C .        NN        = NUMERO DE EQUACOES                             .         
C .        NWK       = NUMERO DE ELEMENTOS ABAIXO DE SKYLINE          .         
C .                    DA MATRIZ                                      .         
C .        NNM       = NN+1                                           .         
C .        KKK       = VARIAVEL DE CONTROL                            .         
C .                    SE = 1 TRIANGULARIZA A MATRIZ DE RIGIDEZ       .         
C .                    SE = 2 REDUZ E FAZ RETRO-SUBSTITUICAO          .         
C .        IOUT      = NUMERO DE DISPOSITIVOS DE SAIDA                .         
C .                                                                   .         
C .      . SAIDA .                                                    .         
C .        A(NNK)    = D  E  L  FATORES DA MATRIZ DE RIGIDEZ          .         
C .        V(NN)     = VETOR DOS DESLOCAMENTOS                        .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      IMPLICIT REAL*8(A-H,O-Z)                                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
C                                                                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION A( 1 ),V(1),MAXA(1)                                             
C                                                                               
C       FATORACAO DA MATRIZ DE RIGIDEZ                                          
C     *********************************************************                 
      IF(IDEPUR(3) .EQ. 0) GO TO 6901                                           
      WRITE(IOUT,6900)                                                          
 6900 FORMAT(/120('*'),//10X,'MATRIZ DE RIGIDEZ GLOBAL',//120('*'))             
         WRITE(IOUT,7000)(A(I),I=1,NWK)                                         
7000     FORMAT(/8G15.5)                                                        
6901  CONTINUE                                                                  
C     *********************************************************                 
      IF(KKK-2)40,150,150                                                       
   40 DO 140 N=1,NN                                                             
      KN=MAXA(N)                                                                
      KL=KN+1                                                                   
      KU=MAXA(N+1)-1                                                            
      KH=KU-KL                                                                  
      IF(KH)110,90,50                                                           
   50 K=N-KH                                                                    
      IC=0                                                                      
      KLT=KU                                                                    
      DO 80 J=1,KH                                                              
      IC=IC+1                                                                   
      KLT=KLT-1                                                                 
      KI=MAXA(K)                                                                
      ND=MAXA(K+1)- KI-1                                                        
      IF(ND)80,80,60                                                            
   60 KK=MIN0(IC,ND)                                                            
      C=0.                                                                      
      DO 70 L=1,KK                                                              
   70 C=C+A(KI+L)*A(KLT+L)                                                      
      A(KLT)=A(KLT)-C                                                           
   80 K=K+1                                                                     
   90 K=N                                                                       
      B=0.                                                                      
      DO 100 KK=KL,KU                                                           
      K=K-1                                                                     
      KI=MAXA(K)                                                                
      C=A(KK)/A(KI)                                                             
      B=B+C*A(KK)                                                               
  100 A(KK)=C                                                                   
      A(KN)=A(KN)-B                                                             
  110 IF (A(KN))120,120,140                                                     
  120 WRITE(IOUT,2000)N,A(KN)                                                   
      CALL CLOSEFILES                                                                      
  140 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C      REDUCAO DO VETOR DE CARGAS                                               
C                                                                               
  150 DO 180 N=1,NN                                                             
      KL=MAXA(N)+1                                                              
      KU=MAXA(N+1)-1                                                            
      IF(KU-KL)180,160,160                                                      
  160 K=N                                                                       
      C=0.                                                                      
      DO 170 KK=KL,KU                                                           
      K=K-1                                                                     
  170 C=C+A(KK)*V(K)                                                            
      V(N)=V(N)-C                                                               
  180 CONTINUE                                                                  
C                                                                               
C      BACK-SUBSTITUTION                                                        
C                                                                               
      DO 200 N=1,NN                                                             
      K=MAXA(N)                                                                 
  200 V(N)=V(N)/A(K)                                                            
      IF(NN.EQ.1) RETURN                                                        
      N=NN                                                                      
      DO 230 L=2,NN                                                             
      KL=MAXA(N)+1                                                              
      KU=MAXA(N+1)-1                                                            
      IF(KU-KL)230,210,210                                                      
  210 K=N                                                                       
      DO 220 KK=KL,KU                                                           
      K=K-1                                                                     
  220 V(K)=V(K)-A(KK)*V(N)                                                      
  230 N=N-1                                                                     
      RETURN                                                                    
 2000 FORMAT(//5X,'PARADA-MATRIZ DE RIGIDEZ NAO E POSITIVA DEFINIDA',//         
     1         5X,'PIVOT NAO-POSITIVO PARA A EQUACAO',I4//                      
     2         5X,'PIVOT = ',G20.12)                                            
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE WRITE (DISP,ID,NEQ,NUMNP,KOD,IPAG)                             
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA IMPRIMIR  DESLOCAMENTOS OU ACELERACOES              . 00        
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                   
      REAL HED                                                                  
      DIMENSION DISP(NEQ),ID(6,NUMNP),TITULO(14)                                
      DIMENSION D(6)                                                            
C     Era assim:
C      DATA TITULO/'DESL','OCAM','ENTO','S  G','ENER','ALIZ','ADOS',             
C     *            'ACEL','ERAC','OES ',' GEN','ERAL','IZAD','AS  '/ 
      CHARACTER*4 TITULO
      TITULO(1)='DESL'
      TITULO(2)='OCAM'
      TITULO(3)='ENTO'
      TITULO(4)='S  G'
      TITULO(5)='ENER'
      TITULO(6)='ALIZ'
      TITULO(7)='ADOS'
      TITULO(8)='ACEL'
      TITULO(9)='ERAC'
      TITULO(10)='OES '
      TITULO(11)=' GEN'
      TITULO(12)='ERAL'
      TITULO(13)='IZAD'
      TITULO(14)='AS  '
      IT=7*(KOD-1) + 1                                                          
      ITF=IT+6                                                                  
      IF( IPAG .EQ. 1 ) CALL KABECA                                             
      WRITE(IOUT,2000)(TITULO(I),I=IT,ITF)                                      
      IC=4                                                                      
      DO 100 II=1,NUMNP                                                         
      IC=IC+1                                                                   
      IF(IC.LT.55) GO TO 105                                                    
      IF( IPAG .EQ. 1 ) CALL KABECA                                             
      WRITE(IOUT,2000)(TITULO(I),I=IT,ITF)                                      
      IC=4                                                                      
  105 DO 110 I=1,6                                                              
  110 D(I)=0.                                                                   
C                                                                               
      DO 120 I=1,6                                                              
      KK=ID(I,II)                                                               
      IL=I                                                                      
  120 IF (KK.NE.0) D(IL)=DISP(KK)                                               
C                                                                               
  100 WRITE(IOUT,2010) II,D                                                     
C                                                                               
C                                                                               
      RETURN                                                                    
 2000 FORMAT(       /10X,           7A4                ,//2X,                   
     1        'NO',18X,'DESLOCAMENTOS',35X,'ROTACOES',                          
     2        /15X,'X',14X,'Y',14X,'Z',14X,'RX',13X,'RY',13X,'RZ',/)            
 2010 FORMAT(I5,3X,6(G13.6,2X))                                                 
C                                                                               
      END                                                                       
                                                                                
      SUBROUTINE WROTE(ID,DISP,D,NROOT,NC,EIGV,BLO,BUP)                         
C                                                                               
C        PROGRAMA DE IMPRESSAO DOS DESLOCAMENTOS E ROTACOES                     
C                                                                               
C                                                                               
C        PARA PRECISAO SIMPLES ELIMINAR O COMANDO ABAIXO                        
C                                                                               
      IMPLICIT REAL *8 (A-H,O-Z)                                                
      COMMON /SOL/ NUMNP,NEQ                                                    
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                   
      REAL HED                                                                  
      DIMENSION ID(6,NUMNP),DISP(NEQ,NC),D(6,NEQ),EIGV(1),BLO(1),BUP(1)         
      LINHA = 1000                                                              
      DO 720 I=1,NROOT                                                          
      IF( LINHA .LE. LPPAG) GO TO 710                                           
      CALL KABECA                                                               
      WRITE(IOUT,5000)                                                          
      WRITE(IOUT,6070)                                                          
      LINHA=14                                                                  
 710  WRITE(IOUT,6080)I,EIGV(I),BLO(I),BUP(I)                                   
      LINHA=LINHA+2                                                             
 720  CONTINUE                                                                  
      IF( INTSAI(8) .NE. 1 ) RETURN                                             
      LINHA=1000                                                                
      DO 200 II = 1,NROOT                                                       
      DO 150 I = 1,6                                                            
      DO 130 J = 1,NUMNP                                                        
      L = ID(I,J)                                                               
      IF (L .LE. 0) GO TO 120                                                   
      D(I,J) = DISP(L,II)                                                       
      GO TO 130                                                                 
120   D(I,J) = 0.                                                               
130   CONTINUE                                                                  
150   CONTINUE                                                                  
C                                                                               
C        IMPRESSAO DO II-ESIMO PONTO NODAL                                      
      IF(INTSAI(8) .NE. 1)RETURN                                                
      I = 1                                                                     
      IF (LINHA .LT. LPPAG) GO TO 170                                           
160   CALL KABECA                                                               
      WRITE(IOUT,1000)                                                          
      LINHA = 13                                                                
170   K = I + 1                                                                 
      WRITE(IOUT,1010) II,I,(D(J,I),J=1,6)                                      
      LINHA = LINHA + 2                                                         
      IF (NUMNP .LT. K) GO TO 200                                               
      DO 180 I=K,NUMNP                                                          
      IF (LINHA .GT. LPPAG) GO TO 160                                           
      WRITE(IOUT,1020) I,(D(J,I),J=1,6)                                         
      LINHA = LINHA + 1                                                         
180   CONTINUE                                                                  
200   CONTINUE                                                                  
      RETURN                                                                    
1000  FORMAT(/47H ** A N A L I S E   D E   A U T O V A L O R E S ,////          
     *67H    D E S L O C A M E N T O S   E   R O T A C O E S   D O S   N        
     A O S   ///                                                                
     B 6X,58HNUMERO   NUMERO    TRANSLACAO    TRANSLACAO    TRANSLACAO ,        
     C 13H      ROTACAO,7X,7HROTACAO,7X,7HROTACAO, /                            
     D 5X,16HDO MODO    DO NO,5X,9HDIRECAO X,5X,9HDIRECAO Y,5X,9HDIRECAO        
     E Z,8X,6HEIXO X,8X,6HEIXO Y,8X,6HEIXO Z )                                  
1010  FORMAT(/I12,I9,6G14.5 )                                                   
1020  FORMAT(12X,I9,6G14.5 )                                                    
 5000 FORMAT(/47H ** A N A L I S E   D E   A U T O V A L O R E S )              
 6070 FORMAT(///53H    I M P R E S S A O   D A S   F R E Q U E N C I A S        
     A,//41X,10HFREQUENCIA,                                                     
     B /25X,6HNUMERO,12X,8HCIRCULAR,10X,10HFREQUENCIA,13X,7HPERIODO             
     C,/24X,7HDO MODO,11X,9H(RAD/SEG),8X,12H(CICLOS/SEG),15X,5H(SEG) )          
 6080 FORMAT(/21X,I10,3G20.5)                                                   
      END                                                                       
                                                                                
      SUBROUTINE STRESS(AA)                                                     
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . CHAMA SUBROTINA DO ELEMENTO PARA CALCULO                   .         
C .        DAS TENSOES                                                .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      INTEGER RIGROT,FORCAS,FNODEQ                                              
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT           
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION AA(1)                                                           
C                                                                               
C      LOOP SOBRE TODOS OS GRUPOS DE ELEMENTOS                                  
C                                                                               
      IF(KODT.NE.2) REWIND FNODEQ                                               
      REWIND IELMNT                                                             
      REWIND IMTV                                                               
      DO 100 N=1,NUMEG                                                          
      NG=N                                                                      
C                                                                               
      READ(IELMNT)NUMEST,NPAR,(AA(I),I=1,NUMEST)                                
      IF ( NPAR(10) .LT. 0 ) GO TO 100                                          
      CALL ELEMNT                                                               
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                                
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA DE DETERMINACAO DOS MENORES AUTO-VALORES E DOS       *        
C*       CORRESPONDENTES AUTO-VETORES,USANDO O METODO DE ITERACAO      *        
C*       DE SUB-ESPACOS .                                              *        
C*                                                                     *        
C***********************************************************************        
C                                                                      *        
C  ** VARIAVEIS DE ENTRADA **                                          *        
C                                                                      *        
C        A(NWK)     = MATRIZ DE RIGIDEZ NA FORMA COMPACTADA.           *        
C                     (ASSUMIDA POSITIVA DEFINIDA)                     *        
C        B(NWM)     = MATRIZ DE MASSA NA FORMA COMPACTADA              *        
C        MAXA(NNM)  = VETOR QUE CONTEM O ENDERECO DOS ELEMENTOS DA     *        
C                     DIAGONAL DA MATRIZ DE RIGIDEZ A .                *        
C        R(NN,NC)   = AUTO-VETORES                                     *        
C        EIGV(NC)   = AUTO-VALORES                                     *        
C        TT(NN)     = VETOR DE TRABALHO                                *        
C        W(NN)      = VETOR DE TRABALHO                                *        
C        AR(NNC)    = MATRIZ DE TRABALHO QUE ARMAZENA PROJECOES DE K.  *        
C        BR(NNC)    = MATRIZ DE TRABALHO QUE ARMAZENA PROJECOES DE M   *        
C        VEC(NC,NC) = MATRIZ DE TRABALHO                               *        
C        ID/D(NC)   = VETOR DE TRABALHO. GUARDA OS AUTO-VALORES        *        
C                     CALCULADOS NO PASSO ANTERIOR PARA O TESTE DE     *        
C                     CONVERGENCIA.                                    *        
C        RTOLV(NC)  = VETOR DE TRABALHO                                *        
C        IBUP/BUP(NC) VETOR DE TRABALHO                                *        
C        BLO(NC)    = VETOR DE TRABALHO                                *        
C        BUPC(NC)   = VETOR DE TRABALHO                                *        
C        NN         = ORDEM DAS MATRIZES DE MASSA E RIGIDEZ .          *        
C        NNM        = NN + 1                                           *        
C        NWK        = NUMERO DE ELEMENTOS DA MATRIZ DE RIGIDEZ QUE     *        
C                     ENTRAM NA FORMA COMPACTA.                        *        
C        NWM        = NUMERO DE ELEMENTOS DA MATRIZ  DE  MASSA  QUE    *        
C                     ENTRAM NA FORMA COMPACTA, I. E.,                 *        
C                     NWM = NWK PARA MATRIZ DE MASSA CONSISTENTE,      *        
C                     NWM = NN  PARA MATRIZ DE MASSA CONCENTRADA.      *        
C        NROOT      = NUMERO DE AUTO-VALORES E AUTO-VETORES SOLICI-    *        
C                     TADOS.                                           *        
C        RTOL       = TOLERANCIA DA CONVERGENCIA NOS AUTO-VALORES      *        
C                     SOLICITADOS (1. E-06 OU MENOS ) .                *        
C        NC         = NUMERO DE VETORES DE ITERACAO USADOS             *        
C                     (GERALMENTE IGUAL A MIN(2*NROOT,NROOT+8), MAS    *        
C                     NC NAO PODE SER MAIOR QUE O NUMERO DE GRAUS      *        
C                     DE LIBERDADE DE MASSA).                          *        
C        NNC        = NC * (NC+1) / 2, DIMENSIONAMENTO DOS VETORES     *        
C                     DE ARMAZENAMENTO: AR E BR.                       *        
C        NITEM      = NUMERO MAXIMO DE ITERACOES DE SUBESPACO  PER-    *        
C                     MITIDAS (GERALMENTE TOMADO COMO 16) .            *        
C                     SE NAO SE ATINGE A CONVERGENCIA, OS PARAMENTROS  *        
C                     NC E/OU NITEM DEVEM SER AUMENTADOS.              *        
C        IFSS       = INDICADOR DE TESTE DA SEQUENCIA DE STURM,        *        
C                     IFSS = 1 TESTA,                                  *        
C                     IFSS = 0 NAO TESTA.                              *        
C        INTSAI(5)  = INDICADOR DE IMPRESSAO DURANTE A ITERACAO,       *        
C                          = 1 IMPRIME,                                *        
C                          = 0 NAO IMPRIME.                            *        
C        NSTIF      = INDICADOR DO ARQUIVO DE TRABALHO PARA ARMAZENAR  *        
C                     A MATRIZ DE RIGIDEZ.                             *        
C        IOUT       = INDICADOR DO ARQUIVO DE SAIDA .                  *        
C                                                                      *        
C ** VARIAVEIS DE SAIDA **                                             *        
C                                                                      *        
C        EIGV(NROOT)= AUTO-VALORES                                     *        
C        R(NN,NROOT)= AUTO-VETORES                                     *        
C                                                                      *        
C***********************************************************************        
C                                                                               
         SUBROUTINE SSPACE (A,B,MAXA,R,EIGV,TT,W,AR,BR,VEC,D,ID,RTOLV,          
     1   BUP,IBUP,BLO,BUPC,NN,NNM,NROOT,RTOL,NC,NNC,NITEM,IFSS)                 
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
         REAL HED                                                               
         INTEGER MAXA(NNM)                                                      
C                                                                               
C***********************************************************************        
C*                                                                     *        
C*       ESTE PROGRAMA, PARA EQUIPAMENTO DA IBM OU UNIVAC, E  USADO    *        
C*       COM ARITMETICA DE DUPLA PRECISAO.  PARA  OUTRAS   MAQUINAS    *        
C*       COM ARITMETICA DE PRECISAO SIMPLES.                           *        
C*       OS CARTOES ACIMA DECIDEM QUE PRECISAO DEVE SER USADA.         *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                
         DIMENSION A(NWK),B(NWM),R(NN,NC),TT(NN),W(NN),EIGV(NC),D(NC),          
     1   ID(NC),VEC(NC,NC),AR(NNC),BR(NNC),RTOLV(NC),BUP(NC),IBUP(NC),          
     2   BLO(NC),BUPC(NC)                                                       
         ABS(X) = DABS(X)                                                       
         ATAN(X)= DATAN(X)                                                      
         SQRT(X)= DSQRT(X)   
		 FLOAT(I)=DFLOAT(I)
C                                                                               
C        ESTABELECER TOLERANCIA PARA ITERACAO DE JACOBI                         
C                                                                               
         TOLJ = 0.000000000001                                                  
C                                                                               
C        TESTE DOS PARAMETROS                                                   
C                                                                               
      IF(IDEPUR(4) .EQ. 0) GO TO 77                                             
         WRITE(IOUT,2000) NN,NNM,NWK,NWM,NROOT,RTOL,NC,NNC,NITEM,IFSS           
2000     FORMAT(50H1WRITE NN,NNM,NWK,NWM,NROOT,RTOL,NC,NNC,NITEM,IFSS,          
     1   5I5,G12.5,4I5)                                                         
         WRITE(IOUT,2001)                                                       
2001     FORMAT(//18H0MATRIZ DE RIGIDEZ )                                       
         WRITE(IOUT,2003) A                                                     
         WRITE(IOUT,2002)                                                       
2002     FORMAT(//16H0MATRIZ DE MASSA )                                         
         WRITE(IOUT,2003)B                                                      
2003     FORMAT(/10G12.5)                                                       
         WRITE(IOUT,2004)                                                       
2004     FORMAT(//11H VETOR MAXA  )                                             
         WRITE(IOUT,2005) MAXA                                                  
2005     FORMAT(/10I12)                                                         
C                                                                               
C        INICIALIZACAO                                                          
C                                     
   77    CONTINUE                                          
         ICONV = 0                                                              
         NSCH = 0                                                               
         NSMAX =12                                                              
         N1=NC+1                                                                
         NC1=NC-1                                                               
         REWIND NSTIF                                                           
         WRITE (NSTIF) A                                                        
         DO 60 I=1,NC                                                           
   60    D(I)=0                                                                 
C                                                                               
C        ESTABELECER OS VETORES DE ITERACAO INICIAIS                            
C                                                                               
         ND=NN/NC                                                               
         IF (NWM.GT.NN) GO TO 4                                                 
         J=0                                                                    
         DO 2 I=1,NN                                                            
         II=MAXA(I)                                                             
         R(I,1)=B(I)                                                            
         IF (B(I) .GT. 0) J = J + 1                                             
   2     W(I)=B(I)/A(II)                                                        
         IF (NC.LE.J) GO TO 16                                                  
            WRITE (IOUT,1007) J      
1007     FORMAT(//,'*** ERRO - DIMENSAO DOS SUBESPACOS MAIOR QUE O NUMER        
     1O DE GRAUS DE LIBERDADE DE MASSA. EXECUCAO INTERROMPIDA.'              
     2  /12X,20HGRAUS DE LIBERDADE =,I5 )                                      			
            CALL CLOSEFILES    
C			
   4     DO 10 I=1,NN                                                           
         II=MAXA(I)                                                             
         R(I,1)=B(II)                                                           
   10    W(I)=B(II)/A(II)                                                       
   16    DO 20 J=2,NC                                                           
         DO 20 I=1,NN                                                           
   20    R(I,J)=0.                                                              
C                                                                               
         L=NN-ND                                                                
         DO 30 J=2,NC                                                           
         RT = 0.                                                                
         DO 40 I=1,L                                                            
         IF (W(I).LT.RT) GO TO 40                                               
         RT=W(I)                                                                
         IJ = I                                                                 
   40    CONTINUE                                                               
         DO 50 I=L,NN                                                           
         IF (W(I).LE.RT) GO TO 50                                               
         RT=W(I)                                                                
         IJ=I                                                                   
   50    CONTINUE                                                               
         TT(J)=FLOAT(IJ)                                                        
         W(IJ) = 0.                                                             
         L=L-ND                                                                 
         IBUP(J) = IJ                                                           
   30    R(IJ,J)=1.                                                             
C                                                     
C  RECOLOCANDO TRECHO DO BATHE Q ADICIONA PERTURBAÇÃO AO ULTIMO VETOR---
C                             
         PI=3.141592654D0
		 XX=0.5D0
		 DO 66 K=1,NN
		    XX=(PI + XX)**5
			IX=INT(XX)
			XX=XX - FLOAT(IX)
			R(K,NC)=R(K,NC) + XX
   66    CONTINUE
C-----------------------------------------------------------------------   
C
         IF (INTSAI(5) .EQ. 0) GO TO 80                                         
            WRITE(IOUT,1008)                                                       
            WRITE(IOUT,1003) (IBUP(J),J = 2,NC)                                    
   80    IF (IDEPUR(4) .EQ. 0) GO TO 71                                         
            WRITE(IOUT,1009)                                                       
            DO 70 I=1,NN                                                           
            WRITE(IOUT,1002) (R(I,J),J=1,NC)                                       
   70       CONTINUE                                                               
   71    CONTINUE                                                                  
C                                                                               
C        FATORAR A MATRIZ A EM (L) * (D) * (L(T))                               
C                                                                               
         ISH=0                                                                  
         CALL DECOMP (A,MAXA,NN,ISH,IOUT)                                       
C                                                                               
C  ****  I N I C I O   D O   B L O C O   D E   I T E R A C A O                  
C                                                                               
         NITE=0                                                                 
  100    NITE=NITE+1                                                            
      IF(INTSAI(5) .EQ. 0) GO TO 90                                             
         CALL KABECA                                                            
         WRITE(IOUT,1000)                                                       
         WRITE (IOUT,1010) NITE   
 1010    FORMAT(///34H    ITERACAO (EM SUBESPACO) NUMERO , I3/)                 		 
   90 CONTINUE
C                                                                               
C        CALCULAR AS PROJECOES DE A E B                                         
C                                                                               
         IJ=0                                                                   
         DO 110 J=1,NC                                                          
         DO 120 K=1,NN                                                          
  120    TT(K)= R(K,J)                                                          
         CALL REDBAK (A,TT,MAXA,NN)                                             
         DO 130 I=J,NC                                                          
         ART=0.                                                                 
         DO 140 K=1,NN                                                          
  140    ART=ART + R(K,I)*TT(K)                                                 
         IJ=IJ + 1                                                              
  130    AR(IJ)=ART                                                             
         DO 150 K=1,NN                                                          
  150    R(K,J)=TT(K)                                                           
  110    CONTINUE                                                               
         IJ = 0                                                                 
         DO 160 J=1,NC                                                          
         CALL MULT (TT,B,R(1,J),MAXA,NN,NWM)                                    
         DO 180 I=J,NC                                                          
         BRT=0.                                                                 
         DO 190 K=1,NN                                                          
  190    BRT = BRT + R(K,I) * TT(K)                                             
         IJ=IJ+1                                                                
  180    BR(IJ)=BRT                                                             
         IF (ICONV.GT.0) GO TO 160                                              
         DO 200 K=1,NN                                                          
  200    R(K,J)=TT(K)                                                           
  160    CONTINUE                                                               
C                                                                               
C        RESOLVER OS AUTO-SISTEMAS DE OPERADORES DE SUB-ESPACO                  
C                                                                               
      IF(INTSAI(5) .EQ. 0) GO TO 320                                            
         IND=1                                                                  
  210    WRITE (IOUT,1020)                                                      
         II=1                                                                   
         DO 300 I=1,NC                                                          
         ITEMP=II+NC-I                                                          
         WRITE(IOUT,1005) (AR(J),J=II,ITEMP)                                    
  300    II=II+N1-I                                                             
         WRITE (IOUT,1030)                                                      
         II=1                                                                   
         DO 310 I=1,NC                                                          
         ITEMP=II+NC-I                                                          
         WRITE (IOUT,1005) (BR(J),J=II,ITEMP)                                   
  310    II=II + N1-I                                                           
         IF (IND .EQ. 2) GO TO 350                                              
         IF (INTSAI(5) .EQ. 1) WRITE(IOUT,1035)         
  320 CONTINUE		 
C                                                                               
         CALL JACOBI (AR,BR,VEC,EIGV,W,NC,NNC,TOLJ,NSMAX,   IOUT)             
C                                                                               
      IF(INTSAI(5) .EQ. 0) GO TO 350                                            
         WRITE (IOUT,1040)                                                      
         IND=2                                                                  
         GO TO 210     
  350 CONTINUE		 
C                                                                               
C        ORDENAR OS AUTO-VALORES EM ORDEM CRESCENTE                             
C                                                                               
         IS=0                                                                   
         II=1                                                                   
         DO 360 I=1,NC1                                                         
         ITEMP=II +N1-I                                                         
         IF (EIGV(I+1).GE.EIGV(I)) GO TO 360                                    
         IS=IS+1                                                                
         EIGVT=EIGV(I+1)                                                        
         EIGV(I+1) = EIGV(I)                                                    
         EIGV(I)= EIGVT                                                         
         BT = BR(ITEMP)                                                         
         BR(ITEMP)=BR(II)                                                       
         BR(II)=BT                                                              
         DO 370 K=1,NC                                                          
         RT=VEC(K,I+1)                                                          
         VEC(K,I+1)=VEC(K,I)                                                    
  370    VEC(K,I)=RT                                                            
  360    II=ITEMP                                                               
         IF (IS.GT.0) GO TO 350                                                 
C                                                                               
C        CALCULAR B VEZES OS AUTO-VETORES APROXIMADOS  (ICONV .EQ. 0)           
C        OU AS APROXIMACOES DO AUTO-VETOR FINAL        (ICONV .GT. 0)           
C                                                                               
  375    DO 420 I=1,NN                                                          
         DO 422 J=1,NC                                                          
  422    TT(J)=R(I,J)                                                           
         DO 424 K=1,NC                                                          
         RT=0.                                                                  
         DO 430 L=1,NC                                                          
  430    RT=RT+TT(L)*VEC(L,K)                                                   
  424    R(I,K)=RT                                                              
  420    CONTINUE                                                               
         IF (ICONV.EQ.0) GO TO 379                                              
            TOLER = RTOLV(1)                                                          
            DO 490 J = 1,NROOT                                                     
            IF (RTOLV(J) .LE. TOLER) GO TO 490                                      
               TOLER = RTOLV(J)                                                          
  490       CONTINUE                                                               
         GO TO 500                                                              
C                                                                               
C        VERIFICAR A CONVERGENCIA DOS AUTO-VALORES                              
C                                                                               
  379    DO 380 I=1,NC                                                          
         DIF= ABS(EIGV(I)-D(I))                                                 
  380    RTOLV(I)=DIF/EIGV(I)                                                   
         IF(INTSAI(5) .EQ. 0) GO TO 385                                            
            WRITE (IOUT,1050) 
 1050    FORMAT(//50H    TOLERANCIA RELATIVA ATINGIDA NOS AUTOVALORES  )        			
            WRITE (IOUT,1005) (RTOLV(I),I=1,NC)                                    
C                                                                               
  385    DO 390 I=1,NROOT                                                       
         IF (RTOLV(I).GT.RTOL) GO TO 400                                        
  390    CONTINUE                                                               
         IF (INTSAI(5) .EQ. 1) WRITE(IOUT,1060)  
 1060    FORMAT(//'   ATINGIDA A CONVERGENCIA DESEJADA NOS AUTOVALORES') 		 
         ICONV=1                                                                
         GO TO 100   
C		 
  400    IF (NITE .LT. NITEM) GO TO 410                                         
            WRITE (IOUT,1070)         
1070     FORMAT(//,'*** ERRO - NAO HOUVE CONVERGENCIA NO NUMERO MAXIMO        
     1 DE ITERACOES. SERAO ACEITOS OS VALORES CORRENTES.',                      
     2 /12X,50HE O TESTE DE SEQUENCIA DE STURM NAO SERA EFETUADO. )           			
            ICONV=2                                                                
            IFSS=0                                                                 
            GO TO 100                                                              
C                                                                               
  410    DO 440 I=1,NC                                                          
  440    D(I) = EIGV(I)
         GO TO 100                                                              
C                                                                               
C  ****  F I N A L   D O   B L O C O   D E   I T E R A C A O                    
C                                                                               
500   CALL KABECA                                                            
      NITE = NITE - 1                                                        
      WRITE(IOUT,1000)
 1000    FORMAT(/47H ** A N A L I S E   D E   A U T O V A L O R E S )           				  
      WRITE(IOUT,2020) NITE,TOLER       
 2020    FORMAT(/// 49H    I N F O R M A C O E S   D E   E X E C U C A O        
     *  ///                                                                     
     A    8X,49HNUMERO DE ITERACOES EFETUADAS . . . . . . . . . =,I3 //         
     B   8X,49HTOLERANCIA RELATIVA ATINGIDA. . . . . . . . . . =,E15.7 )        	  
C                                                              
C        DETERMINACAO DO DESLOCAMENTO PARA O TESTE DE SEQUENCIA DE STURM        
C                                                                               
        IF (IFSS.EQ.1)                                                         
C    COLOCAR UM '*' NA COLUNA 6 DO PROXIMO COMANDO, E ALTERAR A                 
C        IMPRESSAO DA TABELA DE AUTOVALORES                                     
     1   CALL SCHECK (EIGV,RTOLV,BUP,IBUP,BLO,BUPC,ID,NC,NEI,RTOL,SHIFT,        
     2   IOUT )                                                                 
C                                                                               
C        CALCULAR E IMPRIMIR AS NORMAS DOS ERROS                                
C                                                                               
      REWIND NSTIF                                                           
      READ(NSTIF) A                                                          
C                                                                               
      DO 580 L=1,NROOT                                                       
         RT=EIGV(L)                                                             
         CALL MULT(TT,A,R(1,L),MAXA,NN,NWK)                                     
         VNORM = 0.                                                             
         DO 590 I=1,NN                                                          
  590    VNORM=VNORM+TT(I)*TT(I)                                                
         CALL MULT(W,B,R(1,L),MAXA,NN,NWM)                                      
         WNORM = 0.                                                             
         DO 600 I=1,NN                                                          
         TT(I)=TT(I)-RT*W(I)                                                    
  600    WNORM=WNORM +TT(I)*TT(I)                                               
         VNORM=SQRT(VNORM)                                                      
         WNORM=SQRT(WNORM)                                                      
         BLO(L)=WNORM/VNORM                                                     
  580 CONTINUE                                                               
         WRITE(IOUT,2030)                                                       
         J = 1                                                                  
         LINHA = 19                                                             
         WRITE(IOUT,2040) J,EIGV(1),BLO(1),BUPC(1),ID(1),IBUP(1)                
         IF (NROOT .EQ. 1) GO TO 630                                            
         DO 610 I = 2,NROOT                                                     
         IF (EIGV(I) .EQ. EIGV(I-1)) GO TO 610                                  
         J = J + 1                                                              
         LINHA = LINHA + 2                                                      
         IF (LINHA .LE. LPPAG) GO TO 605                                        
            CALL KABECA                                                            
            LINHA = 12                                                             
            WRITE(IOUT,1000)  
            WRITE(IOUT,2030)         
 2030    FORMAT(///53H    I M P R E S S A O   D O S   A U T O V A L O R         
     1E S  //                                                                   
     2   56X,29HLIMITE SUPERIOR   AUTOVALORES /                                 
     3 6X,5HORDEM,11X,9HAUTOVALOR,7X,13HNORMA DO ERRO,8X,                       
     4   40HDO INTERVALO  NO INTERVALO      SUBTOTAL )  			
  605    WRITE(IOUT,2040) J,EIGV(I),BLO(I),BUPC(I),ID(I),IBUP(I)                
  610    CONTINUE                                                               
C                                                                               
C        APLICAR TESTE DE SEQUENCIA DE STURM                                    
C                                                                               
  630    IF (IFSS .EQ. 0) GO TO 700                                             
C                                                                               
C        DESLOCAR A MATRIZ A                                                    
C                                                                               
         REWIND NSTIF                                                           
         READ (NSTIF) A                                                         
         IF (NWM.GT.NN) GO TO 645                                               
            DO 640 I=1,NN                                                          
            II = MAXA(I)                                                           
  640       A(II)=A(II)-B(I)*SHIFT                                                 
         GO TO 660                                                              
  645    DO 650 I=1,NWK                                                         
  650    A(I)=A(I)-B(I)*SHIFT                                                   
C                                                                               
C        FATORAR A MATRIZ DESLOCADA                                             
C                                                                               
  660    ISH=1                                                                  
         CALL DECOMP (A,MAXA,NN,ISH,IOUT)                                       
C                                                                               
C        CALCULAR O NUMERO DE ELEMENTOS NEGATIVOS NA DIAGONAL                   
C                                                                               
         NSCH=0                                                                 
         DO 664 I=1,NN                                                          
         II= MAXA(I)                                                            
         IF (A(II).LT.0.) NSCH= NSCH +1                                         
  664    CONTINUE                                                               
         WRITE(IOUT,2050) SHIFT,NSCH  
2050     FORMAT(///57H    T E S T E   D E   S E Q U E N C I A   D E   S         
     1T U R M  ///                                                              
     2    8X,49HDESLOCAMENTO UTILIZADO. . . . . . . . . . . . . =,G12.5         
     3//, 8X,49HNUMERO DE AUTOVALORES EXISTENTES. . . . . . . . =,I4 )          		 
         IF (NSCH .EQ. NEI) GO TO 700                                           
            NMIS=NSCH-NEI                                                          
            WRITE(IOUT,2060) NMIS
2060     FORMAT(/,'NUMERO DE AUTOVALORES NAO ENCONTRADOS . . . . =',I4)			
C                                                                               
C        IMPRESSAO DAS FREQUENCIAS                                              
C                                                                               
700      DOISPI = 6.28318530717958647                                           
CWROTE   LINHA = 1000                                                           
         DO 720 I = 1,NROOT                                                     
         EIGV(I) = SQRT(EIGV(I))                                                
         BLO(I) = EIGV(I) / DOISPI                                              
         BUP(I) = DOISPI / EIGV(I)                                              
CWROTE   IF (LINHA .LE. LPPAG) GO TO 710                                        
CWROTE   CALL KABECA                                                            
CWROTE   WRITE(IOUT,1000)                                                       
CWROTE   WRITE(IOUT,2070)                                                       
CWROTE   LINHA = 11                                                             
C 710    WRITE(IOUT,2080) I,EIGV(I),BLO(I),BUP(I)                               
CWROTE   LINHA = LINHA + 2                                                      
720      CONTINUE                                                               
         IF (IDEPUR(4) .EQ. 1)                                                  
     *   WRITE(IOUT,2090) ((R(I,J),I=1,NN),J=1,NROOT)                           
         RETURN                                                                 
C                                                                               
1002     FORMAT (1H0,10F10.0)                                                   
1003     FORMAT(/3X,20I6)                                                       
1005     FORMAT(/ 6X,10G12.4 )                                                  
1006     FORMAT (1H0,6G22.14)                                                   
1008     FORMAT(///53H    R E S U L T A D O S   I N T E R M E D I A R I         
     AO S ///48H    BASE INICIAL PARA AS ITERACOES EM SUBESPACO )               
1009     FORMAT (///,39H0VETORES UNITARIOS DA ITERACAO INICIAL:)                
1020     FORMAT(/ 33H    PROJECAO DA MATRIZ DE RIGIDEZ )                        
1030     FORMAT(/ 31H    PROJECAO DA MATRIZ DE MASSA )                          
1035     FORMAT(//33H    APLICACAO DO METODO DE JACOBI )                        
1040     FORMAT(//44H    MATRIZES APOS A DIAGONALIZACAO DE JACOBI )                                                                                                         
2040     FORMAT(/,I11,3G20.5,2I14 )                                                                                                             
2070  FORMAT(///53H    I M P R E S S A O   D A S   F R E Q U E N C I A S        
     A,//41X,10HFREQUENCIA,                                                     
     B /25X,6HNUMERO,12X,8HCIRCULAR,10X,10HFREQUENCIA,13X,7HPERIODO             
     C,/24X,7HDO MODO,11X,9H(RAD/SEG),8X,12H(CICLOS/SEG),15X,5H(SEG) )          
2080     FORMAT(/21X,I10,3G20.5)                                                
2090     FORMAT(12H1AUTOVALORES // (10G12.4) )                                  
C                                                                               
         END                                                                    
C                                                                                                                                         
         SUBROUTINE SSPACB (A,B,MAXA,R,EIGV,TT,W,AR,BR,VEC,D,ID,RTOLV,          
     1   BUP,IBUP,BLO,BUPC,NN,NNM,NROOT,RTOL,NC,NNC,NITEM,IFSS)                 
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
         REAL HED                                                               
         INTEGER MAXA(NNM)                                                      
C                                                                               
C***********************************************************************        
C*                                                                     *        
C*       ESTE PROGRAMA, PARA EQUIPAMENTO DA IBM OU UNIVAC, E  USADO    *        
C*       COM ARITMETICA DE DUPLA PRECISAO.  PARA  OUTRAS   MAQUINAS    *        
C*       COM ARITMETICA DE PRECISAO SIMPLES.                           *        
C*       OS CARTOES ACIMA DECIDEM QUE PRECISAO DEVE SER USADA.         *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         COMMON /KABEC/ HED(20),NPAG,LINHA,LPPAG                                
         DIMENSION A(NWK),B(NWM),R(NN,NC),TT(NN),W(NN),EIGV(NC),D(NC),          
     1   ID(NC),VEC(NC,NC),AR(NNC),BR(NNC),RTOLV(NC),BUP(NC),IBUP(NC),          
     2   BLO(NC),BUPC(NC)                                                       
         ABS(X) = DABS(X)                                                       
         ATAN(X)= DATAN(X)                                                      
         SQRT(X)= DSQRT(X)   
		 FLOAT(I)=DFLOAT(I)
C                                                                               
C        ESTABELECER TOLERANCIA PARA ITERACAO DE JACOBI                         
C                                                                               
         TOLJ = 1.0D-12                                                  
C                                                                               
C        TESTE DOS PARAMETROS                                                   
C                                                                               
      IF(IDEPUR(4) .EQ. 0) GO TO 77                                             
         WRITE(IOUT,2000) NN,NNM,NWK,NWM,NROOT,RTOL,NC,NNC,NITEM,IFSS           
2000     FORMAT(50H1WRITE NN,NNM,NWK,NWM,NROOT,RTOL,NC,NNC,NITEM,IFSS,          
     1   5I5,G12.5,4I5)                                                         
         WRITE(IOUT,2001)                                                       
2001     FORMAT(//18H0MATRIZ DE RIGIDEZ )                                       
         WRITE(IOUT,2003) A                                                     
         WRITE(IOUT,2002)                                                       
2002     FORMAT(//16H0MATRIZ DE MASSA )                                         
         WRITE(IOUT,2003)B                                                      
2003     FORMAT(/10G12.5)                                                       
         WRITE(IOUT,2004)                                                       
2004     FORMAT(//11H VETOR MAXA  )                                             
         WRITE(IOUT,2005) MAXA                                                  
2005     FORMAT(/10I12)                                                         
C                                                                               
C        INICIALIZACAO                                                          
C                                     
   77    CONTINUE                                          
         ICONV = 0                                                              
         NSCH = 0                                                               
         NSMAX =12                                                              
         N1=NC+1                                                                
         NC1=NC-1                                                               
         REWIND NSTIF                                                           
         WRITE (NSTIF) A                                                        
         DO 60 I=1,NC                                                           
   60    D(I)=0                                                                 
C                                                                               
C        ESTABELECER OS VETORES DE ITERACAO INICIAIS                            
C                                                                               
         ND=NN/NC                                                               
         IF (NWM.GT.NN) GO TO 4                                                 
         J=0                                                                    
         DO 2 I=1,NN                                                            
         II=MAXA(I)                                                             
         R(I,1)=B(I)                                                            
         IF (B(I) .GT. 0) J = J + 1                                             
   2     W(I)=B(I)/A(II)                                                        
         IF (NC.LE.J) GO TO 16                                                  
            WRITE (IOUT,1007) J      
1007     FORMAT(//,'*** ERRO - DIMENSAO DOS SUBESPACOS MAIOR QUE O NUMER        
     1O DE GRAUS DE LIBERDADE DE MASSA. EXECUCAO INTERROMPIDA.'              
     2  /12X,20HGRAUS DE LIBERDADE =,I5 )                                      			
            CALL CLOSEFILES    
C			
   4     DO 10 I=1,NN                                                           
         II=MAXA(I)                                                             
         R(I,1)=B(II)                                                           
   10    W(I)=B(II)/A(II)                                                       
   16    DO 20 J=2,NC                                                           
         DO 20 I=1,NN                                                           
   20    R(I,J)=0.                                                              
C                                                                               
         L=NN-ND                                                                
         DO 30 J=2,NC                                                           
         RT = 0.                                                                
         DO 40 I=1,L                                                            
         IF (W(I).LT.RT) GO TO 40                                               
         RT=W(I)                                                                
         IJ = I                                                                 
   40    CONTINUE                                                               
         DO 50 I=L,NN                                                           
         IF (W(I).LE.RT) GO TO 50                                               
         RT=W(I)                                                                
         IJ=I                                                                   
   50    CONTINUE                                                               
         TT(J)=FLOAT(IJ)                                                        
         W(IJ) = 0.                                                             
         L=L-ND                                                                 
C         IBUP(J) = IJ                                                           
   30    R(IJ,J)=1.                                                             
C                                                     
C  RECOLOCANDO TRECHO DO BATHE Q ADICIONA PERTURBAÇÃO AO ULTIMO VETOR---
C                             
         PI=3.141592654D0
		 XX=0.5D0
		 DO 66 K=1,NN
		    XX=(PI + XX)**5
			IX=INT(XX)
			XX=XX - FLOAT(IX)
			R(K,NC)=R(K,NC) + XX
   66    CONTINUE
C-----------------------------------------------------------------------   
         WRITE(IOUT,1071)
 1071    FORMAT(//,' VERSÃO BASEADA NO LIVRO DO BATHE.',//)
C
         IF (INTSAI(5) .EQ. 0) GO TO 80                                         
            WRITE(IOUT,1008) 
1008     FORMAT(///53H    R E S U L T A D O S   I N T E R M E D I A R I         
     AO S ///48H    BASE INICIAL PARA AS ITERACOES EM SUBESPACO )               
C            WRITE(IOUT,1003) (IBUP(J),J = 2,NC)                                    
   80    IF (IDEPUR(4) .EQ. 0) GO TO 71                                         
            WRITE(IOUT,1009)  
1009     FORMAT (///,39H0VETORES UNITARIOS DA ITERACAO INICIAL:)                			
            DO 70 I=1,NN                                                           
            WRITE(IOUT,1002) (R(I,J),J=1,NC)                                       
   70       CONTINUE                                                               
   71    CONTINUE                                                                  
C                                                                               
C        FATORAR A MATRIZ A EM (L) * (D) * (L(T))                               
C                                                                               
         ISH=0                                                                  
         CALL DECOMP (A,MAXA,NN,ISH,IOUT)                                       
C                                                                               
C  ****  I N I C I O   D O   B L O C O   D E   I T E R A C A O                  
C                                                                               
         NITE=0  
         TOLJ2 = 1.0D-24		 
  100    NITE=NITE+1                                                            
      IF(INTSAI(5) .EQ. 0) GO TO 90                                             
         CALL KABECA                                                            
         WRITE(IOUT,1000) 
 1000    FORMAT(/47H ** A N A L I S E   D E   A U T O V A L O R E S )            
         WRITE (IOUT,1010) NITE 
 1010    FORMAT(///34H    ITERACAO (EM SUBESPACO) NUMERO , I3/)                 
   90 CONTINUE
C                                                                               
C        CALCULAR AS PROJECOES DE A E B                                         
C                                                                               
         IJ=0                                                                   
         DO 110 J=1,NC                                                          
         DO 120 K=1,NN                                                          
  120    TT(K)= R(K,J)                                                          
         CALL REDBAK (A,TT,MAXA,NN)                                             
         DO 130 I=J,NC                                                          
         ART=0.                                                                 
         DO 140 K=1,NN                                                          
  140    ART=ART + R(K,I)*TT(K)                                                 
         IJ=IJ + 1                                                              
  130    AR(IJ)=ART                                                             
         DO 150 K=1,NN                                                          
  150    R(K,J)=TT(K)                                                           
  110    CONTINUE                                                               
         IJ = 0                                                                 
         DO 160 J=1,NC                                                          
         CALL MULT (TT,B,R(1,J),MAXA,NN,NWM)                                    
         DO 180 I=J,NC                                                          
         BRT=0.                                                                 
         DO 190 K=1,NN                                                          
  190    BRT = BRT + R(K,I) * TT(K)                                             
         IJ=IJ+1                                                                
  180    BR(IJ)=BRT                                                             
         IF (ICONV.GT.0) GO TO 160                                              
         DO 200 K=1,NN                                                          
  200    R(K,J)=TT(K)                                                           
  160    CONTINUE                                                               
C                                                                               
C        RESOLVER OS AUTO-SISTEMAS DE OPERADORES DE SUB-ESPACO                  
C                                                                               
      IF(INTSAI(5) .EQ. 0) GO TO 320                                            
         IND=1                                                                  
  210    WRITE (IOUT,1020)
 1020    FORMAT(/ 33H    PROJECAO DA MATRIZ DE RIGIDEZ )                        
         II=1                                                                   
         DO 300 I=1,NC                                                          
         ITEMP=II+NC-I                                                          
         WRITE(IOUT,1005) (AR(J),J=II,ITEMP)                                    
  300    II=II+N1-I                                                             
         WRITE (IOUT,1030)        
 1030    FORMAT(/ 31H    PROJECAO DA MATRIZ DE MASSA )                          		 
         II=1                                                                   
         DO 310 I=1,NC                                                          
         ITEMP=II+NC-I                                                          
         WRITE (IOUT,1005) (BR(J),J=II,ITEMP)                                   
  310    II=II + N1-I                                                           
         IF (IND .EQ. 2) GO TO 350                                              
         IF (INTSAI(5) .EQ. 1) WRITE(IOUT,1035)         
  320 CONTINUE		 
C                                                                               
         CALL JACOBI (AR,BR,VEC,EIGV,W,NC,NNC,TOLJ,NSMAX,   IOUT)             
C                                                                               
      IF(INTSAI(5) .EQ. 0) GO TO 350                                            
         WRITE (IOUT,1040)   
 1040    FORMAT(//44H    MATRIZES APOS A DIAGONALIZACAO DE JACOBI )             
         IND=2                                                                  
         GO TO 210     
C                                                                               
C        ORDENAR OS AUTO-VALORES EM ORDEM CRESCENTE                             
C                                                                               
  350    IS=0                                                                   
         II=1                                                                   
         DO 360 I=1,NC1                                                         
         ITEMP=II + N1-I                                                         
         IF (EIGV(I+1).GE.EIGV(I)) GO TO 360                                    
         IS=IS+1                                                                
         EIGVT=EIGV(I+1)                                                        
         EIGV(I+1) = EIGV(I)                                                    
         EIGV(I)= EIGVT                                                         
         BT = BR(ITEMP)                                                         
         BR(ITEMP)=BR(II)                                                       
         BR(II)=BT                                                              
         DO 370 K=1,NC                                                          
         RT=VEC(K,I+1)                                                          
         VEC(K,I+1)=VEC(K,I)                                                    
  370    VEC(K,I)=RT                                                            
  360    II=ITEMP                                                               
         IF (IS.GT.0) GO TO 350                                                 
C                                                                               
C        CALCULAR B VEZES OS AUTO-VETORES APROXIMADOS  (ICONV .EQ. 0)           
C        OU AS APROXIMACOES DO AUTO-VETOR FINAL        (ICONV .GT. 0)           
C                                                                               
  375    DO 420 I=1,NN                                                          
         DO 422 J=1,NC                                                          
  422    TT(J)=R(I,J)                                                           
         DO 424 K=1,NC                                                          
         RT=0.                                                                  
         DO 430 L=1,NC                                                          
  430    RT=RT + TT(L)*VEC(L,K)                                                 
  424    R(I,K)=RT                                                              
  420    CONTINUE     
C
C        CALCULATE ERROR BOUNDS AND CHECK FOR CONVERGENCE OF EIGENVALUES
C  
         DO 380 I=1,NC
		   VDOT = 0.
		   DO 382 J=1,NC
  382      VDOT = VDOT + VEC(I,J)*VEC(I,J)	
           EIGV2 = EIGV(I)*EIGV(I)
           DIF = VDOT-EIGV2
		   RDIF = MAX(DIF,TOLJ2*EIGV2)/EIGV2
		   RDIF = SQRT(RDIF)
		   RTOLV(I) = RDIF
  380    CONTINUE
         IF(INTSAI(5).EQ.0 .AND. ICONV.EQ.0) GO TO 385
		   WRITE(IOUT,1050)
 1050      FORMAT(//'   TOLERANCIA RELATIVA ATINGIDA NOS AUTOVALORES  ')
           WRITE(IOUT,1005)(RTOLV(I),I=1,NC)
  385    IF(ICONV .GT. 0) GO TO 500
C
         DO 390 I=1,NROOT                                                       
           IF (RTOLV(I).GT.RTOL) GO TO 400                                        
  390    CONTINUE
         IF (INTSAI(5) .EQ. 1) WRITE(IOUT,1060) RTOL
 1060    FORMAT(//' ATINGIDA A CONVERGENCIA DESEJADA NOS AUTOVALORES  ',
     *           E10.4)        
         ICONV=1                                                             
         GO TO 100
  400    IF (NITE .LT. NITEM) GO TO 100                                         
         WRITE (IOUT,1070)         
1070     FORMAT(//,'*** ERRO - NAO HOUVE CONVERGENCIA NO NUMERO MAXIMO        
     1 DE ITERACOES. SERAO ACEITOS OS VALORES CORRENTES.',                      
     2 /12X,50HE O TESTE DE SEQUENCIA DE STURM NAO SERA EFETUADO. )           			
         ICONV=2                                                                
         IFSS=0                                                                 
         GO TO 100                                                              
C                                                                               
C  ****  F I N A L   D O   B L O C O   D E   I T E R A C A O                    
C                                                                               
500   CALL KABECA                                                            
      NITE = NITE - 1                                                        
      WRITE(IOUT,1000)                                                       
      WRITE(IOUT,2020) NITE   
2020     FORMAT(///,' I N F O R M A C O E S   D E   E X E C U C A O',        
     *         //,' NUMERO DE ITERACOES EFETUADAS . . . . . . . . =',I4)         
C                                                                               
C        CALCULAR E IMPRIMIR AS NORMAS DOS ERROS                                
C                                                                               
      REWIND NSTIF                                                           
      READ(NSTIF) A                                                          
C                                                                               
      DO 580 L=1,NROOT                                                       
         RT=EIGV(L)                                                             
         CALL MULT(TT,A,R(1,L),MAXA,NN,NWK)                                     
         VNORM = 0.                                                             
         DO 590 I=1,NN                                                          
  590    VNORM=VNORM+TT(I)*TT(I)                                                
         CALL MULT(W,B,R(1,L),MAXA,NN,NWM)                                      
         WNORM = 0.                                                             
         DO 600 I=1,NN                                                          
         TT(I)=TT(I)-RT*W(I)                                                    
  600    WNORM=WNORM +TT(I)*TT(I)                                               
         VNORM=SQRT(VNORM)                                                      
         WNORM=SQRT(WNORM)                                                      
         D(L)=WNORM/VNORM                                                     
  580 CONTINUE                                                               
         WRITE(IOUT,1115) 
 1115    FORMAT(//,' ERROR MEASURES ON THE EIGENVALUES')		 
         WRITE(IOUT,1005)(D(I),I=1,NROOT)                
C                                                                               
C        APLICAR TESTE DE SEQUENCIA DE STURM                                    
C                                                                               
  630    IF (IFSS .EQ. 0) GO TO 671  
         CALL SCHECB(EIGV,RTOLV,BUP,BLO,BUPC,D,NC,NEI,RTOL,SHIFT,IOUT)
C
         WRITE(IOUT,1120) SHIFT	
 1120    FORMAT(///,' CHECK APPLIED AT SHIFT ', E22.14)		 
C                                                                               
C        DESLOCAR A MATRIZ A                                                    
C                                                                               
         REWIND NSTIF                                                           
         READ (NSTIF) A                                                         
         IF (NWM.GT.NN) GO TO 645                                               
            DO 640 I=1,NN                                                          
            II = MAXA(I)                                                           
  640       A(II)=A(II)-B(I)*SHIFT                                                 
         GO TO 660                                                              
  645    DO 650 I=1,NWK                                                         
  650    A(I)=A(I)-B(I)*SHIFT                                                   
C                                                                               
C        FATORAR A MATRIZ DESLOCADA                                             
C                                                                               
  660    ISH=1                                                                  
         CALL DECOMP (A,MAXA,NN,ISH,IOUT)                                       
C                                                                               
C        CALCULAR O NUMERO DE ELEMENTOS NEGATIVOS NA DIAGONAL                   
C                                                                               
         NSCH=0                                                                 
         DO 664 I=1,NN                                                          
           II= MAXA(I)                                                            
           IF (A(II).LT.0.) NSCH= NSCH +1                                         
  664    CONTINUE                                                               
         WRITE(IOUT,2050) SHIFT,NSCH 
 2050    FORMAT(///,' T E S T E   D E   S E Q.   D E   S T U R M',///                                                              
     1   ' DESLOCAMENTO UTILIZADO. . . . . . . . . . . . . =',G12.5,//         
     2   ' NUMERO DE AUTOVALORES EXISTENTES. . . . . . . . =',I4)                                                                           
         IF(NSCH .EQ. NEI) GO TO 670                                           
            NMIS=NSCH-NEI                                                          
            WRITE(IOUT,2060) NMIS 
 2060       FORMAT(/,' NUMERO DE AUTOVALORES NAO ENCONTRADOS =',I4) 
            GO TO 671			
C                                                                               
C        IMPRESSAO DAS FREQUENCIAS                                              
C                                                                               
  670    WRITE(IOUT,1140) NSCH
 1140    FORMAT(//,' WE FOUND THE LOWEST ',I8,' EIGENVALUES.')
  671    DOISPI = 6.28318530717958647                                           
CWROTE   LINHA = 1000                                                           
         DO 720 I = 1,NROOT                                                     
         EIGV(I) = SQRT(EIGV(I))                                                
         BLO(I) = EIGV(I) / DOISPI                                              
         BUP(I) = DOISPI / EIGV(I)                                              
CWROTE   IF (LINHA .LE. LPPAG) GO TO 710                                        
CWROTE   CALL KABECA                                                            
CWROTE   WRITE(IOUT,1000)                                                       
CWROTE   WRITE(IOUT,2070)                                                       
CWROTE   LINHA = 11                                                             
C 710    WRITE(IOUT,2080) I,EIGV(I),BLO(I),BUP(I)                               
CWROTE   LINHA = LINHA + 2                                                      
720      CONTINUE                                                               
         IF (IDEPUR(4) .EQ. 1)                                                  
     *   WRITE(IOUT,2090) ((R(I,J),I=1,NN),J=1,NROOT)                           
         RETURN                                                                 
C                                                                               
1002     FORMAT (1H0,10F10.0)                                                   
1003     FORMAT(/3X,20I6)                                                       
1005     FORMAT(/ 6X,10G12.4 )                                                  
1006     FORMAT (1H0,6G22.14)                                                   
1035     FORMAT(//33H    APLICACAO DO METODO DE JACOBI )                        
2030     FORMAT(///53H    I M P R E S S A O   D O S   A U T O V A L O R         
     AE S  //                                                                   
     B   56X,29HLIMITE SUPERIOR   AUTOVALORES /                                 
     C 6X,5HORDEM,11X,9HAUTOVALOR,7X,13HNORMA DO ERRO,8X,                       
     D   40HDO INTERVALO  NO INTERVALO      SUBTOTAL )                          
2040     FORMAT(/,I11,3G20.5,2I14 )                                             
2070  FORMAT(///53H    I M P R E S S A O   D A S   F R E Q U E N C I A S        
     A,//41X,10HFREQUENCIA,                                                     
     B /25X,6HNUMERO,12X,8HCIRCULAR,10X,10HFREQUENCIA,13X,7HPERIODO             
     C,/24X,7HDO MODO,11X,9H(RAD/SEG),8X,12H(CICLOS/SEG),15X,5H(SEG) )          
2080     FORMAT(/21X,I10,3G20.5)                                                
2090     FORMAT(12H1AUTOVALORES // (10G12.4) )                                  
C                                                                               
         END                                                                    
         SUBROUTINE REDBAK (A,V,MAXA,NN)                                        
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA PARA REDUZIR E FAZER A RETRO-SUBSTITUICAO DOS        *        
C*       VETORES DE ITERACAO.                                          *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         DIMENSION A(1),V(1),MAXA(1)                                            
C                                                                               
         DO 400 N=1,NN                                                          
         KL=MAXA(N)+1                                                           
         KU=MAXA(N+1)-1                                                         
         IF (KU-KL) 400,410,410                                                 
  410    K=N                                                                    
         C=0.                                                                   
         DO 420 KK=KL,KU                                                        
         K=K-1                                                                  
  420    C=C+A(KK)*V(K)                                                         
         V(N)=V(N)-C                                                            
  400    CONTINUE                                                               
C                                                                               
         DO 480 N=1,NN                                                          
         K=MAXA(N)                                                              
  480    V(N)=V(N)/A(K)                                                         
         IF (NN.EQ.1)  RETURN                                                   
         N=NN                                                                   
         DO 500 L=2,NN                                                          
         KL=MAXA(N) + 1                                                         
         KU=MAXA(N+1)-1                                                         
         IF (KU-KL) 500,510,510                                                 
  510    K=N                                                                    
         DO 520 KK=KL,KU                                                        
         K = K - 1                                                              
  520    V(K)=V(K)-A(KK)*V(N)                                                   
  500    N=N-1                                                                  
         RETURN                                                                 
C                                                                               
         END                                                                    
C                                                                                
         SUBROUTINE JACOBI (A,B,X,EIGV,D,N,NWA,RTOL,NSMAX,IOUT)            
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA PARA SOLUCAO DE AUTO-PROBLEMAS GENERALIZADOS USANDO  *        
C*       O METODO DE ITERACAO GENERALIZADO DE JACOBI.                  *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         DIMENSION A(NWA),B(NWA),X(N,N),EIGV(N),D(N)                            
         ABS(XX)=DABS(XX)                                                         
         SQRT(XX)=DSQRT(XX)                                                       
C                                                                               
C        INICIALIZAR AS MATRIZES DE AUTO-VALORES E AUTO-VETORES                 
C                                                                               
         N1=N+1                                                                 
         II=1                                                                   
         DO 10 I=1,N                                                            
         IF(A(II).GT.0. .AND. B(II).GT.0.) GO TO 4                              
            WRITE(IOUT,2020)                                                       
            WRITE(IOUT,2025) II,A(II),B(II)                                        
            CALL CLOSEFILES                                                                   
    4    D(I)=A(II)/B(II)                                                       
         EIGV(I)=D(I)                                                           
   10    II=II+N1-I                                                             
         DO 30 I=1,N                                                            
         DO 20 J=1,N                                                            
   20    X(J,I)=0.                                                              
   30    X(I,I)=1.                                                              
         IF(N.EQ.1) RETURN                                                      
C                                                                               
C        INICIALIZAR O CONTADOR DE VARREDURA E INICIO DE ITERACAO               
C                                                                               
         NSWEEP = 0                                                             
         NR=N-1                                                                 
   40    NSWEEP = NSWEEP + 1                                                    
         CHECK = -1                                                             
C                                                                               
C        TESTAR SE O ELEMENTO FORA DA DIAGONAL ATUAL E                          
C        SUFICIENTEMENTE GRANDE PARA EXIGIR ZERACAO                             
C                                                                               
         EPS = (.01 ** NSWEEP) ** 2                                             
         DO 210 J=1,NR                                                          
         JP1=J + 1                                                            
         JM1=J - 1                                                                
         LJK=JM1*N-JM1*J/2                                                      
         JJ=LJK + J                                                             
         DO 210 K=JP1,N                                                         
         KP1=K+1                                                                
         KM1=K-1                                                                
         JK=LJK+K                                                               
         KK = KM1*N - KM1*K / 2 + K                                         
         EPTOLA = A(JK) * A(JK) / (A(JJ) * A(KK))                               
         EPTOLB = B(JK) * B(JK) / (B(JJ) * B(KK))                               
         IF ((EPTOLA .LT. EPS) .AND. (EPTOLB .LT. EPS)) GO TO 210               
C                                                                               
C        SE ZERACAO FOR NECESSARIA, ENTAO CALCULAR OS ELEMENTOS DA              
C        MATRIZ DE ROTACAO CA E CG                                              
C                                                                               
         AKK=A(KK)*B(JK)-B(KK)*A(JK)                                            
         AJJ=A(JJ)*B(JK)-B(JJ)*A(JK)                                            
         AB =A(JJ)*B(KK)-A(KK)*B(JJ)                                             
         CHECK=(AB*AB+4.*AKK*AJJ)/4.                                            
         IF(CHECK)50,60,60                                                      
   50    WRITE(IOUT,2020)                                                       
         CALL CLOSEFILES                                                                   
   60    SQCH=SQRT(CHECK)                                                       
         D1=AB/2. + SQCH                                                          
         D2=AB/2. - SQCH                                                    
         DEN=D1                                                                 
         IF(ABS(D2).GT.ABS(D1))DEN=D2                                           
         IF(DEN)80,70,80                                                        
   70    CA=0.                                                                  
         CG=-A(JK)/A(KK)                                                        
         GO TO 90                                                               
   80    CA= AKK/DEN                                                         
         CG=-AJJ/DEN                                                            
C                                                                               
C        EXECUTAR A ROTACAO GENERALIZADA PARA ZERAR O ELEMENTO                  
C        FORA DA DIAGONAL                                                       
C                                                                               
   90    IF(N-2)100,190,100                                                     
  100    IF(JM1-1)130,110,110                                                   
  110    DO 120 I=1,JM1                                                         
         IM1=I-1                                                                
         IJ=IM1*N-IM1*I/2+J                                                     
         IK=IM1*N-IM1*I/2+K                                                     
         AJ=A(IJ)                                                               
         BJ=B(IJ)                                                               
         AK=A(IK)                                                               
         BK=B(IK)                                                               
         A(IJ)=AJ+CG*AK                                                         
         B(IJ)=BJ+CG*BK                                                         
         A(IK)=AK+CA*AJ                                                         
  120    B(IK)=BK+CA*BJ                                                         
  130    IF(KP1-N)140,140,160                                                   
  140    LJI=JM1*N-JM1*J/2                                                      
         LKI=KM1*N-KM1*K/2                                            
         DO 150 I=KP1,N                                                         
         JI=LJI + I                                                           
         KI=LKI + I                                                              
         AJ=A(JI)                                                               
         BJ=B(JI)                                                               
         AK=A(KI)                                                               
         BK=B(KI)                                                             
         A(JI)=AJ+CG*AK                                                   
         B(JI)=BJ+CG*BK                                                         
         A(KI)=AK+CA*AJ                                                         
  150    B(KI)=BK+CA*BJ                                                         
  160    IF (JP1 - KM1) 170,170,190                                             
  170    LJI=JM1*N-JM1*J/2                                                      
         DO 180 I=JP1,KM1                                                       
         JI=LJI+I                                                              
         IM1=I-1                                                                
         IK=IM1*N-IM1*I/2+K                                                     
         AJ=A(JI)                                                               
         BJ=B(JI)                                                               
         AK=A(IK)                                                               
         BK=B(IK)                                                               
         A(JI)=AJ+CG*AK                                                         
         B(JI)=BJ+CG*BK                                                         
         A(IK)=AK+CA*AJ                                                         
  180    B(IK)=BK+CA*BJ                                                         
  190    AK=A(KK)                                                               
         BK=B(KK)                                                               
         A(KK)=AK+2.*CA*A(JK)+CA*CA*A(JJ)                                       
         B(KK)=BK+2.*CA*B(JK)+CA*CA*B(JJ)                                       
         A(JJ)=A(JJ)+2.*CG*A(JK)+CG*CG*AK                                       
         B(JJ)=B(JJ)+2.* CG*B(JK)+CG*CG*BK                                      
         A(JK)=0.                                                               
         B(JK)=0.                                                               
C                                                                               
C        ATUALIZAR A MATRIZ DE AUTO-VETORES DEPOIS DE CADA ROTACAO              
C                                                                               
         DO 200 I=1,N                                                           
         XJ=X(I,J)                                                              
         XK=X(I,K)                                                              
         X(I,J)=XJ+CG*XK                                                        
  200    X(I,K)=XK+CA*XJ                                                        
C                                                                               
  210    CONTINUE                                                               
C                                                                               
         IF (CHECK .GE. 0) GOTO 213                                             
         IF (NSWEEP .LT. NSMAX) GO TO 40                                        
         GO TO 255                                                              
C                                                                               
C        ATUALIZAR OS AUTO-VALORES DEPOIS DE CADA VARREDURA                     
C                                                                               
  213    II=1                                                                   
         DO 220 I=1,N                                                           
         IF(A(II).GT.0. .AND. B(II).GT.0.) GO TO 215                            
            WRITE(IOUT,2020)            
2020     FORMAT(///,'*** ERRO JACOBI - MATRIZES NAO POSITIVAS DEFINIDAS.        
     1   EXECUCAO INTERROMPIDA.')  
            WRITE(IOUT,2025) II,A(II),B(II)                                        	 
2025     FORMAT(/12X,4HII =,I5,5X,7HA(II) =,G22.14,5X,7HB(II) =,G22.14 )        			
            CALL CLOSEFILES        
C			
  215    EIGV(I) =A(II)/B(II)                                                   
  220    II=II+N1-I                                                             
      IF(INTSAI(5) .EQ. 0) GO TO 230                                            
         WRITE(IOUT,2000) NSWEEP                                                
2000     FORMAT(/45H    AUTOVALORES CORRENTES NA VARREDURA NUMERO,I5 )   
         WRITE(IOUT,2010) (EIGV(I),I=1,N)        
2010     FORMAT(/6X,10G12.4)                                            		 
C                                                                               
C        TESTAR A CONVERGENCIA                                                  
C                                                                               
  230    DO 240 I=1,N                                                           
         TOL=RTOL*D(I)                                                          
         DIF=ABS(EIGV(I)-D(I))                                                  
         IF(DIF.GT.TOL) GO TO 280                                               
  240    CONTINUE                                                               
C                                                                               
C        TESTAR TODOS OS ELEMENTOS FORA DA DIAGONAL PARA VER SE                 
C        OUTRA VARREDURA E NECESSARIA                                           
C                                                                               
         EPS = RTOL ** 2                                                        
         DO 250 J=1,NR                                                          
         JM1=J - 1                                                                
         JP1=J + 1                                                            
         LJK=JM1*N-JM1*J/2                                                      
         JJ=LJK+J                                                               
         DO 250 K=JP1,N                                                         
         KM1=K-1                                                                
         JK=LJK+K                                                               
         KK=KM1*N - KM1*K/2+K                                                   
         EPTOLA = A(JK) * A(JK) / (A(JJ) * A(KK))                               
         EPTOLB = B(JK) * B(JK) / (B(JJ) * B(KK))                               
         IF ((EPTOLA .LT. EPS) .AND. (EPTOLB .LT. EPS)) GO TO 250               
         GO TO 280                                                              
  250    CONTINUE                                                               
C                                                                               
C        COMPLETAR O TRIANGULO INFERIOR DAS MATRIZES RESULTANTES                
C        E NORMALIZAR OS AUTO-VETORES                                           
C                                                                               
  255    II=1                                                                   
         DO 275 I=1,N                                                           
         BB=SQRT(B(II))                                                         
         DO 270 K=1,N                                                           
  270    X(K,I)=X(K,I)/BB                                                       
  275    II = II + N1 - I                                                       
         RETURN                                                                 
C                                                                               
C        ATUALIZAR A MATRIZ D E INICIAR NOVA VARREDURA, SE POSSIVEL             
C                                                                               
  280    DO 290 I=1,N                                                           
  290    D(I)=EIGV(I)                                                           
  300    IF (NSWEEP .LT. NSMAX) GO TO 40                                        
         GO TO 255                                                              
C                                                                               
         END                                                                    
C                                                                                
         SUBROUTINE DECOMP (A,MAXA,NN,ISH,IOUT)                                 
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA PARA FATORAR A MATRIZ DE RIGIDEZ, NA FORMA           *        
C*                      T                                              *        
C*       (L) * (D) * (L)                                               *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
         DIMENSION A (1),MAXA(1)                                                
         ABS(X)=DABS(X)                                                         
         IF (NN.EQ.1) RETURN                                                    
         DO 200 N=1,NN                                                          
         KN = MAXA(N)                                                           
         KL = KN + 1                                                            
         KU = MAXA(N+1) - 1                                                     
         KH=KU-KL                                                               
         IF (KH) 304,240,210                                                    
  210    K=N-KH                                                                 
         IC=0                                                                   
         KLT = KU                                                               
         DO 260 J=1,KH                                                          
         IC=IC +1                                                               
         KLT=KLT-1                                                              
         KI=MAXA(K)                                                             
         ND=MAXA(K+1)- KI -1                                                    
         IF (ND) 260,260,270                                                    
  270    KK = MIN0(IC,ND)                                                       
         C=0.                                                                   
         DO 280 L=1,KK                                                          
  280    C = C + A(KI+L) * A(KLT+L)                                             
         A(KLT)=A(KLT)-C                                                        
  260    K=K +1                                                                 
  240    K=N                                                                    
         B=0.                                                                   
         DO 300 KK=KL,KU                                                        
         K=K-1                                                                  
         KI=MAXA(K)                                                             
         C=A(KK)/A(KI)                                                          
         IF (ABS(C) .LT. 1.E07) GO TO 290                                       
            WRITE (IOUT,2010) N,C    
2010     FORMAT(//,'*** ERRO DECOMP- FALHA NO TESTE DE SEQ. DE STURM         
     1DURANTE A DECOMPOSICAO DA MATRIZ DE RIGIDEZ.',                             
     2  /12X,40HFATOR DE MULTIPLICACAO RELATIVO A COLUNA,I5,2H =,G22.14)        			
            CALL CLOSEFILES            
C			
  290    CONTINUE
         B=B + C*A(KK)                                                          
  300    A(KK)=C                                                                
         A(KN)=A(KN)-B                                                          
  304    IF (A(KN)) 310,310,200                                                 
  310    IF (ISH.EQ.0) GO TO 320                                                
            IF (A(KN).EQ.0.)  A(KN)=-1.E-16                                        
            GO TO 200                                                              
  320    WRITE(IOUT,2000) N,A(KN) 
2000     FORMAT(//,'*** ERRO DECOMP- A MATRIZ DE RIGIDEZ NAO POSITIVA        
     1 DEFINIDA. PIVOT NAO POSITIVO PARA A EQUACAO   ',I5,1H.,                      
     2   /12X, 7HPIVOT =,G22.14 )                                                 
         CALL CLOSEFILES                                                                   
  200    CONTINUE                                                               
         RETURN                                                                 
C                                                                                                                                                              
         END                                            
C		 
         SUBROUTINE SCHECK (EIGV,RTOLV,BUP,IBUP,BLO,BUPC,NEIV,NC,NEI,           
     1   RTOL,SHIFT,IOUT)                                                       
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA DE AVALIACAO DO DESLOCAMENTO PARA O TESTE DA         *        
C*       SEQUENCIA DE STURM                                            *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION EIGV(NC),RTOLV(NC),BUP(NC),IBUP(NC),BLO(NC),BUPC(NC),        
     *   NEIV(NC)                                                               
C                                                                               
      FTOL = 0.01                                                            
         DO 100 I=1,NC                                                          
         BUP(I)=EIGV(I)*(1.+FTOL)                                               
  100    BLO(I)=EIGV(I)*(1.-FTOL)                                           
         NROOT = 0                                                              
         DO 120 I=1,NC                                                          
  120    IF (RTOLV(I) .LT. RTOL) NROOT = NROOT + 1                              
         IF (NROOT.GE.1) GO TO 200                                              
            WRITE (IOUT,1010) 
 1010    FORMAT(//,'*** ERRO 1010 SCHECK- NENHUM AUTOVALOR ENCONTRADO.        
     1   EXECUCAO INTERROMPIDA.')  		 
            CALL CLOSEFILES                                                                   
C                                                                               
C        ACHAR OS LIMITES SUPERIORES SOBRE OS AGRUPAMENTOS DE AUTO-VALOR        
C                                                                               
  200    DO 240 I=1,NROOT                                                       
  240    NEIV(I)=1                                                              
         IF (NROOT.NE.1) GO TO 260                                              
            BUPC(1)=BUP(1)                                                         
            L=1                                                                    
            I=2                                                                    
            GO TO 295                                                              
  260    L=1                                                                    
         I=2                                                                    
  270    IF (BUP(I-1).LE.BLO(I)) GO TO 280                                      
            NEIV(L) = NEIV(L) + 1                                                  
            I=I+1                                                                  
            IF (I.LE.NROOT) GO TO 270                                              
  280    BUPC(L)=BUP(I-1)                                                       
         IF (I.GT.NROOT)  GO TO 290                                             
            L=L + 1                                                                  
            I=I + 1                                                              
            IF (I.LE.NROOT) GO TO 270                                              
            BUPC(L)=BUP(I-1)                                                       
  290    IF (NROOT.EQ.NC) GO TO 300                                             
  295    IF (BUP(I-1).LE.BLO(I)) GO TO 300                                      
         IF (RTOLV(I).GT.RTOL)  GO TO 300                                       
            BUP(L)=BUP(I)                                                          
            NEIV(L)=NEIV(L)+1                                                      
            NROOT=NROOT+1                                                          
            IF (NROOT.EQ.NC) GO TO 300                                             
               I=I+1                                                                  
               GO TO 295                                                              
C                                                                               
C        ACHAR DESLOCAMENTO                                                     
C                                                                               
  300    IBUP(1) = NEIV(1)                                                      
         IF (L .EQ. 1) GO TO 310                                                
            DO 320 I = 2,L                                                         
  320       IBUP(I) = NEIV(I) + IBUP(I-1)                                          
  310    DO 340 I=1,L                                                           
         IF (IBUP(I).GE.NROOT) GO TO 350                                        
  340    CONTINUE                                                               
         I = L                                                                  
  350    SHIFT=BUPC(I)                                                          
         NEI= IBUP(I)                                                           
C                                                                               
         RETURN                                                                 
C                                                                               
         END
C		 
         SUBROUTINE SCHECB (EIGV,RTOLV,BUP,BLO,BUPC,NEIV,NC,NEI,RTOL,           
     *                      SHIFT,IOUT)                                                       
C***********************************************************************        
C*                                                                     *        
C*       PROGRAMA DE AVALIACAO DO DESLOCAMENTO PARA O TESTE DA         *        
C*       SEQUENCIA DE STURM  ---  VERSAO BATHE ---                     *        
C*                                                                     *        
C***********************************************************************        
C                                                                               
         IMPLICIT REAL*8 (A-H,O-Z)                                              
C                                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION EIGV(NC),RTOLV(NC),BUP(NC),BLO(NC),BUPC(NC),NEIV(NC)                                                               
C                                                                               
      FTOL = 0.01
C	  
         DO 100 I=1,NC                                                          
         BUP(I)=EIGV(I)*(1.+FTOL)                                               
  100    BLO(I)=EIGV(I)*(1.-FTOL)                                           
         NROOT = 0                                                              
         DO 120 I=1,NC                                                          
  120    IF (RTOLV(I) .LT. RTOL) NROOT = NROOT + 1                              
         IF (NROOT.GE.1) GO TO 200                                              
            WRITE (IOUT,1010) 
 1010    FORMAT(//,'*** ERRO 1010 SCHECK- NENHUM AUTOVALOR ENCONTRADO.        
     1   EXECUCAO INTERROMPIDA.')  		 
            CALL CLOSEFILES                                                                   
C                                                                               
C        ACHAR OS LIMITES SUPERIORES SOBRE OS AGRUPAMENTOS DE AUTO-VALOR        
C                                                                               
  200    DO 240 I=1,NROOT                                                       
  240    NEIV(I)=1                                                              
         IF (NROOT.NE.1) GO TO 260                                              
            BUPC(1)=BUP(1) 
            LM=1			
            L=1                                                                    
            I=2                                                                    
            GO TO 295                                                              
  260    L=1                                                                    
         I=2                                                                    
  270    IF (BUP(I-1).LE.BLO(I)) GO TO 280                                      
            NEIV(L) = NEIV(L) + 1                                                  
            I=I+1                                                                  
            IF (I.LE.NROOT) GO TO 270                                              
  280    BUPC(L)=BUP(I-1)                                                       
         IF (I.GT.NROOT)  GO TO 290                                             
            L=L + 1                                                                  
            I=I + 1                                                              
            IF (I.LE.NROOT) GO TO 270                                              
            BUPC(L)=BUP(I-1)    
  290    LM=L			
         IF (NROOT.EQ.NC) GO TO 300                                             
  295    IF (BUP(I-1).LE.BLO(I)) GO TO 300                                      
         IF (RTOLV(I).GT.RTOL)  GO TO 300                                       
            BUPC(L)=BUP(I)                                                          
            NEIV(L)=NEIV(L)+1                                                      
            NROOT=NROOT+1                                                          
            IF (NROOT.EQ.NC) GO TO 300                                             
               I=I+1                                                                  
               GO TO 295                                                              
C                                                                               
C        ACHAR DESLOCAMENTO                                                     
C                                                                               
  300    WRITE(IOUT,1020)
 1020    FORMAT(///,' UPPER BOUNDS ON EIGENVALUE CLUSTERS')
         WRITE(IOUT,1005) (BUPC(I),I=1,LM)
 1005    FORMAT(' ',6E22.14)
         WRITE(IOUT,1030)
 1030    FORMAT(//,' NO. OF EIGENVALUES IN EACH CLUSTER')
         WRITE(IOUT,1006) (NEIV(I),I=1,LM)		 
 1006    FORMAT(' ',6I22)
		 LL=LM-1
         IF (LM .EQ. 1) GO TO 310                                                
  330       DO 320 I = 1,LL                                                         
  320       NEIV(L) = NEIV(L) + NEIV(I)                  
            L=L-1
            LL=LL-1
            IF(L.NE.1) GO TO 330
  310    WRITE(IOUT,1040)
 1040    FORMAT(' NO. OF EIGENVALUES LESS THAN UPPER BOUNDS')
         WRITE(IOUT,1006) (NEIV(I),I=1,LM)
         L=0		 
         DO 340 I=1,LM                  
           L=L+1		 
           IF (NEIV(I).GE.NROOT) GO TO 350                                        
  340    CONTINUE                                                               
  350    SHIFT=BUPC(L)                                                          
         NEI=  NEIV(L)                                                           
C                                                                               
         RETURN                                                                 
C                                                                               
         END                                                                    		 
C                                                                                
      SUBROUTINE TRUSS                                                          
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      . PARA ALOCAR MEMORIA E CHAMAR SUBROTINA                     .         
C .        DO ELEMENTO DE TRELICA ESPACIAL                            .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON A(1)                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
C                                                                               
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT)                               
C                                                                               
      NFIRST=N6                                                                 
      IF(IND.GT.1) NFIRST=N5                                                    
      WRITE(IOUT,100)                                                           
 100  FORMAT(////,' TIPO DE ELEMENTO (TRELICA) NAO IMPLEMENTADO '               
     *     ,' NESTA VERSAO.',///,'PROCESSAMENTO INTERROMPIDO.')                 
      CALL CLOSEFILES                                                                      
C                                                                               
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE TENV3( U,INCID,LM,MATP)                                        
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .      SUBROUTINA PARA CALCULO DAS ACOES NOS EXTREMOS               .         
C .      DUMA VIGA ESPACIAL                                           .         
C .                                                                   .         
C .      SL MATRIZ DO ELEMENTO EM COOR. LOCAIS MULTIPLICADA POR R TRANS.        
C .      P MATRIZ DOS ESFORCOS NOS EXTREMOS DE CADA MEMBRO            .         
C .      OBSERVACAO  A MATRIZ SL ESTA GRAV. NO ARCH. 21               .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL * 8(A-H,O-Z)                                                
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      REAL *8 KAPY,KAPZ                                                         
      REAL HED                                                                  
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
     *              ,KSTIFF,ESFMOD,MODOS,INFLU,IWORK                            
      COMMON A(1)                                                               
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /KABEC / HED(20),NPAG,LINHA,LPPAG                                  
      DIMENSION LM(12,1),MATP(1),U(1),INCID(2,1),AML(12)                        
      DIMENSION  SL(12,12),P(12)                                                
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)               
      DATA NS/12/                                                               
      IPRINT=0                                                                  
      IF( KODT .EQ. 3 ) GO TO 60                                                
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
 60   GO TO (61,61,63,64,65),KODT                                               
 61   ITAPE=FORCAS                                                              
      GO TO 70                                                                  
 64   ITAPE=IWORK                                                               
      GO TO 70                                                                  
 63   ITAPE=ESFMOD                                                              
 65   DO 10 I=1,12                                                              
 10   AML(I)=0.                                                                 
 70   IF( KODT.EQ.1.OR.KODT.EQ.3) WRITE(ITAPE) NS,NUME,NPAR(10)                 
      DO 690 N=1,NUME                                                           
      IF( KODT .EQ. 3 ) GO TO 3                                                 
      IPRINT=IPRINT+2                                                           
      IF(IPRINT.LE.50)GO TO 80                                                  
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
      IPRINT=0                                                                  
 80   GO TO (1,2,3,2,3),KODT                                                    
    1 READ(FNODEQ)AML                                                           
    3 READ(IMTV)SL                                                              
      DO 680 K=1,12                                                             
      STR=0.                                                                    
      DO 670 L=1,6                                                              
      I=LM(L,N)                                                                 
      IF(I.LE.0) GO TO 607                                                      
      STR=STR+SL(K,L)*U(I)                                                      
  607 CONTINUE                                                                  
      J=LM(L+6,N)                                                               
      IF(J.LE.0) GO TO 670                                                      
      STR=STR+SL(K,L+6)*U(J)                                                    
  670 CONTINUE                                                                  
      P(K)=STR+AML(K)                                                           
  680 CONTINUE                                                                  
      GO TO (800,2,661,2,800),KODT                                              
    2 READ(ITAPE)P                                                              
  800 DO 660 J=1,2                                                              
      NO=INCID(J,N)                                                             
      L=(J-1)*6+1                                                               
      L5=L+5                                                                    
      WRITE(IOUT,2070)N,NO,(P(K),K=L,L5)                                        
  660 CONTINUE                                                                  
      GO TO (661,690,661,690,690),KODT                                          
 661  WRITE(ITAPE)P                                                             
  690 CONTINUE                                                                  
 2060 FORMAT(   /10X,'C A L C U L O  D A S  T E N S O E S  P A R A  G R         
     1U P O  D E  E L E M E N T O S  N0 : ',I4//,10X,'ESFORCOS NAS EXTRE        
     2MIDADES DOS ELEMENTOS DE VIGA',//2X,'ELEMENTO',4X,'NO',7X,'F O R C        
     3 A S  N A S  D I R E C O E S',16X,'M O M E N T O S',/3X,'NUMERO',         
     4 3X,'NUMERO',8X,'X',13X,'Y',13X,'Z',12X,'MX',12X,'MY',12X,'MZ')           
 2070 FORMAT(I6,I10,4X,6(G12.5,2X))                                             
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MOLAS                                                          
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C     P R O G R A M A                                                 .         
C        PARA ALOCAR MEMORIA E CHAMAR                                 .         
C        AS SUBROTINAS DO ELEMENTO DE MOLA                            .         
C              SE IND=1  GERA INFORMACOES                                       
C              SE IND=2  CALCULA E MONTA MATRIZES  DO ELEMENTO        .         
C              SE IND=3  CALCULA TENSOES                              .         
C              SE IND=4  CALCULA CARGAS NODAIS EQUIVALENTES           .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /MATRIZ/IMTV                                                       
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/EXTRA/ITIP                                                         
      COMMON/CARGAS/ICARGA(10),N302,N303,N304,N305,N306,N307,N308               
      COMMON A(1)                                                               
      DIMENSION LABEL(5)                                                        
      EQUIVALENCE(NPAR(2),NUME),(NPAR(3),NUMMAT),(NPAR(17),NEGDP)               
C                                                                               
C     Era assim:
C      DATA LABEL/'SUBR','OUTI','NA M','OLAS','    '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OUTI'
      LABEL(3)='NA M'
      LABEL(4)='OLAS'
      LABEL(5)='    '
C                                                                               
      NFIRST=N6                                                                 
      IF(IND.GT.1)NFIRST=N5                                                     
      N101=NFIRST                                                               
      N204=N101+NUMMAT*ITWO                                                     
      N205=N204+NUMMAT*ITWO                                                     
      N206=N205+3*NUME*ITWO                                                     
      N207=N206+7*NUME                                                          
      N208=N207+  NUME                                                          
      N209=N208+6*NUME                                                          
      N210=N209+  NUME                                                          
 5    N211=N210+  NUME                                                          
      NLAST=N211                                                                
      IF(IND.GT.1)GO TO 100                                                     
      IF(NLAST.GT.MTOT)CALL ERROR(NLAST-MTOT,3,LABEL,5)                         
      GO TO 200                                                                 
100   IF(NLAST.GT.MTOT)CALL ERROR(NLAST-MTOT,4,LABEL,5)                         
200   MIDEST=NLAST-NFIRST                                                       
C                                                                               
C**********************************************************************         
      IF(IDEPUR(1).EQ.1)                                                        
     *WRITE(IOUT,2005)N101,N204,N205,N206,N207,N208,N209,N210                   
2005  FORMAT(10I10)                                                             
C**********************************************************************         
C                                                                               
      GO TO (300,600,900,1200),IND                                              
C                                                                               
C     LEITURA DAS PROPRIEDADES DOS ELEMENTOS                                    
C                                                                               
300   CALL PMOLA(A(N1),A(N2),A(N3),A(N4),A(N101),A(N204),A(N205),               
     *           A(N206),A(N207),A(N208),A(N209),A(N210))                       
      RETURN                                                                    
600   CONTINUE                                                                  
C                                                                               
      IF(MODEX.EQ.0)RETURN                                                      
C                                                                               
C     CALCULO DAS MATRIZES DE RIGIDEZ E MASSA                                   
C                                                                               
      CALL RIGMOL(A(N101),A(N204),A(N205),A(N206),A(N207),A(N208),              
     *            A(N209))                                                      
      RETURN                                                                    
900   CONTINUE                                                                  
C                                                                               
C     CALCULO DAS TENSOES                                                       
C                                                                               
      IF(MODEX.EQ.0)RETURN                                                      
                                                                                
      IF(NPAR(10).NE.0)RETURN                                                   
      CALL TENMOL(A(N4),A(N207),A(N208))                                        
C                                                                               
      RETURN                                                                    
1200  CONTINUE                                                                  
C                                                                               
C                                                                               
C     CALCULO DAS CARGAS NODAIS EQUIVALENTES                                    
C                                                                               
      IF(MODEX.EQ.0)RETURN                                                      
      CALL CADMOL(A(N4),A(N101),A(N204),A(N205),A(N307),A(N308),A(N206),        
     1            A(N208),A(N209))                                              
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PMOLA(ID,X,Y,Z,DMOLA,RMOLA,XYZ,NNV,INCID,LM,MATP,IMSE)         
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C     P R O G R A M A                                                 .         
C        PARA ELEMENTOS DE CONTORNO (MOLAS)                           .         
C        LE E GERA INFORMACOES                                        .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL A                                                                    
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     *          ,NEMSE                                                          
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /MATRIZ/IMTV                                                       
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/EXTRA/ITIP                                                         
      COMMON A(1)                                                               
      DIMENSION X(1),Y(1),Z(1),ID(6,1),DMOLA(1),RMOLA(1),XYZ(3,1),              
     *          NNV(7,1),INCID(1,1),LM(6,1),MATP(1),IMSE(1)                     
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)               
      SQRT(XX)=DSQRT(XX)                                                          
      DATA NS,ND/2,6/                                                           
      IF( NPAR(10) .EQ. 0 ) NEQT=NEQT+2*NUME                                    
C                                                                               
C     LE INFORMACOES SOBRE MOLAS                                                
C                                                                               
C                                                                               
      IF(INTSAI(1).EQ.0)GO TO 5                                                 
      WRITE(IOUT,2000)NPAR1,NUME                                                
      WRITE(IOUT,2040)NPAR(10)                                                  
      WRITE(IOUT,2010)NUMMAT                                                    
                                                                                
      WRITE(IOUT,2005)                                                          
5     CONTINUE                                                                  
      DO 10 I=1,NUMMAT                                                          
      READ(IIN,1000)N,DMOLA(N),RMOLA(N)                                         
      IF( DMOLA(N) .EQ. 0.) DMOLA(N)=1.0D17                                     
      IF( RMOLA(N) .EQ. 0.) RMOLA(N)=1.0D17                                     
      IF(N.EQ.I)GO TO 11                                                        
      WRITE(IOUT,2020)N,I                                                       
      CALL CLOSEFILES                                                                      
C**********************************************************************         
11    IF(INTSAI(1).EQ.1)                                                        
     *WRITE(IOUT,2015)N,DMOLA(N),RMOLA(N)                                       
C**********************************************************************         
10    CONTINUE                                                                  
C                                                                               
C      LE INFORMACOES SOBRE O ELEMENTO                                          
C                                                                               
      N=1                                                                       
C**********************************************************************         
      IF(INTSAI(1).EQ.1)WRITE(IOUT,2025)                                        
C**********************************************************************         
210   KG=0                                                                      
      MARK=0                                                                    
200   READ(IIN,1010)NP,NI,NJ,NK,NL,KD,KR,MTYP,MSE,KN                            
      IF(KG.GT.0)GO TO 550                                                      
      KG=KN                                                                     
      IF(NJ.EQ.0)GO TO 250                                                      
      X1=X(NJ)-X(NI)                                                            
      Y1=Y(NJ)-Y(NI)                                                            
      Z1=Z(NJ)-Z(NI)                                                            
      X2=X(NL)-X(NK)                                                            
      Y2=Y(NL)-Y(NK)                                                            
      Z2=Z(NL)-Z(NK)                                                            
      T1=Y1*Z2-Y2*Z1                                                            
      T2=Z1*X2-Z2*X1                                                            
      T3=X1*Y2-X2*Y1                                                            
      GO TO 260                                                                 
250   T1=X(NI)-X(NP)                                                            
      T2=Y(NI)-Y(NP)                                                            
      T3=Z(NI)-Z(NP)                                                            
260   XL=T1**2+T2**2+T3**2                                                      
      XL=SQRT(XL)                                                               
      IF(XL.GT.1.0D-6)GO TO 270                                                 
      WRITE(IOUT,2030)N                                                         
      CALL CLOSEFILES                                                                      
270   CONTINUE                                                                  
      XYZ(1,N)=T1/XL                                                            
      XYZ(2,N)=T2/XL                                                            
      XYZ(3,N)=T3/XL                                                            
      NN=NP                                                                     
      NNI=NI                                                                    
      NNJ=NJ                                                                    
      NNK=NK                                                                    
      NNL=NL                                                                    
      NKD=KD                                                                    
      NKR=KR                                                                    
      MMTP=MTYP                                                                 
      MMSE=MSE                                                                  
      GO TO 560                                                                 
550   MARK=1                                                                    
555   NN=NN+KG                                                                  
      NNI=NNI+KG                                                                
560   NNV(1,N)=NN                                                               
      NNV(2,N)=NNI                                                              
      NNV(3,N)=NNJ                                                              
      NNV(4,N)=NNK                                                              
      NNV(5,N)=NNL                                                              
      NNV(6,N)=KD                                                               
      NNV(7,N)=KR                                                               
      MATP(N)=MMTP                                                              
      IMSE(N)=MMSE                                                              
      DO 400 L=1,ND                                                             
400   LM(L,N)=ID(L,NN)                                                          
C                                                                               
C     INCIDENCIA DO ELEMENTO                                                    
C                                                                               
      INCID(1,N)=NN                                                             
C**********************************************************************         
      IF(INTSAI(1).EQ.1)                                                        
     *WRITE(IOUT,2035)N,(NNV(I,N),I=1,7),KN,MATP(N),IMSE(N)                     
C**********************************************************************         
      IF(N.EQ.NUME) GO TO 500                                                   
      N=N+1                                                                     
      IF(NN.LT.NP)GO TO 555                                                     
      IF(MARK.EQ.1)GO TO 210                                                    
      GO TO 200                                                                 
 500  IF( NG .NE. 1 ) RETURN                                                    
      NEMSE=0                                                                   
      DO 450 I=1,NUME                                                           
 450  NEMSE=NEMSE+IMSE(I)                                                       
      RETURN                                                                    
C                                                                               
C     FORMATOS                                                                  
1000  FORMAT(I5,2E10.3)                                                         
1010  FORMAT(10I5)                                                              
2000  FORMAT(10X,'D E F I N I C A O  D O  E L E M E N T O',///                  
     1        5X,'TIPO DE ELEMENTO',13(2H .),17H( NPAR(1) ) . . =,I5,           
     2        ' (ELEMENTO DE MOLA)',/5X,'NUMERO DE ELEMENTOS ',                 
     3        11(2H .),17H( NPAR(2) ) . . =,I5,//)                              
2005  FORMAT(5X,'MATERIAL',6X,'CONSTANTES DAS MOLAS',/                          
     1       6X,'NUMERO',4X,'LONGITUDINAL',5X,'TORSIONAL',//)                   
2010  FORMAT(10X,'D E F I N I C A O  D O  M A T E R I A L',///,                 
     1        5X,'NUMERO DE MATERIAS DIFERENTES . . . . . .( NPAR(3) )',        
     2        ' . . =',I5//)                                                    
2015  FORMAT(I8,4X,2(3X,G12.5))                                                 
2020  FORMAT(///10X,'*** ERRO NA ESPECIFICACAO DOS MATERIAIS ***',              
     1        //10X,'*** O MATERIAL',I5,' NAO CORRESPONDE COM ***',             
     2        //10X,'*** O CARTAO NUMERO',I5,16X,'***')                         
2025  FORMAT(1H1,9X ,'I N F O R M A C O E S  D O  E L E M E N T O',///          
     *       5X,'ELEMENTO  N O',3X,'DEF. DA DIRECAO PRESCRITA',                 
     1       4X,'CODIGOS',3X,'NUMERO DO                            ',           
     2       /6X,'NUMERO',3X,'(N)',7X,'NI   NJ   NK   NL',6X,'KD  KR',          
     3       '  KN   MATERIAL    IMSE ',//)                                     
2030  FORMAT(10X,'*** ERRO-ELEMENTO ',I5,' SEM DIRECAO DEFINIDA ***',//)        
2035  FORMAT(I10,I8,4X,4I5,3X,3I4,I9,I5)                                        
 2040 FORMAT(5X,'CALCULO DE TENSOES',12(2H .),                                  
     1   17H( NPAR(10)) . . =,I5,                                               
     2   ' ( SE.NE.0 NAO CALCULA )',/                                           
     3   70X,'( SE.EQ.0 CALCULA    )',//)                                       
C                                                                               
      END                                                                       
      SUBROUTINE RIGMOL(DMOLA,RMOLA,XYZ,NNV,INCID,LM,MATP)                      
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C     P R O G R A M A                                                 .         
C        CALCULA MATRIZ DE RIGIDEZ GLOBAL E MATRIZ DE TENSOES         .         
C        GRAVA MATRIZ DE TENSOES, E MONTA MATRIZ DE RIGIDEZ DO        .         
C        ELEMENTO NA MATRIZ GLOBAL                                    .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL A                                                                    
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /MATRIZ/IMTV                                                       
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/EXTRA/ITIP                                                         
      COMMON A(1)                                                               
      DIMENSION SM(21),S(21),SL(2,6),XYZ(3,1),INCID(1,1),LM(6,1),MATP(1)        
      DIMENSION DMOLA(1),RMOLA(1),NNV(7,1)                                      
      EQUIVALENCE(NPAR(2),NUME),(NPAR(3),NUMMAT)                                
      DATA NS,ND/2,6/                                                           
      DO 500 N=1,NUME                                                           
      MTYPE=MATP(N)                                                             
C**********************************************************************         
      IF(IDEPUR(2).EQ.1)                                                        
     *WRITE(IOUT,2000)N,(XYZ(K,N),K=1,3),INCID(1,N),(LM(K,N),K=1,6)             
C**********************************************************************         
C                                                                               
C     INICIALIZACAO                                                             
C                                                                               
      I=0                                                                       
      DO 20 NI=1,ND                                                             
      DO 20 NJ=NI,ND                                                            
      I=I+1                                                                     
      SM(I)=0.                                                                  
20    S(I)=0.                                                                   
      DO 40 NK=1,NS                                                             
      DO 40 NL=1,ND                                                             
40    SL(NK,NL)=0.                                                              
C                                                                               
C     DESLOCAMENTOS PRESCRITOS                                                  
C                                                                               
      DMOL=DMOLA(MTYPE)                                                         
      RMOL=RMOLA(MTYPE)                                                         
      T1=XYZ(1,N)                                                               
      T2=XYZ(2,N)                                                               
      T3=XYZ(3,N)                                                               
      IF(NNV(6,N).EQ.0) GO TO 300                                               
                                                                                
      IF(ITIP.EQ.1) GO TO 311                                                   
      IF(NPAR(10).LT.0) GO TO 311                                               
      SL(1,1)=T1*DMOL                                                           
      SL(1,2)=T2*DMOL                                                           
      SL(1,3)=T3*DMOL                                                           
311   CONTINUE                                                                  
      S(1)=T1*T1*DMOL                                                           
      S(2)=T1*T2*DMOL                                                           
      S(3)=T1*T3*DMOL                                                           
      S(7)=T2*T2*DMOL                                                           
      S(8)=T2*T3*DMOL                                                           
      S(12)=T3*T3*DMOL                                                          
      GO TO 350                                                                 
300   DO 310 I=1,3                                                              
      SL(1,I)=0.                                                                
310   S(I)=0.                                                                   
      S(7)=0.                                                                   
      S(8)=0.                                                                   
      S(12)=0.                                                                  
C                                                                               
C     ROTACAO PRESCRITA                                                         
C                                                                               
350   IF(NNV(7,N).EQ.0) GO TO 400                                               
                                                                                
      IF(ITIP.EQ.1) GO TO 312                                                   
      IF(NPAR(10).LT.0) GO TO 312                                               
      SL(2,4)=T1*RMOL                                                           
      SL(2,5)=T2*RMOL                                                           
      SL(2,6)=T3*RMOL                                                           
312   CONTINUE                                                                  
      S(16)=T1*T1*RMOL                                                          
      S(17)=T1*T2*RMOL                                                          
      S(18)=T1*T3*RMOL                                                          
      S(19)=T2*T2*RMOL                                                          
      S(20)=T2*T3*RMOL                                                          
      S(21)=T3*T3*RMOL                                                          
      GO TO 530                                                                 
400   DO 410 I=4,6                                                              
      SL(2,I)=0.                                                                
      S(12+I)=0.                                                                
410   S(15+I)=0.                                                                
530   CONTINUE                                                                  
C                                                                               
C     ESCREVE MATRIZES DOS ELEMENTOS                                            
C                                                                               
C**********************************************************************         
      IF(INTSAI(2).EQ.1)WRITE(IOUT,2005)(S(I),I=1,21)                           
C**********************************************************************         
      IF(ITIP.EQ.1)GO TO 314                                                    
      IF(NPAR(10).LT.0)GO TO 314                                                
C**********************************************************************         
      IF(INTSAI(2).EQ.1)WRITE(IOUT,2010)((SL(I,J),J=1,6),I=1,2)                 
C**********************************************************************         
314   CONTINUE                                                                  
C                                                                               
C     ESCREVE EM DISCO MATRIZ DE TENSOES                                        
C                                                                               
                                                                                
      IF(ITIP.EQ.1) GO TO 313                                                   
      IF(NPAR(10).LT.0) GO TO 313                                               
      WRITE(IMTV)SL                                                             
313   CONTINUE                                                                  
C                                                                               
C     MONTA ELEMENTO NA MATRIZ GLOBAL                                           
C                                                                               
      CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND,A(N10),SM,N)                         
500   CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C     FORMATOS                                                                  
C                                                                               
2000  FORMAT(I5,4X,3(2X,F10.4),5X,I5,5X,6(2X,I5))                               
2005  FORMAT(//120('*'),//10X,'MATRIZ DE RIGIDEZ DO ELEMENTO',                  
     1       //120('*'),//5X,6(G12.5,3X),/20X,5(G12.5,3X),/35X,                 
     2       4(G12.5,3X),/50X,3(G12.5,3X),/65X,2(G12.5,3X),/80X,G12.5)          
2010  FORMAT(///120('*'),//10X,'MATRIZ DE TENSOES DO ELEMENTO',                 
     1        //120('*'),//,(6G17.5))                                           
C                                                                               
      END                                                                       
                                                                                
      SUBROUTINE CADMOL(R,DMOLA,RMOLA,XYZ,NGENE,SSV,NNV,LM,MATP)                
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C     P R O G R A M A                                                 .         
C        PARA CALCULO DAS CARGAS NODAIS EQUIVALENTES                  .         
C        DO ELEMENTO DE MOLAS, E SUA MONTAGEM                         .         
C        NO VETOR DE CARGAS GLOBAL                                    .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL A                                                                    
      INTEGER RIGROT,FORCAS,FNODEQ                                              
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,              
     *           NLCDIN,LCASE                                                   
      COMMON/DIM/N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15             
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                  
      COMMON/VAR/NG,MODEX                                                       
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ             
      COMMON/MATRIZ/IMTV                                                        
      COMMON/DEPURA/IDEPUR(15),INTSAI(15)                                       
      COMMON/EXTRA/ITIP                                                         
      COMMON/CARGAS/ICARGA                                                      
      COMMON A(1)                                                               
      DIMENSION F(6),MATP(1),DMOLA(1),RMOLA(1),R(1),NGENE(2,1),                 
     1   XYZ(3,1),SSV(2,1),NNV(7,1),LM(6,1),AML(2),ICARGA(10)                   
      EQUIVALENCE (NPAR(2),NUME),(ICARGA(7),NEDP)                               
      DATA NS,ND/2,6/                                                           
      DO 500 N=1,NUME                                                           
      AML(1)=0.                                                                 
      AML(2)=0.                                                                 
      IF( NEDP .LE. 0 ) GO TO 490                                               
      DO 10 IDP=1,NEDP                                                          
      IF( NGENE(1,IDP) .EQ. NG .AND. NGENE(2,IDP) .EQ. N ) GO TO 11             
 10   CONTINUE                                                                  
      GO TO 490                                                                 
 11   MTYPE=MATP(N)                                                             
      DMOL=DMOLA(MTYPE)                                                         
      RMOL=RMOLA(MTYPE)                                                         
      T1=XYZ(1,N)                                                               
      T2=XYZ(2,N)                                                               
      T3=XYZ(3,N)                                                               
      IF(NNV(6,N).EQ.0) GO TO 300                                               
      PP=DMOL*SSV(1,IDP)                                                        
      AML(1)=PP                                                                 
      F(1)=T1*PP                                                                
      F(2)=T2*PP                                                                
      F(3)=T3*PP                                                                
      GO TO 350                                                                 
300   CONTINUE                                                                  
      DO 310 I=1,3                                                              
310   F(I)=0.                                                                   
350   CONTINUE                                                                  
      IF(NNV(7,N).EQ.0) GO TO 400                                               
      PP=RMOL*SSV(2,IDP)                                                        
      AML(2)=PP                                                                 
      F(4)=T1*PP                                                                
      F(5)=T2*PP                                                                
      F(6)=T3*PP                                                                
      GO TO 450                                                                 
400   DO 410 I=4,6                                                              
410   F(I)=0.                                                                   
450   CONTINUE                                                                  
C     *****************************************************************         
      IF(IDEPUR(2).EQ.1)                                                        
     *WRITE(IOUT,2000) N,NG,(F(I),I=1,6)                                        
C     *****************************************************************         
      DO 100 K=1,ND                                                             
      II=LM(K,N)                                                                
      IF(II.EQ.0) GO TO 100                                                     
      R(II)=R(II)+F(K)                                                          
100   CONTINUE                                                                  
      IF( IND .EQ. 5 ) RETURN                                                   
490   IF( IND .NE. 5 ) WRITE(FNODEQ)AML                                         
500   CONTINUE                                                                  
C                                                                               
C     FORMATOS                                                                  
C                                                                               
2000  FORMAT(///120('*'),//10X,'VETOR DE CARGAS NODAIS ',                       
     1       'EQUIVALENTES DO ELEMENTO NO:',I5,' DO GRUPO NO:',I5,//,           
     2       120('*'),//,6G17.5)                                                
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE TENMOL(U,INCID,LM)                                             
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      PARA CALCULO DAS FORCAS NAS MOLAS                            .         
C .      SL - MATRIZ PARA CALCULO DAS TENSOES POR DIRECAO             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL A,HED                                                                
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                      
      COMMON/DIM/N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15             
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1       N208,N209,N210,N211,N104,N105,N212                                 
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON/VAR/NG,MODEX                                                       
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ             
     *              ,KSTIFF,ESFMOD,MODOS,INFLU,IWORK                            
      COMMON/MATRIZ/IMTV                                                        
      COMMON/DEPURA/IDEPUR(15),INTSAI(15)                                       
      COMMON / KABEC /HED(20),NPAG,LINHA,LPPAG                                  
      COMMON/EXTRA/ITIP                                                         
      COMMON A(1)                                                               
      DIMENSION SL(2,6),U(1),P(2),INCID(1,1),LM(6,1),AML(2)                     
      EQUIVALENCE (NPAR(2),NUME)                                                
      DATA NS,ND/2,6/                                                           
      IPRINT=0                                                                  
      IF( KODT .EQ. 3 ) GO TO 60                                                
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
 60   GO TO (61,61,63,64,65),KODT                                               
 61   ITAPE=FORCAS                                                              
      GO TO 70                                                                  
 63   ITAPE=ESFMOD                                                              
      GO TO 65                                                                  
 64   ITAPE=IWORK                                                               
 65   AML(1)=0.                                                                 
      AML(2)=0.                                                                 
 70   IF(KODT.EQ.1.OR.KODT.EQ.3) WRITE(ITAPE) NS,NUME,NPAR(10)                  
C                                                                               
      DO 660 N=1,NUME                                                           
      IF( KODT .EQ. 3 ) GO TO 3                                                 
      IPRINT=IPRINT+1                                                           
      IF(IPRINT.LE.50)GO TO 80                                                  
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
      IPRINT=0                                                                  
 80   GO TO (1,2,3,2,3),KODT                                                    
C                                                                               
C     LE DO DISCO MATRIZ DE TENSOES                                             
C                                                                               
    1 READ(FNODEQ)AML                                                           
    3 READ(IMTV) SL                                                             
      DO 680 K=1,NS                                                             
      STR=0.                                                                    
      DO 670 L=1,ND                                                             
      I=LM(L,N)                                                                 
      IF(I.LE.0) GO TO 670                                                      
      STR=STR+SL(K,L)*U(I)                                                      
  670 CONTINUE                                                                  
      P(K)=STR-AML(K)                                                           
  680 CONTINUE                                                                  
      GO TO (800,2,860,2,800),KODT                                              
    2 READ(ITAPE)P                                                              
  800 WRITE(IOUT,2070)N,INCID(1,N),(P(K),K=1,2)                                 
      GO TO (860,660,860,660,660),KODT                                          
 860  WRITE(ITAPE)P                                                             
  660 CONTINUE                                                                  
      RETURN                                                                    
 2060 FORMAT(   /10X,'C A L C U L O  D A S  T E N S O E S  P A R A  ',          
     1   'G R U P O  D E  E L E M E N T O S : ',I4,//                           
     2   27X,'ESFORCOS NAS EXTREMIDADES DOS ELEMENTOS DE MOLA',                 
     3   //5X,'ELEMENTO',8X,'NO',10X,'FORCA',11X,'MOMENTO',                     
     4   /6X,'NUMERO',7X,'NUMERO',8X,'AXIAL',12X,'TORSOR',//)                   
 2070 FORMAT(I10,3X,I10,2X,2(5X,G12.5))                                         
      END                                                                       
                                                                                
      SUBROUTINE VETOR(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,S)              
      IMPLICIT REAL *8 (A-H,O-Z)                                                
      DIMENSION S(78),ST1(3,3),ST2(3,3),ST3(3,3),ST4(3,3),ST5(3,3),             
     1                ST6(3,3),ST7(3,3),ST8(3,3),ST9(3,3),ST10(3,3)             
C                                                                               
C        MONTAGEM DAS MATRIZES DO ELEMENTO EM FORMA DE VETOR                    
C                                                                               
      KL=0                                                                      
      LIM=3                                                                     
      DO 90 I=1,LIM                                                             
      I1=I-1                                                                    
      DO 90 J=I,LIM                                                             
      KL=KL+1                                                                   
      KL1=I1*9 + KL                                                             
      KL2=I1*6 + KL + 33                                                        
      KL3=I1*3 + KL + 57                                                        
      KL4=       KL + 72                                                        
      S(KL1)=ST1(I,J)                                                           
      S(KL2)=ST5(I,J)                                                           
      S(KL3)=ST8(I,J)                                                           
      S(KL4)=ST10(I,J)                                                          
 90   CONTINUE                                                                  
      DO 110 I=1,LIM                                                            
      I1=I-1                                                                    
      KC=(7-I)*I/2                                                              
      DO 110 J=1,LIM                                                            
      KC1=I1*9 + KC + J                                                         
      KC2=I1*6 + KC + J + 33                                                    
      KC3=I1*3 + KC + J + 57                                                    
      KD1=KC1 + 3                                                               
      KD2=KC2 + 3                                                               
      KE1=KC1 + 6                                                               
      S(KC1)=ST2(I,J)                                                           
      S(KD1)=ST3(I,J)                                                           
      S(KE1)=ST4(I,J)                                                           
      S(KC2)=ST6(I,J)                                                           
      S(KD2)=ST7(I,J)                                                           
      S(KC3)=ST9(I,J)                                                           
 110  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE STS(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,S)                
      IMPLICIT REAL *8 (A-H,O-Z)                                                
      DIMENSION S(12,12),ST1(3,3),ST2(3,3),ST3(3,3),ST4(3,3),ST5(3,3),          
     1                   ST6(3,3),ST7(3,3),ST8(3,3),ST9(3,3),ST10(3,3)          
      DO 20 I=1,3                                                               
      I3=I+3                                                                    
      I6=I+6                                                                    
      I9=I+9                                                                    
      DO 20 J=1,3                                                               
      J3=J+3                                                                    
      J6=J+6                                                                    
      J9=J+9                                                                    
      ST1(I,J)=S(I,J )                                                          
      ST2(I,J)=S(I,J3)                                                          
      ST3(I,J)=S(I,J6)                                                          
      ST4(I,J)=S(I,J9)                                                          
      ST5(I,J)=S(I3,J3)                                                         
      ST6(I,J)=S(I3,J6)                                                         
      ST7(I,J)=S(I3,J9)                                                         
      ST8(I,J)=S(I6,J6)                                                         
      ST9(I,J)=S(I6,J9)                                                         
 20   ST10(I,J)=S(I9,J9)                                                        
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE DIAG(VM,NGLN,NNPE)                                             
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .        GERA MATRIZ DE MASSA DIAGONAL A PARTIR DA                  .         
C .        MATRIZ DE MASSA CONSISTENTE DO ELEMENTO                    .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL *8 (A-H,O-Z)                                                
      DIMENSION VM(1),SM(12),SDVM(12)                                           
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
C                                                                               
      IND(I,J,NF)=((2*(NF+1)-I)*(I-1))/2+1+J-I                                  
      NF=NGLN*NNPE                                                              
C                                                                               
      NT=NF*(NF+1)/2                                                            
      WRITE(IOUT,1000)(VM(I),I=1,NT)                                            
1000  FORMAT(' ',10G11.3)                                                       
C                                                                               
      DO 30 K=1,NGLN                                                            
      DO 20 I=K,NF,NGLN                                                         
      S=0.                                                                      
      DO 10 J=K,NF,NGLN                                                         
      IF(J-I) 11,12,12                                                          
11    IV=IND(J,I,NF)                                                            
      GO TO 13                                                                  
12    IV=IND(I,J,NF)                                                            
13    S=S+VM(IV)                                                                
10    CONTINUE                                                                  
20    SM(I)=S                                                                   
30    CONTINUE                                                                  
C                                                                               
      DO 80 K=1,NGLN                                                            
      S=0.                                                                      
      SCMD=0.                                                                   
      DO 90 I=K,NF,NGLN                                                         
      S=S+SM(I)                                                                 
 90   SCMD=SCMD+VM(IND(I,I,NF))                                                 
      SM(K)=S                                                                   
80    SDVM(K)=SCMD                                                              
C                                                                               
      DO 120 J=1,NGLN                                                           
      F=SM(J)/SDVM(J)                                                           
      DO 120 I=J,NF,NGLN                                                        
      IV=IND(I,I,NF)                                                            
120   VM(IV)=VM(IV)*F                                                           
C                                                                               
      NF1=NF-1                                                                  
      DO 140 I=1,NF1                                                            
      II=I+1                                                                    
      DO 140 J=II,NF                                                            
140   VM(IND(I,J,NF))=0.                                                        
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE FLEXI(DE,ESP,FRAC,FFP,FFO,RC,RM,PI,E,IGOMO,NFLAN)              
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
      IF( FRAC )20,10,30                                                        
 10   RE=0.5*DE                                                                 
      RI=RE-ESP                                                                 
      FRAC=4./3.*(RE**3-RI**3)/((RE*RE + RI*RI)*ESP)                            
      GO TO 30                                                                  
 20   FRAC=0.                                                                   
 30   CONTINUE                                                                  
      IF( FFP .GE. 1. .AND. FFO .GE. 1.) RETURN                                 
      H=ESP*RC/(RM*RM)                                                          
      AUX=1. + 6.*PI/E*(RM/ESP)**(7./3.)*(RC/RM)**(1./3.)                       
      IF( IGOMO )50,40,50                                                       
 40   C=1.65/H                                                                  
      GO TO 60                                                                  
 50   C=1.52/H**(5./6.)                                                         
 60   CONTINUE                                                                  
      IF( NFLAN-1 )90,70,80                                                     
 70   C=C*H**(1./6.)                                                            
      GO TO 90                                                                  
 80   C=C*H**(1./3.)                                                            
 90   F=C/AUX                                                                   
      IF( F .LT. 1. ) F=1.                                                      
C                                                                               
      IF(FFP .LT. 1.)FFP=F                                                      
      IF(FFO .LT. 1.)FFO=F                                                      
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE INVER(S,N)                                                     
C                                                                               
C        INVERSAO DE MATRIZES                                                   
C        USADA PELA SUBROTINA RIGTBC                                            
C                                                                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION S(N,N),G(12),H(12)                                              
      NN=N-1                                                                    
      S(1,1)=1./S(1,1)                                                          
      IF( NN .EQ. 0 ) RETURN                                                    
      DO 110 M=1,NN                                                             
      K=M+1                                                                     
      DO 60 I=1,M                                                               
      G(I)=0.                                                                   
      DO 60 J=1,M                                                               
 60   G(I)=G(I) + S(I,J)*S(J,K)                                                 
      D=0.                                                                      
      DO 70 I=1,M                                                               
 70   D=D + S(K,I)*G(I)                                                         
      E=S(K,K) - D                                                              
      S(K,K)=1./E                                                               
      DO 80 I=1,M                                                               
 80   S(I,K)= - G(I)*S(K,K)                                                     
      DO 90 J=1,M                                                               
      H(J)=0.                                                                   
      DO 90 I=1,M                                                               
 90   H(J)=H(J) + S(K,I)*S(I,J)                                                 
      DO 100 J=1,M                                                              
 100  S(K,J)= - H(J)*S(K,K)                                                     
      DO 110 I=1,M                                                              
      DO 110 J=1,M                                                              
 110  S(I,J)=S(I,J) - G(I)*S(K,J)                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RIGTBR(PMAT,PSEC,IMAT,ISEC,NTEE,NELTEE,TEQ,NERIG,LRIG,         
     1                  NELIB,KODLIB,LM,XYZ,IBRAN)                              
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL *8 LRIG,ML,IRT,L1,L2,IX                                              
      REAL    A,CONV,GRAVID                                                     
      REAL *8 M(78),MT1(3,3),MT2(3,3),MT3(3,3),MT4(3,3),MT5(3,3),               
     1              MT6(3,3),MT7(3,3),MT8(3,3),MT9(3,3),MT10(3,3)               
      INTEGER RIGROT                                                            
      COMMON /MATRIZ/ IMTV                                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT                         
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /EXTRA/ ITIP                                                       
      COMMON A(1)                                                               
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /NOVDIM/ N101,N102,N103,N104,N105,N106,N107,N108,N109,N110,        
     1                N111,N112,N113,N114,N115,N116,N117,N118,N119              
      DIMENSION IMAT(1),ISEC(1),PMAT(8,1),PSEC(7,1),NERIG(1),LRIG(2,1),         
     1          NTEE(1),NELTEE(3,1),NELIB(1),KODLIB(12,1),LM(12,1),             
     2          XYZ(6,1),PXYZ(3),FNE(12),TEQ(1),IBRAN(1)                        
      DIMENSION S(78),ST1(3,3),ST2(3,3),ST3(3,3),ST4(3,3),ST5(3,3),             
     1                ST6(3,3),ST7(3,3),ST8(3,3),ST9(3,3),ST10(3,3),            
     2          ROT(3,3),D(3),SR(12,12),                                        
     3       S2T(3,3),S3T(3,3),S4T(3,3),S6T(3,3),S7T(3,3),S9T(3,3)              
      EQUIVALENCE (NPAR(1),NPAR1) , (NPAR(2),NUME) , (NPAR(3),NUMMAT) ,         
     1       (NPAR(14),NELRIG) , (NPAR(15),NELLIB) , (NPAR(4),MASSA)            
      SQRT(X)=DSQRT(X)                                                          
      ND=12                                                                     
      XPI=3.1415926                                                             
      DO 200 N=1,NUME                                                           
      NMAT=IMAT(N)                                                              
      NSEC=ISEC(N)                                                              
           E=PMAT(1,NMAT)                                                       
          CP=PMAT(2,NMAT)                                                       
          DE=PSEC(1,NSEC)                                                       
         ESP=PSEC(2,NSEC)                                                       
          WL=PSEC(3,NSEC)                                                       
      ESPREV=PSEC(4,NSEC)                                                       
        WREV=PSEC(5,NSEC)                                                       
      IDEF=0                                                                    
      FFY=1.                                                                    
      FFZ=1.                                                                    
      IF( IDEPUR(2) .EQ. 1 ) WRITE(IOUT,11) N,(XYZ(K,N),K=1,6)                  
 11   FORMAT(5X,I5,5X,6G12.5)                                                   
C                                                                               
C        VERIFICACAO SE O ELEMENTO FAZ PARTE DE UMA INTERSECAO T                
C                                                                               
      NT=NTEE(N)                                                                
      IF( NT .LE. 0 ) GO TO 5                                                   
      IDEF=1                                                                    
      NEL3=NELTEE(3,NT)                                                         
C                                                                               
C        VERIFICACAO SE O ELEMENTO E 'BRANCH'                                   
C                                                                               
      IF( N .EQ. NEL3 ) GO TO 3                                                 
      DO 2 K=1,3                                                                
 2    PXYZ(K)=XYZ(K+3,NEL3)-XYZ(K,NEL3)                                         
      GO TO 5                                                                   
 3    NEL1=NELTEE(1,NT)                                                         
      DO 4 K=1,3                                                                
 4    PXYZ(K)=XYZ(K,NEL1)-XYZ(K+3,NEL1)                                         
      IF( IBRAN(NT) .EQ. 0 ) GO TO 5                                            
      NSECR=ISEC(NEL1)                                                          
      DER=PSEC(1,NSECR)                                                         
      ESPR=PSEC(2,NSECR)                                                        
      IF( TEQ(NT) .LE. 0. ) TEQ(NT)=ESPR                                        
      FFZ=0.09*(DER/TEQ(NT))**1.5*(ESP/ESPR)*(DE/DER)                           
      FFY=3.*FFZ                                                                
 5    XL2=0.                                                                    
      DO 10 L=1,3                                                               
      D(L)=XYZ(L+3,N)-XYZ(L,N)                                                  
 10   XL2=XL2 + D(L)*D(L)                                                       
      XL=SQRT(XL2)                                                              
      CX=D(1)/XL                                                                
      CY=D(2)/XL                                                                
      CZ=D(3)/XL                                                                
      SAL=0.                                                                    
      CAL=1.                                                                    
C                                                                               
C        MONTAGEM DA MATRIZ DE ROTACAO DO ELEMENTO                              
C                                                                               
      CALL ROTA(CX,CY,CZ,SAL,CAL,PXYZ(1),PXYZ(2),PXYZ(3),ROT,IDEF)              
      IF( IDEPUR(2) .EQ. 1 ) WRITE(IOUT,6800)((ROT(I,J),J=1,3),I=1,3)           
 6800 FORMAT(/5X,3G15.5)                                                        
      DO 13 I=1,12                                                              
      DO 13 J=I,12                                                              
 13   SR(I,J)=0.                                                                
      DO 14 I=1,3                                                               
      DO 14 J=1,3                                                               
      MT1(I,J)=0.                                                               
      MT2(I,J)=0.                                                               
      MT3(I,J)=0.                                                               
      MT4(I,J)=0.                                                               
      MT5(I,J)=0.                                                               
      MT6(I,J)=0.                                                               
      MT7(I,J)=0.                                                               
      MT8(I,J)=0.                                                               
      MT9(I,J)=0.                                                               
 14   MT10(I,J)=0.                                                              
      AX=0.25*XPI*(DE**2 - (DE-2.*ESP)**2)                                      
      IX=0.5*XPI*((0.5*DE)**4 - (0.5*DE-ESP)**4)                                
      EIX=E*IX                                                                  
      EIY=0.5*EIX/FFY                                                           
      EIZ=0.5*EIX/FFZ                                                           
      EAX=E*AX                                                                  
      GIX=E/(2. + 2.*CP)*IX                                                     
C     WRITE(IOUT,7001) GIX                                                      
      L1=0.                                                                     
      L2=0.                                                                     
C                                                                               
C        VERIFICACAO DE EXISTENCIA DE EXTREMIDADE RIGIDA                        
C                                                                               
      IF( NELRIG .LT. 1 ) GO TO 17                                              
      DO 15 I=1,NELRIG                                                          
      IF( NERIG(I) .EQ. N ) GO TO 16                                            
 15   CONTINUE                                                                  
      GO TO 17                                                                  
 16   L1=LRIG(1,I)                                                              
      L2=LRIG(2,I)                                                              
 17   CONTINUE                                                                  
      H=XL-L1-L2                                                                
      H2=H*H                                                                    
      H3=H2*H                                                                   
      IF( MASSA .NE. 0 ) GO TO 30                                               
C     WRITE(IOUT,7001) GRAVID,L1,L2,H,AX                                        
      ML=(WL + WREV*XPI*ESPREV*(DE + ESPREV))/GRAVID                            
C     WRITE(IOUT,7001) GRAVID                                                   
      IRT=0.                                                                    
C                                                                               
C        TESTE PARA CONSIDERACAO DE INERCIA DE ROTACAO                          
C                                                                               
      IF( NPAR(5) .NE. 0 ) IRT=1.                                               
      IRT=IRT*ML*IX*0.5/AX                                                      
      TT=ML*IX/AX                                                               
      MT1(1,1)=ML*(L1 + H/3.)                                                   
      MT1(2,2)=ML*(13./35.*H + L1) + IRT*1.2/H                                  
      MT1(3,3)=MT1(2,2)                                                         
      MT2(2,3)=ML*(L1*(0.5*L1 + 13./35.*H) + 11./210.*H2) +                     
     1         IRT*(L1*1.2/H + 0.1)                                             
      MT2(3,2)=-MT2(2,3)                                                        
      MT3(1,1)=ML*H/6.                                                          
      MT3(2,2)=ML*H*9./70. - IRT*1.2/H                                          
      MT3(3,3)=MT3(2,2)                                                         
      MT4(2,3)=-ML*(13./420.*H2 + 9./70.*L2*H) + IRT*(L2*1.2/H+0.1)             
      MT4(3,2)=-MT4(2,3)                                                        
      MT5(1,1)=TT*(L1 + H/3.)                                                   
      MT5(2,2)=ML*(L1*(L1*L1/3. + L1*H*13./35. + H2*11./105.) + H3/105.)        
     1         + IRT*(L1*(1.2+L1*1.2/H)+2./15.*H)                               
      MT5(3,3)=MT5(2,2)                                                         
      MT6(2,3)=-ML*(L1*H*9./70. + H2*13./420.) + IRT*(L1*1.2/H + 0.1)           
      MT6(3,2)=-MT6(2,3)                                                        
      MT7(1,1)=TT*H/6.                                                          
      MT7(2,2)=-ML*(L1*H2*13./420. + L1*L2*H*9./70. + H3/140. +                 
     1              L2*H2*13./420.) + IRT*(0.1*L1 + L1*L2*1.2/H -               
     2                                     H/30. + 0.1*L2)                      
      MT7(3,3)=MT7(2,2)                                                         
      MT8(1,1)=ML*(L2 + H/3.)                                                   
      MT8(2,2)=ML*(13./35.*H + L2) + IRT*1.2/H                                  
      MT8(3,3)=MT8(2,2)                                                         
      MT9(2,3)=-ML*(H2*11./210. + L2*H*13./35. + 0.5*L2*L2)                     
     1         -IRT*(L2*1.2/H + 0.1)                                            
      MT9(3,2)=-MT9(2,3)                                                        
      MT10(1,1)=TT*(L2 + H/3.)                                                  
      MT10(2,2)=ML*(H3/105. + L2*H2*11./105. + L2*L2*13./35.*H +                
     1              L2**3/3.) + IRT*(L2 + 2.*H/15. + 0.2*L2 +                   
     2                               L2*L2*1.2/H)                               
      MT10(3,3)=MT10(2,2)                                                       
 30   CONTINUE                                                                  
      SR(1,1)=EAX/H                                                             
      SR(2,2)=EIZ*12./H3                                                        
      SR(3,3)=EIY*12./H3                                                        
      SR(2,6)= EIZ*(6./H2 + L1*12./H3)                                          
      SR(3,5)=-EIY*(6./H2 + L1*12./H3)                                          
      SR(1,7)=-SR(1,1)                                                          
      SR(2,8)=-SR(2,2)                                                          
      SR(3,9)=-SR(3,3)                                                          
      SR(2,12)= EIZ*(6./H2 + L2*12./H3)                                         
      SR(3,11)=-EIY*(6./H2 + L2*12./H3)                                         
      SR(4,4)=GIX/H                                                             
      SR(5,5)=EIY*(L1*L1*12./H3 + 12.*L1/H2 + 4./H)                             
      SR(6,6)=EIZ*(L1*L1*12./H3 + 12.*L1/H2 + 4./H)                             
      SR(5,9)=-SR(3,5)                                                          
      SR(6,8)=-SR(2,6)                                                          
      SR(4,10)=-SR(4,4)                                                         
      SR(5,11)=EIY*(L1*6./H2 + L1*L2*12./H3 + 2./H + L2*6./H2)                  
      SR(6,12)=EIZ*(L1*6./H2 + L1*L2*12./H3 + 2./H + L2*6./H2)                  
      SR(7,7)=SR(1,1)                                                           
      SR(8,8)=SR(2,2)                                                           
      SR(9,9)=SR(3,3)                                                           
      SR(8,12)=-SR(2,12)                                                        
      SR(9,11)=-SR(3,11)                                                        
      SR(10,10)=SR(4,4)                                                         
      SR(11,11)=EIY*(4./H + 12.*L2/H2 + L2*L2*12./H3)                           
      SR(12,12)=EIZ*(4./H + 12.*L2/H2 + L2*L2*12./H3)                           
      DO 32 I=1,12                                                              
      DO 32 J=I,12                                                              
 32   SR(J,I)=SR(I,J)                                                           
      WRITE(RIGROT)SR,ROT,XL                                                    
      IF( NELLIB .LT. 1 ) GO TO 50                                              
C                                                                               
C        INTRODUCAO DE LIBERACOES                                               
C                                                                               
      DO 40 I=1,NELLIB                                                          
      IF( N .NE. NELIB(I) ) GO TO 40                                            
      CALL LIBER(I,SR,FNE,KODLIB,1)                                             
      GO TO 50                                                                  
 40   CONTINUE                                                                  
 50   CONTINUE                                                                  
      CALL STS(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,SR)                     
      CALL MULTI(2,S2T,ROT,ST2)                                                 
      CALL MULTI(2,S3T,ROT,ST3)                                                 
      CALL MULTI(2,S4T,ROT,ST4)                                                 
      CALL MULTI(2,S6T,ROT,ST6)                                                 
      CALL MULTI(2,S7T,ROT,ST7)                                                 
      CALL MULTI(2,S9T,ROT,ST9)                                                 
      CALL MULTI(1,ST1,ROT,ST1)                                                 
      CALL MULTI(1,ST2,ROT,ST2)                                                 
      CALL MULTI(1,ST3,ROT,ST3)                                                 
      CALL MULTI(1,ST4,ROT,ST4)                                                 
      CALL MULTI(1,ST5,ROT,ST5)                                                 
      CALL MULTI(1,ST6,ROT,ST6)                                                 
      CALL MULTI(1,ST7,ROT,ST7)                                                 
      CALL MULTI(1,ST8,ROT,ST8)                                                 
      CALL MULTI(1,ST9,ROT,ST9)                                                 
      CALL MULTI(1,ST10,ROT,ST10)                                               
C                                                                               
C                                                                               
      IF( MASSA .NE. 0 ) GO TO 56                                               
      CALL MULTI(1,MT1,ROT,MT1)                                                 
      CALL MULTI(1,MT2,ROT,MT2)                                                 
      CALL MULTI(1,MT3,ROT,MT3)                                                 
      CALL MULTI(1,MT4,ROT,MT4)                                                 
      CALL MULTI(1,MT5,ROT,MT5)                                                 
      CALL MULTI(1,MT6,ROT,MT6)                                                 
      CALL MULTI(1,MT7,ROT,MT7)                                                 
      CALL MULTI(1,MT8,ROT,MT8)                                                 
      CALL MULTI(1,MT9,ROT,MT9)                                                 
      CALL MULTI(1,MT10,ROT,MT10)                                               
 56   CONTINUE                                                                  
      IF( IDEPUR(2) .EQ. 1 ) WRITE(IOUT,7001) ROT,ST1,ST2,ST3,ST4,              
     1                             ST5,ST6,ST7,ST8,ST9,ST10                     
C                                                                               
C        GRAVA A MATRIZ EM DISCO                                                
C                                                                               
      IF( NPAR(10) .LT. 0 ) GO TO 60                                            
      WRITE(IMTV)((ST1(I,J),I=1,3),(S2T(I,J),I=1,3),                            
     1            (S3T(I,J),I=1,3),(S4T(I,J),I=1,3),J=1,3) ,                    
     2           ((ST2(I,J),I=1,3),(ST5(I,J),I=1,3),                            
     3            (S6T(I,J),I=1,3),(S7T(I,J),I=1,3),J=1,3) ,                    
     4           ((ST3(I,J),I=1,3),(ST6(I,J),I=1,3),                            
     5            (ST8(I,J),I=1,3),(S9T(I,J),I=1,3),J=1,3) ,                    
     6           ((ST4(I,J),I=1,3),(ST7(I,J),I=1,3),                            
     7            (ST9(I,J),I=1,3),(ST10(I,J),I=1,3),J=1,3)                     
 60   CONTINUE                                                                  
      CALL MULTI(2,ST1,ST1,ROT)                                                 
      CALL MULTI(2,ST2,ST2,ROT)                                                 
      CALL MULTI(2,ST3,ST3,ROT)                                                 
      CALL MULTI(2,ST4,ST4,ROT)                                                 
      CALL MULTI(2,ST5,ST5,ROT)                                                 
      CALL MULTI(2,ST6,ST6,ROT)                                                 
      CALL MULTI(2,ST7,ST7,ROT)                                                 
      CALL MULTI(2,ST8,ST8,ROT)                                                 
      CALL MULTI(2,ST9,ST9,ROT)                                                 
      CALL MULTI(2,ST10,ST10,ROT)                                               
C                                                                               
C                                                                               
      IF( MASSA .NE. 0 ) GO TO 70                                               
      CALL MULTI(2,MT1,MT1,ROT)                                                 
      CALL MULTI(2,MT2,MT2,ROT)                                                 
      CALL MULTI(2,MT3,MT3,ROT)                                                 
      CALL MULTI(2,MT4,MT4,ROT)                                                 
      CALL MULTI(2,MT5,MT5,ROT)                                                 
      CALL MULTI(2,MT6,MT6,ROT)                                                 
      CALL MULTI(2,MT7,MT7,ROT)                                                 
      CALL MULTI(2,MT8,MT8,ROT)                                                 
      CALL MULTI(2,MT9,MT9,ROT)                                                 
      CALL MULTI(2,MT10,MT10,ROT)                                               
 70   CONTINUE                                                                  
      IF(IDEPUR(2) .EQ. 1 ) WRITE(IOUT,7001)ROT,ST1,ST2,ST3,ST4,ST5,            
     1                                          ST6,ST7,ST8,ST9,ST10            
 7001 FORMAT(9G13.5,/)                                                          
C                                                                               
C        MONTAGEM DAS MATRIZES DO ELEMENTO EM FORMA DE VETOR                    
C                                                                               
      CALL VETOR(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,S)                    
      IF( MASSA .EQ. 0 ) CALL VETOR(MT1,MT2,MT3,MT4,MT5,                        
     1                              MT6,MT7,MT8,MT9,MT10,M)                     
      IF( INTSAI(2) .EQ. 0 ) GO TO 120                                          
      WRITE(IOUT,6900)                                                          
 6900 FORMAT(//120('*')//10X,'MATRIZ DE RIGIDEZ DO ELEMENTO'//,120('*'))        
      WRITE(IOUT,7000)S                                                         
 7000 FORMAT(/,1X,12G11.4,/,1X,11G11.4,/,1X,10G11.4,/,1X,9G11.4,/,              
     1         1X, 8G11.4,/,1X, 7G11.4,/,1X, 6G11.4,/,1X,5G11.4,/,              
     2         1X, 4G11.4,/,1X, 3G11.4,/,1X, 2G11.4,/,1X, G11.4)                
      IF( MASSA .NE. 0 ) GO TO 120                                              
      WRITE(IOUT,7900)                                                          
 7900 FORMAT(/, /,10X,'MATRIZ DE MASSA DO ELEMENTO',//,120('*')//)              
      WRITE(IOUT,7000)M                                                         
 120  CONTINUE                                                                  
      CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND,A(N10),M,N)                          
 200  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
      SUBROUTINE TENTBR(U,INCID,LM)                                             
      IMPLICIT REAL * 8(A-H,O-Z)                                                
      REAL HED                                                                  
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
     *              ,KSTIFF,ESFMOD,MODOS,INFLU,IWORK                            
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /KABEC / HED(20),NPAG,LINHA,LPPAG                                  
      DIMENSION LM(12,1),INCID(2,1),U(1),SL(12,12),P(19),AML(12),               
     *          PV(15)                                                          
      EQUIVALENCE (NPAR(2),NUME)                                                
      DATA NS/19/                                                               
      IPRINT=0                                                                  
      IF( KODT .EQ. 3 ) GO TO 60                                                
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
 60   GO TO (61,61,63,64,65),KODT                                               
 61   ITAPE=FORCAS                                                              
      GO TO 20                                                                  
 64   ITAPE=IWORK                                                               
      GO TO 20                                                                  
 63   ITAPE=ESFMOD                                                              
 65   DO 10 I=1,12                                                              
 10   AML(I)=0.                                                                 
 20   IF( KODT.EQ.1.OR.KODT.EQ.3) WRITE(ITAPE) NS,NUME,NPAR(10)                 
      DO 690 N=1,NUME                                                           
      IF( KODT .EQ. 3 ) GO TO 3                                                 
      IPRINT=IPRINT+2                                                           
      IF(IPRINT.LE.50)GO TO 80                                                  
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
      IPRINT=0                                                                  
 80   GO TO (1,2,3,2,3),KODT                                                    
    1 READ(FNODEQ)AML,P(13),(P(J),J=14,19)                                      
    3 READ(IMTV)SL                                                              
      DO 680 K=1,12                                                             
      STR=0.                                                                    
      DO 670 L=1,6                                                              
      I=LM(L,N)                                                                 
      IF(I.LE.0) GO TO 607                                                      
      STR=STR+SL(K,L)*U(I)                                                      
  607 CONTINUE                                                                  
      J=LM(L+6,N)                                                               
      IF(J.LE.0) GO TO 670                                                      
      STR=STR+SL(K,L+6)*U(J)                                                    
  670 CONTINUE                                                                  
      P(K)=STR+AML(K)                                                           
  680 CONTINUE                                                                  
      IF(KODT.EQ.1) GO TO 93                                                    
      DO 92 JY=13,19                                                            
   92 P(JY)=0.                                                                  
   93 CONTINUE                                                                  
      GO TO (800,2,661,2,800),KODT                                              
    2 READ(ITAPE)P                                                              
C     IF(NPAR(10).EQ.2) READ(FNODEQ)PV                                          
  800 DO 660 J=1,2                                                              
      L=(J-1)*6+1                                                               
      L5=L+5                                                                    
      WRITE(IOUT,2070)N,INCID(J,N),(P(K),K=L,L5),P(13)                          
C     LA=(J-1)*7+1                                                              
C     L5A=LA+6                                                                  
C     IF(NPAR(10).EQ.2.AND.KODT.EQ.2) WRITE(IOUT,5000)N,INCID(J,N),             
C    *(PV(KA),KA=LA,L5A),PV(15)                                                 
  660 CONTINUE                                                                  
      GO TO (661,690,661,690,690),KODT                                          
 661  WRITE(ITAPE)P                                                             
  690 CONTINUE                                                                  
 2060 FORMAT(   /10X,'C A L C U L O  D A S  T E N S O E S  P A R A  G R         
     1U P O  D E  E L E M E N T O S  N0 : ',I4//,10X,'ESFORCOS NAS EXTRE        
     2MIDADES DOS ELEMENTOS * TBR *',//2X,'ELEMENTO',4X,'NO',7X,'F O R C        
     3 A S  N A S  D I R E C O E S',16X,'M O M E N T O S',19X,'HOOP',/,         
     4 3X,'NUMERO',3X,'NUMERO',                                                 
     5 8X,'X',13X,'Y',13X,'Z',12X,'MX',12X,'MY',12X,'MZ',10X,'STRESS')          
 2070 FORMAT(I6,I10,4X,7(G12.5,2X))                                             
C5000 FORMAT(2I5,5X,8(G12.5,2X))                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RIGTBC(PMAT,PSEC,IMAT,ISEC,NELIB,KODLIB,RAIO,PXYZ,LM,          
     1                  XYZ,TABFAT,NFLANG,IFAT)                                 
      IMPLICIT REAL *8 (A-H,O-Z)                                                
      REAL *8 M(78),MT1(3,3),MT2(3,3),MT3(3,3),MT4(3,3),MT5(3,3),               
     1 MR(12,12),   MT6(3,3),MT7(3,3),MT8(3,3),MT9(3,3),MT10(3,3)               
      REAL *8 IP                                                                
      REAL A,CONV,GRAVID                                                        
      INTEGER RIGROT                                                            
      DIMENSION PMAT(8,1),PSEC(7,1),IMAT(1),ISEC(1),NELIB(1),                   
     1          KODLIB(12,1),XYZ(6,1),PXYZ(3,1),RAIO(1),FNE(12),                
     2          TABFAT(5,1),NFLANG(1),IFAT(1),LM(12,1),D(3)                     
      DIMENSION S(78),ST1(3,3),ST2(3,3),ST3(3,3),ST4(3,3),ST5(3,3),             
     1                ST6(3,3),ST7(3,3),ST8(3,3),ST9(3,3),ST10(3,3),            
     2          SR(12,12),FM(6,6),X(12,12),H(12,12),AUX(12,12),ROT(3,3),        
     3       S2T(3,3),S3T(3,3),S4T(3,3),S6T(3,3),S7T(3,3),S9T(3,3)              
      COMMON /MATRIZ/ IMTV                                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT                         
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /EXTRA/ ITIP                                                       
      COMMON A(1)                                                               
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /NOVDIM/ N101,N102,N103,N104,N105,N106,N107,N108,N109,N110,        
     1                N111,N112,N113,N114,N115,N116,N117,N118,N119              
      EQUIVALENCE (NPAR(1),NPAR1) , (NPAR(2),NUME) , (NPAR(3),NUMAT) ,          
     1         (NPAR(5),INROT) , (NPAR(15),NELLIB) , (NPAR(4),MASSA)            
      ATAN(XX)=DATAN(XX)                                                          
      SIN(XX)=DSIN(XX)                                                            
      COS(XX)=DCOS(XX)                                                            
      SQRT(XX)=DSQRT(XX)                                                          
      ND=12                                                                     
      XPI=3.1415926                                                             
      DO 200 N=1,NUME                                                           
      NMAT=IMAT(N)                                                              
      NSEC=ISEC(N)                                                              
      NFAT=IFAT(N)                                                              
      IDEF=1                                                                    
      XL2=0.                                                                    
      DO 4 L=1,3                                                                
      D(L)=XYZ(L+3,N)-XYZ(L,N)                                                  
 4    XL2=XL2 + D(L)*D(L)                                                       
      XL=SQRT(XL2)                                                              
      CX=D(1)/XL                                                                
      CY=D(2)/XL                                                                
      CZ=D(3)/XL                                                                
      SAL=0.                                                                    
      CAL=1.                                                                    
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C        MONTAGEM DA MATRIZ DE ROTACAO DO ELEMENTO                              
C                                                                               
      CALL ROTA(CX,CY,CZ,SAL,CAL,PXYZ(1,N),PXYZ(2,N),PXYZ(3,N),ROT,IDEF)        
      R=RAIO(N)                                                                 
      ESPAC=TABFAT(4,NFAT)                                                      
        ANG=TABFAT(5,NFAT)                                                      
         DE=PSEC(1,NSEC)                                                        
        ESP=PSEC(2,NSEC)                                                        
      RM=(DE-ESP)*0.5                                                           
      IGOMO=0                                                                   
      IF( ESPAC*ANG .NE. 0. ) IGOMO=1                                           
      CC=SQRT(R*R - 0.25*XL*XL)                                                 
      IF( CC .LT. 0.0001 ) GO TO 13                                             
      FI2=ATAN(0.5*XL/CC)                                                       
      GO TO 14                                                                  
 13   FI2=XPI*0.5                                                               
 14   CONTINUE                                                                  
      FI=2.*FI2                                                                 
      SF=SIN(FI)                                                                
      CF=COS(FI)                                                                
      SF2=SIN(FI2)                                                              
      CF2=COS(FI2)                                                              
      SS=FI+SF                                                                  
      DD=FI-SF                                                                  
      R2=R*R                                                                    
      R3=R2*R                                                                   
C     WRITE(IOUT,1591) R,FI                                                     
       E=PMAT(1,NMAT)                                                           
      CP=PMAT(2,NMAT)                                                           
      PI=PSEC(7,NSEC)                                                           
      FRAC=TABFAT(1,NFAT)                                                       
        FP=TABFAT(2,NFAT)                                                       
        FO=TABFAT(3,NFAT)                                                       
C                                                                               
C        DETERMINACAO DOS FATORES DE FLEXIBILIDADE                              
C                                                                               
      CALL FLEXI(DE,ESP,FRAC,FP,FO,R,RM,PI,E,IGOMO,NFLANG(NFAT))                
C     WRITE(IOUT,1591) FRAC,FP,FO                                               
 1591 FORMAT(/,6G12.5)                                                          
      ESPREV=PSEC(4,NSEC)                                                       
        WREV=PSEC(5,NSEC)                                                       
          WL=PSEC(3,NSEC)                                                       
      AX=0.25*XPI*(DE**2 - (DE-2.*ESP)**2)                                      
      IP=0.5*XPI*((0.5*DE)**4 - (0.5*DE-ESP)**4)                                
      EAX2=2.*E*AX                                                              
       EI2=   E*IP                                                              
      FGAX2=FRAC*(1. + CP)/(E*AX)                                               
      GIP2=E/(1. + CP)*IP                                                       
      DO 20 I=1,6                                                               
      DO 20 J=1,6                                                               
 20   FM(I,J)=0.                                                                
      FM(1,1)=R*SS/EAX2 + FGAX2*R*DD + FP*R3*(FI-3.*SF+2.*FI*CF2**2)/EI2        
      FM(1,2)=FP*R3*(4.*SF2**2 - FI*SF)/EI2                                     
      FM(1,6)=FP*R2*(4.*SF2 - 2.*FI*CF2)/EI2                                    
      FM(2,2)=R*DD/EAX2 + FGAX2*R*SS + FP*R3*(2.*FI*SF2**2+DD)/EI2              
      FM(2,6)=FP*R2*FI*SF2/(0.5*EI2)                                            
      FM(6,6)=FP*R*FI/(0.5*EI2)                                                 
      FM(3,3)=R3*((3.*FI+SF*(CF-4.))/GIP2 + FO*(FI-SF*CF)/EI2)                  
     1        + 2.*FGAX2*R*FI                                                   
      FM(3,4)=R2*((CF2*SS-4.*SF2)/GIP2 + FO*CF2*DD/EI2)                         
      FM(3,5)=R2*(-SF2*DD/GIP2 - FO*SF2*SS/EI2)                                 
      FM(4,4)=R*(SS/GIP2 + FO*DD/EI2)                                           
      FM(5,5)=R*(DD/GIP2 + FO*SS/EI2)                                           
      FM(2,1)=FM(1,2)                                                           
      FM(6,1)=FM(1,6)                                                           
      FM(6,2)=FM(2,6)                                                           
      FM(4,3)=FM(3,4)                                                           
      FM(5,3)=FM(3,5)                                                           
C     WRITE(IOUT,1591) FM                                                       
      CALL INVER(FM,6)                                                          
C     WRITE(IOUT,1591) FM                                                       
      DO 28 J=1,12                                                              
      DO 28 K=1,J                                                               
 28   SR(J,K)=0.                                                                
      DO 30 J=7,12                                                              
      JT=J-6                                                                    
      DO 30 K=7,J                                                               
      KT=K-6                                                                    
 30   SR(J,K)=FM(JT,KT)                                                         
      SR(1,1)=SR(7,7)                                                           
      SR(2,1)=SR(8,7)                                                           
      SR(2,2)=SR(8,8)                                                           
      SR(3,3)=SR(9,9)                                                           
      SR(4,3)=SR(10,9)                                                          
      SR(4,4)=SR(10,10)                                                         
      SR(5,3)=SR(11,9) - SR(9,9)*XL                                             
      SR(5,4)=SR(11,10) - SR(10,9)*XL                                           
      SR(5,5)=SR(11,11) - 2.*SR(11,9)*XL + SR(9,9)*XL*XL                        
      SR(6,1)=SR(8,7)*XL + SR(12,7)                                             
      SR(6,2)=SR(12,8) + SR(8,8)*XL                                             
      SR(6,6)=SR(12,12) + SR(8,8)*XL*XL + 2.*SR(12,8)*XL                        
      SR(7,1)=-SR(7,7)                                                          
      SR(7,2)=-SR(8,7)                                                          
      SR(7,6)=-(SR(8,7)*XL + SR(12,7))                                          
      SR(8,1)=-SR(8,7)                                                          
      SR(8,2)=-SR(8,8)                                                          
      SR(8,6)=-(SR(8,8)*XL + SR(12,8))                                          
      SR(9,3)=-SR(9,9)                                                          
      SR(9,4)=-SR(10,9)                                                         
      SR(9,5)=-SR(11,9) + SR(9,9)*XL                                            
      SR(10,3)=-SR(10,9)                                                        
      SR(10,4)=-SR(10,10)                                                       
      SR(10,5)=-SR(11,10) + SR(10,9)*XL                                         
      SR(11,3)=-SR(11,9)                                                        
      SR(11,4)=-SR(11,10)                                                       
      SR(11,5)=-SR(11,11) + SR(11,9)*XL                                         
      SR(12,1)=-SR(12,7)                                                        
      SR(12,2)=-SR(12,8)                                                        
      SR(12,6)=-(SR(12,8)*XL + SR(12,12))                                       
      DO 40 J=1,12                                                              
      DO 40 K=1,J                                                               
 40   SR(K,J)=SR(J,K)                                                           
      WRITE(RIGROT)SR,ROT,XL,FI2,FRAC,FP,FO                                     
      IF( MASSA .NE. 0 ) GO TO 90                                               
      DO 50 J=1,12                                                              
      DO 50 K=1,12                                                              
      X(J,K)=0.                                                                 
      H(J,K)=0.                                                                 
      MR(J,K)=0.                                                                
 50   AUX(J,K)=0.                                                               
      FI=FI2                                                                    
      SF=SF2                                                                    
      CF=CF2                                                                    
      F2I=2.*FI                                                                 
      S2F=SIN(F2I)                                                              
      C2F=COS(F2I)                                                              
      FP=1./FP                                                                  
      FG=1./FO                                                                  
      CSI=FP*0.5*IP/(AX*R2)                                                     
      C=(1. - CSI)/(1. + CSI)                                                   
      C1=C+1.                                                                   
      C2=0.5*GIP2/R2                                                            
      C3=FG*0.5*EI2/R2                                                          
      C4=2.*R*C2*C3/(C2+C3)                                                     
      C5=2.*C3/(C2+C3)                                                          
      C6=(C2-C3)/(C2+C3)                                                        
      C7=0.5*IP/AX                                                              
C                                                                               
C        CONSIDERACAO DE INERCIA DE ROTACAO NA FLEXAO                           
C                                                                               
      IF( INROT .EQ. 0 ) C7=0.                                                  
      C8=IP/AX                                                                  
      C9=R2 + C7 + C8                                                           
      C10=R2 - C7 + C8                                                          
      C11=-C10                                                                  
      C12=C7 + C5*C8                                                            
      C13=C7 - C5*C8                                                            
      C14=C7 + C5*C5*C8                                                         
      C15=C7 - C5*C5*C8                                                         
      C16=C1*C1*C7                                                              
      C17=C1*C7                                                                 
      X(3,3)= R                                                                 
      X(3,4)= R*CF                                                              
      X(3,5)=-R*SF                                                              
      X(3,9)=-R*FI*CF                                                           
      X(3,10)=-R*FI                                                             
      X(3,11)= R*FI*SF                                                          
      X(4,4)= 1.                                                                
      X(4,9)=-FI + C6*SF*CF                                                     
      X(4,10)= SF                                                               
      X(4,11)=-1. + C6*CF*CF                                                    
      X(5,5)=-1.                                                                
      X(5,9)=-1. + C6*SF*SF                                                     
      X(5,10)=-CF                                                               
      X(5,11)= FI + C6*SF*CF                                                    
      X(9,3)= X(3,3)                                                            
      X(9,4)= X(3,4)                                                            
      X(9,5)=-X(3,5)                                                            
      X(9,9)=-X(3,9)                                                            
      X(9,10)=-X(3,10)                                                          
      X(9,11)= X(3,11)                                                          
      X(10,4)= X(4,4)                                                           
      X(10,9)=-X(4,9)                                                           
      X(10,10)=-X(4,10)                                                         
      X(10,11)= X(4,11)                                                         
      X(11,5)= X(5,5)                                                           
      X(11,9)= X(5,9)                                                           
      X(11,10)= X(5,10)                                                         
      X(11,11)=-X(5,11)                                                         
      X(1,1)=-R*(C*SF*CF - FI)                                                  
      X(1,2)= R*C*CF*CF                                                         
      X(1,6)=-R                                                                 
      X(1,8)=-R*(FI*CF - SF)                                                    
      X(1,12)=-R*CF                                                             
      X(2,1)=-R*C*SF*SF                                                         
      X(2,2)= R*(C*CF*SF + FI)                                                  
      X(2,7)=-R                                                                 
      X(2,8)=-R*(FI*SF + CF)                                                    
      X(2,12)=-R*SF                                                             
      X(6,1)= C1*SF                                                             
      X(6,2)=-C1*CF                                                             
      X(6,8)= FI                                                                
      X(6,12)=1.                                                                
      X(7,1)=-X(1,1)                                                            
      X(7,2)= X(1,2)                                                            
      X(7,6)= X(1,6)                                                            
      X(7,8)=-X(1,8)                                                            
      X(7,12)=X(1,12)                                                           
      X(8,1)= X(2,1)                                                            
      X(8,2)=-X(2,2)                                                            
      X(8,7)= X(2,7)                                                            
      X(8,8)= X(2,8)                                                            
      X(8,12)=-X(2,12)                                                          
      X(12,1)=-X(6,1)                                                           
      X(12,2)= X(6,2)                                                           
      X(12,8)=-X(6,8)                                                           
      X(12,12)=X(6,12)                                                          
C     WRITE(IOUT,1591) X                                                        
      CALL INVER(X,12)                                                          
C     WRITE(IOUT,1591) X                                                        
      H(3,3)=2.*R2*FI                                                           
      H(4,3)=2.*R2*SF                                                           
      H(3,4)=H(4,3)                                                             
      H(4,4)=C9*FI + C10*SF*CF                                                  
      H(5,5)=C9*FI + C11*SF*CF                                                  
      H(9,5)=0.25*(C10*S2F + 2.*C11*FI*C2F + 4.*C12*FI + 4.*C13*SF*CF)          
      H(5,9)=H(9,5)                                                             
      H(9,9)=(4.*C9*FI**3 + 6.*C10*FI*FI*S2F + 12.*C14*FI +                     
     1                     12.*C15*SF*CF + 6.*(C10+2.*C13)*FI*C2F +             
     2        3.*(C11-2.*C13)*S2F)/12.                                          
      H(10,5)=2.*(R2*(SF-FI*CF) + C7*SF)                                        
      H(5,10)=H(10,5)                                                           
      H(10,9)=2.*R2*(FI*FI*SF + 2.*FI*CF - 2.*SF) + 2.*C7*CF*FI                 
      H(9,10)=H(10,9)                                                           
      H(10,10)=2.*(FI**3*R2/3. + C7*FI)                                         
      H(11,3)=2.*R2*(SF - FI*CF)                                                
      H(3,11)=H(11,3)                                                           
      H(11,4)=0.25*C10*S2F + 0.5*C11*FI*C2F - C12*FI + C13*SF*CF                
      H(4,11)=H(11,4)                                                           
      H(11,11)=(4.*C9*FI**3 - 6.*C10*FI*FI*S2F + 12.*C14*FI -                   
     1          12.*C15*SF*CF - 6.*(C10+2.*C13)*FI*C2F -                        
     2          3.*(C11-2.*C13)*S2F)/12.                                        
      H(1,1)=R2*(2.*FI**3/3. - C*(0.5*S2F-FI*C2F) + C*C*(FI-0.5*S2F)) +         
     1       C16*(FI-0.5*S2F)                                                   
      H(1,7)=R2*C*(FI - 0.5*S2F)                                                
      H(7,1)=H(1,7)                                                             
      H(1,8)=2.*R2*(2.*SF - FI*FI*SF - 2.*FI*CF + C1*(SF-FI*CF)) +              
     1       2.*C17*(SF-FI*CF)                                                  
      H(8,1)=H(1,8)                                                             
      H(2,2)=R2*(2.*FI**3/3. + C*(0.5*S2F-FI*C2F) + C*C*(FI+0.5*S2F)) +         
     1       C16*(FI+0.5*S2F)                                                   
      H(2,6)=-R2*C*(FI + 0.5*S2F)                                               
      H(6,2)=H(2,6)                                                             
      H(2,12)=2.*R2*(FI*CF-C1*SF) - 2.*C17*SF                                   
      H(12,2)=H(2,12)                                                           
      H(6,6)=2.*R2*FI                                                           
      H(6,12)=2.*R2*SF                                                          
      H(12,6)=H(6,12)                                                           
      H(7,7)=H(6,6)                                                             
      H(7,8)=2.*R2*(2.*SF - FI*CF)                                              
      H(8,7)=H(7,8)                                                             
      H(8,8)=R2*(2.*FI**3/3.+2.*FI) + 2.*C7/3.*FI**3                            
      H(12,12)=2.*FI*(R2 + C7)                                                  
C     WRITE(IOUT,1591) H                                                        
      DO 60 J=1,12                                                              
      DO 60 K=1,12                                                              
      DO 60 JK=1,12                                                             
 60   AUX(J,K)=AUX(J,K) + H(J,JK)*X(JK,K)                                       
      DO 70 J=1,12                                                              
      DO 70 K=1,12                                                              
      DO 70 JK=1,12                                                             
 70   MR(J,K)=MR(J,K) + X(JK,J)*AUX(JK,K)                                       
      RML=R*(WL + WREV*XPI*ESPREV*(DE+ESPREV))/GRAVID                           
      DO 80 J=1,12                                                              
      DO 80 K=1,12                                                              
 80   MR(J,K)=RML*MR(J,K)                                                       
C                                                                               
C        INTRODUCAO DE LIBERACOES                                               
C                                                                               
 90   CONTINUE                                                                  
      IF( NELLIB .LT. 1 ) GO TO 120                                             
      DO 100 I=1,NELLIB                                                         
      IF( N .NE. NELIB(I) ) GO TO 100                                           
      CALL LIBER(I,SR,FNE,KODLIB,1)                                             
      GO TO 120                                                                 
 100  CONTINUE                                                                  
 120  CONTINUE                                                                  
      CALL STS(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,SR)                     
      IF( MASSA .EQ. 0 ) CALL STS(MT1,MT2,MT3,MT4,MT5,                          
     1                            MT6,MT7,MT8,MT9,MT10,MR)                      
      CALL MULTI(2,S2T,ROT,ST2)                                                 
      CALL MULTI(2,S3T,ROT,ST3)                                                 
      CALL MULTI(2,S4T,ROT,ST4)                                                 
      CALL MULTI(2,S6T,ROT,ST6)                                                 
      CALL MULTI(2,S7T,ROT,ST7)                                                 
      CALL MULTI(2,S9T,ROT,ST9)                                                 
      CALL MULTI(1,ST1,ROT,ST1)                                                 
      CALL MULTI(1,ST2,ROT,ST2)                                                 
      CALL MULTI(1,ST3,ROT,ST3)                                                 
      CALL MULTI(1,ST4,ROT,ST4)                                                 
      CALL MULTI(1,ST5,ROT,ST5)                                                 
      CALL MULTI(1,ST6,ROT,ST6)                                                 
      CALL MULTI(1,ST7,ROT,ST7)                                                 
      CALL MULTI(1,ST8,ROT,ST8)                                                 
      CALL MULTI(1,ST9,ROT,ST9)                                                 
      CALL MULTI(1,ST10,ROT,ST10)                                               
      IF( MASSA .NE. 0 ) GO TO 130                                              
      CALL MULTI(1,MT1,ROT,MT1)                                                 
      CALL MULTI(1,MT2,ROT,MT2)                                                 
      CALL MULTI(1,MT3,ROT,MT3)                                                 
      CALL MULTI(1,MT4,ROT,MT4)                                                 
      CALL MULTI(1,MT5,ROT,MT5)                                                 
      CALL MULTI(1,MT6,ROT,MT6)                                                 
      CALL MULTI(1,MT7,ROT,MT7)                                                 
      CALL MULTI(1,MT8,ROT,MT8)                                                 
      CALL MULTI(1,MT9,ROT,MT9)                                                 
      CALL MULTI(1,MT10,ROT,MT10)                                               
 130  CONTINUE                                                                  
      IF( NPAR(10) .LT. 0 ) GO TO 140                                           
C                                                                               
C        GRAVA A MATRIZ SR EM DISCO                                             
C                                                                               
      WRITE(IMTV)((ST1(I,J),I=1,3),(S2T(I,J),I=1,3),                            
     1            (S3T(I,J),I=1,3),(S4T(I,J),I=1,3),J=1,3) ,                    
     2           ((ST2(I,J),I=1,3),(ST5(I,J),I=1,3),                            
     3            (S6T(I,J),I=1,3),(S7T(I,J),I=1,3),J=1,3) ,                    
     4           ((ST3(I,J),I=1,3),(ST6(I,J),I=1,3),                            
     5            (ST8(I,J),I=1,3),(S9T(I,J),I=1,3),J=1,3) ,                    
     6           ((ST4(I,J),I=1,3),(ST7(I,J),I=1,3),                            
     7            (ST9(I,J),I=1,3),(ST10(I,J),I=1,3),J=1,3)                     
 140  CONTINUE                                                                  
      CALL MULTI(2,ST1,ST1,ROT)                                                 
      CALL MULTI(2,ST2,ST2,ROT)                                                 
      CALL MULTI(2,ST3,ST3,ROT)                                                 
      CALL MULTI(2,ST4,ST4,ROT)                                                 
      CALL MULTI(2,ST5,ST5,ROT)                                                 
      CALL MULTI(2,ST6,ST6,ROT)                                                 
      CALL MULTI(2,ST7,ST7,ROT)                                                 
      CALL MULTI(2,ST8,ST8,ROT)                                                 
      CALL MULTI(2,ST9,ST9,ROT)                                                 
      CALL MULTI(2,ST10,ST10,ROT)                                               
      IF( MASSA .NE. 0 ) GO TO 150                                              
      CALL MULTI(2,MT1,MT1,ROT)                                                 
      CALL MULTI(2,MT2,MT2,ROT)                                                 
      CALL MULTI(2,MT3,MT3,ROT)                                                 
      CALL MULTI(2,MT4,MT4,ROT)                                                 
      CALL MULTI(2,MT5,MT5,ROT)                                                 
      CALL MULTI(2,MT6,MT6,ROT)                                                 
      CALL MULTI(2,MT7,MT7,ROT)                                                 
      CALL MULTI(2,MT8,MT8,ROT)                                                 
      CALL MULTI(2,MT9,MT9,ROT)                                                 
      CALL MULTI(2,MT10,MT10,ROT)                                               
 150  CONTINUE                                                                  
C                                                                               
C        MONTAGEM DAS MATRIZES DO ELEMENTO EM FORMA DE VETOR                    
C                                                                               
      CALL VETOR(ST1,ST2,ST3,ST4,ST5,ST6,ST7,ST8,ST9,ST10,S)                    
      IF( MASSA .EQ. 0 ) CALL VETOR(MT1,MT2,MT3,MT4,MT5,                        
     1                              MT6,MT7,MT8,MT9,MT10,M)                     
      IF( INTSAI(2) .EQ. 0 ) GO TO 198                                          
      WRITE(IOUT,6900)                                                          
 6900 FORMAT(//120('*')//10X,'MATRIZ DE RIGIDEZ DO ELEMENTO'//,120('*'))        
      WRITE(IOUT,7000)S                                                         
 7000 FORMAT(/,1X,12G11.4,/,1X,11G11.4,/,1X,10G11.4,/,1X,9G11.4,/,              
     1         1X, 8G11.4,/,1X, 7G11.4,/,1X, 6G11.4,/,1X,5G11.4,/,              
     2         1X, 4G11.4,/,1X, 3G11.4,/,1X, 2G11.4,/,1X, G11.4)                
      IF( MASSA .NE. 0 ) GO TO 198                                              
      WRITE(IOUT,7900)                                                          
 7900 FORMAT(/120('*')//,10X,'MATRIZ DE MASSA DO ELMENTO',//,120('*')//)        
      WRITE(IOUT,7000)M                                                         
 198  CONTINUE                                                                  
      CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND,A(N10),M,N)                          
 200  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
C                                                                               
C                                                                               
      SUBROUTINE TENTBC(U,INCID,LM)                                             
      IMPLICIT REAL * 8(A-H,O-Z)                                                
      REAL HED                                                                  
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
     *              ,KSTIFF,ESFMOD,MODOS,INFLU,IWORK                            
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON / KABEC / HED(20),NPAG,LINHA,LPPAG                                 
      DIMENSION LM(12,1),INCID(2,1),U(1),SL(12,12),P(19),AML(12),               
     *          PV(15)                                                          
      EQUIVALENCE (NPAR(2),NUME)                                                
      DATA NS/19/                                                               
      IPRINT=0                                                                  
      IF( KODT .EQ. 3 ) GO TO 60                                                
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
 60   GO TO (61,61,63,64,65),KODT                                               
 61   ITAPE=FORCAS                                                              
      GO TO 20                                                                  
 64   ITAPE=IWORK                                                               
      GO TO 20                                                                  
 63   ITAPE=ESFMOD                                                              
 65   DO 10 I=1,12                                                              
 10   AML(I)=0.                                                                 
 20   IF(KODT.EQ.1.OR.KODT.EQ.3) WRITE(ITAPE) NS,NUME,NPAR(10)                  
      DO 690 N=1,NUME                                                           
      IF( KODT .EQ. 3 ) GO TO 3                                                 
      IPRINT=IPRINT+2                                                           
      IF(IPRINT.LE.50)GO TO 80                                                  
      CALL KABECA                                                               
      WRITE(IOUT,2060)NG                                                        
 80   GO TO (1,2,3,2,3),KODT                                                    
    1 READ(FNODEQ)AML,P(13),(P(J),J=14,19)                                      
    3 READ(IMTV)SL                                                              
      DO 680 K=1,12                                                             
      STR=0.                                                                    
      DO 670 L=1,6                                                              
      I=LM(L,N)                                                                 
      IF(I.LE.0) GO TO 607                                                      
      STR=STR+SL(K,L)*U(I)                                                      
  607 CONTINUE                                                                  
      J=LM(L+6,N)                                                               
      IF(J.LE.0) GO TO 670                                                      
      STR=STR+SL(K,L+6)*U(J)                                                    
  670 CONTINUE                                                                  
      P(K)=STR+AML(K)                                                           
  680 CONTINUE                                                                  
      IF(KODT.EQ.1) GO TO 93                                                    
      DO 92 JY=13,19                                                            
   92 P(JY)=0.                                                                  
   93 CONTINUE                                                                  
      GO TO (800,2,661,2,800),KODT                                              
    2 READ(ITAPE)P                                                              
C     IF(NPAR(10).EQ.2) READ(FNODEQ)PV                                          
 800  DO 660 J=1,2                                                              
      L=(J-1)*6+1                                                               
      L5=L+5                                                                    
      WRITE(IOUT,2070)N,INCID(J,N),(P(K),K=L,L5),P(13)                          
      LA=(J-1)*7+1                                                              
C     L5A=LA+6                                                                  
C     IF(NPAR(10).EQ.2.AND.KODT.EQ.2) WRITE(IOUT,5000)N,INCID(J,N),             
C    *(PV(KA),KA=LA,L5A),PV(15)                                                 
  660 CONTINUE                                                                  
      GO TO (661,690,661,690,690),KODT                                          
 661  WRITE(ITAPE)P                                                             
  690 CONTINUE                                                                  
 2060 FORMAT(   /10X,'C A L C U L O  D A S  T E N S O E S  P A R A  G R         
     1U P O  D E  E L E M E N T O S  NO : ',I4//,10X,'ESFORCOS NAS EXTRE        
     2MIDADES DOS ELEMENTOS * TBC *',//2X,'ELEMENTO',4X,'NO',7X,'F O R C        
     3 A S  N A S  D I R E C O E S',16X,'M O M E N T O S',19X,'HOOP',/,         
     4 3X,'NUMERO',3X,'NUMERO',                                                 
     5 8X,'X',13X,'Y',13X,'Z',12X,'MX',12X,'MY',12X,'MZ',10X,'STRESS')          
 2070 FORMAT(I6,I10,4X,7(G12.5,2X))                                             
C5000 FORMAT(2I5,5X,8(G12.5,2X))                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE STIF6                                                          
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST                                  
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /DIM/ N1,N2,N3,N4,N5,N6                                            
      COMMON A(1)                                                               
      DIMENSION LABEL(5)                                                        
      EQUIVALENCE (NPAR(2),NUME)                                                
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A ST','IF6 ','    '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A ST'
      LABEL(4)='IF6 '
      LABEL(5)='    '
      NFIRST=N6                                                                 
      L=3                                                                       
      IF ( IND .EQ. 1 ) GO TO 10                                                
      NFIRST=N5                                                                 
      L=4                                                                       
 10   N101=NFIRST                                                               
      N102=N101+  NUME                                                          
      N103=N102+6*NUME                                                          
      N104=N103+21*NUME*ITWO                                                    
      N105=N104+21*NUME*ITWO                                                    
 5    NLAST=N105                                                                
      IF( NLAST .GT. MTOT ) CALL ERROR(NLAST-MTOT,L,LABEL,5)                    
      MIDEST=NLAST-NFIRST                                                       
      GO TO (300,600,900,1200),IND                                              
C                                                                               
C          LEITURA DAS PROPRIEDADES                                             
C                                                                               
 300  CALL PST6(A(N1),A(N101),A(N102),A(N103),A(N104))                          
      RETURN                                                                    
C                                                                               
C          ACUMULACAO DE MATRIZES                                               
C                                                                               
 600  CALL RIGST6(A(N102),A(N103),A(N104))                                      
      RETURN                                                                    
 900  RETURN                                                                    
 1200 RETURN                                                                    
      END                                                                       
      SUBROUTINE PST6(ID,INCID,LM,GS,GSM)                                       
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      COMMON /EL/ IND,NPAR(20)                                                  
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION LM(6,1),GS(1),GSM(1),ID(6,1),INCID(1,1)                         
      EQUIVALENCE (NPAR(2),NUME)                                                
      DATA ND/6/                                                                
      IF(INTSAI(1) .EQ. 1)WRITE(IOUT,2000) NPAR(1),NUME                         
      DO 800 N=1,NUME                                                           
C                                                                               
C        LEITURA DA MATRIZ DE RIGIDEZ                                           
C                                                                               
      II=(N-1)*21+1                                                             
      IF=II+20                                                                  
      READ(IIN,1010)NP,(GS(K),K=II,IF)                                          
 1010 FORMAT(I5/(7G11.4))                                                       
      IF(INTSAI(1) .EQ. 1)WRITE(IOUT,2010)NP,(GS(K),K=II,IF)                    
C                                                                               
C        INCIDENCIA DO ELEMENTO                                                 
C                                                                               
      INCID(1,N)=NP                                                             
      DO 400 L=1,ND                                                             
 400  LM(L,N)=ID(L,NP)                                                          
 800  CONTINUE                                                                  
      RETURN                                                                    
 2000 FORMAT(1H1,//,10X,'D E F I N I C A O   D O   E L E M E N T O',//          
     1 ,5X,'TIPO DO ELEMENTO . . . . . . . . . . ( NPAR(1) ) =',I1,             
     2     ' ( ELEMENTO STIF6 )',//                                             
     3 ,5X,'NUMERO DE ELEMENTOS  . . . . . . . . ( NPAR(2) ) =',I3,//)          
 2010 FORMAT(10X,'NUMERO DO NO =',I3,///,5X,'MATRIZ DE RIGIDEZ',///,            
     1 6G15.7//15X,5G15.7//30X,4G15.7//45X,3G15.7//60X,2G15.7//75X,             
     2 G15.7,//)                                                                
      END                                                                       
      SUBROUTINE RIGST6(LM,GS,GSM)                                              
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL A                                                                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10                               
      COMMON /EL/ IND,NPAR(20)                                                  
      COMMON A(1)                                                               
      DIMENSION GS(1),GSM(1),LM(6,1),S(21),SM(21)                               
      EQUIVALENCE (NPAR(2),NUME)                                                
      DATA ND/6/                                                                
      DO 500 N=1,NUME                                                           
      II=(N-1)*21                                                               
      DO 100 I=1,21                                                             
      J=II+I                                                                    
      S(I)=GS(J)                                                                
 100  SM(I)=GSM(J)                                                              
      CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND,A(N10),SM,N)                         
 500  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LIBER(I,SM,AML,KODLIB,KOD)                                     
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION SM(12,12),AML(12),KODLIB(12,1),SMT(12,12),AMLT(12)              
      GO TO (1,2),KOD                                                           
 1    DO 10 L=1,12                                                              
      IF( KODLIB(L,I) .EQ. 0. ) GO TO 10                                        
      DO 4 J=1,12                                                               
      DO 4 K=1,12                                                               
 4    SMT(J,K)=SM(J,K) - SM(J,L)*SM(L,K)/SM(L,L)                                
      DO 6 J=1,12                                                               
      DO 6 K=1,12                                                               
 6    SM(J,K)=SMT(J,K)                                                          
 10   CONTINUE                                                                  
      RETURN                                                                    
 2    DO 60 L=1,12                                                              
      IF( KODLIB(L,I) .EQ. 0 ) GO TO 60                                         
      DO 20 K=1,12                                                              
 20   AMLT(K)=AML(K) - AML(L)*SM(L,K)/SM(L,L)                                   
      DO 40 J=1,12                                                              
      DO 40 K=1,12                                                              
 40   SMT(J,K)=SM(J,K) - SM(J,L)*SM(L,K)/SM(L,L)                                
      DO 50 J=1,12                                                              
      AML(J)=AMLT(J)                                                            
      DO 50 K=1,12                                                              
 50   SM(J,K)=SMT(J,K)                                                          
 60   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MASCON(ID,MAXA,M,NOMASS)                                       
C                                                                               
C           LE E SOMA MASSAS CONCENTRADAS NA ESTRUTURA                          
C                                                                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL *8 M(1),MC(6)                                                        
      DIMENSION ID(6,1),MAXA(1)                                                 
      COMMON/SOL/ NUMNP,NEQ,IDUM(5),NWM                                         
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /VAR/NG,MODEX                                                      
      COMMON /DEPURA/IDEPUR(15),INTSAI(15)                                      
      COMMON /EXTRA/ITIP,IMASSA                                                 
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,1000)                                   
      DO 200 I=1,NOMASS                                                         
      READ(IIN,2000) NO,MC                                                      
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,1100) NO,MC                             
      IF( MODEX .EQ. 0 ) GO TO 200                                              
      DO 100 K=1,6                                                              
      J=ID(K,NO)                                                                
      IF( J .LE. 0 ) GO TO 100                                                  
      IF( IMASSA .NE. 0 ) J=MAXA(J)                                             
      M(J)=M(J)+MC(K)                                                           
 100  CONTINUE                                                                  
 200  CONTINUE                                                                  
      IF(INTSAI(3).EQ.0) GO TO 300                                              
      WRITE(IOUT,3000)                                                          
      IF(NWM.EQ.NEQ) GO TO 320                                                  
      DO 310 I=1,NEQ                                                            
      J=MAXA(I)                                                                 
      J1=MAXA(I+1)-1                                                            
!     Era assim:
!  310 WRITE(IOUT,3010)(I,(M(K),K=J,J1))                                         
  310 WRITE(IOUT,3010)I,(M(K),K=J,J1)
      GO TO 300                                                                 
  320 WRITE(IOUT,3020)(K,M(K),K=1,NWM)                                          
  300 CONTINUE                                                                  
 3000 FORMAT(//120('*'),//10X,'MATRIZ DE MASSA GLOBAL',                         
     1       //120('*'))                                                        
 3010 FORMAT(I5,/,(8G15.5))                                                     
 3020 FORMAT(I5,G20.7)                                                          
 1000 FORMAT(////,10X,'M A S S A S   C O N C E N T R A D A S ',///,             
     1  5X,'NO',7X,'MX',13X,'MY',13X,'MZ',12X,'MRX',12X,'MRY',12X,              
     2  'MRZ',//)                                                               
 2000 FORMAT(I5,6F10.0)                                                         
 1100 FORMAT(I7,6G15.7)                                                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE RSTDIN(ID,MAXA,S,M,NEQ,NOREDI)                                 
C                                                                               
C          INTRODUZ RESTRICOES(APOIOS FIXOS) NA ANALISE DINAMICA                
C                                                                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL *8 M(1)                                                              
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /VAR/NG,MODEX                                                      
      COMMON /DEPURA/IDEPUR(15),INTSAI(15)                                      
      DIMENSION ID(6,1),MAXA(1),S(1),KOD(6)                                     
C                                                                               
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,1000)                                   
      ITOT=0                                                                    
      DO 800 I=1,NOREDI                                                         
      READ(IIN,2000) NO,KOD                                                     
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,1100) NO,KOD                            
      IF( MODEX .EQ. 0 ) GO TO 800                                              
      DO 600 K=1,6                                                              
      IF( KOD(K) .LE. 0 ) GO TO 600                                             
      L=ID(K,NO)                                                                
      IF( L ) 600,600,100                                                       
 100  ITOT=ITOT+1                                                               
      II=MAXA(L)                                                                
      IF=MAXA(L+1)-1                                                            
      DO 200 J=II,IF                                                            
      S(J)=0.                                                                   
C200  M(J)=0.  <----------                                                      
 200  CONTINUE                                                                  
C     M(II)=1. <----------                                                      
      S(II)=1.0E+17                                                             
      L1=L+1                                                                    
      DO 400 J=L1,NEQ                                                           
      JL=MAXA(J)+J-L                                                            
      IF( JL .GE. MAXA(J+1) ) GO TO 400                                         
      S(JL)=0.                                                                  
C     M(JL)=0. <-----------                                                     
 400  CONTINUE                                                                  
 600  CONTINUE                                                                  
 800  CONTINUE                                                                  
      NOREDI=ITOT                                                               
 1000 FORMAT(////,10X,'R E S T R I C O E S   D I N A M I C A S ',///,           
     *       5X,'NO',5X,'CODIGO',//)                                            
 2000 FORMAT(I5,4X,6I1)                                                         
 1100 FORMAT(I7,4X,6I1)                                                         
      RETURN                                                                    
      END                                                                       
      SUBROUTINE PVIGA                                                          
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .      PARA ALOCAR MEMORIA E CHAMAR SUBROTINA                       .         
C .      DO ELEMENTO DE VIGA ESPACIAL                                 .         
C .            SE IND=1  GERA INFORMACOES                             .         
C .            SE IND=2  CALCULA E MONTA MATRIZ DE RIGIDEZ            .         
C .                      E GRAVA MATRIZ PARA CALCULO DAS TENSOES      .         
C .            SE IND=3  CALCULA ESFORCOS NOS EXTREMOS E TENSOES      .         
C .            SE IND=4  CALCULA CARGAS NODAIS EQUIVALENTES           .         
C .            SE IND=5 OU IND=6 PROBLEMA ELASTOPLASTICO              .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      INTEGER ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                          
      COMMON/ARQUIV/ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                    
      COMMON/PLAS/N27,N28,N29,N30,N31,N32                                       
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /CARGAS/ ICARGA,N302,N303,N304                                     
      COMMON /PROPRI/ N5A,N5B,N5C                                               
      COMMON A(1)                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      DIMENSION LABEL(5),ICARGA(10)                                             
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT),(NPAR(15),NELLIB),            
     1            (NPAR(11),NUMSEC)                                             
C                                                                               
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A ST','IGA ','    '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A ST'
      LABEL(4)='IGA '
      LABEL(5)='    '
      NFIRST=N6                                                                 
      IF(IND.GT.1)NFIRST=N5                                                     
      N101=NFIRST                                                               
      N102=N101+NUMMAT*ITWO                                                     
      N103=N102+NUMSEC*ITWO                                                     
      N204=N103+NUMMAT*ITWO                                                     
      N205=N204+NUMSEC*ITWO                                                     
      N206=N205+NUMSEC*ITWO                                                     
      N207=N206+NUMSEC*ITWO                                                     
      N208=N207+NUMSEC*ITWO                                                     
      N209=N208+NUMSEC*ITWO                                                     
      N210=N209+NUMSEC*ITWO                                                     
      N211=N210+NUMSEC*ITWO                                                     
      N212=N211+2*NUME                                                          
      N104=N212+12*NUME                                                         
      N105=N104+6*NUME*ITWO                                                     
      N106=N105+NUME                                                            
      N107=N106+NUMMAT*ITWO                                                     
      N108=N107+NUME                                                            
      N109=N108+   NELLIB                                                       
      N110=N109+12*NELLIB                                                       
      N111=N110+NUME                                                            
      N1111=N111+NUMMAT*ITWO                                                    
   5  NLAST=N1111                                                               
C                                                                               
      IF(IND.GT.1)GO TO 100                                                     
      IF(NLAST.GT.MTOT)CALL ERROR(NLAST-MTOT,3,LABEL,5)                         
      GO TO 200                                                                 
  100 IF(NLAST.GT.MTOT)CALL ERROR(NLAST-MTOT,4,LABEL,5)                         
  200 MIDEST=NLAST-NFIRST                                                       
C                                                                               
C **********************************************************************        
      IF(IDEPUR(1) .EQ. 1)                                                      
     *WRITE(IOUT,7171)N101,N102,N103,N204,N205,N206,N207,N208,N209,             
     * N210,N211,N212,N104,N105                                                 
7171  FORMAT(10I10)                                                             
C **********************************************************************        
C                                                                               
      GO TO(300,600,900,1200,1500,1700),IND                                     
C                                                                               
C     LEITURA DAS PROPRIEDADES DOS ELEMENTOS                                    
C                                                                               
  300 CALL VIGA3(A(N1),A(N2),A(N3),A(N4),A(N4),A(N5),A(N101),A(N102),           
     1   A(N103),A(N204),A(N205),A(N206),A(N207),A(N208),A(N209),               
     2   A(N210),A(N106),A(N211),A(N212),A(N104),A(N105),A(N107),               
     3   A(N108),A(N109),A(N110),A(N5A),A(N5B),A(N111))                         
      RETURN                                                                    
  600 CONTINUE                                                                  
C                                                                               
C     CALCULO DAS MATRIZES DE RIGIDEZ E MASSA                                   
C                                                                               
      CALL RIGV3(A(N101),A(N102),A(N103),A(N204),A(N205),A(N206),               
     1   A(N207),A(N208),A(N209),A(N210),A(N212),A(N104),A(N105),               
     2   A(N108),A(N109),A(N110))                                               
      RETURN                                                                    
C                                                                               
C     CALCULO DAS TENSOES                                                       
C                                                                               
 900  IF ( NPAR(10) .LT. 0 ) RETURN                                             
      CALL TENV3(A(N4),A(N211),A(N212),A(N105))                                 
      RETURN                                                                    
C                                                                               
C     CALCULO DAS CARGAS NODAIS EQUIVALENTES                                    
C                                                                               
1200  CALL FNEV3(A(N4),A(N101),A(N102),A(N210),A(N106),A(N211),A(N105),         
     1   A(N107),A(N1),A(N108),A(N109),A(N303),A(N304),A(N110))                 
      RETURN                                                                    
C                                                                               
C     CALCULO DE FORCAS INTERNAS PARA O PROBLEMA PLASTICO                       
C                                                                               
 1500 ITAP4=ARQ5                                                                
      CALL PLASV3(A(N105),A(N110),A(N206),A(N102),A(N103),A(N101),              
     *            A(N211),A(N1),A(N29),A(N30),A(N32),ITAP4)                     
      RETURN                                                                    
C                                                                               
C     INICIALIZACAO DO ARQUIVO ITAP4 ONDE SERAO GRAVADAS TENSOES                
C     NO CASO ELASTOPLASTICO                                                    
C                                                                               
 1700 ITAP4=ARQ5                                                                
      CALL INICIV(A(N111),A(N105),A(N32),ITAP4)                                 
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE VIGA3(ID,X,Y,Z,U,MHT,E,AREA,CP,XI,YI,ZI,ALFA,                  
     1   KAPY,KAPZ,RO,COEXPT,INCID,LM,XYZ,MATP,IRUN,NELIB,KODLIB,               
     2   ISEC,TABMAT,TABSEC,SIGY)                                               
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .      SUBROTINA PARA ELEMENTO DE VIGA ESPACIAL                     .         
C .      LE E GERA INFORMACOES                                        .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL *8 KAPY,KAPZ                                                         
      REAL    A                                                                 
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLE ELIMINAR O CARTAO ANTERIOR             .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON A(1)                                                               
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/EXTRA/ITIP                                                         
C                                                                               
      DIMENSION X(1),Y(1),Z(1),ID(6,1),E(1),AREA(1),CP(1),XI(1),                
     1   YI(1),ZI(1),ALFA(1),KAPY(1),KAPZ(1),LM(12,1),XYZ(6,1),                 
     2   MATP(1),U(1),MHT(1),INCID(2,1),RO(1),COEXPT(1),IRUN(1),                
     3   NELIB(1),KODLIB(12,1),TABMAT(8,1),TABSEC(8,1),ISEC(1),        
     4   KAUX(1000),SIGY(1)                                                                
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT),              
     *            (NPAR(15),NELLIB),(NPAR(11),NUMSEC)                           
      SQRT(XX)=DSQRT(XX)                                                      
      ND=12                                                                     
      IF(NPAR(10).EQ.0) NEQT=NEQT+12*NUME                                       
C                                                                               
C                                                                               
C                                                                               
      IF(INTSAI(1) .EQ. 0)GO TO 5                                               
      WRITE(IOUT,2000)(NPAR(I),I=1,5)                                           
      WRITE(IOUT,2055)NPAR(10),NPAR(11)                                         
C                                                                               
   5  CONTINUE                                                                  
C                                                                               
C        LE INFORMACOES SOBRE O ELEMENTO                                        
C                                                                               
      IF(INTSAI(1) .EQ. 1)                                                      
     *WRITE(IOUT,2040)                                                          
      N=1                                                                       
  100 READ(IIN,1020)M,IRAMAL,II,JJ,MTYP,MTYS,KG                                 
      IF(KG.EQ.0)KG=1                                                           
  120 IF(M.NE.N)GO TO 200                                                       
      IR=IRAMAL                                                                 
      I=II                                                                      
      J=JJ                                                                      
      MTYPE=MTYP                                                                
      NSEC=MTYS                                                                 
      KKK=KG                                                                    
C                                                                               
C        GUARDA INFORMACOES SOBRE O ELEMENTO                                    
C                                                                               
  200 XYZ(1,N)=X(I)                                                             
      XYZ(2,N)=Y(I)                                                             
      XYZ(3,N)=Z(I)                                                             
      XYZ(4,N)=X(J)                                                             
      XYZ(5,N)=Y(J)                                                             
      XYZ(6,N)=Z(J)                                                             
      IRUN(N)=IR                                                                
      MATP(N)=MTYPE                                                             
      ISEC(N)=NSEC                                                              
      DO 390 L=1,12                                                             
  390 LM(L,N)=0                                                                 
      DO 400 L=1,6                                                              
      LM(L,N)=ID(L,I)                                                           
  400 LM(L+6,N)=ID(L,J)                                                         
C                                                                               
C        ATUALIZA AS ALTURAS DAS COLUNAS E A LARGURA DE BANDA                   
C                                                                               
      CALL COLHT(MHT,ND,LM(1,N))                                                
C                                                                               
      INCID(1,N)=I                                                              
      INCID(2,N)=J                                                              
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2050)N,I,J,MTYPE,NSEC                   
      IF( N.EQ.NUME ) GO TO 700                                                 
      N=N+1                                                                     
      I=I+KKK                                                                   
      J=J+KKK                                                                   
      IF(N.GT.M)GO TO 100                                                       
      GO TO 120                                                                 
C                                                                               
C     DEFINICAO DAS PROPRIEDADES DOS MATERIAIS                                  
C                                                                               
  700 NCONT=1                                                                   
      DO 750 N=1,NUME                                                           
      MTYPE=MATP(N)                                                             
      IF(N.EQ.1) GO TO 730                                                      
      DO 720 I=1,NCONT                                                          
      IF(MTYPE-KAUX(I)) 720,745,720                                             
  720 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  730 KAUX(NCONT)=MTYPE                                                         
      MATP(N)=NCONT                                                             
      E(NCONT)=TABMAT(1,MTYPE)                                                  
      CP(NCONT)=TABMAT(2,MTYPE)                                                 
      COEXPT(NCONT)=TABMAT(3,MTYPE)                                             
      SIGY(NCONT)=TABMAT(4,MTYPE)                                               
      GO TO 750                                                                 
  745 MATP(N)=I                                                                 
  750 CONTINUE                                                                  
C                                                                               
C     DEFINICAO DAS PROPRIEDADES DAS SECOES                                     
C                                                                               
      NCONT=1                                                                   
      DO 850 N=1,NUME                                                           
      NSEC=ISEC(N)                                                              
      IF(N.EQ.1) GO TO 830                                                      
      DO 820 I=1,NCONT                                                          
      IF(NSEC-KAUX(I)) 820,845,820                                              
  820 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  830 KAUX(NCONT)=NSEC                                                          
      ISEC(N)=NCONT                                                             
      AREA(NCONT)=TABSEC(1,NSEC)                                                
      XI(NCONT)=TABSEC(2,NSEC)                                                  
      YI(NCONT)=TABSEC(3,NSEC)                                                  
      ZI(NCONT)=TABSEC(4,NSEC)                                                  
      RO(NCONT)=TABSEC(5,NSEC)                                                  
      KAPY(NCONT)=TABSEC(6,NSEC)                                                
      KAPZ(NCONT)=TABSEC(7,NSEC)                                                
      ALFA(NCONT)=TABSEC(8,NSEC)                                                
      GO TO 850                                                                 
  845 ISEC(N)=I                                                                 
  850 CONTINUE                                                                  
C                                                                               
      IF( NELLIB .LT. 1 ) RETURN                                                
C                                                                               
C        LE LISTA DE ELEMENTOS COM LIBERACOES                                   
C                                                                               
      READ(IIN,1900)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                  
      IF(INTSAI(1) .NE. 1 ) RETURN                                              
      WRITE(IOUT,2150)                                                          
      WRITE(IOUT,2160)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                
      RETURN                                                                    
C                                                                               
C     FORMATOS                                                                  
C                                                                               
 1020 FORMAT (7I5)                                                              
 2000 FORMAT(//,10X,'D E F I N I C A O  D O  E L E M E N T O',//                
     1   5X,'TIPO DE ELEMENTO',13(2H .),17H( NPAR(1) ) . . =,I5,                
     2   ' ( ELEMENTO DE  VIGA )',//                                            
     3 5X,'NUMERO DE ELEMENTOS ',11(2H .),17H( NPAR(2) ) . . =,I5,//            
     4 5X,'NUMERO DE MATERIAIS ',11(2H .),17H( NPAR(3) ) . . =,I5,//            
     55X,'CALCULO DA MATRIZ DE MASSA',8(2H .),17H( NPAR(4) ) . . =, I5,         
     6' ( SE.NE.0 DISCRETA    )'/70X,'( SE.EQ.0 CONSISTENTE )',//               
     7   5X,'INERCIA DE ROTACAO',12(2H .),17H( NPAR(5) ) . . =,I5,              
     8' ( SE.NE.0 CONSIDERA     )',/70X,'( SE.EQ.0 NAO CONSIDERA ) ')           
 2055 FORMAT(//,5X,'CALCULO DE TENSOES',12(2H .),17H( NPAR(10)) . . =           
     1,I5,' ( SE.NE.0 NAO CALCULA )',/70X,'( SE.EQ.0 CALCULA    )',//           
     2 5X,'NUMERO DE SECOES .',12(2H .),17H( NPAR(11)) . . =,I5,//)             
 1900 FORMAT(I5,4X,6I1,4X,6I1)                                                  
 2040 FORMAT(1H1,10X,'I N F O R M A C O E S  D O  E L E M E N T O'///,          
     1' NUMERO-N',7X,'NO',7X,'NO',9X,'NUMERO'/,                                 
     2' ELEMENTO',6X,' I',8X,' J',5X,'MATERIAL'/)                               
 2050 FORMAT(I5,6X,I6,4X,I5,7X,I5,7X,I5)                                        
 2150 FORMAT(////,10X,'INDICADORES DE LIBERACOES NAS EXTREMIDADES',             
     1                ' DOS ELEMENTOS',///,10X,                                 
     2   '                         DIRECOES (SISTEMA LOCAL)',//,10X,            
     3   'NUMERO DO           * NO 1 *                * NO 2 *',/,              
     411X,'ELEMENTO       X  Y  Z  RX  RY  RZ     X  Y  Z  RX  RY  RZ',         
     5   /)                                                                     
 2160 FORMAT(/,10X,I5,8X,3I3,3I4,3X,3I3,3I4)                                    
      END                                                                       
                                                                                
      SUBROUTINE RIGV3(E,AREA,CP,XI,YI,ZI,ALFA,KAPY,KAPZ,                       
     1       RO,LM,XYZ,MATP,NELIB,KODLIB,ISEC)                                  
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .      SUBROTINA PARA FORMACAO DA MATRIZ DE RIGIDEZ                 .         
C .      DUM ELEMENTO DE VIGA ESPACIAL                                .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL *8 KAPY,KAPZ                                                         
      INTEGER RIGROT                                                            
      REAL    A                                                                 
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .                ESTE PROGRAMA E USADO EM DUPLA PRECISAO            .         
C .                PARA SIMPLES ELIMINAR O CARTAO ANTERIOR            .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON/NOVDIM/N101,N102,N103,N204,N205,N206,N207,                         
     1              N208,N209,N210,N211,N104,N105,N212                          
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                  
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT                         
      COMMON A(1)                                                               
      COMMON/MATRIZ/IMTV                                                        
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/EXTRA/ITIP                                                         
C                                                                               
      DIMENSION SR(12,12),FNE(12),RO(1),E(1),AREA(1),CP(1),XI(1),               
     1   YI(1),ZI(1),ALFA(1),KAPY(1),KAPZ(1),LM(12,1),XYZ(6,1),                 
     2   MATP(1),DUMMY(3,3),NELIB(1),KODLIB(12,1),ISEC(1)                       
      DIMENSION S(78 ),ST1(3,3),ST2(3,3),ST3(3,3),ST4(3,3),                     
     1   ST2T(3,3),SL(12,12),ROT(3,3),D(3)                                      
      DIMENSION AM(3,3),BM(3,3),CM(3,3),DM(3,3),EM(3,3),FM(3,3),EMT(3,3)        
     1      ,SM(78)                                                             
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)               
     1            ,(NPAR(4),MASSA),(NPAR(15),NELLIB)                            
      SQRT(X)=DSQRT(X)                                                          
      SIN(X)=DSIN(X)                                                            
      COS(X)=DCOS(X)                                                            
C                                                                               
C        MONTAGEM DA MATRIZ DE RIGIDEZ DO ELEMENTO                              
C                                                                               
      ND=12                                                                     
      DO 500 N=1,NUME                                                           
      MTYPE=MATP(N)                                                             
      NSEC=ISEC(N)                                                              
      XL2=0.                                                                    
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,11)N,(XYZ(K,N),K=1,6),ALFA(NSEC)                               
   11 FORMAT(5X,I5,5X,7G12.5)                                                   
      DO 505 L=1,3                                                              
      D(L)=XYZ(L+3,N)-XYZ(L,N)                                                  
  505 XL2=XL2+D(L)*D(L)                                                         
      XL=SQRT(XL2)                                                              
      CX=D(1)/XL                                                                
      CY=D(2)/XL                                                                
      CZ=D(3)/XL                                                                
      SAL=SIN(ALFA(NSEC))                                                       
      CAL=COS(ALFA(NSEC))                                                       
      IDEF=0                                                                    
C                                                                               
      CALL ROTA(CX,CY,CZ,SAL,CAL,XP,YP,ZP,ROT,IDEF)                             
C                                                                               
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,6800)((ROT(I,J),J=1,3),I=1,3)                                  
 6800 FORMAT(/5X,3G15.5)                                                        
      DO 510 KI=1,3                                                             
      DO 510 KJ=1,3                                                             
      ST1(KI,KJ)=0.                                                             
      ST2(KI,KJ)=0.                                                             
      ST3(KI,KJ)=0.                                                             
      ST4(KI,KJ)=0.                                                             
      ST2T(KI,KJ)=0.                                                            
      AM(KI,KJ)=0.                                                              
      BM(KI,KJ)=0.                                                              
      CM(KI,KJ)=0.                                                              
      DM(KI,KJ)=0.                                                              
      EM(KI,KJ)=0.                                                              
      FM(KI,KJ)=0.                                                              
      EMT(KI,KJ)=0.                                                             
  510 CONTINUE                                                                  
                                                                                
      XIA=XI(NSEC)/AREA(NSEC)                                                   
      YIA=YI(NSEC)/AREA(NSEC)                                                   
      ZIA=ZI(NSEC)/AREA(NSEC)                                                   
      RZ=SQRT(ZIA)                                                              
      RY=SQRT(YIA)                                                              
      IF( MASSA .NE. 0 ) GO TO 515                                              
      ROAL=AREA(NSEC)*XL*RO(NSEC)                                               
C                                                                               
C     TESTE PARA CONSIDERAR INERCIA DE ROTACAO                                  
C                                                                               
      IRT=0                                                                     
      IF(NPAR(5).NE.0) IRT=1                                                    
      CM1=1./3.*ROAL                                                            
      CM2=(13./35.+IRT*6.*ZIA/(5.*XL**2))*ROAL                                  
      CM3=(13./35.+IRT*6.*YIA/(5.*XL**2))*ROAL                                  
      CM4=XIA/3.*ROAL                                                           
      CM5=(11.*XL/210.+IRT*YIA/(10.*XL))*ROAL                                   
      CM6=(XL**2/105.+IRT*2.*YIA/15.)*ROAL                                      
      CM7=(11.*XL/210.+IRT*ZIA/(10.*XL))*ROAL                                   
      CM8=(XL**2/105.+IRT*2.*ZIA/15.)*ROAL                                      
      CM9=(-XL**2/140.-IRT*ZIA/30.)*ROAL                                        
      CM10=(9./70.-IRT*6.*ZIA/(5.*XL**2))*ROAL                                  
      CM11=(13.*XL/420.-IRT*ZIA/(10.*XL))*ROAL                                  
      CM12=(9./70.-IRT*6.*YIA/(5.*XL**2))*ROAL                                  
      CM13=(13.*XL/420.-IRT*YIA/(10.*XL))*ROAL                                  
      CM14=(-XL**2/140.-IRT*YIA/30.)*ROAL                                       
  515 CONTINUE                                                                  
      PHY=24.*(1+CP(MTYPE))*KAPY(NSEC)*(RZ/XL)**2                               
      PHZ=24.*(1+CP(MTYPE))*KAPZ(NSEC)*(RY/XL)**2                               
      G=E(MTYPE)/(2.+2.*CP(MTYPE))                                              
      DO 10 I=1,12                                                              
      DO 10 J=1,I                                                               
 10   SR(I,J)=0.                                                                
      SR(1,1)=E(MTYPE)*AREA(NSEC)/XL                                            
      SR(2,2)=12.*E(MTYPE)*ZI(NSEC)/(XL**3*(1+PHY))                             
      SR(3,3)=12.*E(MTYPE)*YI(NSEC)/(XL**3*(1+PHZ))                             
      SR(5,3)=-6.*E(MTYPE)*YI(NSEC)/(XL**2*(1+PHZ))                             
      SR(6,2)=6.*E(MTYPE)*ZI(NSEC)/(XL**2*(1+PHY))                              
      SR(4,4)=G*XI(NSEC)/XL                                                     
      SR(5,5)=(4.+PHZ)*E(MTYPE)*YI(NSEC)/(XL*(1+PHZ))                           
      SR(6,6)=(4.+PHY)*E(MTYPE)*ZI(NSEC)/(XL*(1.+PHY))                          
      SR(11,5)=(2.-PHZ)*E(MTYPE)*YI(NSEC)/(XL*(1+PHZ))                          
      SR(12,6)=(2.-PHY)*E(MTYPE)*ZI(NSEC)/(XL*(1+PHY))                          
      SR(7,1)=-SR(1,1)                                                          
      SR(8,2)=-SR(2,2)                                                          
      SR(12,2)=SR(6,2)                                                          
      SR(9,3)=-SR(3,3)                                                          
      SR(11,3)=SR(5,3)                                                          
      SR(10,4)=-SR(4,4)                                                         
      SR(9,5)=-SR(5,3)                                                          
      SR(8,6)=-SR(6,2)                                                          
      SR(7,7)=SR(1,1)                                                           
      SR(8,8)=SR(2,2)                                                           
      SR(12,8)=SR(8,6)                                                          
      SR(9,9)=SR(3,3)                                                           
      SR(11,9)=SR(9,5)                                                          
      SR(10,10)=SR(4,4)                                                         
      SR(11,11)=SR(5,5)                                                         
      SR(12,12)=SR(6,6)                                                         
      DO 20 I=1,12                                                              
      DO 20 J=1,I                                                               
 20   SR(J,I)=SR(I,J)                                                           
      WRITE(RIGROT) SR,ROT,XL                                                   
      IF( NELLIB .LT. 1 ) GO TO 50                                              
C                                                                               
C          INTRODUCAO DE LIBERACOES                                             
C                                                                               
      DO 40 I=1,NELLIB                                                          
      IF( N .NE. NELIB(I) ) GO TO 40                                            
      CALL LIBER(I,SR,FNE,KODLIB,1)                                             
      GO TO 50                                                                  
 40   CONTINUE                                                                  
 50   CONTINUE                                                                  
      CALL STS(ST1,ST2,DUMMY,DUMMY,ST3,DUMMY,ST4,DUMMY,DUMMY,DUMMY,SR)          
      IF( MASSA .NE. 0 ) GO TO 518                                              
      AM(1,1)=CM1                                                               
      AM(2,2)=CM2                                                               
      AM(3,3)=CM3                                                               
      BM(1,1)=CM4                                                               
      BM(2,2)=CM6                                                               
      BM(3,3)=CM8                                                               
      CM(1,1)=.5*CM1                                                            
      CM(2,2)=CM10                                                              
      CM(3,3)=CM12                                                              
      DM(3,2)=-CM5                                                              
      DM(2,3)=CM7                                                               
      EM(2,3)=-CM11                                                             
      EM(3,2)=CM13                                                              
      FM(1,1)=.5*CM4                                                            
      FM(2,2)=CM14                                                              
      FM(3,3)=CM9                                                               
  518 CONTINUE                                                                  
      DO 520 I=1,3                                                              
      DO 520 J=1,3                                                              
      ST2T(I,J)=ST2(J,I)                                                        
                                                                                
      IF( MASSA .NE. 0 ) GO TO 520                                              
      EMT(I,J)=EM(J,I)                                                          
  520 CONTINUE                                                                  
      ITR=1                                                                     
C     ******************************************************************        
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,7001)ROT,ST1,ST2,ST2T,ST3,ST4                                  
      CALL MULTI(  ITR,ST1,ROT,ST1)                                             
      CALL MULTI(  ITR,ST2,ROT,ST2)                                             
      CALL MULTI(  ITR,ST2T,ROT,ST2T)                                           
      CALL MULTI(  ITR,ST3,ROT,ST3)                                             
      CALL MULTI(  ITR,ST4,ROT,ST4)                                             
                                                                                
      IF( MASSA .NE. 0 ) GO TO 550                                              
      CALL MULTI(ITR,AM,ROT,AM)                                                 
      CALL MULTI(ITR,BM,ROT,BM)                                                 
      CALL MULTI(ITR,CM,ROT,CM)                                                 
      CALL MULTI(ITR,DM,ROT,DM)                                                 
      CALL MULTI(ITR,EM,ROT,EM)                                                 
      CALL MULTI(ITR,FM,ROT,FM)                                                 
      CALL MULTI(ITR,EMT,ROT,EMT)                                               
  550 CONTINUE                                                                  
C     ******************************************************************        
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,7001)ROT,ST1,ST2,ST2T,ST3,ST4                                  
C                                                                               
C        ESCREVE NO DISCO A MATRIZ  "SL" DO ELEMENTO,                           
C        EM COORDENADAS LOCAIS MULTIPLICADA POR RT (SL=RT.SM)                   
C                                                                               
      IF(NPAR(10) .LT. 0) GO TO 300                                             
      DO 410 I=1,3                                                              
      DO 410 J=1,3                                                              
      SL(I,J)=ST1(I,J)                                                          
      I3=I+3                                                                    
      SL(I3,J)=ST2T(I,J)                                                        
      I6=I+6                                                                    
      SL(I6,J)=-ST1(I,J)                                                        
      I9=I+9                                                                    
      SL(I9,J)=ST2T(I,J)                                                        
      J3=J+3                                                                    
      J6=J+6                                                                    
      J9=J+9                                                                    
      SL(I,J3)=ST2(I,J)                                                         
      SL(I3,J3)=ST3(I,J)                                                        
      SL(I6,J3)=-ST2 (I,J)                                                      
      SL(I9,J3)=ST4(I,J)                                                        
      SL(I,J6)=-ST1(I,J)                                                        
      SL(I3,J6)=-ST2T(I,J)                                                      
      SL(I6,J6)=ST1(I,J)                                                        
      SL(I9,J6)=-ST2T(I,J)                                                      
      SL(I,J9)=ST2(I,J)                                                         
      SL(I3,J9)=ST4(I,J)                                                        
      SL(I6,J9)=-ST2(I,J)                                                       
      SL(I9,J9)=ST3(I,J)                                                        
  410 CONTINUE                                                                  
      WRITE(IMTV)SL                                                             
300   CONTINUE                                                                  
      ITR=2                                                                     
C     ******************************************************************        
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,7001)ROT,ST1,ST2,ST2T,ST3,ST4                                  
7001  FORMAT(6(//3(3G20.5/)))                                                   
      CALL MULTI(  ITR,ST1,ST1,ROT)                                             
      CALL MULTI(  ITR,ST2,ST2,ROT)                                             
      CALL MULTI(  ITR,ST2T,ST2T,ROT)                                           
      CALL MULTI(  ITR,ST3,ST3,ROT)                                             
      CALL MULTI(  ITR,ST4,ST4,ROT)                                             
                                                                                
      IF( MASSA .NE. 0 ) GO TO 450                                              
      CALL MULTI(ITR,AM,AM,ROT)                                                 
      CALL MULTI(ITR,BM,BM,ROT)                                                 
      CALL MULTI(ITR,CM,CM,ROT)                                                 
      CALL MULTI(ITR,DM,DM,ROT)                                                 
      CALL MULTI(ITR,EM,EM,ROT)                                                 
      CALL MULTI(ITR,FM,FM,ROT)                                                 
      CALL MULTI(ITR,EMT,EMT,ROT)                                               
  450 CONTINUE                                                                  
C     ******************************************************************        
      IF(IDEPUR(2) .EQ. 1)                                                      
     *WRITE(IOUT,7001)ROT,ST1,ST2,ST2T,ST3,ST4                                  
C                                                                               
C        MONTAGEM DA MATRIZ DE RIGIDEZ DOS ELEMENTOS                            
C        NA FORMA DE UM VECTOR                                                  
C                                                                               
      KL=0                                                                      
      LIM=3                                                                     
       LBX=(LIM+1)*LIM/2                                                        
      DO 530  I=1,LIM                                                           
      DO 530 J=I,LIM                                                            
      KL=KL+1                                                                   
       KL1=(I-1)*9+KL                                                           
       KL2=(I-1)*6+KL+33                                                        
       KL3=(I-1)*3+KL+57                                                        
       KL4=KL+72                                                                
      S(KL1)=ST1(I,J)                                                           
      S(KL2)=ST3(I,J)                                                           
      S(KL3)=ST1(I,J)                                                           
      S(KL4)=ST3(I,J)                                                           
                                                                                
      IF( MASSA .NE. 0 ) GO TO 460                                              
      SM(KL1)=AM(I,J)                                                           
      SM(KL2)=BM(I,J)                                                           
      SM(KL3)=AM(I,J)                                                           
      SM(KL4)=BM(I,J)                                                           
  460 CONTINUE                                                                  
  530 CONTINUE                                                                  
      DO 540 I=1,LIM                                                            
       KC=(7-I)*I/2                                                             
      DO 540 J=1,LIM                                                            
      KC1=(I-1)*9+KC+J                                                          
       KC2=(I-1)*6+KC+J+33                                                      
       KC3=KC+J+57+(I-1)*3                                                      
       KD1=KC1+3                                                                
       KE1=KC1+6                                                                
       KD2=KC2+3                                                                
      S(KC1)=ST2(I,J)                                                           
      S(KD1)=-ST1(I,J)                                                          
      S(KE1)=ST2(I,J)                                                           
      S(KC2)=-ST2T(I,J)                                                         
      S(KD2)=ST4(I,J)                                                           
      S(KC3)=-ST2(I,J)                                                          
                                                                                
      IF( MASSA .NE. 0 ) GO TO 470                                              
      SM(KC1)=DM(I,J)                                                           
      SM(KD1)=CM(I,J)                                                           
      SM(KE1)=EM(I,J)                                                           
      SM(KC2)=-EMT(I,J)                                                         
      SM(KD2)=FM(I,J)                                                           
      SM(KC3)=-DM(I,J)                                                          
  470 CONTINUE                                                                  
  540 CONTINUE                                                                  
C                                            MAT. DE MASSA DIAGONAL             
C     IF( MASSA .EQ. 1 ) CALL DIAG(SM,6,2)                                      
C .                                                                   .         
C     *********************************************************                 
       IF(INTSAI(2) .EQ. 0) GO TO 480                                           
      WRITE(IOUT,6900)                                                          
 6900 FORMAT(//120('*'),//10X,'MATRIZ DE RIGIDEZ DO ELEMENTO',//120('*')        
     1)                                                                         
         WRITE(IOUT,7000)S                                                      
7000  FORMAT(/,1X,12G11.4,/,1X,11G11.4,/,1X,10G11.4,/,1X,9G11.4,/,              
     1         1X, 8G11.4,/,1X, 7G11.4,/,1X, 6G11.4,/,1X,5G11.4,/,              
     2         1X, 4G11.4,/,1X, 3G11.4,/,1X, 2G11.4,/,1X, G11.4)                
C     *********************************************************                 
                                                                                
      IF( MASSA .NE. 0 ) GO TO 480                                              
      WRITE(IOUT,7900)                                                          
 7900 FORMAT(1H,//120('*'),//10X,'MATRIZ DE MASSA DO ELEMENTO',//120('*'        
     *),//)                                                                     
      WRITE(IOUT,7000)SM                                                        
  480 CONTINUE                                                                  
C     *********************************************************                 
      CALL ADDBAN(A(N3),A(N2),S,LM(1,N),ND,A(N10),SM,N)                         
  500 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE FNEV3(R,E,AREA,RO,COEXPT,INCID,MATP,IRUN,ID,NELIB,             
     *                 KODLIB,NRVT,TEMP,ISEC)                                   
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                    .        
C .   P R O G R A M A                                                  .        
C .      CALCULO DE CARGAS NODAIS EQUIVALENTES                         .        
C .      ELEMENTO DE VIGA ESPACIAL                                     .        
C .                                                                    .        
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      INTEGER FNODEQ,RIGROT,FORCAS                                              
      REAL CONV,GRAVID,GX,GY,GZ                                                 
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
      COMMON /EL/ IND,NPAR(20)                                                  
      COMMON /CARGAS/ ICARGA                                                    
      COMMON /VAR/ NG,MODEX,CONV,GRAVID,GX,GY,GZ                                
      DIMENSION AML(12),V(3),ROT(3,3),SM(12,12),NELIB(1),KODLIB(12,1),          
     1         INCID(2,1),NRVT(1),TEMP(3,1),ID(6,1),R(1),ICARGA(10),            
     2         E(1),AREA(1),RO(1),COEXPT(1),MATP(1),IRUN(1),ISEC(1)             
      EQUIVALENCE (ICARGA(3),NRT),(ICARGA(6),INPP),(NPAR(2),NUME),              
     *            (NPAR(15),NELLIB)                                             
      DO 800 NE=1,NUME                                                          
      IRAMAL=IRUN(NE)                                                           
      IMAT=MATP(NE)                                                             
      NSEC=ISEC(NE)                                                             
      PESO=RO(NSEC)*AREA(NSEC)*GRAVID                                           
      READ(RIGROT)SM,ROT,XL                                                     
      DO 1 I=1,12                                                               
 1    AML(I)=0.                                                                 
C                                                                               
C         FORCAS NODAIS EQUIVALENTES DEVIDAS A UMA VARIACAO DE TEMPERATU        
C                                                                               
 100  IF( NRT .EQ. 0 ) GO TO 200                                                
      DO 3 IT=1,NRT                                                             
      IF( NRVT(IT) .NE. IRAMAL ) GO TO 3                                        
      CVT=-COEXPT(IMAT)*E(IMAT)*AREA(NSEC)*TEMP(1,IT)                           
      AML(1)=AML(1)-CVT                                                         
      AML(7)=AML(7)+CVT                                                         
 3735 FORMAT(/6G15.7)                                                           
      GO TO 200                                                                 
 3    CONTINUE                                                                  
C                                                                               
C         FORCAS NODAIS EQUIVALENTES DEVIDAS AO PESO PROPRIO                    
C                                                                               
 200  IF( INPP .EQ. 0 ) GO TO 300                                               
C                                                                               
      CPX=(ROT(1,1)*GX + ROT(1,2)*GY + ROT(1,3)*GZ)*PESO                        
      CPY=(ROT(2,1)*GX + ROT(2,2)*GY + ROT(2,3)*GZ)*PESO                        
      CPZ=(ROT(3,1)*GX + ROT(3,2)*GY + ROT(3,3)*GZ)*PESO                        
      XL2=0.5*XL                                                                
      C=XL*XL/12.                                                               
      AML(1)=AML(1)-XL2*CPX                                                     
      AML(7)=AML(7)-XL2*CPX                                                     
      AML(2)=AML(2)-XL2*CPY                                                     
      AML(8)=AML(8)-XL2*CPY                                                     
      AML(6)=AML(6)  -C*CPY                                                     
      AML(12)=AML(12)+C*CPY                                                     
      AML(3)=AML(3)-XL2*CPZ                                                     
      AML(9)=AML(9)-XL2*CPZ                                                     
      AML(5)=AML(5)  -C*CPZ                                                     
      AML(11)=AML(11)+C*CPZ                                                     
      WRITE(IOUT,3735)AML                                                       
C                                                                               
C         MODIFICACAO DOS AML DEVIDA AS LIBERACOES                              
C                                                                               
 300  IF( NELLIB .LE. 0 ) GO TO 500                                             
      DO 400 I=1,NELLIB                                                         
      IF( NE .NE. NELIB(I) ) GO TO 400                                          
      CALL LIBER(I,SM,AML,KODLIB,2)                                             
      GO TO 500                                                                 
 400  CONTINUE                                                                  
 500  CONTINUE                                                                  
C                                                                               
C         MONTAGEM DOS AML NO VETOR GLOBAL E GRAVACAO EM FNODEQ                 
C                                                                               
      WRITE(FNODEQ)AML                                                          
      DO 320 KJ=1,2                                                             
      KK=(KJ-1)*6                                                               
      NP=INCID(KJ,NE)                                                           
      DO 315 K=1,2                                                              
      IS=(K-1)*3                                                                
      IK=KK+IS                                                                  
      DO 310 LI=1,3                                                             
      II=LI+IS                                                                  
      KI=ID(II,NP)                                                              
      IF(KI)310,310,301                                                         
 301  V(LI)=0.                                                                  
      DO 305 J=1,3                                                              
 305  V(LI)=V(LI)-ROT(J,LI)*AML(IK+J)                                           
      R(KI)=R(KI)+V(LI)                                                         
 310  CONTINUE                                                                  
 315  CONTINUE                                                                  
 320  CONTINUE                                                                  
 800  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TUBOR                                                          
C                                                                               
C        IND=1     GERA INFORMACOES                                             
C        IND=2     CALCULA MATRIZES DO ELEMENTO                                 
C        IND=3     CALCULA ESFORCOS                                             
C        IND=4     CALCULA CARGAS NODAIS EQUIVALENTES                           
C        IND=5 OU IND=6 PROBLEMA ELASTOPLASTICO                                 
C                                                                               
      INTEGER ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                          
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /NOVDIM/ N101,N102,N103,N104,N105,N106,N107,N108,N109,N110,        
     1                N111,N112,N113,N114,N115,N116,N117,N118,N119              
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /CARGAS/ ICARGA,N302,N303,N304,N305,N306,N307,N308,                
     *                 N309,N310                                                
      COMMON A(1)                                                               
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /PROPRI/ N5A,N5B,N5C                                               
      COMMON/PLAS/N27,N28,N29,N30,N31,N32                                       
      COMMON/ARQUIV/ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                    
      DIMENSION LABEL(5),ICARGA(10)                                             
      EQUIVALENCE (NPAR(2),NUME) , (NPAR(3),NUMMAT) , (NPAR(11),NUMSEC),        
     1            (NPAR(12),NUMINT) , (NPAR(13),NUMTEE) , (NPAR(14),            
     2             NELRIG) , (NPAR(15),NELLIB)                                  
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A TU','BOR ','    '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A TU'
      LABEL(4)='BOR '
      LABEL(5)='    '
C                                                                               
      NFIRST=N6                                                                 
      L=3                                                                       
      IF( IND .EQ. 1 ) GO TO 10                                                 
      NFIRST=N5                                                                 
      L=4                                                                       
 10   N101=NFIRST                                                               
      N102=N101+8*NUMMAT*ITWO                                                   
      N103=N102+7*NUMSEC*ITWO                                                   
      N104=N103+NUMMAT                                                          
      N105=N104+3*NUMTEE                                                        
      N106=N105+ NUMTEE                                                         
      N107=N106+ NUMTEE*ITWO                                                    
      N108=N107+ NUME                                                           
      N109=N108+ NUME                                                           
      N110=N109+ NUME                                                           
      N111=N110+2*NUME                                                          
      N116=N111+  NUME                                                          
      N119=N116+2*NUME                                                          
      N112=N119+NUMTEE                                                          
      N113=N112+NELRIG                                                          
      N114=N113+2*NELRIG*ITWO                                                   
      N115=N114+NELLIB                                                          
      N117=N115+12*NELLIB                                                       
      N118=N117+12*NUME                                                         
      N120=N118+6*NUME*ITWO                                                     
 5    NLAST=N120                                                                
      IF( NLAST .GT. MTOT ) CALL ERROR(NLAST-MTOT,L,LABEL,5)                    
 200  MIDEST=NLAST-NFIRST                                                       
      NPAR(20)=N112-NFIRST                                                      
      IF( IDEPUR(1) .EQ. 1 ) WRITE(IOUT,7171) NPAR                              
 7171 FORMAT(20I5)                                                              
      GO TO (300,600,900,1200,1500,1700),IND                                    
C                                                                               
C        LEITURA DAS PROPRIEDADES DOS ELEMENTOS                                 
C                                                                               
 300  CALL TBR(A(N1),A(N2),A(N3),A(N4),A(N5),                                   
     1         A(N101),A(N102),A(N103),A(N104),A(N105),A(N106),A(N107),         
     2         A(N108),A(N109),A(N110),A(N111),A(N112),A(N113),A(N114),         
     3         A(N115),A(N116),A(N117),A(N118),A(N119),A(N5A),A(N5B),           
     4         A(N5C))                                                          
      RETURN                                                                    
C                                                                               
C        CALCULO DAS MATRIZES DO ELEMENTO                                       
C                                                                               
 600  CALL RIGTBR(A(N101),A(N102),A(N108),A(N109),A(N111),A(N104),              
     1    A(N106),A(N112),A(N113),A(N114),A(N115),A(N117),A(N118),              
     2    A(N119))                                                              
      RETURN                                                                    
C                                                                               
C        CALCULO DE TENSOES                                                     
C                                                                               
 900  CALL TENTBR(A(N4),A(N116),A(N117))                                        
                                                                                
      RETURN                                                                    
C                                                                               
C        CALCULO DE FORCAS NODAIS EQUIVALENTES                                  
C                                                                               
 1200 CALL FNETBR(A(N4),A(N101),A(N102),A(N107),A(N108),A(N109),                
     1            A(N112),A(N113),A(N114),A(N115),A(N116),A(N6),                
     2            A(N302),A(N303),A(N304),A(N309),A(N310),A(N1))                
      RETURN                                                                    
C                                                                               
C        CALCULO DE FORCAS INTERNAS PARA O PROBLEMA PLASTICO                    
C                                                                               
 1500 ITAP4=ARQ5                                                                
      CALL PLASTR(A(N108),A(N109),A(N101),A(N102),A(N116),A(N1),                
     *            A(N29),A(N30),A(N31),A(N32),ITAP4)                            
      RETURN                                                                    
C                                                                               
C         INICIALIZACAO DO ARQUIVO ITAP4 ONDE SERAO GRAVADAS TENSOES            
C         NO CASO ELASTOPLASTICO                                                
C                                                                               
 1700 ITAP4=ARQ5                                                                
      CALL INICIR(A(N101),A(N108),A(N32),ITAP4)                                 
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE TBR(ID,X,Y,Z,MHT,PMAT,PSEC,NCURVA,NELTEE,ISITEE,TEQ,           
     1               IRUN,IMAT,ISEC,ISI,NTEE,NERIG,LRIG,NELIB,KODLIB,           
     2               INCID,LM,XYZ,IBRAN,TABMAT,TABSEC,KCURVA)                   
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL *8 INDTEN,LRIG                                                       
      REAL A,CONV,GRAVID                                                        
      DIMENSION ID(6,1),X(1),Y(1),Z(1),MHT(1),PMAT(8,1),PSEC(7,1),              
     1          NCURVA(1),IRUN(1),IMAT(1),ISEC(1),NERIG(1),LRIG(2,1),           
     2          ISI(2,1),NELIB(1),KODLIB(12,1),INCID(2,1),LM(12,1),             
     3          XYZ(6,1),NTEE(1),NELTEE(3,1),ISITEE(1),TEQ(1),                  
     4          IDADO1(8),IDADO2(8),IBRAN(1),TABMAT(8,1),TABSEC(8,1),           
     5          KCURVA(1),KAUX(1000)                                              
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     1         ,NEMSE,NEQTV1,NEQTV2                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /EXTRA/ ITIP                                                       
      COMMON A(1)                                                               
      EQUIVALENCE (IDADO2(4),II) , (IDADO2(5),IJ) ,                             
     1            (NPAR(1),NPAR1) , (NPAR(2),NUME) , (NPAR(3),NUMMAT) ,         
     2            (NPAR(11),NUMSEC) , (NPAR(12),NUMINT) , (NPAR(13),            
     3             NUMTEE) , (NPAR(14),NELRIG) , (NPAR(15),NELLIB)              
      ND=12                                                                     
      IF(NPAR(10).GE.0) NEQT=NEQT+19*NUME                                       
      IF(NPAR(10).EQ.1) NEQTV1=NEQTV1+15*NUME                                   
      IF(NPAR(10).EQ.2) NEQTV2=NEQTV2+3*NUME                                    
      IF( INTSAI(1) .EQ. 0 ) GO TO 5                                            
      WRITE(IOUT,2000) (NPAR(I),I=1,5)                                          
      WRITE(IOUT,2055) NPAR(10),NPAR(11),NPAR(13),NPAR(14),NPAR(15)             
C                                                                               
   5  CONTINUE                                                                  
C                                                                               
C                                                                               
C        LE INFORMACOES SOBRE OS ELEMENTOS                                      
C                                                                               
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2110)                                   
      N=1                                                                       
 100  READ(IIN,1700) M,(IDADO1(K),K=1,8),KG                                     
      IF( KG .EQ. 0 ) KG=1                                                      
 120  IF( M .NE. N ) GO TO 200                                                  
      DO 50 K=1,8                                                               
 50   IDADO2(K)=IDADO1(K)                                                       
      KKK=KG                                                                    
C                                                                               
C        GUARDA INFORMACOES DO ELEMENTO                                         
C                                                                               
 200  IRUN(N)=IDADO2(1)                                                         
      IMAT(N)=IDADO2(2)                                                         
      ISEC(N)=IDADO2(3)                                                         
      XYZ(1,N)=X(II)                                                            
      XYZ(2,N)=Y(II)                                                            
      XYZ(3,N)=Z(II)                                                            
      XYZ(4,N)=X(IJ)                                                            
      XYZ(5,N)=Y(IJ)                                                            
      XYZ(6,N)=Z(IJ)                                                            
      ISI(1,N)=IDADO2(6)                                                        
      ISI(2,N)=IDADO2(7)                                                        
      NTEE(N)=IDADO2(8)                                                         
      DO 390 L=1,12                                                             
 390  LM(L,N)=0                                                                 
      DO 400 L=1,6                                                              
      LM(L,N)=ID(L,II)                                                          
 400  LM(L+6,N)=ID(L,IJ)                                                        
C                                                                               
C        ATUALIZA A ALTURA DAS COLUNAS E A LARGURA DE BANDA                     
C                                                                               
      CALL COLHT(MHT,ND,LM(1,N))                                                
      INCID(1,N)=II                                                             
      INCID(2,N)=IJ                                                             
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2120) M,(IDADO2(K),K=1,8),KG            
      IF( N .EQ. NUME ) GO TO 500                                               
      N=N+1                                                                     
      II=II+KKK                                                                 
      IJ=IJ+KKK                                                                 
      IF( N .GT. M ) GO TO 100                                                  
      GO TO 120                                                                 
C                                                                               
C     DEFINICAO DE TABELAS DE MATERIAIS                                         
C                                                                               
  500 NCONT=1                                                                   
      DO 750 N=1,NUME                                                           
      NMAT=IMAT(N)                                                              
      IF(N.EQ.1) GO TO 730                                                      
      DO 720 I=1,NCONT                                                          
      IF(NMAT-KAUX(I)) 720,745,720                                              
  720 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  730 KAUX(NCONT)=NMAT                                                          
      IMAT(N)=NCONT                                                             
      DO 740 J=1,8                                                              
      PMAT(J,NCONT)=TABMAT(J,NMAT)                                              
  740 CONTINUE                                                                  
      NCURVA(NCONT)=KCURVA(NMAT)                                                
      GO TO 750                                                                 
  745 IMAT(N)=I                                                                 
  750 CONTINUE                                                                  
C                                                                               
C     DEFINICAO DE PROPRIEDADES DE SECOES                                       
C                                                                               
       NCONT=1                                                                  
      DO 850 N=1,NUME                                                           
      NSEC=ISEC(N)                                                              
      IF(N.EQ.1) GO TO 830                                                      
      DO 820 I=1,NCONT                                                          
      IF(NSEC-KAUX(I)) 820,845,820                                              
  820 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  830 KAUX(NCONT)=NSEC                                                          
      ISEC(N)=NCONT                                                             
      DO 840 J=1,7                                                              
      PSEC(J,NCONT)=TABSEC(J,NSEC)                                              
  840 CONTINUE                                                                  
      PSEC(3,NCONT)=PSEC(3,NCONT)/CONV                                          
      GO TO 850                                                                 
  845 ISEC(N)=I                                                                 
  850 CONTINUE                                                                  
C                                                                               
      IF(NUMTEE .LT. 1 ) GO TO 41                                               
C                                                                               
C        LE LISTA DE INTERSECOES 'TEE'                                          
C                                                                               
      IF(INTSAI(1) .EQ. 1)WRITE(IOUT,2090)                                      
      READ(IIN,1500)(N,NELTEE(1,N),NELTEE(2,N),NELTEE(3,N),                     
     1                 ISITEE(N),IBRAN(N),TEQ(N),K=1,NUMTEE)                    
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2100) (N,NELTEE(1,N),                   
     1                       NELTEE(2,N),NELTEE(3,N),ISITEE(N),                 
     2                       IBRAN(N),TEQ(N),N=1,NUMTEE)                        
 41   CONTINUE                                                                  
      IF( NELRIG .LT. 1 ) GO TO 700                                             
C                                                                               
C        LE LISTA DE ELEMENTOS COM EXTREMIDADE RIGIDA                           
C                                                                               
      READ(IIN,1800)(NERIG(K),LRIG(1,K),LRIG(2,K),K=1,NELRIG)                   
      IF( INTSAI(1) .NE. 1 ) GO TO 600                                          
      WRITE(IOUT,2130)                                                          
      WRITE(IOUT,2140)(NERIG(K),LRIG(1,K),LRIG(2,K),K=1,NELRIG)                 
 600  DO 650 K=1,NELRIG                                                         
      LRIG(1,K)=LRIG(1,K)*CONV                                                  
 650  LRIG(2,K)=LRIG(2,K)*CONV                                                  
 700  IF( NELLIB .LT. 1 ) RETURN                                                
C                                                                               
C        LE LISTA DE ELEMENTOS COM LIBERACOES                                   
C                                                                               
      READ(IIN,1900)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                  
      IF(INTSAI(1) .NE. 1 ) RETURN                                              
      WRITE(IOUT,2150)                                                          
      WRITE(IOUT,2160)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                
      RETURN                                                                    
C                                                                               
C        FORMATOS                                                               
C                                                                               
 2000 FORMAT(//,10X,'D E F I N I C A O  D O  E L E M E N T O',//                
     1   5X,'TIPO DE ELEMENTO',13(2H .),17H( NPAR(1) ) . . =,I5,                
     2   ' ( ELEMENTO DE TUBO RETO )',//                                        
     3 5X,'NUMERO DE ELEMENTOS ',11(2H .),17H( NPAR(2) ) . . =,I5,//            
     4 5X,'NUMERO DE MATERIAIS ',11(2H .),17H( NPAR(3) ) . . =,I5,//            
     55X,'CALCULO DA MATRIZ DE MASSA',8(2H .),17H( NPAR(4) ) . . =, I5,         
     6' ( SE.NE.0 DISCRETA    )'/70X,'( SE.EQ.0 CONSISTENTE )',//               
     7   5X,'INERCIA DE ROTACAO',12(2H .),17H( NPAR(5) ) . . =,I5,              
     8' ( SE.NE.0 CONSIDERA     )',/70X,'( SE.EQ.0 NAO CONSIDERA ) ')           
 2055 FORMAT(//,5X,'CALCULO DE TENSOES',12(2H .),17H( NPAR(10)) . . =           
     1,I5,' ( SE.NE.0 NAO CALCULA )',/70X,'( SE.EQ.0 CALCULA    )',//           
     2 5X,'NUMERO DE SECOES .',12(2H .),17H( NPAR(11)) . . =,I5,//              
     3 ,5X,'NUMERO DE INTERSECOES',5H 'T' ,8(2H .),17H( NPAR(13)) . . =         
     4 ,I5,//                                                                   
     5 5X,'NUMERO DE ELEM. COM EXTREM.RIGIDAS ',3(2H .),17H( NPAR(14)) .        
     6 . =,I5,//                                                                
     7 5X,'NUMERO DE ELEM. COM LIBERACAO ',6(2H .),17H( NPAR(I5)) . . =         
     8  ,I5,//)                                                                 
C                                                                               
 2090 FORMAT(///,10X,'LISTA DE INTERSECOES T  ',//,3X,                          
     1       'N   NELTEE   ISITEE    TEQ',//)                                   
 2100 FORMAT(/,I3,5X,3I3,10X,2I5,F15.3)                                         
 1500 FORMAT(6I5,F10.0)                                                         
 1700 FORMAT(10I5)                                                              
 2110 FORMAT(///,10X,'CARACTERISTICAS DOS ELEMENTOS',//,5X,                     
     1'N   IRUN  IMAT  ISEC   II    IJ   ISI1  ISI2  NTEE   KG'//)              
 2120 FORMAT(/,10I6)                                                            
 1800 FORMAT(I5,2F10.0)                                                         
 2130 FORMAT(///,10X,'ELEMENTOS COM EXTREMIDADE RIGIDA',///,10X,                
     1       'ELEM.    L.RIG.(1)        L.RIG.(2)'///)                          
 2140 FORMAT(/,9X,I3,5X,F10.4,5X,F10.4)                                         
 1900 FORMAT(I5,4X,6I1,4X,6I1)                                                  
 2150 FORMAT(////,10X,'INDICADORES DE LIBERACOES NAS EXTREMIDADES',             
     1                ' DOS ELEMENTOS',///,10X,                                 
     2   '                         DIRECOES (SISTEMA LOCAL)',//,10X,            
     3   'NUMERO DO           * NO 1 *                * N0 2 *',/,              
     411X,'ELEMENTO       X  Y  Z  RX  RY  RZ     X  Y  Z  RX  RY  RZ',         
     5   /)                                                                     
 2160 FORMAT(/,10X,I5,8X,3I3,3I4,3X,3I3,3I4)                                    
      END                                                                       
      SUBROUTINE TUBOC                                                          
C                                                                               
C        IND=1     GERA INFORMACOES                                             
C        IND=2     CALCULA MATRIZES DO ELEMENTO                                 
C        IND=3     CALCULA ESFORCOS                                             
C        IND=4     CALCULA CARGAS NODAIS EQUIVALENTES                           
C        IND=5 OU IND=6  PROBLEMA ELASTOPLASTICO                                
C                                                                               
      INTEGER ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                          
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM                    
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15           
      COMMON /NOVDIM/ N101,N102,N103,N104,N105,N106,N107,N108,N109,N110,        
     1                N111,N112,N113,N114,N115,N116,N117,N118                   
      COMMON /EL/ IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO                
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON A(1)                                                               
      COMMON /PROPRI/ N5A,N5B,N5C                                               
      COMMON /CARGAS/ ICARGA,N302,N303,N304,N305,N306,N307,N308,                
     *                 N309,N310                                                
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON/PLAS/ N27,N28,N29,N30,N31,N32                                      
      COMMON/ARQUIV/ ARQ1,ARQ2,ARQ3,ARQ4,ARQ5                                   
      DIMENSION LABEL(5),ICARGA(10)                                             
      EQUIVALENCE (NPAR(2),NUME) , (NPAR(3),NUMMAT) , (NPAR(11),NUMSEC),        
     1            (NPAR(12),NUMINT) , (NPAR(15),NELLIB) , (NPAR(16),            
     2             NUMFAT)                                                      
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A TU','BOC ','    '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A TU'
      LABEL(4)='BOC '
      LABEL(5)='    '
C                                                                               
      NFIRST=N6                                                                 
      L=3                                                                       
      IF( IND .EQ. 1 ) GO TO 10                                                 
      NFIRST=N5                                                                 
      L=4                                                                       
 10   N101=NFIRST                                                               
      N102=N101+8*NUMMAT*ITWO                                                   
      N103=N102+7*NUMSEC*ITWO                                                   
      N104=N103+NUMMAT                                                          
      N105=N104+  NUME                                                          
      N106=N105+  NUME                                                          
      N107=N106+  NUME                                                          
      N110=N107+2*NUME                                                          
      N113=N110+2*NUME                                                          
      N115=N113+NUME*ITWO                                                       
      N117=N115+5*NUMFAT*ITWO                                                   
      N108=N117+NUME                                                            
      N109=N108+NELLIB                                                          
      N111=N109+12*NELLIB                                                       
      N112=N111+12*NUME                                                         
      N114=N112+6*NUME*ITWO                                                     
      N116=N114+3*NUME*ITWO                                                     
      N118=N116+NUMFAT                                                          
C     WRITE(IOUT,1591)N101,N102,N103,N104,N105,N106,N107,N108,                  
C    1                N109,N110,N111,N112,N113,N114,N115,N116,                  
C    2                N117,N118                                                 
 1591 FORMAT(/,18I5,/)                                                          
 5    NLAST=N118                                                                
      IF( NLAST .GT. MTOT ) CALL ERROR(NLAST-MTOT,L,LABEL,5)                    
 200  MIDEST=NLAST-NFIRST                                                       
      NPAR(20)=N108-NFIRST                                                      
      IF( IDEPUR(1) .EQ. 1 ) WRITE(IOUT,7171) NPAR                              
 7171 FORMAT(20I5)                                                              
      GO TO (300,600,900,1200,1500,1800),IND                                    
C                                                                               
C        LEITURA DAS PROPRIEDADES DOS ELEMENTOS                                 
C                                                                               
 300  CALL TBC(A(N1),A(N2),A(N3),A(N4),A(N5),                                   
     1         A(N101),A(N102),A(N103),A(N104),A(N105),A(N106),A(N113),         
     2         A(N114),A(N107),A(N108),A(N109),A(N110),A(N111),A(N112),         
     3         A(N115),A(N116),A(N117),A(N5A),A(N5B),A(N5C))                    
      RETURN                                                                    
C                                                                               
C        CALCULO DAS MATRIZES DO ELEMENTO                                       
C                                                                               
 600  CALL RIGTBC(A(N101),A(N102),A(N105),A(N106),A(N108),A(N109),              
     1         A(N113),A(N114),A(N111),A(N112),A(N115),A(N116),A(N117))         
      RETURN                                                                    
C                                                                               
C        CALCULO DE TENSOES                                                     
C                                                                               
                                                                                
 900  CALL TENTBC(A(N4),A(N110),A(N111))                                        
      RETURN                                                                    
C                                                                               
C        CALCULO DE FORCAS NODAIS EQUIVALENTES                                  
C                                                                               
 1200 CALL FNETBC(A(N4),A(N101),A(N102),A(N104),A(N105),A(N106),A(N113),        
     1 A(N108),A(N109),A(N110),A(N6),A(N302),A(N303),A(N304),                   
     2 A(N309),A(N310),A(N1))                                                   
       RETURN                                                                   
 1500  ITAP4=ARQ5                                                               
       CALL PLASTC(A(N105),A(N106),A(N101),A(N102),A(N110),                     
     1 A(N1),A(N29),A(N30),A(N31),A(N32),ITAP4,A(N113))                         
       RETURN                                                                   
C                                                                               
C      INICIALIZACAO DO ARQUIVO ITAP4 ONDE SERAO GRAVADAS TENSOES               
C      NO CASO ELASTOPLASTICO                                                   
C                                                                               
 1800  ITAP4=ARQ5                                                               
       CALL INICIR(A(N101),A(N105),A(N32),ITAP4)                                
       RETURN                                                                   
       END                                                                      
                                                                                
      SUBROUTINE TBC(ID,X,Y,Z,MHT,PMAT,PSEC,NCURVA,IRUN,IMAT,ISEC,              
     1               RAIO,PXYZ,ISI,NELIB,KODLIB,INCID,LM,XYZ,TABFAT,            
     2               NFLANG,IFAT,TABMAT,TABSEC,KCURVA)                          
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL A,CONV,GRAVID                                                        
      REAL *8 INDTEN                                                            
      DIMENSION ID(6,1),X(1),Y(1),Z(1),MHT(1),PMAT(8,1),PSEC(7,1),              
     1          NCURVA(1),IRUN(1),IMAT(1),ISEC(1),RAIO(1),PXYZ(3,1),            
     2          ISI(2,1),NELIB(1),KODLIB(12,1),INCID(2,1),LM(12,1),             
     3          XYZ(6,1),TABFAT(5,1),NFLANG(1),IFAT(1),                         
     4          IDADO1(8),RDADO1(4),IDADO2(8),RDADO2(4),                        
     5          TABMAT(8,1),TABSEC(8,1),KCURVA(1),KAUX(1000)                      
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     1         ,NEMSE,NEQTV1,NEQTV2                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF                                
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /EXTRA/ ITIP                                                       
      COMMON A(1)                                                               
      EQUIVALENCE (IDADO2(5),II) , (IDADO2(6),IJ) ,                             
     1            (NPAR(1),NPAR1) , (NPAR(2),NUME) , (NPAR(3),NUMMAT) ,         
     2            (NPAR(11),NUMSEC) , (NPAR(12),NUMINT) , (NPAR(15),            
     3             NELLIB) , (NPAR(16),NUMFAT)                                  
      SIN(XX)=DSIN(XX)                                                            
      COS(XX)=DCOS(XX)                                                            
      ND=12                                                                     
      IF(NPAR(10).GE.0) NEQT=NEQT+19*NUME                                       
      IF(NPAR(10).EQ.1) NEQTV1=NEQTV1+15*NUME                                   
      IF(NPAR(10).EQ.2) NEQTV2=NEQTV2+3*NUME                                    
      IF( INTSAI(1) .EQ. 0 ) GO TO 5                                            
      WRITE(IOUT,2000) (NPAR(I),I=1,5)                                          
      WRITE(IOUT,2055) NPAR(10),NPAR(11),NPAR(15),NPAR(16)                      
C                                                                               
   5  CONTINUE                                                                  
C                                                                               
C                                                                               
C        LE TABELAS DE FATORES ESPECIAIS                                        
C                                                                               
      IF(NUMFAT .LT. 1 ) GO TO 41                                               
      IF(INTSAI(1) .EQ. 1)WRITE(IOUT,2090)                                      
      DO 40 K=1,NUMFAT                                                          
      READ(IIN,1350) N,(TABFAT(L,N),L=1,5),NFLANG(N)                            
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2100) N,(TABFAT(L,N),L=1,5),            
     1                                        NFLANG(N)                         
      TABFAT(4,K)=TABFAT(4,K)*CONV                                              
 40   CONTINUE                                                                  
C                                                                               
C        LE INFORMACOES SOBRE OS ELEMENTOS                                      
C                                                                               
 41   IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2110)                                   
      N=1                                                                       
 100  READ(IIN,1700) M,(IDADO1(K),K=1,6),(RDADO1(K),K=1,4),                     
     1                 (IDADO1(K),K=7,8),KG                                     
      IF( KG .EQ. 0 ) KG=1                                                      
 120  IF( M .NE. N ) GO TO 200                                                  
      DO 50 K=1,8                                                               
 50   IDADO2(K)=IDADO1(K)                                                       
      DO 60 K=1,4                                                               
 60   RDADO2(K)=RDADO1(K)                                                       
      KKK=KG                                                                    
C                                                                               
C        GUARDA INFORMACOES DO ELEMENTO                                         
C                                                                               
 200  IRUN(N)=IDADO2(1)                                                         
      IMAT(N)=IDADO2(2)                                                         
      ISEC(N)=IDADO2(3)                                                         
      IFAT(N)=IDADO2(4)                                                         
      NFAT=IFAT(N)                                                              
      ESPAC=TABFAT(4,NFAT)                                                      
      ANG=TABFAT(5,NFAT)                                                        
      XYZ(1,N)=X(II)                                                            
      XYZ(2,N)=Y(II)                                                            
      XYZ(3,N)=Z(II)                                                            
      XYZ(4,N)=X(IJ)                                                            
      XYZ(5,N)=Y(IJ)                                                            
      XYZ(6,N)=Z(IJ)                                                            
      ISI(1,N)=IDADO2(7)                                                        
      ISI(2,N)=IDADO2(8)                                                        
      RAIO(N)=RDADO2(1)                                                         
      IF( ESPAC*ANG .EQ. 0. ) GO TO 15                                          
C                                                                               
C     CALCULO DE RAIO EQUIVALENTE PARA CURVAS EM GOMOS                          
C                                                                               
      TANG=SIN(ANG)/COS(ANG)                                                    
      NSEC=ISEC(N)                                                              
      RM=(TABSEC(1,NSEC)-TABSEC(2,NSEC))/2.                                     
      IF( ESPAC/RM-TANG-1. ) 12,12,13                                           
   12 RAIO(N)=0.5*ESPAC/TANG/CONV                                               
      GO TO 14                                                                  
   13 RAIO(N)=0.5*RM*(1.+1./TANG)/CONV                                          
C                                                                               
C     FLANGES EM 'MITTER' C/ GRANDE ESPACAMENTO NAO AFETAM FLEXIBILIDADE        
C                                                                               
      NFLANG(NFAT)=0                                                            
   14 RDADO2(1)=RAIO(N)                                                         
C                                                                               
   15 PXYZ(1,N)=RDADO2(2)                                                       
      PXYZ(2,N)=RDADO2(3)                                                       
      PXYZ(3,N)=RDADO2(4)                                                       
      DO 390 L=1,12                                                             
 390  LM(L,N)=0                                                                 
      DO 400 L=1,6                                                              
      LM(L,N)=ID(L,II)                                                          
 400  LM(L+6,N)=ID(L,IJ)                                                        
C                                                                               
C        ATUALIZA A ALTURA DAS COLUNAS E A LARGURA DE BANDA                     
C                                                                               
      CALL COLHT(MHT,ND,LM(1,N))                                                
      INCID(1,N)=II                                                             
      INCID(2,N)=IJ                                                             
      IF( INTSAI(1) .EQ. 1 ) WRITE(IOUT,2120) M,(IDADO2(K),K=1,6),              
     1                            (RDADO2(K),K=1,4),(IDADO2(K),K=7,8),KG        
      IF( N .EQ. NUME ) GO TO 500                                               
      N=N+1                                                                     
      II=II+KKK                                                                 
      IJ=IJ+KKK                                                                 
      IF( N .GT. M ) GO TO 100                                                  
      GO TO 120                                                                 
C                                                                               
C      DEFINICAO DE PROPRIEDADES DE MATERIAIS                                   
C                                                                               
  500 NCONT=1                                                                   
      DO 750 N=1,NUME                                                           
      NMAT=IMAT(N)                                                              
      IF(N.EQ.1) GO TO 730                                                      
      DO 720 I=1,NCONT                                                          
      IF(NMAT-KAUX(I)) 720,745,720                                              
  720 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  730 KAUX(NCONT)=NMAT                                                          
      IMAT(N)=NCONT                                                             
      DO 740 J=1,8                                                              
      PMAT(J,NCONT)=TABMAT(J,NMAT)                                              
  740 CONTINUE                                                                  
      NCURVA(NCONT)=KCURVA(NMAT)                                                
      GO TO 750                                                                 
  745 IMAT(N)=I                                                                 
  750 CONTINUE                                                                  
C                                                                               
C     DEFINICAO DE PROPRIEDADES DE SECOES                                       
C                                                                               
      NCONT=1                                                                   
      DO 850 N=1,NUME                                                           
      NSEC=ISEC(N)                                                              
      IF(N.EQ.1) GO TO 830                                                      
      DO 820 I=1,NCONT                                                          
      IF(NSEC-KAUX(I)) 820,845,820                                              
  820 CONTINUE                                                                  
      NCONT=NCONT+1                                                             
  830 KAUX(NCONT)=NSEC                                                          
      ISEC(N)=NCONT                                                             
      DO 840 J=1,7                                                              
      PSEC(J,NCONT)=TABSEC(J,NSEC)                                              
  840 CONTINUE                                                                  
      PSEC(3,NCONT)=PSEC(3,NCONT)/CONV                                          
      GO TO 850                                                                 
  845 ISEC(N)=I                                                                 
  850 CONTINUE                                                                  
C                                                                               
      DO 550 K=1,NUME                                                           
      RAIO(K)=RAIO(K)*CONV                                                      
      PXYZ(1,K)=PXYZ(1,K)*CONV                                                  
      PXYZ(2,K)=PXYZ(2,K)*CONV                                                  
 550  PXYZ(3,K)=PXYZ(3,K)*CONV                                                  
      IF( NELLIB .LT. 1 ) RETURN                                                
C                                                                               
C        LE LISTA DE ELEMENTOS COM LIBERACOES                                   
C                                                                               
      READ(IIN,1900)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                  
      IF(INTSAI(1) .NE. 1 ) RETURN                                              
      WRITE(IOUT,2140)                                                          
      WRITE(IOUT,2150)(NELIB(K),(KODLIB(L,K),L=1,12),K=1,NELLIB)                
      RETURN                                                                    
C                                                                               
C        FORMATOS                                                               
C                                                                               
 2000 FORMAT(//,10X,'D E F I N I C A O  D O  E L E M E N T O',//                
     1  5X,'TIPO DE ELEMENTO',13(2H .),17H( NPAR(1) ) . . =,I5,                 
     2  ' (ELEMENTO DE TUBO CURVO )',//                                         
     3  5X,'NUMERO DE ELEMENTOS ',11(2H .),17H( NPAR(2) ) . . =,I5,//           
     4  5X,'NUMERO DE MATERIAIS ',11(2H .),17H( NPAR(3) ) . . =,I5,//           
     5  5X,'CALCULO DA MATRIZ DE MASSA',8(2H .),17H( NPAR(4) ) . . =,I5,        
     6  ' ( SE.NE.0 DISCRETA    )',/70X,'( SE.EQ.0 CONSISTENTE )',//            
     7  5X,'INERCIA DE ROTACAO',12(2H .),17H( NPAR(5) ) . . =,I5,               
     8  ' ( SE.NE.0 CONSIDERA     )',/70X,'( SE.EQ.0 NAO CONSIDERA ) ')         
 2055 FORMAT(/,5X,'CALCULO DE TENSOES',12(2H .),17H( NPAR(10)) . . =,           
     1I5,' ( SE.NE.0 NAO CALCULA )',/70X,'( SE.EQ.0 CALCULA    )',//            
     2  5X,'NUMERO DE SECOES .',12(2H .),17H( NPAR(11)) . . =,I5,//             
     3  5X,'NUMERO DE ELEM. COM LIBERACOES',6(2H .),17H( NPAR(15)) . . =        
     4 ,I5,//5X,                                                                
     5'NUMERO DE FATORES ESPECIAIS ',7(2H .),17H( NPAR(16)) . . =,I5//)         
 2060 FORMAT(//,10X,'NPAR(10)=',I5)                                             
 2110 FORMAT(///,10X,'CARACTERISTICAS DOS ELEMENTOS',//,5X,                     
     1'N   IRUN  IMAT  ISEC  IFAT   II    IJ      RAIO         XP',             
     2'          YP          ZP      ISI1  ISI2   KG',//)                       
 1700 FORMAT(7I5,4F10.0,2I2,I1)                                                 
 2120 FORMAT(/,7I6,4F12.4,3I6)                                                  
 1900 FORMAT(I5,4X,6I1,4X,6I1)                                                  
 2140 FORMAT(////,10X,'INDICADORES DE LIBERACOES NAS EXTREMIDADES',             
     1                ' DOS ELEMENTOS',///,10X,                                 
     2   '                         DIRECOES (SISTEMA LOCAL)',//,10X,            
     3   'NUMERO DO           * NO 1 *                * NO 2 *',/,              
     411X,'ELEMENTO       X  Y  Z  RX  RY  RZ     X  Y  Z  RX  RY  RZ',         
     5   /)                                                                     
 2150 FORMAT(/,10X,I5,8X,3I3,3I4,3X,3I3,3I4)                                    
 1350 FORMAT(I5,5F10.0,I5)                                                      
 2090 FORMAT(///,10X,'TABELA DE DADOS ESPECIAIS PARA ELEMENTOS CURVOS'          
     1       ///,10X,'N   FRAC     FFP      FF0     ESPAC     ANG     ',        
     2         'NFLANG'/)                                                       
 2100 FORMAT(I11,5(2X,F7.3),2X,I5)                                              
      END                                                                       
                                                                                
      SUBROUTINE TEMPO(T,N)                                                     
C                                                                               
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .        CALCULA OS TEMPOS DE EXECUCAO                              .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C                                                                               
      DIMENSION T(1)                                                            
      N1=N-1                                                                    
      TT=0.                                                                     
      DO 5 I=1,N1                                                               
      T(I)=T(I+1)-T(I)                                                          
    5 TT=TT+T(I)                                                                
      T(N)=TT                                                                   
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE SECOND(TIM)                                                    
C                                                                               
C      SUBROTINA PARA OBTER O TEMPO DE CPU EM SEGUNDOS NO 'MTS'                 
C                                                                               
      CALL TIME(1,0,I)                                                          
      TIM=0.001*I                                                               
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE TUBNUC                                                         
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,            
     *             NLCDIN,LCASE,NLCOMB,NOMASS,NOREDI,ISAVE,NIVMAX               
      COMMON /DIM/N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15            
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     *         ,NEMSE,NEQTV1,NEQTV2                                             
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
     *              ,KSTIFF,ESFMOD,MODOS,INFLU                                  
      COMMON /MATRIZ/ IMTV                                                      
      COMMON /DEPURA/ IDEPUR(15),INTSAI(15)                                     
      COMMON /EXTRA/ ITIP,IMASSA                                                
      COMMON /CARGAS/ ICARGA(10),N302,N303,N304,N305,N306,N307,N308,            
     *                N309,N310                                                 
      COMMON /VAR/ NG,MODEX                                                     
      COMMON A(1)                             
      REAL*8 RTOL, TMIN	  
      DIMENSION TIM(5),LABEL(4),SIMNAO(2),HED(20)                               
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A TU','BNUC'/                            
      CHARACTER*4 LABEL
      CHARACTER*3 SIMNAO
      EQUIVALENCE (ICARGA(1),NRP),(ICARGA(3),NRT),(ICARGA(5),NNOSC),            
     *            (ICARGA(6),INPP),(ICARGA(7),NEDP),(ICARGA(4),NNDT)
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A TU'
      LABEL(4)='BNUC'
!     Era assim:      
!      DATA SIMNAO/'SIM','NAO'/                            
      SIMNAO(1)='SIM'
      SIMNAO(2)='NAO'
C---------N1-ID   N2-MAXA                                                       
      CALL SECOND(TIM(1))                                                       
C                                                                               
C         DEFINICAO DAS AREAS P/ MATRIZES DE RIGIDEZ,MASSA,VETOR DE CARGAS      
C                                                                               
      NWM=NWK                                                                   
      IF( IMASSA .EQ. 0 ) NWM=NEQ                                               
      NEQ1=NEQ+1                                                                
      N10=N2+NEQ1                                                               
      N3=N10+NWM*ITWO                                                           
      N4=N3+NWK*ITWO                                                            
      N5=N4+NEQ*ITWO                                                            
 1    N6=N5+MAXEST                                                              
      IF( N6 .GT. MTOT ) CALL ERROR(N6-MTOT,4,LABEL,1)                          
C                                                                               
C         ZERAR O VETOR A DA POSICAO N10 ATE N5                                 
C                                                                               
      NNL=NWM+NWK+NEQ                                                           
      CALL CLEAR(A(N10),NNL)                                                    
      IND=2                                                                     
C                                                                               
C         MONTAGEM DAS MATRIZES DE RIGIDEZ E DE MASSA                           
C                                                                               
      CALL ASSEM(A(N5))
      write(IOUT,6666)NLCDIN,LCASE,NLCOMB,NOMASS,NOREDI
 6666 format(' NOMASS = ',i10)	  
      IF( NOMASS .GT. 0 ) CALL MASCON(A(N1),A(N2),A(N10),NOMASS)                
      CALL SECOND(TIM(2))                                                       
C                                                                               
C         SALVAR MATRIZ DE RIGIDEZ EM KSTIFF                                    
C     CALL WRIGID(A(N3),A(N2),NEQ)                                              
      REWIND KSTIFF                                                             
      CALL LOADV(A(N3),NWK,KSTIFF,1,0)                                          
      IF( NLCEST .LE. 0 ) GO TO 400                                             
C                                                                               
C         TRIANGULARIZACAO DA MATRIZ DE RIGIDEZ                                 
C                                                                               
      IF(MODEX .EQ. 1)CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,1)             
      CALL SECOND(TIM(3))                                                       
C                                                                               
C         PERCORRE TODOS OS CASOS DE CARREGAMENTO                               
C                                                                               
      REWIND FORCAS                                                             
      REWIND ILOAD                                                              
      DO 300 LCASE=1,NLCEST                                                     
      READ  (IIN,1010) HED,LL,ICARGA                                            
      WRITE(IOUT,2010) HED,LL,ICARGA                                            
      CONTINUE                                                                  
C                                                                               
C         DEFINICAO DE AREAS                                                    
C                                                                               
      N302= N6 +NRP                                                             
      N303=N302+NRP*ITWO                                                        
      N304=N303+NRT                                                             
      N305=N304+3*NRT*ITWO                                                      
      N306=N305+NNOSC                                                           
      N307=N306+6*NNOSC*ITWO                                                    
      N308=N307+2*NEDP                                                          
      N309=N308+2*NEDP*ITWO                                                     
      N310=N309+NNDT                                                            
  2   N311=N310+3*NNDT*ITWO                                                     
      IF( N311 .GT. MTOT ) CALL ERROR(N311-MTOT,2,LABEL,2)                      
C                                                                               
C         LEITURA DAS CARGAS NO CARREGAMENTO LCASE                              
C                                                                               
      CALL TBLOAD(A(N1),A(N4),A(N6),A(N302),A(N303),A(N304),A(N305),            
     *            A(N306),A(N307),A(N308),A(N309),A(N310))                      
C                                                                               
C         MONTAGEM DOS VETORES DE CARGAS NODAIS EQUIVALENTES E SOLUCAO          
C                         DO SISTEMA DE EQUACOES                                
C                                                                               
      IND=4                                                                     
      IF(MODEX .EQ. 0)GO TO 300                                                 
      CALL CARGA(A(N5))                                                         
C                                                                               
C                                                                               
 5000 FORMAT(5E20.7,/)                                                          
C                                                                               
C                                                                               
      IND=3                                                                     
      CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,2)                             
C                                                                               
C         ESCREVE DESLOCAMENTOS NODAIS                                          
C                                                                               
      CALL WRITE(A(N4),A(N1),NEQ,NUMNP,1,1)                                     
C                                                                               
      CALL LOADV(A(N4),NEQ,ILOAD,1,INTSAI(4))                                   
      CALL SECOND(TIM(4))                                                       
      T4=T4+TIM(4)-TIM(5)                                                       
C                                                                               
C         CALCULO DAS TENSOES                                                   
C                                                                               
      CALL STRESS(A(N5))                                                        
      CALL SECOND(TIM(5))                                                       
 300  CONTINUE                                                                  
C                                                                               
C         CALCULO E IMPRESSAO DOS TEMPOS DE SOLUCAO                             
      CALL TEMPO(TIM,5)                                                         
      WRITE(IOUT,1000)TIM                                                       
C                                                                               
C         LEITURA DA MATRIZ DE RIGIDEZ EM KSTIFF                                
C                                                                               
      REWIND KSTIFF                                                             
      CALL LOADV(A(N3),NWK,KSTIFF,2,0)                                          
 400  CONTINUE                                                                  
      IF( NOREDI .LE. 0 ) GO TO 410                                             
C                                                                               
C         INTRODUCAO DE RESTRICOES DINAMICAS E GRAVACAO                         
C             DA MATRIZ DE RIGIDEZ EM KSTIFF                                    
C                                                                               
      CALL RSTDIN(A(N1),A(N2),A(N3),A(N10),NEQ,NOREDI)                          
      REWIND KSTIFF                                                             
      CALL LOADV(A(N3),NWK,KSTIFF,1,0)                                          
 410  CONTINUE                                                                  
      IF( NEMSE .LE. 0 .OR. NIVMAX .LE. 0 ) GO TO 420                           
C                                                                               
C         CALCULO DAS FUNCOES DE INFLUENCIA PARA EXCIT. MULT.                   
C                                                                               
      N7=N6+NEMSE                                                               
      CALL MSE1(A(N5),N5,N4,N6,INFLU,INFLU)                                     
 420  CONTINUE                                                                  
      READ(IIN,1040)NROOT,NC,NITEM,IFSS,RTOL                                    
      IF( NROOT .LE. 0. ) GO TO 500  
      IF( NC .GE. 2 .AND. NC .LE. NEQ ) go to 421
	  NC=MAX0(2*NROOT,NROOT+8)
	  if( NC .GT. NEQ ) NC=NEQ
 421  IF( NC .GE. NROOT ) GO TO 422
          WRITE(IOUT,2045)
 2045     FORMAT(' NC não superior a NROOT. Proc. interrompido.')
          CALL CLOSEFILES	  
 422  IF( NITEM .LE. 0  ) NITEM=16 
      IF( RTOL  .LE. 0. ) RTOL=1.0E-08                                           
      WRITE(IOUT,2000)                                                          
      WRITE(IOUT,2040)NROOT,NC,NITEM,RTOL,SIMNAO(2-IFSS),                       
     *                SIMNAO(2-INTSAI(5))                                       
C                                                                               
C         AREAS PARA RESOLUCAO DO AUTOPROBLEMA                                  
C                                                                               
      NNC=NC*(NC+1)/2                                                           
      N4 =N10 +NWM*ITWO                                                         
      N11= N4 +NEQ*NC*ITWO                                                      
      N3 =N11 +NC*ITWO                                                          
      N12= N3 +NWK*ITWO                                                         
      N400=N12+NEQ*ITWO                                                         
      N401=N400+NEQ*ITWO                                                        
      N402=N401+NNC*ITWO                                                        
      N403=N402+NNC*ITWO                                                        
      N404=N403+NC*NC*ITWO                                                      
      N405=N404+NC*ITWO                                                         
      N406=N405+NC*ITWO                                                         
      N407=N406+NC*ITWO                                                         
      N408=N407+NC*ITWO                                                         
 3    N409=N408+NC*ITWO                                                         
      IF( N409 .GT. MTOT ) CALL ERROR (N409-MTOT,6,LABEL,3)                     
      IF( MODEX .EQ. 0 ) GO TO 500                                              
C                                                                               
C         LEITURA DA MATRIZ DE RIGIDEZ EM KSTIFF                                
C                                                                               
      REWIND KSTIFF                                                             
      CALL LOADV(A(N3),NWK,KSTIFF,2,0)                                          
C                                                                               
C   ISAVE=0  RESULTADOS DO AUTOPROBLEMA NAO SERAO GRAVADOS EM 'MODOS'           
C   ISAVE=1  RESULTADOS DO AUTOPROBLEMA SERAO GRAVADOS EM 'MODOS'               
C   ISAVE=2  RESULTADOS DO AUTOPROBLEMA SERAO LIDOS EM 'MODOS'                  
C                                                                               
      NNL=2*(NNC+NEQ)+NC*(6+NC+NEQ)+NWK                                         
      REWIND MODOS                                                              
      IF( ISAVE .EQ. 2 ) GO TO 452   
C USE CALL SSPACE(....... PARA ATIVAR VERSAO BASEADA NO TUBO
C USE CALL SSPACB(....... PARA ATIVAR VERSAO BASEADA NO LIVRO DO BATHE
      CALL SSPACB(A(N3),A(N10),A(N2),A(N4),A(N11),A(N12),A(N400),A(N401) 
     1          ,A(N402),A(N403),A(N404),A(N404),A(N405),A(N406),A(N406)        
     2          ,A(N407),A(N408),NEQ,NEQ1,NROOT,RTOL,NC,NNC,NITEM,IFSS)         
      IF( ISAVE .NE. 1 ) GO TO 451                                              
      CALL LOADV(A(N4),NNL,MODOS,1,0)                                           
      GO TO 451                                                                 
 452  CALL LOADV(A(N4),NNL,MODOS,2,0)                                           
 451  CALL WROTE(A(N1),A(N4),A(N400),NROOT,NC,A(N11),A(N407),A(N406))           
 500  CONTINUE                                                                  
      NMODOS=NROOT                                                              
      IF( NLCDIN .LE. 0 ) GO TO 520                                             
C                                                                               
C        LEITURA E DEFINICAO DE PARAMETROS P/ A RESPOSTA ESPECTRAL              
C                                                                               
      READ(IIN,1060)TMIN,NSPEC,NMAXPT,IFQ                                       
 1060 FORMAT(F10.0,3I5)                                                         
C                                                                               
      IF( MODEX .EQ. 0 ) GO TO 530                                              
      CALL MODSIG(A(N406),A(N407),TMIN,IFQ,NROOT,NMODOS,A(N11))                 
C                                                                               
C            AREAS PARA O CALCULO DE MASSAS MODAIS EFETIVAS                     
C                                                                               
 520  IF( NROOT .LE. 0 ) GO TO 700                                              
      IF (MODEX .EQ. 0 ) GO TO 530                                              
      N12 =N3                                                                   
      N400=N12+ NEQ*ITWO                                                        
      N401=N400+NEQ*ITWO                                                        
      N402=N401+6*NROOT*ITWO                                                    
C                                                                               
      CALL MMODEF(A(N10),A(N12),A(N400),A(N4),NROOT,A(N1),A(N2),                
     *            A(N401))                                                      
C                                                                               
 530  IF( NLCDIN .LE. 0 ) GO TO 700                                             
C                                                                               
C        ZERAR VETOR A DA POSICAO N12 ATE N409                                  
C                                                                               
      NNL=2*(NEQ+NNC)+5*NC+NC*NC                                                
      CALL CLEAR(A(N12),NNL)                                                    
C                                                                               
C        AREAS PARA A DEFINICAO DOS ESPECTROS                                   
C                                                                               
 540  N400=N12+NMODOS*NSPEC*ITWO                                                
      N401=N400+2*NSPEC                                                         
 4    N402=N401+2*NMAXPT*ITWO                                                   
      IF( N402 .GT. MTOT ) CALL ERROR(N402-MTOT,6,LABEL,4)                      
C                                                                               
C        LEITURA E DETERMINACAO DOS VALORES ESPECTRAIS                          
C                                                                               
      CALL LESPEC(A(N11),A(N12),A(N400),A(N401),NMODOS,NSPEC,IFQ)               
C                                                                               
C         CALCULO DAS TENSOES PARA DESLOCAMENTOS                                
C         MODAIS E ARMAZENAMENTO EM ESFMOD                                      
C                                                                               
      IF( MODEX .EQ. 0 ) GO TO 570                                              
      IF( NGTC .LE. 0 ) GO TO 565                                               
 5    N402=N401+MAXEST                                                          
      IF( N402 .GT. MTOT ) CALL ERROR(N402-MTOT,6,LABEL,5)                      
      N4AUX=N4                                                                  
      N5=N401                                                                   
      KODT=3                                                                    
      IND=3                                                                     
      REWIND ESFMOD                                                             
      DO 560 I=1,NMODOS                                                         
      N4=N4AUX+(I-1)*NEQ*ITWO                                                   
 560  CALL STRESS(A(N401))                                                      
      N4=N4AUX                                                                  
C                                                                               
C         "SHIFT" DENTRO DO VETOR A                                             
C                                                                               
 565  NI=N4+NEQ*NMODOS*ITWO                                                     
      CALL MOVE(A,N11,N401-1,NI)                                                
      N11 =NI                                                                   
      N12 =N11 +NC*ITWO                                                         
      N400=N12 +NMODOS*NSPEC*ITWO                                               
 570  IF( NIVMAX .GT. 0 ) GO TO 800                                             
C                                                                               
C         RESPOSTA ESPECTRAL VIA *RESPEC*                                       
C                                                                               
      NAREA = MAX0(NEQ,NEQT)                                                    
      N401=N400+2*NSPEC                                                         
      N402=N401+ NAREA*ITWO                                                     
      N403=N402+ NAREA*ITWO                                                     
      N404=N403+ NAREA*ITWO                                                     
      N405=N404+NMODOS*ITWO                                                     
      N406=N405+NMODOS*ITWO                                                     
      N407=N406+NMODOS*ITWO                                                     
      N408=N407+NEQ       *ITWO                                                 
      N409=N408+NEQT*NMODOS*ITWO                                                
      N410=N409+NUMEG                                                           
      N411=N410+NUMEG                                                           
 6    N413=N411+NUMEG                                                           
      IF( N413 .GT. MTOT ) CALL ERROR(N413-MTOT,6,LABEL,6)                      
 7    N412=N408+MAXEST                                                          
      IF( N412 .GT. MTOT ) CALL ERROR(N412-MTOT,6,LABEL,7)                      
      N5=N408                                                                   
      CALL RESPEC(NMODOS,NSPEC,A(N1),A(N2),A(N10),A(N4),A(N4),A(N11),           
     1A(N12),A(N401),A(N402),A(N403),A(N404),A(N405),A(N406),A(N407),           
     2        A(N408),A(N408),A(N409),A(N410),A(N401),A(N402),A(N403),          
     3A(N411))                                                                  
      GO TO 700                                                                 
C                                                                               
C         RESPOSTA ESPECTRAL VIA *MSE3*                                         
C                                                                               
 800  NAREA=MAX0(NIVMAX,2)                                                      
      N401=N400+NEQ*NMODOS*ITWO                                                 
      N402=N401+NMODOS*ITWO                                                     
      N403=N401+NMODOS*NAREA*ITWO                                               
      N404=N403+NEQ*ITWO                                                        
      N405=N404+NEQ*NEMSE*ITWO                                                  
 11   N406=N405+NEMSE                                                           
      IF( N406 .GT. MTOT ) CALL ERROR(N406-MTOT,6,LABEL,11)                     
      N414=N10+ NMODOS*ITWO                                                     
      N407=N414+NMODOS*ITWO                                                     
      N408=N407+NEQT*NMODOS*ITWO                                                
      N409=N408+NUMEG                                                           
      N410=N409+NUMEG                                                           
      N411=N410+NUMEG                                                           
 9    N412=N411+NEQT*ITWO                                                       
      IF( N412 .GT. MTOT ) CALL ERROR(N412-MTOT,6,LABEL,9)                      
      N5=N407                                                                   
 10   N413=N407+MAXEST                                                          
      IF( N413 .GT. MTOT ) CALL ERROR(N413-MTOT,6,LABEL,10)                     
      CALL MSE3(NMODOS,NSPEC,A(N1),A(N2),A(N10),A(N4),A(N4),A(N11),             
     1   A(N12),A(N400),A(N400),A(N401),A(N401),A(N402),A(N403),A(N404),        
     2   A(N405),A(N10),A(N414),A(N407),A(N407),A(N408),A(N409),A(N410),        
     3           NC,NAREA,A(N411))                                              
 700  IF( NLCOMB .LE. 0 ) RETURN                                                
C                                                                               
C     COMBINACOES DE CARREGAMENTOS                                              
C                                                                               
      KODT=2                                                                    
      N500=N2                                                                   
      N500A=N500+NLCOMB                                                         
      N501=N500A+NLCOMB*7                                                       
      N502=N501+NLCOMB*7*ITWO                                                   
C                                                                               
      CALL TBCOMB(A(N501),A(N500A),A(N500),LID,LCAS,1)                          
C                                                                               
      REWIND ILOAD                                                              
      REWIND FORCAS                                                             
C                                                                               
C     COMBINACOES DE DESLOCAMENTOS                                              
C                                                                               
      NLCTOT=NLCEST+NLCDIN                                                      
      N503=N502+NEQ*NLCTOT*ITWO                                                 
      N504=N503+NEQ*ITWO                                                        
      IF(N504.GT.MTOT) CALL ERROR(N504-MTOT,7,LABEL,2)                          
      IF( MODEX .EQ. 0 )GO TO 750                                               
      CALL COMDES(A(N503),A(N502),A(N501),A(N500A),A(N500),NLCEST,              
     *     NLCTOT,NLCOMB,NEQ)                                                   
C                                                                               
C     COMBINACOES DE TENSOES                                                    
C                                                                               
 750  N503=N502+NEQT*NLCTOT*ITWO                                                
      N504=N503+NEQT*ITWO                                                       
      N505=N504+NUMEG                                                           
      N506=N505+NUMEG                                                           
   8  N507=N506+NUMEG                                                           
      IF(N507.GT.MTOT) CALL ERROR(N507-MTOT,8,LABEL,2)                          
      IF( MODEX .EQ. 0 ) RETURN                                                 
      CALL COMTEN(A(N503),A(N502),A(N501),A(N500A),A(N500),A(N504),             
     *            A(N505),A(N506),NLCEST,NLCTOT,NLCOMB,NEQT,NGTC)               
C     ESCREVE DESLOCAMENTOS E TENSOES COMBINADAS                                
C                                                                               
      REWIND ILOAD                                                              
      REWIND FORCAS                                                             
      REWIND FNODEQ                                                             
      N5=N502                                                                   
      DO 600 LID=1,NLCOMB                                                       
      CALL TBCOMB(A(N501),A(N500A),A(N500),LID,LCAS,2)                          
      IF(LCAS.EQ.0) GO TO 550                                                   
      CALL LOADV(A(N502),NEQ,ILOAD,2,0)                                         
      CALL WRITE(A(N502),A(N1),NEQ,NUMNP,1,1)                                   
      CALL STRESS(A(N502))                                                      
      GO TO 600                                                                 
 550  CALL DWRITE(A(N503),A(N500A),A(N504),A(N505),NGTC,LID,NEQT)               
  600 CONTINUE                                                                  
      CALL SECAO3                                                               
C                                                                               
C         FORMATOS                                                              
C                                                                               
 1010 FORMAT(20A4,/,11I5)                                                       
 1040 FORMAT(4I5,F10.0)                                                         
 2010 FORMAT(1H1,120('*'),//,20X,20A4,///,20X,'PARAMETROS DO CARRE'             
     1,'GAMENTO',//,                                                            
     2' CARREGAMENTO    NRP      IHOOP      NRT     NNDT      NNOSC',           
     3'      PESO     DESL.',                                                   
     4/,'    NUMERO',55X,'PROPRIO    PRESC.',//,I8,I11,6I10)                    
 2000    FORMAT(/48H ** A N A L I S E   D E   A U T O V A L O R E S ////        
     A51H    RESOLUCAO PELO METODO DE ITERACAO EM SUBESPACO       )             
 2040    FORMAT(///50H    I N F O R M A C O E S   D E   C O N T R O L E         
     D///8X,49HNUMERO DE AUTOVALORES SOLICITADO. . . . . . . . =,I5 //          
     E   8X,49HDIMENSAO DOS SUBESPACOS . . . . . . . . . . . . =,I5 //          
     F   8X,49HNUMERO MAXIMO DE ITERACOES. . . . . . . . . . . =,I5 //          
     G   8X,49HTOLERANCIA RELATIVA DESEJADA. . . . . . . . . . =,E12.5 ,        
     H// 8X,49HTESTE DE SEQUENCIA DE STURM . . . . . . . . . . =,1X,A4//        
     I   8X,49HRESULTADOS INTERMEDIARIOS . . . . . . . . . . . =,1X,A4 )        
 1000 FORMAT(//10X,'E S T A T I S T I C A  D E  T E M P O',//5G12.5)            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TBLOAD(ID,R,NRPI,PRESS,NRVT,TEMP,NOCAR,FCARGA,                 
     *                  NGENE,SSV,NOSDT,GTEMPN)                                 
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      COMMON /SOL/ NUMNP,NEQ                                                    
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /CARGAS/ ICARGA                                                    
      DIMENSION ICARGA(10),R(1),NRPI(1),PRESS(1),TEMP(3,1),NOCAR(1),            
     1          FCARGA(6,1),ID(6,1),NRVT(1),NGENE(2,1),SSV(2,1),                
     2          NOSDT(1),GTEMPN(3,1)                                            
      EQUIVALENCE (ICARGA(1),NRP),(ICARGA(3),NRT),(ICARGA(5),NNOSC),            
     *            (ICARGA(7),NEDP),(ICARGA(4),NNDT)                             
      DO 210 I=1,NEQ                                                            
  210 R(I)=0.                                                                   
C                                                                               
C     LEITURA DA PRESSAO INTERNA POR RAMAL                                      
C                                                                               
      IF(NRP.LE.0) GO TO 200                                                    
      WRITE(IOUT,2000)                                                          
      DO 5 IP=1,NRP                                                             
      READ(IIN,1000)NRPI(IP),PRESS(IP)                                          
      WRITE(IOUT,2010)NRPI(IP),PRESS(IP)                                        
    5 CONTINUE                                                                  
C                                                                               
C     LEITURA DA VARIACAO DE TEMPERATURA POR RAMAL                              
C                                                                               
  200 IF(NRT.LE.0) GO TO 250                                                    
      WRITE(IOUT,2020)                                                          
      DO 10 IT=1,NRT                                                            
      READ(IIN,1010)NRVT(IT),(TEMP(J,IT),J=1,3)                                 
      WRITE(IOUT,2030)NRVT(IT),(TEMP(J,IT),J=1,3)                               
   10 CONTINUE                                                                  
C                                                                               
C     LEITURA DOS GRADIENTES TERMICOS EM DESCONTINUIDADES                       
C                                                                               
250   IF(NNDT.EQ.0) GO TO 300                                                   
      WRITE(IOUT,2035)                                                          
      DO 20 ND=1,NNDT                                                           
      READ(IIN,1030)NOSDT(ND),(GTEMPN(J,ND),J=1,3)                              
      WRITE(IOUT,2037)NOSDT(ND),(GTEMPN(J,ND),J=1,3)                            
  20  CONTINUE                                                                  
C                                                                               
C     LEITURA DAS CARGAS APLICADAS NOS NOS E,                                   
C             MONTAGEM DO VETOR DE CARGAS CORRESPONDENTE                        
C                                                                               
  300 IF(NNOSC.LE.0) GO TO 400                                                  
      WRITE(IOUT,2040)                                                          
      DO 15 IN=1,NNOSC                                                          
      READ(IIN,1020)NOCAR(IN),(FCARGA(J,IN),J=1,6)                              
      WRITE(IOUT,2050)NOCAR(IN),(FCARGA(J,IN),J=1,6)                            
   15 CONTINUE                                                                  
C                                                                               
C                                                                               
      DO 330 NC=1,NNOSC                                                         
      LN=NOCAR(NC)                                                              
      DO 320 LI=1,6                                                             
      II=ID(LI,LN)                                                              
      IF(II)320,320,310                                                         
  310 R(II)=R(II)+FCARGA(LI,NC)                                                 
  320 CONTINUE                                                                  
  330 CONTINUE                                                                  
C                                                                               
C            LEITURA DOS DESLOCAMENTOS PRESCRITOS                               
C                                                                               
  400 IF( NEDP .LE. 0 ) RETURN                                                  
      WRITE(IOUT,2060)                                                          
      DO 420 I=1,NEDP                                                           
      READ (IIN,2070) NGENE(1,I),NGENE(2,I),SSV(1,I),SSV(2,I)                   
      WRITE(IOUT,2080)NGENE(1,I),NGENE(2,I),SSV(1,I),SSV(2,I)                   
 420  CONTINUE                                                                  
C                                                                               
C                                                                               
C     FORMATOS                                                                  
C                                                                               
 1000 FORMAT(I5,F10.0)                                                          
 1010 FORMAT(I5,3F10.0)                                                         
 1020 FORMAT(I5,6F10.0)                                                         
 1030 FORMAT(I5,3F10.0)                                                         
 2000 FORMAT(////,5X,'RAMAIS COM PRESSAO INTERNA',///,                          
     1      3X,'NUMERO DO',3X,'   PRESSAO',/,                                   
     2      3X,'  RAMAL  ',3X,'   INTERNA')                                     
 2010 FORMAT(1H0,I7,7X,G12.5)                                                   
 2020 FORMAT(////,5X,'RAMAIS COM VARIACAO DE TEMPERATURA',///,                  
     1            3X,'NUMERO DO',3X,'   DELTAT',8X,'DELTAT1',                   
     2            8X,'DELTAT2',/,3X,'  RAMAL')                                  
 2030 FORMAT(1H0,I7,4X,3G15.5)                                                  
 2040 FORMAT(////,5X,'NOS COM CARGAS CONCENTRADAS',///,                         
     1            3X,'NUMERO',21X,'FORCAS',38X,'MOMENTOS',/,                    
     2            3X,'DO  NO',8X,'FX',13X,'FY',13X,'FZ',13X,'MX',13X,           
     3        'MY',13X,'MZ')                                                    
 2050 FORMAT(1H0,I6,3X,6G15.5)                                                  
 2060 FORMAT(   ///,'DESLOCAMENTOS PRESCRITOS ASSOCIADOS A CASOS DE CA',        
     1       'RREGAMENTOS',//,                                                  
     2'     NUMERO DO      NUMERO DO      DESLOCAMENTO     ROTACAO'             
     3,/,'       GRUPO        ELEMENTO         PRESCRITO     PRESCRITA'         
     4    ,/)                                                                   
 2070 FORMAT(2I5,2F10.0)                                                        
 2080 FORMAT(/,I11,I15,9X,G12.5,G14.5)                                          
 2035 FORMAT(////,5X,'GRADIENTES TERMICOS EM NOS COM DESCONTINUIDADE',          
     1 ///,3X,'NUMERO DO',5X,'DELTAT1',8X,'DELTAT2',8X,'SIGMAD',/,              
     2  6X,'NO')                                                                
 2037 FORMAT(1H0,I7,4X,3G15.5)                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE FNETBR(R,PMAT,PSEC,IRUN,IMAT,ISEC,NERIG,LRIG,NELIB,            
     *           KODLIB,INCID,NRPI,PRESS,NRVT,TEMP,NOSDT,GTEMPN,ID)             
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      INTEGER FNODEQ,RIGROT,FORCAS                                              
      REAL CONV,GRAVID                                                          
      REAL *8 LRIG                                                              
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
      COMMON /EL/ IND,NPAR(20)                                                  
      COMMON /CARGAS/ ICARGA                                                    
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      DIMENSION AML(12),V(3),ROT(3,3),SM(12,12),PMAT(8,1),PSEC(7,1),            
     *          IRUN(1),IMAT(1),ISEC(1),NERIG(1),LRIG(2,1),NELIB(1),            
     *   KODLIB(12,1),INCID(2,1),NRPI(1),PRESS(1),NRVT(1),TEMP(3,1),            
     *   ID(6,1),R(1),ICARGA(10),NOSDT(1),GTEMPN(3,1),GRADT(6)                  
      EQUIVALENCE (ICARGA(1),NRP),(ICARGA(3),NRT),(ICARGA(6),INPP),             
     *           (NPAR(14),NELRIG),(NPAR(15),NELLIB),(NPAR(2),NUME),            
     *            (ICARGA(2),IHOOP),(ICARGA(4),NNDT)                            
      XPI=3.141592653                                                           
      DO 800 NE=1,NUME                                                          
      IRAMAL=IRUN(NE)                                                           
      NMAT=IMAT(NE)                                                             
      NSEC=ISEC(NE)                                                             
      DE=PSEC(1,NSEC)                                                           
      ESP=PSEC(2,NSEC)                                                          
      WL=PSEC(3,NSEC)                                                           
      ESPREV=PSEC(4,NSEC)                                                       
      WREV=PSEC(5,NSEC)                                                         
      WC=PSEC(6,NSEC)                                                           
      E=PMAT(1,NMAT)                                                            
      CP=PMAT(2,NMAT)                                                           
      COEXPT=PMAT(3,NMAT)                                                       
      DI=DE-2.*ESP                                                              
      AX=(DE**2-DI**2)*XPI/4.                                                   
      READ(RIGROT) SM,ROT,XL                                                    
      DO 1 I=1,12                                                               
      IF(I.LE.6) GRADT(I)=0.                                                    
    1 AML(I)=0.                                                                 
      SHOOP=0.                                                                  
C                                                                               
C     FORCAS NODAIS EQUIVALENTES DEVIDAS A PRESSAO INTERNA                      
C                                                                               
      IF(NRP.EQ.0) GO TO 100                                                    
      DO 2 IP=1,NRP                                                             
      IF( NRPI(IP) .NE. IRAMAL ) GO TO 2                                        
      CALL PRETBR(AX,PRESS(IP),DE,DI,CP,ESP,SHOOP,AML,IHOOP)                    
      GO TO 100                                                                 
    2 CONTINUE                                                                  
C                                                                               
C     FORCAS NODAIS EQUIVALENTES DEVIDAS A UMA VARIACAO DE TEMPERATURA          
C                                                                               
  100 IF(NRT.EQ.0) GO TO 150                                                    
      DO 3 IT=1,NRT                                                             
      IF( NRVT(IT) .NE. IRAMAL ) GO TO 3                                        
      CALL TEMTBR(COEXPT,E,AX,CP,TEMP(1,IT),AML,GRADT)                          
                                                                                
      GO TO 150                                                                 
    3 CONTINUE                                                                  
C                                                                               
C     TENSOES TERMICAS EM DESCONTINUIDADES                                      
C                                                                               
  150 IF(NNDT.EQ.0) GO TO 200                                                   
      DO 5 NT=1,NNDT                                                            
      DO 4 JT=1,2                                                               
      IF(NOSDT(NT).NE.INCID(JT,NE)) GO TO 4                                     
      IS=3*(JT-1)                                                               
      GRADT(IS+1)=E*COEXPT*GTEMPN(1,NT)/(2.*(1.-CP))                            
      GRADT(IS+2)=E*COEXPT*GTEMPN(2,NT)/(1.-CP)                                 
      GRADT(IS+3)=GTEMPN(3,NT)                                                  
      GO TO 200                                                                 
    4 CONTINUE                                                                  
    5 CONTINUE                                                                  
                                                                                
                                                                                
C                                                                               
C     FORCAS NODAIS EQUIVALENTES DEVIDAS AO PESO PROPRIO                        
C                                                                               
  200 IF(INPP.EQ.0) GO TO 300                                                   
      DREV=DE+2.*ESPREV                                                         
      AREV=(DREV**2-DE**2)*XPI/4.                                               
      PESO=(WL+WREV*AREV)                                                       
      PESOC=DI**2*XPI*WC*0.25                                                   
      CALL PPRTBR(PESO,PESOC,ROT,XL,NELRIG,NERIG,LRIG,AML,NE)                   
C                                                                               
C         MODIFICACAO DOS AML DEVIDA AS LIBERACOES                              
C                                                                               
 300  IF( NELLIB .LE. 0 ) GO TO 500                                             
      DO 400 I=1,NELLIB                                                         
      IF( NE .NE. NELIB(I) ) GO TO 400                                          
      CALL LIBER(I,SM,AML,KODLIB,2)                                             
      GO TO 500                                                                 
 400  CONTINUE                                                                  
 500  CONTINUE                                                                  
C                                                                               
C     MONTAGEM DOS AML NO VETOR GLOBAL E GRAVACAO EM FNODEQ                     
C                                                                               
      WRITE(FNODEQ)AML,SHOOP,GRADT                                              
      DO 320 KJ=1,2                                                             
      KK=(KJ-1)*6                                                               
      NP=INCID(KJ,NE)                                                           
      DO 315 K=1,2                                                              
      IS=(K-1)*3                                                                
      IK=KK+IS                                                                  
      DO 310 LI=1,3                                                             
      II=LI+IS                                                                  
      KI=ID(II,NP)                                                              
      IF(KI)310,310,301                                                         
  301 V(LI)=0.                                                                  
      DO 305 J=1,3                                                              
  305 V(LI)=V(LI)-ROT(J,LI)*AML(IK+J)                                           
      R(KI)=R(KI)+V(LI)                                                         
  310 CONTINUE                                                                  
  315 CONTINUE                                                                  
  320 CONTINUE                                                                  
 800  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PRETBR(AX,PI,DE,DI,CP,ESP,SHOOP,AML,IHOOP)                     
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION AML(12)                                                         
      IF(IHOOP.GE.1) GO TO 1                                                    
      CPI=AX*PI*(DE+DI)*(1.-2.*CP)/(8.*ESP)                                     
      AML(1)=AML(1)-CPI                                                         
      AML(7)=AML(7)+CPI                                                         
C                                                                               
    1 SHOOP=PI*DE/(2.*ESP)                                                      
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE TEMTBR(COEXPT,E,AX,CP,DELT,AML,GRADT)                          
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION AML(12),GRADT(6),DELT(3)                                        
      CVT=-COEXPT*E*AX*DELT(1)                                                  
      AML(1)=AML(1)-CVT                                                         
      AML(7)=AML(7)+CVT                                                         
      GRADT(1)=E*COEXPT*DELT(2)/(2.*(1.-CP))                                    
      GRADT(2)=E*COEXPT*DELT(3)/(1.-CP)                                         
      GRADT(4)=GRADT(1)                                                         
      GRADT(5)=GRADT(2)                                                         
C     WRITE(6,100)COEXPT,E,AX,DELT,AML                                          
 100  FORMAT(4E15.7,/)                                                          
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PPRTBR(PESO,PESOC,ROT,XL,NELRIG,NERIG,LRIG,AML,NE)             
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION AML(12),LRIG(2,1),NERIG(1),ROT(3,3)                             
      REAL CONV,GRAVID,GX,GY,GZ                                                 
      REAL *8 L1,L2,LRIG                                                        
      COMMON /VAR/ NG,MODEX,CONV,GRAVID,GX,GY,GZ                                
C                                                                               
C     COMPONENTES DO VETOR DE PESO PROPRIO NO SISTEMA LOCAL                     
C                                                                               
      CPX=(ROT(1,1)*GX + ROT(1,2)*GY + ROT(1,3)*GZ)* PESO                       
      CPY=(ROT(2,1)*GX + ROT(2,2)*GY + ROT(2,3)*GZ)*(PESO+PESOC)                
      CPZ=(ROT(3,1)*GX + ROT(3,2)*GY + ROT(3,3)*GZ)*(PESO+PESOC)                
C                                                                               
C     VERIFICACAO DA EXISTENCIA DE EXTREMIDADE RIGIDA                           
C                                                                               
      L1=0.                                                                     
      L2=0.                                                                     
      IF(NELRIG.LT.1) GO TO 5                                                   
      DO 1 I=1,NELRIG                                                           
      IF(NERIG(I).EQ.NE) GO TO 3                                                
    1 CONTINUE                                                                  
      GO TO 5                                                                   
    3 L1=LRIG(1,I)                                                              
      L2=LRIG(2,I)                                                              
      XL=XL-L1-L2                                                               
    5 CONTINUE                                                                  
C                                                                               
C     COMPONENTES DO VETOR DE FORCAS NODAIS, NO SISTEMA LOCAL                   
C                                                                               
      XL1=L1+XL/2.                                                              
      XL2=L2+XL/2.                                                              
      AML(1)=AML(1)-XL1*CPX                                                     
      AML(7)=AML(7)-XL2*CPX                                                     
      AML(2)=AML(2)-XL1*CPY                                                     
      AML(8)=AML(8)-XL2*CPY                                                     
      AML(6)=AML(6)-(XL1*XL1/2.-XL*XL/24.)*CPY                                  
      AML(12)=AML(12)+(XL2*XL2/2.-XL*XL/24.)*CPY                                
      AML(3)=AML(3)-XL1*CPZ                                                     
      AML(9)=AML(9)-XL2*CPZ                                                     
      AML(5)=AML(5)-(XL1*XL1/2.-XL*XL/24.)*CPZ                                  
      AML(11)=AML(11)+(XL2*XL2/2.-XL*XL/24.)*CPZ                                
C     WRITE(6,100)PESO,PESOC,AML                                                
 100  FORMAT(2E15.7,/(6E15.7,/))                                                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FNETBC(R,PMAT,PSEC,IRUN,IMAT,ISEC,RAIO,NELIB,KODLIB,           
     *           INCID,NRPI,PRESS,NRVT,TEMP,NOSDT,GTEMPN,ID)                    
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      INTEGER RIGROT,FORCAS,FNODEQ                                              
      REAL CONV,GRAVID                                                          
      REAL *8 IP                                                                
      COMMON /EL/ IND,NPAR(20)                                                  
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ           
      COMMON /CARGAS/ ICARGA                                                    
      COMMON /VAR/ NG,MODEX,CONV,GRAVID                                         
      DIMENSION AML(12),V(3),ICARGA(10),R(1),SM(12,12),PMAT(8,1),               
     *          PSEC(7,1),IRUN(1),IMAT(1),ISEC(1),RAIO(1),NRPI(1),              
     *          PRESS(1),NRVT(1),TEMP(3,1),NELIB(1),KODLIB(12,1),               
     *  INCID(2,1),ID(6,1),ROT(3,3),NOSDT(1),GTEMPN(3,1),GRADT(6)               
      EQUIVALENCE (ICARGA(1),NRP),(ICARGA(3),NRT),(ICARGA(6),INPP),             
     1          (NPAR(15),NELLIB),(NPAR(2),NUME),(ICARGA(2),IHOOP),             
     2            (ICARGA(4),NNDT)                                              
C                                                                               
      XPI=3.141592653                                                           
      DO 800 NE=1,NUME                                                          
      IRAMAL=IRUN(NE)                                                           
      NMAT=IMAT(NE)                                                             
      NSEC=ISEC(NE)                                                             
      RC=RAIO(NE)                                                               
      DE=PSEC(1,NSEC)                                                           
      ESP=PSEC(2,NSEC)                                                          
      WL=PSEC(3,NSEC)                                                           
      ESPREV=PSEC(4,NSEC)                                                       
      WREV=PSEC(5,NSEC)                                                         
      WC=PSEC(6,NSEC)                                                           
      E=PMAT(1,NMAT)                                                            
      CP=PMAT(2,NMAT)                                                           
      COEXPT=PMAT(3,NMAT)                                                       
C                                                                               
C                                                                               
      DO 1 I=1,12                                                               
      IF(I.LE.6) GRADT(I)=0.                                                    
    1 AML(I)=0.                                                                 
      SHOOP=0.                                                                  
C                                                                               
      READ(RIGROT)SM,ROT,XL,FI2,FRAC,FP,FO                                      
C                                                                               
C     MONTAGEM DOS AML DEVIDOS A PRESSAO INTERNA                                
C                                                                               
      IF(NRP.EQ.0) GO TO 100                                                    
      DO 2 KP=1,NRP                                                             
      IF( NRPI(KP) .NE. IRAMAL ) GO TO 2                                        
      CALL PRETBC(SM,FI2,PRESS(KP),RC,DE,ESP,E,CP,SHOOP,AML,IHOOP)              
    2 CONTINUE                                                                  
C                                                                               
C     MONTAGEM DOS AML DEVIDOS A UMA VARIACAO DE TEMPERATURA                    
C                                                                               
  100 IF(NRT.EQ.0) GO TO 150                                                    
      DO 3 IT=1,NRT                                                             
      IF( NRVT(IT) .NE. IRAMAL ) GO TO 3                                        
      CALL TEMTBC(COEXPT,E,CP,TEMP(1,IT),XL,SM,AML,GRADT)                       
    3 CONTINUE                                                                  
C                                                                               
C     TENSOES TERMICAS EM DESCONTINUIDADES                                      
C                                                                               
  150 IF(NNDT.EQ.0) GO TO 200                                                   
      DO 5 NT=1,NNDT                                                            
      DO 4 JT=1,2                                                               
      IF(NOSDT(NT).NE.INCID(JT,NE)) GO TO 4                                     
      IS=3*(JT-1)                                                               
      GRADT(IS+1)=E*COEXPT*GTEMPN(1,NT)/(2.*(1.-CP))                            
      GRADT(IS+2)=E*COEXPT*GTEMPN(2,NT)/(1.-CP)                                 
      GRADT(IS+3)=GTEMPN(3,NT)                                                  
      GO TO 200                                                                 
    4 CONTINUE                                                                  
    5 CONTINUE                                                                  
C                                                                               
C     MONTAGEM DOS AML DEVIDOS AO PESO PROPRIO                                  
C                                                                               
  200 IF(INPP.EQ.0) GO TO 300                                                   
      AX=.25*XPI*(DE**2-(DE-2.*ESP)**2)                                         
      IP=.5*XPI*((.5*DE)**4-(.5*DE-ESP)**4)                                     
      EAX2=2.*E*AX                                                              
      EI2=E*IP                                                                  
      GAR2=FRAC*(1.+CP)/(E*AX)                                                  
      GIP2=E*IP/(1.+CP)                                                         
      PESO=WL+WREV*XPI*ESPREV*(DE+ESPREV)                                       
      PESOC=XPI*WC*(.5*DE-ESP)**2                                               
      CALL PPRTBC(PESO,PESOC,SM,ROT,XL,FI2,FP,FO,EAX2,EI2,GAR2,GIP2,AML,        
     *RC)                                                                       
C                                                                               
C     MONTAGEM DOS AML NO VETOR GLOBAL E GRAVACAO EM FNODEQ                     
C                                                                               
  300 CONTINUE                                                                  
      WRITE(FNODEQ)AML,SHOOP,GRADT                                              
      DO 320 KJ=1,2                                                             
      KK=(KJ-1)*6                                                               
      NP=INCID(KJ,NE)                                                           
      DO 315 K=1,2                                                              
      IS=(K-1)*3                                                                
      IK=KK+IS                                                                  
      DO 310 LI=1,3                                                             
      II=LI+IS                                                                  
      KI=ID(II,NP)                                                              
      IF(KI)310,310,301                                                         
  301 V(LI)=0.                                                                  
      DO 305 J=1,3                                                              
  305 V(LI)=V(LI)-ROT(J,LI)*AML(IK+J)                                           
      R(KI)=R(KI)+V(LI)                                                         
  310 CONTINUE                                                                  
  315 CONTINUE                                                                  
  320 CONTINUE                                                                  
 800  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PRETBC(SR,FI2,PI,R,DE,ESP,E,CP,SHOOP,AML,IHOOP)                
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION AML(12),SR(12,12)                                               
      SIN(X)=DSIN(X)                                                            
      COS(X)=DCOS(X)                                                            
      IF(IHOOP.GE.1) GO TO 1                                                    
C                                                                               
      FI=2.*FI2                                                                 
      SIG=.25*PI*(DE-ESP)/(E*ESP)                                               
      SF2=SIN(FI2)                                                              
      CF2=COS(FI2)                                                              
C                                                                               
      DP7=R*SIG*(FI*CF2-(FI*CF2+2.*SF2)*CP)                                     
      DP8=-R*SIG*FI*SF2*(1.-CP)                                                 
      DP12=-SIG*FI*(1.-CP)                                                      
      AUX1=-(SR(7,7)*DP7+SR(7,8)*DP8+SR(7,12)*DP12)                             
      AUX2=-(SR(8,7)*DP7+SR(8,8)*DP8+SR(8,12)*DP12)                             
      AUX3=-(SR(12,7)*DP7+SR(12,8)*DP8+SR(12,12)*DP12)                          
C                                                                               
C     MONTAGEM DOS AML                                                          
C                                                                               
      AML(7)=AML(7)+AUX1                                                        
      AML(8)=AML(8)+AUX2                                                        
      AML(12)=AML(12)+AUX3                                                      
      AML(1)=AML(1)-AUX1                                                        
      AML(2)=AML(2)+AUX2                                                        
      AML(6)=AML(6)-AUX3                                                        
C                                                                               
    1 SHOOP=PI*DE/(2.*ESP)                                                      
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE TEMTBC(COEXPT,E,CP,DELT,XL,SR,AML,GRADT)                       
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION AML(12),AUX(6),SR(12,12),GRADT(6),DELT(3)                       
      DL1=COEXPT*DELT(1)*XL                                                     
      DO 10 J=7,12                                                              
      AUX(J-6)=-SR(J,7)*DL1                                                     
      AML(J-6)=AML(J-6)-AUX(J-6)                                                
      AML(J)=AML(J)+AUX(J-6)                                                    
   10 CONTINUE                                                                  
      AML(5)=AML(5)-AUX(3)*XL                                                   
      AML(6)=AML(6)+AUX(2)*XL                                                   
      GRADT(1)=E*COEXPT*DELT(2)/(2.*(1.-CP))                                    
      GRADT(2)=E*COEXPT*DELT(3)/(1.-CP)                                         
      GRADT(4)=GRADT(1)                                                         
      GRADT(5)=GRADT(2)                                                         
C     WRITE(6,100)AML                                                           
 100  FORMAT(' TEMTBC ',(6E15.7))                                               
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE PPRTBC(PESO,PESOC,SR,ROT,XL,FI2,FP,FO,EAX2,EI2,GAR2,           
     *                  GIP2,AML,R)                                             
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL CONV,GRAVID,GX,GY,GZ                                                 
      COMMON /VAR/ NG,MODEX,CONV,GRAVID,GX,GY,GZ                                
      DIMENSION DL(6),ROT(3,3),SR(12,12),AML(12),AUX(6)                         
      SIN(X)=DSIN(X)                                                            
      COS(X)=DCOS(X)                                                            
C                                                                               
C     COMPONENTES DO VETOR DE PESO PROPRIO NO SISTEMA LOCAL                     
C                                                                               
      CPX=-(ROT(1,1)*GX+ROT(1,2)*GY+ROT(1,3)*GZ)*PESO                           
      CPY=-(ROT(2,1)*GX+ROT(2,2)*GY+ROT(2,3)*GZ)*PESO                           
      CPZ=(ROT(3,1)*GX+ROT(3,2)*GY+ROT(3,3)*GZ)*(PESO+PESOC)                    
      CPXC=-(ROT(1,1)*GX+ROT(1,2)*GY+ROT(1,3)*GZ)*PESOC                         
      CPYC=-(ROT(2,1)*GX+ROT(2,2)*GY+ROT(2,3)*GZ)*PESOC                         
C                                                                               
C                                                                               
      DO 1 K=1,6                                                                
    1 DL(K)=0.                                                                  
C                                                                               
      FI=2.*FI2                                                                 
      SFI=FI*FI                                                                 
      SF=SIN(FI)                                                                
      SSF=SF*SF                                                                 
      CF=COS(FI)                                                                
      SCF=CF*CF                                                                 
      SF2=SIN(FI2)                                                              
      SSF2=SF2*SF2                                                              
      CF2=COS(FI2)                                                              
      SCF2=CF2*CF2                                                              
C                                                                               
C                                                                               
      IF(CPX)10,11,10                                                           
C                                                                               
   10 DL(1)=CPX*R**2*((FI*SF-FI**2)*GAR2-(FI**2+FI*SF)/EAX2+FP*R                
     1**2*(8.*SF2**2-FI*SF-FI**2)/EI2)/2.                                       
C                                                                               
      DL(2)=CPX*R**2*((FI*CF-SF)*(1./EAX2 - GAR2 )+FP*R**2*(FI*CF               
     1+2.*FI-3.*SF)/EI2)/2.                                                     
C                                                                               
   11 IF(CPY)15,16,15                                                           
C                                                                               
   15 DL(1)=DL(1)+CPY*R**2*((FI*CF-SF)*(1./EAX2 - GAR2 )+FP*R**2*               
     1(9.*SF-3.*FI*CF-6.*FI)/EI2)/2.                                            
C                                                                               
      DL(2)=DL(2)+CPY*R**2*((FI*SF-FI**2)/EAX2-(FI*SF+FI**2)*GAR2+              
     1FP*R**2*(5.*FI*SF-16.*SSF2-SFI)/EI2)/2.                                   
C                                                                               
      DL(6)=4.*FP*CPY*R**3*(FI*CF2-2.*SF2)/EI2                                  
C                                                                               
   16 IF(CPZ)19,20,19                                                           
C                                                                               
   19 DL(3)=CPZ*R**4*((SFI+SSF-2.*FI*SF)/GIP2+FO*(2.-2.*CF-SSF)/                
     1EI2)+CPZ*(R*FI)**2*GAR2                                                   
C                                                                               
      DL(4)=CPZ*R**3*SF2*(SF-FI)*(1./GIP2-FO/EI2)                               
C                                                                               
      DL(5)=CPZ*R**3*((3.*FI*CF2-4.*SF2-SF*CF2)/GIP2+FO*(FI*CF2+                
     1SF*CF2-4.*SF2)/EI2)                                                       
C                                                                               
   20 IF(CPXC)21,25,21                                                          
C                                                                               
   21 DL(1)=DL(1)+CPXC*R**2*((SSF-SFI)/EAX2+(2.*FI*SF-SSF-SFI)                  
     1*GAR2  +FP*R**2*(2.*FI*SF-SFI-SSF)/EI2)/4.                                
C    2                                                                          
C                                                                               
      DL(2)=DL(2)+CPXC*R**2*((FI-2.*SF-SF*CF+2.*FI*CF)/EAX2+                    
     1(SF*CF-FI)*GAR2  +FP*R**2*(FI-2.*SF+2.*FI*CF-4.*FI*SSF2                   
     2+4.*SF*SSF2-SF*CF)/EI2)/4.                                                
C                                                                               
      DL(6)=DL(6)+FP*CPXC*R**3*(SF*SF2-FI*SF2)/EI2                              
C    1                                                                          
C                                                                               
   25 IF(CPYC)29,30,29                                                          
C                                                                               
   29 DL(1)=DL(1)+CPYC*R**2*((2.*FI*CF-2.*SF-FI+SF*CF)/(4.*EAX2)+               
     1(FI-SF*CF)*GAR2/4.  +FP*R**2*(10.*SF/3.-FI/2.-SF*CF/2.-                   
     24.*SF/3.+FI*CF-2.*FI*SCF2)/(2.*EI2))                                      
C    3                                                                          
C                                                                               
      DL(2)=DL(2)+CPYC*R**2*((SSF-SFI)/(4.*EAX2)-(SFI+2.*FI*SF+                 
     1SSF)*GAR2/4.+FP*R**2*(SSF/2.-SFI/2.-6.*SSF2+2.*CF*SSF2                    
     2+FI*SF)/(2.*EI2))                                                         
C    3                                                                          
C                                                                               
      DL(6)=DL(6)+FP*CPYC*R**3*(CF*SF2-3.*SF2+FI*CF2)/EI2                       
C    1                                                                          
C                                                                               
C                                                                               
C                                                                               
   30 CONTINUE                                                                  
C                                                                               
      AUX(1)=-(SR(7,7)*DL(1)+SR(7,8)*DL(2)+SR(7,12)*DL(6))                      
      AUX(2)=-(SR(8,7)*DL(1)+SR(8,8)*DL(2)+SR(8,12)*DL(6))                      
      AUX(6)=-(SR(12,7)*DL(1)+SR(12,8)*DL(2)+SR(12,12)*DL(6))                   
      AUX(3)=-(SR(9,9)*DL(3)+SR(9,10)*DL(4)+SR(9,11)*DL(5))                     
      AUX(4)=-(SR(10,9)*DL(3)+SR(10,10)*DL(4)+SR(10,11)*DL(5))                  
      AUX(5)=-(SR(11,9)*DL(3)+SR(11,10)*DL(4)+SR(11,11)*DL(5))                  
C                                                                               
C                                                                               
      XG=XL/2.                                                                  
      XGC=8.*R*SSF2*SF2/(1.-CF)/3.                                              
      YG=R*(2.*SF2-FI*CF2)/FI                                                   
      YGXC=R*(4.*SSF2*SF2/(3.*(FI-SF))-CF2)                                     
      QXC=R*CPXC*(FI-SF)/2.                                                     
      QX=FI*R*CPX                                                               
      QY=FI*R*(CPY+CPYC/2.)+R*CPYC*SF/2.                                        
      QYC=CPXC*R*(1.-CF)/4.                                                     
      QZ=CPZ*R*FI                                                               
C                                                                               
C                                                                               
      DO 41 M=7,12                                                              
      AML(M)=AML(M)+AUX(M-6)                                                    
   41 CONTINUE                                                                  
C                                                                               
      AML(1)=AML(1)-(AUX(1)-QX-QXC)                                             
      AML(2)=AML(2)-(AUX(2)-QY)                                                 
      AML(3)=AML(3)-(AUX(3)+QZ)                                                 
      AML(4)=AML(4)-(AUX(4)+QZ*YG)                                              
      AML(5)=AML(5)-(AUX(5)-QZ*XG-AUX(3)*XL)                                    
      AML(6)=AML(6)-(AUX(6)+QX*YG+QXC*YGXC-QY*XG-QYC*XGC+AUX(2)*XL)             
C                                                                               
C     WRITE(6,100)CPX,CPY,CPZ,CPXC,CPYC,AML                                     
 100  FORMAT(' PPRTBC ',5E20.7,//,(6E15.7))                                     
      RETURN                                                                    
      END                                                                       
      SUBROUTINE TBCOMB(FLOC,LCOM,NUCOM,LID,K,KOD)                              
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT                                        
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,              
     *           NLCDIN,LCASE,NLCOMB                                            
      DIMENSION FLOC(7,1),LCOM(7,1),NUCOM(1)                                    
      GO TO (1,2),KOD                                                           
 1    WRITE(IOUT,110)                                                           
      DO 100 LID=1,NLCOMB                                                       
      READ(IIN,120) NUCOM(LID),(LCOM(I,LID),FLOC(I,LID),I=1,7)                  
      IK=NUCOM(LID)                                                             
      WRITE(IOUT,130)LID,NUCOM(LID),(LCOM(J,LID),FLOC(J,LID),J=1,IK)            
  100 CONTINUE                                                                  
      GO TO 105                                                                 
    2 K=NUCOM(LID)                                                              
      IF(K.EQ.0) RETURN                                                         
      WRITE(IOUT,110)                                                           
      WRITE(IOUT,130)LID,NUCOM(LID),(LCOM(J,LID),FLOC(J,LID),J=1,K)             
C                                                                               
C                                                                               
  105 CONTINUE                                                                  
C                                                                               
  110 FORMAT(////,33X,'C O M B I N A C O E S  D E  C A R R E G A M E N T        
     * O S',///,                                                                
     *3X,'NUM. DA  NUM. DE CARREG. FATOR CARREG. FATOR CARREG. FATOR CAR        
     *REG. FATOR CARREG. FATOR CARREG. FATOR CARREG. FATOR',/,                  
     *3X,' COMB.   CARREG. NUMERO',8X,'NUMERO',8X,'NUMERO',8X,'NUMERO',8        
     *X,'NUMERO',8X,'NUMERO',8X,'NUMERO')                                       
C                                                                               
  120 FORMAT(I5,7(I5,F5.0))                                                     
  130 FORMAT(1H0,I6,I9,3X,7(I4,4X,F6.3))                                        
C                                                                               
      RETURN                                                                    
      END                                                                       
      SUBROUTINE COMDES(V,AA,FLOC,LCOM,NUCOM,NLCEST,NLCTOT,NLCOMB,NEQ)          
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C     ROTINA PARA COMBINACAO DE DESLOCAMENTOS                                   
C                                                                               
      COMMON/TAPES/IELMNT,ILOAD                                                 
      DIMENSION AA(1),FLOC(7,1),V(NEQ),LCOM(7,1),NUCOM(1)                       
C                                                                               
      DO 200 I=1,NLCTOT                                                         
      II=(I-1)*NEQ+1                                                            
      IJ=II+NEQ-1                                                               
 200  READ(ILOAD)(AA(J),J=II,IJ)                                                
      REWIND ILOAD                                                              
      DO 500 LID=1,NLCOMB                                                       
      DO 1 N=1,NEQ                                                              
    1 V(N)=0.                                                                   
      NLCAS=NUCOM(LID)                                                          
      DO 100 IK=1,NLCAS                                                         
      LCAS=LCOM(IK,LID)                                                         
      FAT=FLOC(IK,LID)                                                          
      IF(NLCAS.EQ.1.AND.FAT.EQ.1.) GO TO 499                                    
      IJ=(LCAS-1)*NEQ                                                           
      IF(LCAS.GT.NLCEST) GO TO 40                                               
      DO 10 IS=1,NEQ                                                            
      II=IJ+IS                                                                  
      V(IS)=V(IS)+FAT*AA(II)                                                    
   10 CONTINUE                                                                  
      GO TO 100                                                                 
   40 CONTINUE                                                                  
      DO 50 IS=1,NEQ                                                            
      II=IJ+IS                                                                  
      X=AA(II)                                                                  
      Y=V(IS)                                                                   
      V(IS)=V(IS)+FAT*DSIGN(X,Y)                                                
   50 CONTINUE                                                                  
  100 CONTINUE                                                                  
      WRITE(ILOAD)V                                                             
      GO TO 500                                                                 
  499 NUCOM(LID)=0                                                              
  500 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE COMTEN(V,AA,FLOC,LCOM,NUCOM,NS,NM,NPART,NLCEST,                
     *           NLCTOT,NLCOMB,NEQT,NG)                                         
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C     ROTINA PARA COMBINACAO DE TENSOES                                         
C                                                                               
      INTEGER RIGROT,FORCAS,FNODEQ                                              
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ             
      DIMENSION AA(1),FLOC(7,1),V(NEQT),LCOM(7,1),NS(1),NM(1),                  
     *          NPART(1),P(15),NUCOM(1)                                         
      SIGN(X,Y)=DSIGN(X,Y)                                                      
      IJ=0                                                                      
      IK=0                                                                      
      DO 3 N=1,NLCTOT                                                           
      DO 2 NU=1,NG                                                              
      READ(FORCAS) NS(NU),NM(NU),NPART(NU)                                      
      NUME=NM(NU)                                                               
      DO 2 NN=1,NUME                                                            
      IJ=IK+1                                                                   
      IK=IK+NS(NU)                                                              
      READ(FORCAS)(AA(I),I=IJ,IK)                                               
 2    CONTINUE                                                                  
 3    CONTINUE                                                                  
      REWIND FNODEQ                                                             
      REWIND FORCAS                                                             
      DO 500 LID=1,NLCOMB                                                       
      IJ=0                                                                      
      IK=0                                                                      
      DO 1 N=1,NEQT                                                             
    1 V(N)=0.                                                                   
      NLCAS=NUCOM(LID)                                                          
      DO 100 LK=1,NLCAS                                                         
      LCAS=LCOM(LK,LID)                                                         
      FAT=FLOC(LK,LID)                                                          
      IF(NLCAS.EQ.1.AND.FAT.EQ.1.) NUCOM(LID)=0                                 
      LJ=(LCAS-1)*NEQT                                                          
      IF(LCAS.GT.NLCEST) GO TO 40                                               
      DO 10 IS=1,NEQT                                                           
      II=LJ+IS                                                                  
      V(IS)=V(IS)+FAT*AA(II)                                                    
   10 CONTINUE                                                                  
      GO TO 100                                                                 
   40 CONTINUE                                                                  
      DO 50 IS=1,NEQT                                                           
      II=LJ+IS                                                                  
      X=AA(II)                                                                  
      Y=V(IS)                                                                   
      V(IS)=V(IS)+FAT*SIGN(X,Y)                                                 
   50 CONTINUE                                                                  
  100 CONTINUE                                                                  
      DO 5 NU=1,NG                                                              
      NUME=NM(NU)                                                               
      NPR=NPART(NU)                                                             
      IF(NPR.GE.1) GO TO 16                                                     
      DO 18 NN=1,NUME                                                           
      IJ=IK+1                                                                   
      IK=IK+NS(NU)                                                              
      WRITE(FORCAS)(V(I),I=IJ,IK)                                               
 18   CONTINUE                                                                  
      GO TO 5                                                                   
   16 WRITE(FNODEQ)NPR,NUME                                                     
      IF(NPR.EQ.2) GO TO 17                                                     
      DO 15 L=1,NUME                                                            
      IJ=IK+1                                                                   
      IK=IK+NS(NU)                                                              
      P(1)=(V(IJ+3)**2+V(IJ+4)**2+V(IJ+5)**2)**0.5                              
      P(8)=(V(IJ+9)**2+V(IJ+10)**2+V(IJ+11)**2)**0.5                            
      P(2)=V(IJ+3)                                                              
      P(3)=V(IJ+4)                                                              
      P(4)=V(IJ+5)                                                              
      P(5)=V(IJ+13)                                                             
      P(6)=V(IJ+14)                                                             
      P(7)=V(IJ+15)                                                             
      P(9)=V(IJ+9)                                                              
      P(10)=V(IJ+10)                                                            
      P(11)=V(IJ+11)                                                            
      P(12)=V(IJ+16)                                                            
      P(13)=V(IJ+17)                                                            
      P(14)=V(IJ+18)                                                            
      P(15)=V(IJ+12)                                                            
      WRITE(FNODEQ)P                                                            
      WRITE(FORCAS)(V(I),I=IJ,IK)                                               
   15 CONTINUE                                                                  
      GO TO 5                                                                   
   17 DO 19 L=1,NUME                                                            
      IJ=IK+1                                                                   
      IK=IK+NS(NU)                                                              
      P(1)=(V(IJ+3)**2+V(IJ+4)**2+V(IJ+5)**2)**0.5                              
      P(2)=(V(IJ+9)**2+V(IJ+10)**2+V(IJ+11)**2)**0.5                            
      P(3)=V(IJ+12)                                                             
      WRITE(FNODEQ)(P(I),I=1,3)                                                 
   19 WRITE(FORCAS)(V(KT),KT=IJ,IK)                                             
    5 CONTINUE                                                                  
  500 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DWRITE(V,LCOM,NS,NM,NG,LID,NEQT)                               
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C                                                                               
C                                                                               
      INTEGER RIGROT,FORCAS                                                     
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS                  
      DIMENSION V(NEQT),LCOM(7,1),NS(1),NM(1)                                   
      IJ=0                                                                      
      IK=0                                                                      
      DO 5 NU=1,NG                                                              
      NUME=NM(NU)                                                               
      DO 5 NN=1,NUME                                                            
      IJ=IK+1                                                                   
      IK=IK+NS(NU)                                                              
      READ(FORCAS)(V(I),I=IJ,IK)                                                
 5    CONTINUE                                                                  
      WRITE(IOUT,1000)LID,LCOM(1,LID)                                           
 1000 FORMAT(//,1X,120('-')//,12X,'A  C O M B I N A C A O ',I4,                 
     1       '  E  R E P E T I C A O  D O  C A R R E G A M E N T O  ',          
     2       'N U M E R O',I4,//,1X,120('-'))                                   
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MODSIG(PERIOD,FREQHZ,TMIN,IFQ,NROOT,NMODOS,X)                  
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /KABEC/ HED(20),NPAG,LINHA                                         
C                                                                               
C         DETERMINA O NUMERO DE MODOS A SEREM INCLUIDOS NA ANALISE ESPECTRAL    
C                                                                               
      DIMENSION FREQHZ(1),PERIOD(1),X(1)                                        
      CALL KABECA                                                               
      WRITE(IOUT,2000)IFQ,TMIN                                                  
2000  FORMAT(//,10X,'IFQ= ',I1,//,                                              
     1       10X,'(EQ. 0 PERIODOS FORNECIDOS EM SEG.)',/                        
     2       10X,'(EQ. 1 FREQUENCIAS DADAS EM HZ.   )',                         
     3     ////,10X,'TMIN (FMAX) = ',E10.4)                                     
      NMODOS=0                                                                  
      IF( IFQ .EQ. 1 ) GO TO 2                                                  
 1    DO 10 I=1,NROOT                                                           
      X(I)=FREQHZ(I)                                                            
      IF( PERIOD(I) .LT. TMIN ) GO TO 10                                        
      NMODOS=NMODOS+1                                                           
 10   CONTINUE                                                                  
      GO TO 3                                                                   
 2    FMAX=TMIN                                                                 
      IF(FMAX)22,22,24                                                          
 22   WRITE(IOUT,2010)                                                          
 2010 FORMAT(///,'FREQUENCIA MAXIMA (FMAX) NULA',                               
     1        //,'PRECESSAMENTO INTERROMPIDO.')                                 
      CALL CLOSEFILES                                                                      
 24   DO 20 I=1,NROOT                                                           
      X(I)=FREQHZ(I)                                                            
      IF( FREQHZ(I) .GT. FMAX ) GO TO 20                                        
      NMODOS=NMODOS+1                                                           
 20   CONTINUE                                                                  
 3    IF( NMODOS .GT. 0 ) GO TO 52                                              
      WRITE(IOUT,2060)                                                          
 2060 FORMAT(///,'NAO HA MODOS CUJA(O) FREQUENCIA(PERIODO) SEJA '               
     1,'INFERIOR(SUPERIOR) A(AO) MAXIMA(MINIMO).'//' PROCESSAMENTO'             
     2,' INTERROMPIDO.')                                                        
      CALL CLOSEFILES                                                                      
 52   IF( NMODOS .EQ. NROOT ) WRITE(IOUT,2070)                                  
 2070 FORMAT(///,'TODOS OS MODOS CALCULADOS SERAO USADOS NA RESPOSTA'           
     1,' ESPECTRAL',//,'POSSIBILIDADE DE EXISTENCIA DE MODOS '                  
     2,'SUPERIORES SIGNIFICATIVOS.')                                            
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LESPEC(X,SW,ISPEC,VSPEC,NMODOS,NSPEC,IFQ)                      
      IMPLICIT REAL *8(A-H,O-Z)                                                 
C                                                                               
C          LE ESPECTROS E DETERMINA VALORES ESPECTRAIS CORRESPONDENTES          
C                      AS FREQUENCIAS DA ESTRUTURA                              
C                                                                               
      COMMON /TAPES/IELMNT,ILOAD,IIN,IOUT                                       
      COMMON /VAR/ NG,MODEX                                                     
      COMMON /KABEC/HED(20),NPAG,LINHA                                          
      DIMENSION X(1),SW(NMODOS,NSPEC),ISPEC(2,1),VSPEC(2,1),NOME1(6),           
     *          NOME2(9)  
!     Estava assim:      
!      DATA NOME1/'   P','ERIO','DO  ','FREQ','UENC','IA  '/      
!     DATA NOME2/'DESL','OCAM','ENTO',' VEL','OCID','ADE ',' ACE','LERA'        
!     *           ,'CAO '/                                                       
      CHARACTER*4 NOME1
      CHARACTER*4 NOME2
!
      ATAN(XX)=DATAN(XX)                                                          
      ALOG(XX)=DLOG(XX)                                                           
      NOME1(1)='   P'
      NOME1(2)='ERIO'
      NOME1(3)='DO  '
      NOME1(4)='FREQ'
      NOME1(5)='UENC'
      NOME1(6)='IA  '
      NOME2(1)='DESL'
      NOME2(2)='OCAM'
      NOME2(3)='ENTO'
      NOME2(4)=' VEL'
      NOME2(5)='OCID'
      NOME2(6)='ADE '
      NOME2(7)=' ACE'
      NOME2(8)='LERA'
      NOME2(9)='CAO '
      DOISPI=.6283185307179586D+1                                               
      DO 20 I=1,NSPEC                                                           
      READ(IIN,1000)ISPEC(1,I),ISPEC(2,I),ILOG                                  
 1000 FORMAT(3I5)                                                               
      ILOG=ILOG+1                                                               
      K=3*IFQ+1                                                                 
      L=3*(ISPEC(1,I)-1)+1                                                      
      N=ISPEC(2,I)                                                              
      CALL KABECA                                                               
      WRITE(IOUT,2000)I,NOME1(K),NOME1(K+1),NOME1(K+2),                         
     *                  NOME2(L),NOME2(L+1),NOME2(L+2)                          
 2000 FORMAT(///'   ESPECTRO NUMERO',I5,///,5X,3A4,5X,3A4,//)                   
C                                                                               
C    OS PERIODOS DEVERAO SER FORNECIDOS EM SEG. OU                              
C    AS FREQUENCIAS EM HZ - AMBOS EM ORDEM CRESCENTE                            
C                                                                               
      READ(IIN,1010)(VSPEC(1,J),VSPEC(2,J),J=1,N)                               
 1010 FORMAT(8F10.0)                                                            
      WRITE(IOUT,2010)(J,VSPEC(1,J),VSPEC(2,J),J=1,N)                           
 2010 FORMAT(I5,2G15.7)                                                         
C--------------------- VERIF. DA CONSISTENCIA DO ESPECTRO LIDO                  
      DO 55 J=2,N                                                               
      IF( VSPEC(2,J) )54,54,52                                                  
 52   IF( VSPEC(1,J) .GT. VSPEC(1,J-1) ) GO TO 55                               
 54   WRITE(IOUT,2015)I                                                         
      CALL CLOSEFILES                                                                      
 55   CONTINUE                                                                  
      IF( VSPEC(2,1) )57,57,56                                                  
 56   IF( VSPEC(1,1) )57,57,58                                                  
 57   WRITE(IOUT,2015)I                                                         
      CALL CLOSEFILES                                                                      
 58   CONTINUE                                                                  
 2015 FORMAT(///,' ERRO NO ESPECTRO NO. ',I2,///,                               
     1           ' -ABSCISSAS NAO ESTAO EM ORDEM CRESCENTE OU',                 
     2          /' -ABSCISSAS REPETIDAS                    OU',                 
     3          /' -VALORES ESPECTRAIS NEGATIVOS OU NULOS')                     
      IF( MODEX .EQ. 0 ) GO TO 20                                               
      IF( IFQ .EQ. 1 ) GO TO 2                                                  
C--------------------- CONVERSAO DE PERIODOS P/ FREQUENCIAS (HZ)                
      NOP=N/2                                                                   
      MED=N-2*NOP                                                               
      DO 60 J=1,NOP                                                             
      JN=N-J+1                                                                  
      TEMP=1./VSPEC(1,J)                                                        
      VSPEC(1,J)=1./VSPEC(1,JN)                                                 
      VSPEC(1,JN)=TEMP                                                          
      TEMP=VSPEC(2,J)                                                           
      VSPEC(2,J)=VSPEC(2,JN)                                                    
      VSPEC(2,JN)=TEMP                                                          
 60   CONTINUE                                                                  
      IF( MED .NE. 0 ) VSPEC(1,NOP+1)=1./VSPEC(1,NOP+1)                         
C--------------------- INTERPOLACAO LINEAR                                      
 2    JI=1                                                                      
      DO 200 M=1,NMODOS                                                         
      XM=X(M)                                                                   
      IF( XM .LT. VSPEC(1,JI) ) GO TO 120                                       
 80   JF=JI+1                                                                   
      IF( JF .GT. N ) GO TO 120                                                 
      IF( XM .GE. VSPEC(1,JF) ) GO TO 100                                       
      GO TO (111,222),ILOG                                                      
C------------- INTERPOLACAO LINEAR DIRETA                                       
 111  ALFA=(VSPEC(2,JF)-VSPEC(2,JI))/(VSPEC(1,JF)-VSPEC(1,JI))                  
      SW(M,I)=VSPEC(2,JI)+ALFA*(XM-VSPEC(1,JI))                                 
      GO TO 200                                                                 
C------------- INTERPOLACAO LINEAR NUM GRAFICO LOG-LOG                          
 222  ALFA=ALOG(VSPEC(2,JF)/VSPEC(2,JI))/ALOG(VSPEC(1,JF)/VSPEC(1,JI))          
      SW(M,I)=VSPEC(2,JI)*(XM/VSPEC(1,JI))**ALFA                                
      GO TO 200                                                                 
 100  JI=JI+1                                                                   
      GO TO 80                                                                  
 120  SW(M,I)=0.                                                                
      WRITE(IOUT,2018)M,I                                                       
 2018 FORMAT(/,' FREQUENCIA DO MODO ',I2,' FORA DO DOMINIO DE DEFI',            
     *         'NICAO DO ESPECTRO NO. ',I2,//)                                  
 200  CONTINUE                                                                  
      IF( ILOG .EQ. 2 ) WRITE(IOUT,2020)                                        
 2020 FORMAT(//,'          INTERPOLACAO LINEAR NUM GRAFICO LOG-LOG'//)          
      WRITE(IOUT,2000)I,NOME1(4),NOME1(5),NOME1(6),                             
     *                  NOME2(L),NOME2(L+1),NOME2(L+2)                          
      WRITE(IOUT,2010)(M,X(M),SW(M,I),M=1,NMODOS)                               
 20   CONTINUE                                                                  
      IF( MODEX .EQ. 0 ) RETURN                                                 
C---------------------- FREQUENCIAS CONVERTIDAS PARA RAD/S                      
      DO 220 J=1,NMODOS                                                         
 220  X(J)=DOISPI*X(J)                                                          
      CALL KABECA                                                               
      DO 40 I=1,NSPEC                                                           
      IF( ISPEC(1,I) .EQ. 1) GO TO 40                                           
      K=3*IFQ + 1                                                               
      IEXP=ISPEC(1,I)-1                                                         
C---------------------- ESPECTRO DE DESLOCAMENTOS                               
      DO 400 M=1,NMODOS                                                         
 400  SW(M,I)=SW(M,I)/(X(M)**IEXP)                                              
      WRITE(IOUT,2000)I,NOME1(4),NOME1(5),NOME1(6),                             
     *                  NOME2(1),NOME2(2)  ,NOME2(3)                            
      WRITE(IOUT,2010)(M,X(M),SW(M,I),M=1,NMODOS)                               
 40   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE COMODO(Q,NEQ,NMODOS,U,FPM,X,TD,CSI,KOD,IOUT)                   
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      DIMENSION Q(NEQ,NMODOS),U(NEQ),FPM(NMODOS),X(NMODOS)                      
      ABS(XX)=DABS(XX)                                                            
      SQRT(XX)=DSQRT(XX)                                                          
C                                                                               
C          EFETUA COMBINACAO DAS RESPOSTAS MODAIS                               
C                                                                               
      DO 97 I=1,NEQ                                                             
 97   U(I)=0.                                                                   
      IF( KOD .GE. 1 .AND. KOD .LE. 9 ) GO TO 99                                
      WRITE(IOUT,980) KOD                                                       
 980  FORMAT(///,' CODIGO DE COMBINACAO MODAL INVALIDO ....',I5,                
     *        //,' PROCESSAMENTO INTERROMPIDO.')                                
      CALL CLOSEFILES                                                                      
 99   GO TO (1,2,3,4,5,6,7,8,5),KOD                                             
C                                                                               
C        SOMA ABSOLUTA - SABS -                                                 
C                                                                               
 1    DO 1000 J=1,NMODOS                                                        
      F=FPM(J)                                                                  
      DO 100 I=1,NEQ                                                            
 100  U(I)=U(I)+ABS(F*Q(I,J))                                                   
 1000 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C        RAIZ QUADRADA DA SOMA DOS QUADRADOS - RQSQ                             
C                                                                               
 2    DO 2000 J=1,NMODOS                                                        
      F2=FPM(J)**2                                                              
      DO 200 I=1,NEQ                                                            
 200  U(I)=U(I)+F2*Q(I,J)**2                                                    
 2000 CONTINUE                                                                  
      GO TO 9998                                                                
C                                                                               
C        RQSQ COM AGRUPAMENTOS DE CSF%                                          
C                                                                               
 3    IK=1                                                                      
      IG=1                                                                      
      CSF=1.1                                                                   
 303  DO 3000 IS=IK,NMODOS                                                      
      IF( IK .NE. IS ) GO TO 30                                                 
      F=1.                                                                      
      GO TO 33                                                                  
 30   IF( IS-IG )330,3030,333                                                   
 333  IF( X(IS)/X(IK) .GT. CSF ) GO TO 3030                                     
 330  F=2.                                                                      
C     WRITE(IOUT,1020)IK,IS                                                     
 33   DO 300 I=1,NEQ                                                            
 300  U(I)=U(I)+F*ABS(FPM(IK)*FPM(IS)*Q(I,IK)*Q(I,IS))                          
 3000 CONTINUE                                                                  
      IS=NMODOS+1                                                               
 3030 IG=IS                                                                     
      IK=IK+1                                                                   
      IF( IK .LE. NMODOS ) GO TO 303                                            
      GO TO 9998                                                                
C                                                                               
C        METODO DE 10%                                                          
C                                                                               
 4    DO 4000 IK=1,NMODOS                                                       
      DO 440 IS=IK,NMODOS                                                       
      IF( IK .NE. IS ) GO TO 40                                                 
      F=1.                                                                      
      GO TO 44                                                                  
 40   IF( (X(IS)-X(IK))/X(IK) .GT. 0.1 ) GO TO 4000                             
      F=2.                                                                      
C     WRITE(IOUT,1020)IK,IS                                                     
 44   DO 400 I=1,NEQ                                                            
 400  U(I)=U(I)+F*ABS(FPM(IK)*FPM(IS)*Q(I,IK)*Q(I,IS))                          
 440  CONTINUE                                                                  
 4000 CONTINUE                                                                  
      GO TO 9998                                                                
C                                                                               
C        SOMA DUPLA ( NRC OU ROSENBLUETH-ELOURDUY )                             
C                                                                               
 5    IF( CSI .GE. 0. .AND. TD .GT. 0. ) GO TO 50                               
      WRITE(IOUT,1050)CSI,TD                                                    
 1050 FORMAT(///,' VALOR(ES) FORNECIDO(S) INVALIDO(S)  CSI = ',G10.3            
     1        ,/,'                                      TD = ',G10.3            
     2       ,//,' EXECUCAO INTERROMPIDA.')                                     
      CALL CLOSEFILES                                                                      
 50   BK=CSI                                                                    
      BS=CSI                                                                    
      DO 5050 IK=1,NMODOS                                                       
      BLK=BK+2./(TD*X(IK))                                                      
      WLK=X(IK)*SQRT(1.-BK*BK)                                                  
      DO 5000 IS=IK,NMODOS                                                      
      IF( IK .NE. IS ) GO TO 55                                                 
        F=1.0                                                                   
      EKS=1.0                                                                   
      GO TO 500                                                                 
 55   WLS=X(IS)*SQRT(1.-BS*BS)                                                  
      BLS=BS+2./(TD*X(IS))                                                      
      EKS=1./(1.+((WLK-WLS)/(BLK*X(IK) + BLS*X(IS)))**2)                        
      F=2.0                                                                     
 500  IF( KOD .EQ. 9 ) GO TO 550                                                
      DO 505 I=1,NEQ                                                            
 505  U(I)=U(I)+F*ABS(FPM(IK)*FPM(IS)*Q(I,IK)*Q(I,IS)*EKS)                      
      GO TO 5000                                                                
 550  DO 555 I=1,NEQ                                                            
 555  U(I)=U(I)+F*FPM(IK)*FPM(IS)*Q(I,IK)*Q(I,IS)*EKS                           
 5000 CONTINUE                                                                  
 5050 CONTINUE                                                                  
      GO TO 9998                                                                
C                                                                               
C        VAL. MAX. + RQSQ DOS DEMAIS                                            
C                                                                               
 6    DO 6000 I=1,NEQ                                                           
      S=0.                                                                      
      RMAX=ABS(Q(I,1)*FPM(1))                                                   
      DO 600 J=1,NMODOS                                                         
      R=ABS(Q(I,J)*FPM(J))                                                      
      IF( R .GT. RMAX ) RMAX=R                                                  
 600  S=S+R*R                                                                   
      U(I)=RMAX+SQRT(S-RMAX*RMAX)                                               
 6000 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C         COMB. QUADRATICA COMPLETA - CQC -                                     
C                                                                               
 7    IF( CSI .GE. 0 ) GO TO 70                                                 
      WRITE(IOUT,1070) CSI                                                      
 1070 FORMAT(///,' VARIAVEL FORNECIDA COM VALOR INVALIDO   CSI = ',G10.3,       
     1        //,' EXECUCAO INTERROMPIDA.')                                     
      CALL CLOSEFILES                                                                      
 70   C=4.0*CSI*CSI                                                             
      DO 7000 IK=1,NMODOS                                                       
      DO 770  IS=IK,NMODOS                                                      
      IF( IK .NE. IS ) GO TO 77                                                 
        F=1.0                                                                   
      EKS=1.0                                                                   
      GO TO 700                                                                 
 77   R=X(IS)/X(IK)                                                             
      EKS=(2.*C*(1.+R)*R**1.5)/((1.-R*R)**2 + C*R*(1.+R)**2)                    
      F=2.0                                                                     
 700  DO 707 I=1,NEQ                                                            
 707  U(I)=U(I)+F*FPM(IK)*FPM(IS)*Q(I,IK)*Q(I,IS)*EKS                           
 770  CONTINUE                                                                  
 7000 CONTINUE                                                                  
      GO TO 9998                                                                
C                                                                               
C          ( SABS + RQSQ ) / 2                                                  
C                                                                               
 8    DO 88 J=1,NMODOS                                                          
      F2=FPM(J)**2                                                              
      DO 80 I=1,NEQ                                                             
 80   U(I)=U(I)+F2*Q(I,J)**2                                                    
 88   CONTINUE                                                                  
      DO 800 I=1,NEQ                                                            
 800  U(I)=SQRT(U(I))                                                           
      DO 888 J=1,NMODOS                                                         
      F=FPM(J)                                                                  
      DO 880 I=1,NEQ                                                            
 880  U(I)=U(I)+ABS(F*Q(I,J))                                                   
 888  CONTINUE                                                                  
      DO 8000 I=1,NEQ                                                           
 8000 U(I)=0.5*U(I)                                                             
      RETURN                                                                    
C                                                                               
 9998 DO 9999 I=1,NEQ                                                           
 9999 U(I)=SQRT(U(I))                                                           
      RETURN                                                                    
C                                                                               
 1020 FORMAT(2I10)                                                              
      END                                                                       
                                                                                
                                                                                
      SUBROUTINE MMODEF(MASSA,EK,FM,VQ,NROOT,ID,MAXA,FPM)                       
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL *8 MASSA,MK,MKJ                                                      
      COMMON /SOL/ NUMNP,NEQ,NMK,NUMEST,MIDSET,MAXEST,IMK,NWM                   
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
      COMMON /KABEC/HED(20),NPAG,LINHA                                          
      DIMENSION MASSA(1),EK(1),FM(1),VQ(1),ID(6,1),MAXA(1),FPM(6,1)             
C                                                                               
      DO 60 K=1,3                                                               
C                                                                               
C        MONTAGEM DO VETOR EK                                                   
C                                                                               
        DO 10 I=1,NEQ                                                             
           EK(I)=0. 
 10     CONTINUE 
        DO 20 I=1,NUMNP                                                           
           J=ID(K,I)                                                                 
           IF( J .NE. 0 ) EK(J)=1.                                                   
 20     CONTINUE                          
      WRITE(IOUT,1111)(EK(I),I=1,NEQ)
 1111 FORMAT(8(2X,E12.4))	  
C                                                                               
C       CALCULO DE MK=EKT*M*EK                                                 
C                                                                               
        CALL MULT(FM,MASSA,EK,MAXA,NEQ,NWM)
      WRITE(IOUT,1111)(FM(I),I=1,NEQ)
		
        MK=0.                                                                     
        DO 30 I=1,NEQ                                                             
           MK=MK+FM(I)*EK(I)    
 30     CONTINUE 
        IF( MK .GT. 0. ) GO TO 34                                                 
        DO 32 J=1,NROOT                                                           
           FPM(K,J)=0.0
 32     CONTINUE 
        GO TO 60  
C		
 34     CONTINUE   
      WRITE(IOUT,1111) MK 
C                                                                               
C       CALCULO DAS MASSAS MODAIS EFETIVAS                                     
C                                                                               
        DO 50 J=1,NROOT                                                           
           IVQ=(J-1)*NEQ + 1                                                         
           CALL MULT(FM,MASSA,VQ(IVQ),MAXA,NEQ,NWM)                                  
           MKJ=0.                                                                    
           DO 40 I=1,NEQ                                                             
              MKJ=MKJ+FM(I)*EK(I)  
 40        CONTINUE 
           FPM(K,J)=MKJ*MKJ/MK*100.                                                  
 50     CONTINUE                                                                  
 60   CONTINUE                                                                  
C                                                                               
C        VALORES ACUMULADOS                                                     
C                                                                               
      DO 80 K=1,3                                                               
         K3=K+3                                                                    
         FPM(K3,1)=FPM(K,1)                                                        
         DO 70 J=2,NROOT                                                           
            FPM(K3,J)=FPM(K3,J-1)+FPM(K,J)
 70      CONTINUE 
 80   CONTINUE  
C 
      CALL KABECA                                                               
      WRITE(IOUT,1000)                                                          
      WRITE(IOUT,2000)(J,(FPM(K,J),K=1,6),J=1,NROOT)                            
 1000 FORMAT(///,'            MASSAS MODAIS EFETIVAS (%)',///,                  
     1'  MODO         MX       MY        MZ',                                   
     2 15X,'MX ACM.   MY ACM.   MZ ACM.',//)                                     
 2000 FORMAT(I5,3X,3F10.3,10X,3F10.3)                                            
      RETURN                                                                    
      END                               
C	  
      SUBROUTINE MSE1(AA,NI,NR,NMS,ITAPE1,ITAPE2)                               
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST                                           
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     *          ,NEMSE                                                          
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT                                        
      COMMON A(1)                                                               
      DIMENSION AA(1),LABEL(4)                                                  
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT)                               
!     Era assim:      
!      DATA LABEL/'SUBR','OTIN','A MS','E1  '/                            
      CHARACTER*4 LABEL
      LABEL(1)='SUBR'
      LABEL(2)='OTIN'
      LABEL(3)='A MS'
      LABEL(4)='E1  '
C                                                                               
C         AA     ... AREA DE DADOS DO GRUPO DE ELEM. DE MOLAS                   
C         NI     ... POINTER DE AA                                              
C         NR     ... POINTER  DA FUNCAO DE INFLUENCIA R                         
C         NMS    ... POINTER DE MSE                                             
C         ITAPE1 ... ARQ. COM AS FUNCOES DE INFLUENCIA                          
C         ITAPE2 ... ARQ. COM MSE                                               
C                                                                               
C--------------------------------------------------------------                 
C         LEITURA DAS PROP. DO GRUPO DE MOLAS E DEF. DE AREAS                   
C                                                                               
      REWIND IELMNT                                                             
      READ(IELMNT)NUMEST,NPAR,(AA(I),I=1,NUMEST)                                
      IF( NPAR(1) .EQ. 4 ) GO TO 1                                              
      WRITE(IOUT,100)                                                           
 100  FORMAT(///,' PRIMEIRO GRUPO DE ELEMENTOS NAO E O GRUPO COM',              
     1 ' AS MOLAS USADAS PARA CALCULO DAS FUNCOES DE INFLUENCIA.',              
     2        //,' PROCESSAMENTO INTERROMPIDO.')                                
      CALL CLOSEFILES                                                                      
 1    N101= NI                                                                  
      N204=N101+NUMMAT*ITWO                                                     
      N205=N204+NUMMAT*ITWO                                                     
      N206=N205+3*NUME*ITWO                                                     
      N207=N206+7*NUME                                                          
      N208=N207+  NUME                                                          
      N209=N208+6*NUME                                                          
      N210=N209+  NUME                                                          
      N211=N210+  NUME                                                          
 2    NLAST=N211                                                                
      IF( NLAST .GT. MTOT ) CALL ERROR(NLAST-MTOT,9,LABEL,2)                    
      CALL MSE2(A(NR),A(NMS),A(N101),A(N204),A(N205),A(N206),A(N208),           
     *                      A(N209),A(N210),NEMSE,ITAPE1,ITAPE2)                
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MSE2(R,MSE,DMOLA,RMOLA,XYZ,NNV,LM,MATP,IMSE,NEMSE,             
     *                ITAPE1,ITAPE2)                                            
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      REAL A                                                                    
      COMMON/SOL/NUMNP,NEQ,NWK                                                  
      COMMON/DIM/N1,N2,N3,N4                                                    
      COMMON/EL/IND,NPAR(20)                                                    
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT                                        
      COMMON/DEPURA/IDEPUR(15),INTSAI(15)                                       
      COMMON/CARGAS/ICARGA(10)                                                  
      COMMON/VAR/NG,MODEX                                                       
      COMMON A(1)                                                               
      DIMENSION IMSE(1),R(NEQ),MSE(NEMSE),DMOLA(1),RMOLA(1),XYZ(3,1)            
     *          ,NGENE(2,1),SSV(2,1),NNV(7,1),LM(6,1),MATP(1)                   
      EQUIVALENCE (NPAR(2),NUME)                                                
      IF( MODEX .EQ. 0 ) GO TO 5                                                
      NEQ1=NEQ+1                                                                
      CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,1)                             
      NG=1                                                                      
      IND=5                                                                     
      ICARGA(7)=1                                                               
 5    REWIND ITAPE1                                                             
      IF( ITAPE2 .GT. 0 ) REWIND ITAPE2                                         
      NGENE(1,1)=1                                                              
      SSV(1,1)=1.0                                                              
      SSV(2,1)=0.0                                                              
      L=0                                                                       
      DO 1000 I=1,NUME                                                          
      IF( IMSE(I) .NE. 1 ) GO TO 1000                                           
      NGENE(2,1)=I                                                              
      L=L+1                                                                     
      MSE(L)=I                                                                  
      DO 10 J=1,NEQ                                                             
 10   R(J)=0.                                                                   
      IF( MODEX .EQ. 0 ) GO TO 20                                               
      CALL CADMOL(R,DMOLA,RMOLA,XYZ,NGENE,SSV,NNV,LM,MATP)                      
      CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,2)                             
 20   WRITE(ITAPE1)R                                                            
      IF( MODEX .EQ. 0 .OR. INTSAI(6) .EQ. 0 ) GO TO 1000                       
      WRITE(IOUT,100) I                                                         
      CALL WRITE(R,A(N1),NEQ,NUMNP,1,0)                                         
 1000 CONTINUE                                                                  
      IF( ITAPE2 .GT. 0 ) WRITE(ITAPE2)MSE                                      
 100  FORMAT(1H1,//,'FUNCAO DE INFLUENCIA CORRESPONDENTE AO ELEMENTO'           
     *             ,' DE MOLA NO.',I3)                                          
      RETURN                                                                    
      END                                                                       
      SUBROUTINE MSE3(NMODOS,NSPEC,ID,MAXA,MASSA,Q,VQ,X,SW,FM,VFM,FPM,          
     *                FPMC,FPMA,U,R,MSE,FPMT,XT,TM,VTM,NS,NM,NPART,             
     *                NC,NAREA,UT)                                              
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      REAL A                                                                    
      REAL *8 MASSA                                                             
      DIMENSION HED(20),ID(6,1),MAXA(1),MASSA(NWM),Q(NEQ,NMODOS),VQ(1),         
     *    X(NC),SW(NMODOS,NSPEC),FM(NEQ,NMODOS),VFM(1),FPMT(NMODOS),            
     *   FPM(NMODOS,NAREA),FPMA(1),FPMC(1),U(1),R(NEQ,NEMSE),UT(1),             
     *   MSE(NEMSE),TM(NEQT,NMODOS),VTM(1),NS(1),NM(1),NPART(1),                
     *   XT(NMODOS),HED1(20)                                                    
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,NLCDIN        
     *          ,LCASE,NLCOMB,NOMASS,NOREDI,ISAVE,NIVMAX                        
      COMMON/DIM/N1,N2,N3,N4,N5,N6,N7                                           
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ,            
     *      KSTIFF,ESFMOD,MODOS,INFLU,IWORK                                     
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
     *         ,NEMSE                                                           
      COMMON/DEPURA/IDEPUR(15),INTSAI(15)                                       
      COMMON /KABEC/ HED,NPAG,LINHA                                             
      COMMON A(1)                                                               
      ABS(XX)=DABS(XX)                                                            
      SQRT(XX)=DSQRT(XX)                                                          
C                                                                               
C            CALCULO DE FM , PRODUTO DO AUTOVETOR PELA MAT. DE MASSA            
C                                                                               
      DO 10 I=1,NMODOS                                                          
      IFM=(I-1)*NEQ + 1                                                         
 10   CALL MULT(VFM(IFM),MASSA,VQ(IFM),MAXA,NEQ,NWM)                            
      DO 9000 LDIN=1,NLCDIN                                                     
      CALL KABECA                                                               
      READ(IIN,1010)LCASE,                                                      
     1              HED1,                                                       
     2              NIVEIS,KODMOD,KODDIR,TD,CSI                                 
 1010 FORMAT(I5/20A4/3I5,2F10.0)                                                
      WRITE(IOUT,2010) LDIN,HED1,NIVEIS,KODMOD,KODDIR,TD,CSI                    
C                                                                               
C            LEITURA DAS FUNCOES DE INFLUENCIA EM INFLU                         
C                                                                               
      REWIND INFLU                                                              
      DO 20 J=1,NEMSE                                                           
 20   READ(INFLU)(R(I,J),I=1,NEQ)                                               
      READ(INFLU)MSE                                                            
      IF( LDIN .NE. 1 ) READ(INFLU)MASSA,Q,X,SW,FM,FPM                          
      DO 4000 NIVEL=1,NIVEIS                                                    
C                                                                               
C            LEITURA DOS PARAMETROS DO NIVEL                                    
C                                                                               
      READ(IIN,1020)NE,ISPEC                                                    
 1020 FORMAT(2I5)                                                               
      WRITE(IOUT,2020)NIVEL,NE,ISPEC                                            
      DO 25 J=1,NEQ                                                             
 25   U(J)=0.                                                                   
      DO 60 I=1,NE                                                              
      READ(IIN,1030)NEL,FAT                                                     
 1030 FORMAT(I5,F10.0)                                                          
      WRITE(IOUT,2030)NEL,FAT                                                   
      DO 30 L=1,NEMSE                                                           
      IF( NEL .EQ. MSE(L) ) GO TO 40                                            
 30   CONTINUE                                                                  
      WRITE(IOUT,2040)LDIN,NEL                                                  
      CALL CLOSEFILES                                                                      
 40   CONTINUE                                                                  
 2040 FORMAT(///'  ERRO NO CARREGAMENTO DINAMICO NO.',I3,//,'  NAO FOI '        
     *       ,'CALCULADA A FUNCAO DE INFLUENCIA PARA O ELEMENTO NO.',I3)        
      DO 50 J=1,NEQ                                                             
 50   U(J)=U(J)+R(J,L)*FAT                                                      
 60   CONTINUE                                                                  
C                                                                               
C         IMPRESSAO DA FUNCAO DE INFLUENCIA DO NIVEL                            
C                                                                               
      IF( INTSAI(7) .NE. 1 ) GO TO 70                                           
      WRITE(IOUT,2050)NIVEL                                                     
      CALL WRITE(U,ID,NEQ,NUMNP,1,0)                                            
 2050 FORMAT(1H1,///,'  FUNCAO DE INFLUENCIA PARA O NIVEL NO.',I3)              
 70   CONTINUE                                                                  
C                                                                               
C          CALCULO DOS FATORES DE PARTICIPACAO MODAL                            
C                                                                               
      DO 90 J=1,NMODOS                                                          
      S=0.                                                                      
      DO 80 K=1,NEQ                                                             
 80   S=S+FM(K,J)*U(K)                                                          
      WRITE(IOUT,2080)J,S                                                       
      FPM(J,NIVEL)=S*SW(J,ISPEC)                                                
 90   WRITE(IOUT,2080)J,FPM(J,NIVEL)                                            
 2080 FORMAT(I5,G15.7)                                                          
 4000 CONTINUE                                                                  
C                                                                               
C          COMBINACAO DAS COMPONENTES EM NIVEL MODAL                            
C                                                                               
      GO TO (1000,2000),KODDIR                                                  
C------------------- POR SOMA ABSOLUTA                                          
 1000 DO 110 J=1,NMODOS                                                         
      S=0.                                                                      
      DO 100 L=1,NIVEIS                                                         
 100  S=S+ABS(FPM(J,L))                                                         
 110  FPMC(J)=S                                                                 
      GO TO 2500                                                                
C------------------- POR RQSQ                                                   
 2000 DO 210 J=1,NMODOS                                                         
      S=0.                                                                      
      DO 200 L=1,NIVEIS                                                         
 200  S=S+FPM(J,L)**2                                                           
 210  FPMC(J)=SQRT(S)                                                           
 2500 CONTINUE                                                                  
C                                                                               
C           COMBINACAO DAS RESPOSTAS MODAIS                                     
C                                                                               
C-------------------- DESLOCAMENTOS                                             
      CALL COMODO(Q,NEQ,NMODOS,U,FPMC,X,TD,CSI,KODMOD,IOUT)                     
      WRITE(ILOAD)(U(I),I=1,NEQ)                                                
      CALL WRITE(U,ID,NEQ,NUMNP,1,1)                                            
C-------------------- ACELERACOES                                               
      DO 300 J=1,NMODOS                                                         
 300  FPMA(J)=FPMC(J)*X(J)*X(J)                                                 
      CALL COMODO(Q,NEQ,NMODOS,U,FPMA,X,TD,CSI,KODMOD,IOUT)                     
      CALL WRITE(U,ID,NEQ,NUMNP,2,1)                                            
C                                                                               
C            SALVA VARIAVEIS EM INFLU                                           
C                                                                               
      IF( LDIN .EQ. 1 ) WRITE(INFLU)MASSA,Q,X,SW,FM,FPM                         
      DO 350 J=1,NMODOS                                                         
      FPMT(J)=FPMC(J)                                                           
 350  XT(J)=X(J)                                                                
C--------------------- ESFORCOS                                                 
C                                                                               
C           LEITURA DOS ESFORCOS MODAIS EM ESFMOD                               
C                                                                               
      IF( NGTC .LE. 0 ) GO TO 9000                                              
      REWIND ESFMOD                                                             
      IF=0                                                                      
      DO 400 J=1,NMODOS                                                         
      DO 400 N=1,NGTC                                                           
      READ(ESFMOD)NS(N),NM(N),NPART(N)                                          
      NUME=NM(N)                                                                
      NSTRES=NS(N)                                                              
      DO 400 I=1,NUME                                                           
      II=IF+1                                                                   
      IF=IF+NSTRES                                                              
 400  READ(ESFMOD)(VTM(K),K=II,IF)                                              
      CALL COMODO(TM,NEQT,NMODOS,UT,FPMT,XT,TD,CSI,KODMOD,IOUT)                 
C                                                                               
C           GRAVACAO DOS ESFORCOS EM FORCAS                                     
C           GRAVACAO DOS ESFORCOS EM IWORK PARA IMPRESSAO                       
C                                                                               
      REWIND IWORK                                                              
      IF=0                                                                      
      DO 500 N=1,NGTC                                                           
      NSTRES=NS(N)                                                              
      NUME=NM(N)                                                                
      WRITE(FORCAS)NSTRES,NUME,NPART(N)                                         
      DO 500 I=1,NUME                                                           
      II=IF+1                                                                   
      IF=IF+NSTRES                                                              
      WRITE(FORCAS)(UT(K),K=II,IF)                                              
 500  WRITE(IWORK) (UT(K),K=II,IF)                                              
C                                                                               
C           IMPRESSAO DOS ESFORCOS                                              
C                                                                               
      REWIND IWORK                                                              
      KODT=4                                                                    
      CALL STRESS(A(N5))                                                        
 9000 CONTINUE                                                                  
      RETURN                                                                    
 2010 FORMAT(//,' CARREGAMENTO DINAMICO NO. ',I2,1X,20A4,                       
     1      ///,' NUMERO DE NIVEIS              (NIVEIS)= ',I2,                 
     2        /,' TIPO DE COMBINACAO MODAL      (KODMOD)= ',I1,                 
     3        /,' TIPO DE COMBINACAO POR NIVEL  (KODDIR)= ',I1,                 
     4       //,' TEMPO DE DURACAO                  (TD)= ',G11.4,              
     5        /,' FRACAO DO AMORTECIMENTO CRITICO  (CSI)= ',G11.4 )             
 2020 FORMAT(//,' DIRECOES EXCITADAS NO NIVEL ',I2,' :',I3,                     
     1          '   ESPECTRO NO. ',I2,//,'    ELEM.',6X,'FATOR',//)             
 2030 FORMAT(4X,I2,4X,G11.4)                                                    
      END                                                                       
                                                                                
      SUBROUTINE IMAGEM(INFILE,IIN,IOUT)                                        
C                                                                               
C     INFILE = ARQ. COM TODOS OS DADOS DE ENTRADA                               
C     IIN    = ARQ. LIDO INTERNAMENTE PELO SISTEMA TUBO                         
C     IOUT   = ARQ. DE SAIDA DO SISTEMA TUBO                                    
C                                                                               
      CHARACTER*4 CARTAO
      DIMENSION CARTAO(20)                                                      
C     Estava assim:      
C     DATA FIM /'FIM '/                                                         
      CHARACTER*4 FIM 
C     Definicao de cartao foi inventada para concordar com a comparacao a frente
      FIM='FIM '
C
      LPPAG=57                                                                  
      NLINHA=7                                                                  
      WRITE(IOUT,1000)                                                          
      N=0                                                                       
 2    READ(INFILE,100,END=4) CARTAO                                             
      N=N+1                                                                     
      NLINHA=NLINHA+1                                                           
      IF( NLINHA .LE. LPPAG ) GO TO 3                                           
      NLINHA=8                                                                  
      WRITE(IOUT,999)                                                           
      WRITE(IOUT,1000)                                                          
 3    WRITE(IOUT,2000) N,CARTAO                                                 
      IF( CARTAO(1) .EQ. FIM ) GO TO 4                                          
      WRITE(IIN,100) CARTAO                                                     
      GO TO 2                                                                   
 4    WRITE(IOUT,999)                                                           
      REWIND IIN                                                                
 100  FORMAT(20A4)                                                              
 999  FORMAT(//,11X,8('123456789 '))                                            
 1000 FORMAT(1H1,//,24X,'I M A G E M   D O S   D A D O S   D E   E N T '        
     1,'R A D A ',//,11X,8('123456789 '),//)                                    
 2000 FORMAT(I8,3X,20A4)                                                        
      RETURN                                                                    
      END                                                                       
                                                                                
      SUBROUTINE MOVE(A,N1,N2,N3)                                               
C                                                                               
C     MOVE ELEMENTOS DE A DA POSICAO N1 ATE' A POSICAO N2                       
C          COLOCANDO-OS A PARTIR DA POSICAO N3                                  
C     OBS :                                                                     
C          N3 DEVERA' SER MENOR QUE N1                                          
C          N2 DEVERA' SER MAIOR QUE N1                                          
C                                                                               
      DIMENSION A(1)                                                            
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                                      
 1    IF( N2 .GT. N1 ) GO TO 2                                                  
      WRITE(IOUT,1000)                                                          
      RETURN                                                                    
 2    IF( N3 .LT. N1 ) GO TO 3                                                  
      WRITE(IOUT,2000)                                                          
      RETURN                                                                    
 3    DO 100 I=N1,N2                                                            
      II=N3+I-N1                                                                
 100  A(II)=A(I)                                                                
      RETURN                                                                    
 1000 FORMAT(/' ADVERTENCIA : SHIFT DA SUBRT. MOVE NAO EFETUADO DEC.1')         
 2000 FORMAT(/' ADVERTENCIA : SHIFT DA SUBRT. MOVE NAO EFETUADO DEC.2')         
      END                                                                       
      SUBROUTINE RESPEC(NMODOS,NSPEC,ID,MAXA,MASSA,Q,VQ,X,SW,EX,EY,EZ,          
     *           FPMX,FPMY,FPMZ,FM,    TM,VTM,NS,NM,FPM,R,FPMA,NPART)           
      IMPLICIT REAL *8(A-H,O-Z)                                                 
      INTEGER RIGROT,FORCAS,FNODEQ,ESFMOD                                       
      REAL *8 MASSA                                                             
      REAL A                                                                    
      DIMENSION HED(20),MAXA(1),MASSA(1),Q(NEQ,NMODOS),VQ(1),X(1),              
     *  NS(1),NM(1),SW(NMODOS,NSPEC),ID(6,1),TM(NEQT,NMODOS),VTM(1),            
     *       EX(1),EY(1),EZ(1),FPMX(1),FPMY(1),FPMZ(1),FM(1)                    
     *      ,FPM(1),R(1),FPMA(1),NPART(1),HED1(20)                              
      COMMON/SOL/NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK,NWM,NLCEST,              
     *           NLCDIN,LCASE                                                   
      COMMON /DIM/ N1,N2,N3,N4,N5                                               
      COMMON/TAPES/IELMNT,ILOAD,IIN,IOUT,NSTIF,RIGROT,FORCAS,FNODEQ,            
     *              KSTIFF,ESFMOD,MODOS,INFLU,IWORK                             
      COMMON /VAR/ NG,MODEX                                                     
      COMMON/EL/IND,NPAR(20),NUMEG,NGTC,MTOT,NFIRST,NLAST,ITWO,KODT,NEQT        
      COMMON /KABEC/ HED,NPAG,LINHA                                             
      COMMON A(1)                                                               
      ABS(XX)=DABS(XX)                                                            
      SQRT(XX)=DSQRT(XX)                                                          
C                                                                               
C         CALCULO DE FM , PRODUTO DA MATRIZ DE MASSA PELO AUTOVETOR             
C                                                                               
C     DO 10 I=1,NMODOS                                                          
C     IFM=(I-1)*NEQ+1                                                           
C10   CALL MULT(VFM(IFM),MASSA,VQ(IFM),MAXA,NEQ,NWM)                            
      DO 9000 LDIN=1,NLCDIN                                                     
      CALL KABECA                                                               
      READ(IIN,1010)LCASE,                                                      
     1              HED1,                                                       
     2              ISX,ISY,ISZ,FATX,FATY,FATZ,TD,CSI,KODMOD,KODDIR             
      IF( FATX .LE. 0. ) FATX=1.                                                
      IF( FATY .LE. 0. ) FATY=1.                                                
      IF( FATZ .LE. 0. ) FATZ=1.                                                
      WRITE(IOUT,2010) LDIN,HED1,ISX,ISY,ISZ,FATX,FATY,FATZ,TD,CSI,             
     *                 KODMOD,KODDIR                                            
      IF( MODEX .EQ. 0 ) GO TO 9000                                             
      DO 30 I=1,NEQ                                                             
      EX(I)=0.                                                                  
      EY(I)=0.                                                                  
 30   EZ(I)=0.                                                                  
      DO 40 I=1,NUMNP                                                           
      IX=ID(1,I)                                                                
      IY=ID(2,I)                                                                
      IZ=ID(3,I)                                                                
      IF( IX .NE. 0 ) EX(IX)=1.0                                                
      IF( IY .NE. 0 ) EY(IY)=1.0                                                
      IF( IZ .NE. 0 ) EZ(IZ)=1.0                                                
 40   CONTINUE                                                                  
C                                                                               
C        CALCULO DOS FATORES DE PARTICIPACAO MODAL                              
C                                                                               
      WRITE(IOUT,2020)                                                          
      DO 60 M=1,NMODOS                                                          
      FPMX(M)=0.                                                                
      FPMY(M)=0.                                                                
      FPMZ(M)=0.                                                                
      IVQ=(M-1)*NEQ + 1                                                         
      CALL MULT(FM,MASSA,VQ(IVQ),MAXA,NEQ,NWM)                                  
      SX=0.                                                                     
      SY=0.                                                                     
      SZ=0.                                                                     
      DO 50 N=1,NEQ                                                             
      SX=SX+FM(N)*EX(N)                                                         
      SY=SY+FM(N)*EY(N)                                                         
 50   SZ=SZ+FM(N)*EZ(N)                                                         
      IF( ISX .GT. 0 ) FPMX(M)=SX*SW(M,ISX)*FATX                                
      IF( ISY .GT. 0 ) FPMY(M)=SY*SW(M,ISY)*FATY                                
      IF( ISZ .GT. 0 ) FPMZ(M)=SZ*SW(M,ISZ)*FATZ                                
      WRITE(IOUT,2030)M,SX,SY,SZ,FPMX,FPMY,FPMZ                                 
 60   CONTINUE                                                                  
C                                                                               
C         LEITURA DOS ESFORCOS MODAIS EM ESFMOD                                 
C                                                                               
      IF( NGTC .LE. 0 ) GO TO 81                                                
      REWIND ESFMOD                                                             
      IF=0                                                                      
      DO 80 M=1,NMODOS                                                          
      DO 80 N=1,NGTC                                                            
      READ(ESFMOD)NS(N),NM(N),NPART(N)                                          
      NUME=NM(N)                                                                
      NSTRES=NS(N)                                                              
      DO 80 I=1,NUME                                                            
      II=IF+1                                                                   
      IF=IF+NSTRES                                                              
 80   READ(ESFMOD)(VTM(K),K=II,IF)                                              
 81   GO TO (1000,2000,3000,3000),KODDIR                                        
C                                                                               
C    **  OPCAO COMBINACAO DAS COMPONENTES EM NIVEL MODAL **                     
C                     KODDIR = 1 OU 2                                           
C                                                                               
C------------------- POR SOMA ABSOLUTA                                          
 1000 DO 100 I=1,NMODOS                                                         
 100  FPM(I)=ABS(FPMX(I))+ABS(FPMY(I))+ABS(FPMZ(I))                             
      GO TO 2500                                                                
C------------------- POR RQSQ                                                   
 2000 DO 200 I=1,NMODOS                                                         
 200  FPM(I)=SQRT(FPMX(I)**2+FPMY(I)**2+FPMZ(I)**2)                             
C                                                                               
C        COMBINACAO DAS RESPOSTAS MODAIS : DESLOC.                              
C                                                                               
 2500 CALL COMODO(Q,NEQ,NMODOS,R,FPM,X,TD,CSI,KODMOD,IOUT)                      
      WRITE(ILOAD)(R(I),I=1,NEQ)                                                
      CALL WRITE(R,ID,NEQ,NUMNP,1,1)                                            
C                                                                               
C        COMBINACAO DAS RESPOSTAS MODAIS : ACEL.                                
C                                                                               
      DO 90 I=1,NMODOS                                                          
 90   FPMA(I)=FPM(I)*X(I)*X(I)                                                  
      CALL COMODO(Q,NEQ,NMODOS,R,FPMA,X,TD,CSI,KODMOD,IOUT)                     
      CALL WRITE(R,ID,NEQ,NUMNP,2,1)                                            
C                                                                               
C             COMBINACAO DAS RESPOSTAS MODAIS : TENSOES                         
C                                                                               
      IF( NGTC .LE. 0 ) GO TO 9000                                              
      CALL COMODO(TM,NEQT,NMODOS,R,FPM,X,TD,CSI,KODMOD,IOUT)                    
      GO TO 8000                                                                
C                                                                               
C    ** OPCAO CONSIDERACAO DE CADA COMPONENTE AGINDO ISOLADAMENTE **            
C                      KODDIR = 3 OU 4                                          
C                                                                               
 3000 DO 333 I=1,NEQ                                                            
        EX(I) = 0.                                                              
        EY(I) = 0.                                                              
 333    EZ(I) = 0.                                                              
      IF( ISX .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EX,FPMX,X,TD,CSI,KODMOD,IOUT)                    
      IF( ISY .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EY,FPMY,X,TD,CSI,KODMOD,IOUT)                    
      IF( ISZ .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EZ,FPMZ,X,TD,CSI,KODMOD,IOUT)                    
C                                                                               
C        COMBINACAO DAS COMPONENTES : DESLOC.                                   
C                                                                               
      GO TO (9000,9000,3500,4000),KODDIR                                        
C------------------- POR SOMA ABSOLUTA                                          
 3500 DO 300 I=1,NEQ                                                            
 300  R(I)=ABS(EX(I))+ABS(EY(I))+ABS(EZ(I))                                     
      GO TO 5000                                                                
C------------------- POR RQSQ                                                   
 4000 DO 400 I=1,NEQ                                                            
 400  R(I)=SQRT(EX(I)**2+EY(I)**2+EZ(I)**2)                                     
 5000 CONTINUE                                                                  
      WRITE(ILOAD)(R(I),I=1,NEQ)                                                
      CALL WRITE(R,ID,NEQ,NUMNP,1,1)                                            
C                                                                               
C             COMBINACAO DAS COMPONENTES : TENSOES                              
C                (RESULTADOS GUARDADOS EM VTM)                                  
C                                                                               
      IF(NGTC .LE. 0 ) GO TO 7000                                               
      DO 444 I=1,NEQT                                                           
        EX(I) = 0.                                                              
        EY(I) = 0.                                                              
 444    EZ(I) = 0.                                                              
      IF( ISX .GT. 0 )                                                          
     *CALL COMODO(TM,NEQT,NMODOS,EX,FPMX,X,TD,CSI,KODMOD,IOUT)                  
      IF( ISY .GT. 0 )                                                          
     *CALL COMODO(TM,NEQT,NMODOS,EY,FPMY,X,TD,CSI,KODMOD,IOUT)                  
      IF( ISZ .GT. 0 )                                                          
     *CALL COMODO(TM,NEQT,NMODOS,EZ,FPMZ,X,TD,CSI,KODMOD,IOUT)                  
      GO TO (9000,9000,6000,6500),KODDIR                                        
C------------------ POR SOMA ABSOLUTA                                           
 6000 DO 600 I=1,NEQT                                                           
 600  VTM(I)=ABS(EX(I))+ABS(EY(I))+ABS(EZ(I))                                   
      GO TO 7000                                                                
C------------------ POR RQSQ                                                    
 6500 DO 650 I=1,NEQT                                                           
 650  VTM(I)=SQRT(EX(I)**2+EY(I)**2+EZ(I)**2)                                   
C                                                                               
C             COMBINACAO DAS COMPONENTES : ACEL.                                
C                                                                               
 7000 DO 555 I=1,NEQ                                                            
        EX(I) = 0.                                                              
        EY(I) = 0.                                                              
 555    EZ(I) = 0.                                                              
      DO 110 I=1,NMODOS                                                         
      X2=X(I)*X(I)                                                              
      FPMX(I)=FPMX(I)*X2                                                        
      FPMY(I)=FPMY(I)*X2                                                        
 110  FPMZ(I)=FPMZ(I)*X2                                                        
      IF( ISX .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EX,FPMX,X,TD,CSI,KODMOD,IOUT)                    
      IF( ISY .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EY,FPMY,X,TD,CSI,KODMOD,IOUT)                    
      IF( ISZ .GT. 0 )                                                          
     *CALL COMODO(Q,NEQ,NMODOS,EZ,FPMZ,X,TD,CSI,KODMOD,IOUT)                    
      GO TO (9000,9000,3501,4001),KODDIR                                        
C--------------------- POR SOMA ABSOLUTA                                        
 3501 DO 301 I=1,NEQ                                                            
 301  R(I)=ABS(EX(I))+ABS(EY(I))+ABS(EZ(I))                                     
      GO TO 5001                                                                
C--------------------- POR RQSQ                                                 
 4001 DO 401 I=1,NEQ                                                            
 401  R(I)=SQRT(EX(I)**2+EY(I)**2+EZ(I)**2)                                     
 5001 CONTINUE                                                                  
      CALL WRITE(R,ID,NEQ,NUMNP,2,1)                                            
      IF( NGTC .LE. 0 ) GO TO 9000                                              
      DO 750 I=1,NEQT                                                           
 750  R(I)=VTM(I)                                                               
C                                                                               
C            GRAVACAO DOS ESFORCOS EM FORCAS                                    
C            GRAVACAO DOS ESFORCOS EM IWORK PARA IMPRESSAO                      
C                                                                               
 8000 REWIND IWORK                                                              
      IF=0                                                                      
      DO 700 N=1,NGTC                                                           
      NSTRES=NS(N)                                                              
      NUME=NM(N)                                                                
      NPRR=NPART(N)                                                             
      WRITE(FORCAS)NSTRES,NUME,NPRR                                             
      DO 700 I=1,NUME                                                           
      II=IF+1                                                                   
      IF=IF+NSTRES                                                              
      WRITE(FORCAS)(R(K),K=II,IF)                                               
 700  WRITE(IWORK) (R(K),K=II,IF)                                               
C                                                                               
C             IMPRESSAO DAS TENSOES                                             
C                                                                               
      REWIND IWORK                                                              
      KODT=4                                                                    
      CALL STRESS(A(N5))                                                        
 9000 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
C        FORMATOS                                                               
C                                                                               
 1010 FORMAT(I5/20A4/,3I5,5F10.0,2I5)                                           
 2010 FORMAT(//,' CARREGAMENTO DINAMICO NO. ',I2,1X,20A4,                       
     1      ///,' NO. DO ESPECTRO PARA A DIRECAO X  (ISX) = ',I2,               
     2        /,' NO. DO ESPECTRO PARA A DIRECAO Y  (ISY) = ',I2,               
     3        /,' NO. DO ESPECTRO PARA A DIRECAO Z  (ISZ) = ',I2,               
     4       //,' FATOR MULTIPLICATIVO - DIRECAO X  (FATX)= ',G11.4,            
     5        /,' FATOR MULTIPLICATIVO - DIRECAO Y  (FATY)= ',G11.4,            
     6        /,' FATOR MULTIPLICATIVO - DIRECAO Z  (FATZ)= ',G11.4,            
     7       //,' TEMPO DE DURACAO                    (TD)= ',G11.4,            
     8        /,' FRACAO DO AMORTECIMENTO CRITICO    (CSI)= ',G11.4,            
     9       //,' TIPO DE COMBINACAO MODAL        (KODMOD)= ',I1,               
     *        /,' CONSIDERACAO DAS COMPONENTES    (KODDIR)= ',I1 )              
 2020 FORMAT(///,' FATORES DE PARTICIPACAO MODAL ',                             
     1        //,' MODO    DIR. X      DIR. Y      DIR. Z',/)                   
 2030 FORMAT(I3,6G12.4)                                                         
 2040 FORMAT(/,'  T E N S O E S    P A R A   O   M O D O   NO. ',/)             
      END                                                                       
C*******************************************************************            
C                                                                               
C                                                                               
C     SUBROTINA PARA ALOCACAO AUTOMATICA DOS ARQUIVOS                           
C             TEMPORARIOS DE UM PROGRAMA                                        
C                                                                               
      SUBROUTINE TFILES                                                         
      DIMENSION VETOR(14)
      CHARACTER*4 VETOR
C                                                                               
C    Era assim:
C     LOGICAL*4 VETOR(14)
C      DATA VETOR / '-7  ', '-8  ', '-9  ', '-10 ', '-11 ', '-12 ',              
C     1             '-13 ', '-14 ', '-15 ', '-16 ', '-17 ','-1  ',               
C     2             '-2  ','-4  '/                                               
      VETOR(1)= '-7  '
      VETOR(2)= '-8  '
      VETOR(3)= '-9  '
      VETOR(4)= '-10 '
      VETOR(5)= '-11 '
      VETOR(6)= '-12 '
      VETOR(7)= '-13 '
      VETOR(8)= '-14 '
      VETOR(9)= '-15 '
      VETOR(10)='-16 '
      VETOR(11)='-17 '
      VETOR(12)='-1  '
      VETOR(13)='-2  '
      VETOR(14)='-4  '
C                                                                               
      DO 10 I=7,17                                                              
C         CALL SETLIO(I, VETOR(I-6))                                             
         OPEN(UNIT=I, STATUS='SCRATCH', FORM='UNFORMATTED')
 10   CONTINUE                                                                  
C         CALL SETLIO(1,VETOR(12))                                               
C         CALL SETLIO(2,VETOR(13))                                               
C         CALL SETLIO(4,VETOR(14))                                               
         OPEN(UNIT=1, STATUS='SCRATCH', FORM='UNFORMATTED')
         OPEN(UNIT=2, STATUS='SCRATCH', FORM='UNFORMATTED')
         OPEN(UNIT=4, FILE='ENTRADA.TXT', STATUS='OLD')
         OPEN(UNIT=6, FILE='SAIDA.TXT', STATUS='OLD')
      RETURN                                                                    
      END                                                                       
      SUBROUTINE AUTOV                                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C .                                                                   .         
C .   P R O G R A M A                                                 .         
C .        MONTA AS MATRIZES DE MASSA E DE RIGIDEZ                    .         
C .        E CALCULA OS AUTOVALORES E AUTOVETORES                     .         
C .                                                                   .         
C .   CHAMADA PELO: PROGRAMA PRINCIPAL                                .         
C .                                                                   .         
C .   CHAMA: SECOND,ERROR,CLEAR,ASSEM,MASCON,RSTDIN,                  .         
C .          KABECA,SSPACE,WROTE,TEMPO                                .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
      RETURN                                                                          
      END
C	  
      SUBROUTINE MODAL                                                          
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .               
C .                                                                             
C .   P R O G R A M A                                                           
C .        PARA CALCULAR A RESPOSTA DINAMICA POR ANALISE MODAL                  
C .                                                                             
C .        CONSIDERA : CARGAS NODAIS DEPENDENTES DO TEMPO                       
C .                    MOVIMENTO UNIFORME DOS APOIOS                            
C .                    EXCITACAO MULTIPLA DOS APOIOS                            
C .                                                                             
C .        CHAMADA PELO: PROGRAMA PRINCIPAL                                     
C .                                                                             
C .        CHAMA: SECOND,ERROR,LOADV,MSE1,CINIC,MOVUAP,MOVMAP,                  
C .               COMPAC,CARDI,CARDI1,CARDI2,RESNEW,DESDIN                      
C .               STRESD,TEMPO                                                  
C .                                                                             
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .               
      RETURN
      END
C	  
      SUBROUTINE STEPBY                                                         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .               
C .                                                                             
C .   P R O G R A M A                                                           
C .      PARA CALCULAR A RESPOSTA DINAMICA POR INTEGRACAO             .         
C .                  PASSO-A-PASSO,USANDO O ALGORITMO  DE             .         
C .                  INTEGRACAO DE NEWMARK                            .         
C .      CONSIDERA : CARGAS NODAIS DEPENDENTES DO TEMPO               .         
C .                  MOVIMENTO UNIFORME DOS APOIOS                    .         
C .                  EXCITACAO MULTIPLA DOS APOIOS                    .         
C .                                                                   .         
C .   CHAMADA PELO: PROGRAMA PRINCIPAL                                .         
C .                                                                   .         
C .   CHAMA: SECOND,ERROR,CLEAR,ASSEM,MASCON,RSTDIN,CONDI,            .         
C .          LOADV,MSE1,MOVUAP,MOVMAP,CARDI,DEFASA,FUNCAO,            .         
C .          CARDIN,NEWMAR,STRESD,TEMPO                               .         
C .                                                                   .         
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .         
C        
      RETURN                                                                       
      END
C	  
      SUBROUTINE SECAO3                                                         
C*********************************************************************          
C                                                                               
C     PROGRAMA PARA VERIFICACAO DE TENSOES EM TUBULACOES, SEGUNDO               
C     OS CRITERIOS DA ASME SECTION III                                          
C     CHAMADA POR TUBNUC                                                        
C                                                                               
C*********************************************************************          
      RETURN                                                                    
      END                                                                       
C                                                                               
C ******************************************************************************
C
      SUBROUTINE TUBOCP
      END
C
C ******************************************************************************
C
      SUBROUTINE PLASTC(N105,N106,N101,N102,N110,N1,N29,N30,N31,N32,
     *ITAP4,N113)
      END
C ******************************************************************************
C
      SUBROUTINE INICIR(N101,N108,N32,ITAP4)
      END
C ******************************************************************************
C
      SUBROUTINE PLASTR(N108,N109,N101,N102,N116,N1,N29,N30,N31,N32,
     *ITAP4)                            
      END
C ******************************************************************************
C
      SUBROUTINE PLASV3(N105,N110,N206,N102,N103,N101,N211,N1,N29,N30,
     *N32,ITAP4)                     
      END
C ******************************************************************************
C
      SUBROUTINE INICIV(N111,N105,N32,ITAP4)
      END
C ******************************************************************************
C
      SUBROUTINE TIME(A, B, I)
      END
C ******************************************************************************
C
      SUBROUTINE ESTAT
      END
C*******************************************************************            
C                                                                               
C                                                                               
C     SUBROTINA PARA FECHAMENTO DE ARQUIVOS                           
C                                                                               
      SUBROUTINE CLOSEFILES
C                                                                               
      DO 10 I=6,17
         CLOSE(I)
   10 CONTINUE
      CLOSE(1)
      CLOSE(2)
      CLOSE(4)
      STOP
      END                                                                       
