NON effettua calcolo degli errori
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