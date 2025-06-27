;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
; Open key file
;
; M Pinkerton
;
; Sep 2014 - for sensitivity - Stand alone SVDbalance FUNCTION version - no fails
;
; Jan2022 - if extra column in key, returns as isobal (TL values)
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO open_ecosystem_file, infile, error, N, groups, area, type, key, header,VERB=verb, ISOBAL=isobal

  IF (N_ELEMENTS(verb) ne 1) THEN verb=1
  IF (N_ELEMENTS(infile) ne 1) THEN STOP,rewlkjfdsl

  files=FINDFILE(infile,COUNT=Nfiles)
  IF (Nfiles eq 0) THEN BEGIN
      print,'open_ecosystem_file: Cannot find: ',infile
      STOP,asdsada
  ENDIF
  IF (verb) THEN print,'open_ecosystem_file: Found and opening: ',infile
  
  ;0  Type[0]
  ;1-9  B[1]  P/B[2]  Q/B[3]  EE[4] P/Q[5]  Acc[6]  Export[7] Fishery[8]  Unassim[9]
  ;10-12: Seasonal_TP[10] Spawn_TP[11]  Growth_TP[12]
  ;13-17: Detritus_fate[13] Carcass_fate[14]  Seasonal_fate[15] Spawn_fate[16]  Growth_fate[17]
  
  num_cols=18
  min_groups=4    ;minumum: [2 groups] + [1 detritus] + [1 phytoplankton]
  
  ;---open file----
  data=open_named(infile, groups, header, error, VERB=verb)
   
  ;---get rid of empty lines----
  OK=WHERE(groups ne '',N)
  IF (N lt min_groups) THEN STOP,'open_ecosystem_file: Not enough groups: N=',N
  data=data[*,OK]
  groups=groups[OK]
  ;groups=STRUPCASE(groups)
  ;FOR i=0,N-1 DO print,i+1,' ',groups[i]
  
  ;---check columns----
  Nin=N_ELEMENTS(data[*,0])
  IF (Nin eq num_cols) THEN isobal=error $       ;no TLdata
  ELSE IF (Nin eq num_cols+1) THEN BEGIN
    isobal=REFORM(data[num_cols,*],N)
    data=data[0:num_cols-1,*]
  ENDIF ELSE STOP,'open_ecosystem_file: Not right number columns: ',Nin
  
  ;---check detritus----
  OK=WHERE(data[0,*] ge 2,Ndet)
  OK=WHERE(data[0,*] eq 1,Nprod)
  OK=WHERE(data[0,*] le 0,Ncon)
  IF (verb) THEN print,'Ncon= ',Ncon,'  Nprod=',Nprod,'  Ndet=',Ndet
  IF (Ndet+Nprod+Ncon ne N) THEN STOP,'open_ecosystem_file: Does not match total=',N
  
  ;---make outputs----
  area=MAKE_ARRAY(N,/DOUBLE,VALUE=1.)   ;hard coded for now
  type=data[0,*]
  key=data[1:*,*]
  
  IF (verb) THEN print,'Ecosystem data read in successfully'
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------