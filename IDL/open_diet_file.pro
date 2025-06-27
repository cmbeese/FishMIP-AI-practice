PRO open_diet_file, infile, error, N, type, diets, header, VERB=verb

  IF (N_ELEMENTS(verb) ne 1) THEN verb=1


  files=FINDFILE(infile,COUNT=Nfiles)
  IF (Nfiles eq 0) THEN STOP,'open_diet_file: Cannot find: ',infile
  IF (verb) THEN print,'open_diet_file: Found and opening: ',infile
  
  ;---open file----
  diets=open_named(infile, groups, header, error, VERB=verb)
  
  ;---check columns----
  Nin=N_ELEMENTS(diets[*,0])
  IF (Nin ne N) THEN STOP,'open_diet_file: Not right number columns: ',Nin
  
  ;---get rid of empty lines----
  OK=WHERE(groups ne '',Nin)
  IF (Nin ne N) THEN STOP,'open_diet_file: Not right number columns: ',Nin
  diets=diets[*,OK]
  groups=groups[OK]
  
  ;---check col totals----
  FOR i=0,N-1 DO $
    IF (type[i] eq 0) THEN BEGIN
    tot=TOTAL(diets[i,*])
    diff=ABS(tot-1.)
    IF (diff gt 1.E-4) THEN BEGIN
      IF (verb) THEN print,'WARNING:  open_diet_file: bad diet total: ',groups[i]
      STOP,'FAIL: open_diet_file: bad diet total: ',groups[i]
    ENDIF
  ENDIF
  
  IF (verb) THEN BEGIN
      print,'---Diet data read in successfully---'
      print,''
  ENDIF
END