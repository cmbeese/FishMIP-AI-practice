;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;   opens key and diet files and checks data
;   
;   M. Pinkerton
;   
;   Jan 2022 - added isobal (extra column with TL)
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION OLD_import_check, KEYfile, DIETfile, error, key, diets, type, groups, NN, keyhed, dietshed
  ;COMMON globals
  
  ;---open files-----
  open_ecosystem_file, keyfile, error, NN, groups, area, type, key, keyhed
  open_diet_file, dietfile, error, NN, type, diets, dietshed
  
  ;0-8  B[0]  P/B[1]  Q/B[2]  EE[3] P/Q[4]  Acc[5]  Export[6] Fishery[7]  Unassim[8]
  ;9-16: Seasonal_TP[9] Spawn_TP[10]  Growth_TP[11] Detritus_fate[12] Carcass_fate[13]  Seasonal_fate[14] Spawn_fate[15]  Growth_fate[16]
  
  ;----get some selections---
  OKdet=WHERE(type eq 2, Ndet)
  OKprod=WHERE(type eq 1, Nprod)
  OKcon=WHERE(type eq 0, Ncon)
  OKnondet=WHERE(type lt 2, Nnondet)
  
  ;---check----
  IF (N_ELEMENTS(key[0,*]) ne NN) THEN STOP,'import_check - FAIL Bad size key 0'
  Nkey=N_ELEMENTS(key[*,0])
  IF (Nkey ne 17) THEN STOP,'import_check - FAIL Bad size key 1'
  IF (N_ELEMENTS(diets[0,*]) ne NN) THEN STOP,'import_check - FAIL Bad size diet 0'
  IF (N_ELEMENTS(diets[*,0]) ne NN) THEN STOP,'import_check - FAIL Bad size diet 1'
  
  ;---replace negatives by errors----
  do_replace_KEY_negs=0
  IF (do_replace_KEY_negs) THEN BEGIN
    cols=[0,1,2,3,4,7,8,9,10,11]
    Ncols=N_ELEMENTS(cols)
    FOR i=0,Ncols-1 DO BEGIN
      bad=WHERE(key[cols[i],*] lt 0,Nbad)
      IF (Nbad gt 0) THEN STOP,'import_check - FAIL: Negative in key col(s)'
      IF (Nbad gt 0) THEN key[cols[i],bad]=error
    ENDFOR
  ENDIF
  
  ;---catch negative diets----
  bad=WHERE(diets lt 0., Nbad)
  IF (Nbad gt 0) THEN STOP,'import_check - FAIL negative diets: ',groups[bad]
  
  ;---force small and neg diets to zero----
  SMALL=1.0E-5
  bad=WHERE(diets lt 0., Nbad)
  IF (Nbad gt 0) THEN diets[bad]=0.0
  
  ;---make all diets sum to 1---
  diets=reconcile_diets_simple(diets, 0., error)    ;diets, min_diet, error
  
  ;---check P/Q----
  bad=WHERE(key[4,OKcon] le 0., Nbad)
  IF (Nbad gt 0) THEN STOP,'import_check - FAIL P/Q<=0: ',groups[OKcon[bad]]
  diff=ABS(key[1,OKcon]/key[2,OKcon] - key[4,OKcon])/key[4,OKcon]     ;((P/B)/(Q/B)-(P/Q))/P/Q
  bad=WHERE(diff ge 0.01, Nbad)
  IF (Nbad gt 0) THEN STOP,'import_check - FAIL bad P/Q: ',groups[OKcon[bad]]
  
  ;----check resp OK for CONSUMERS----
  R=key[2,OKcon]*(1.-key[8,OKcon])-key[1,OKcon]
  lo=WHERE(R lt 0., Nlo)
  IF (Nlo gt 0) THEN STOP,'FAIL import_check -  negative respirations: ',groups[OKcon[lo]]
  
  ;----check TRANSFERS (seasonal, spawn, growth) >0 ----
  FOR i=9,10 DO BEGIN
    txt=['SEASONAL','SPAWN']
    bad=WHERE(key[i,*] ge 1. OR key[i,*] lt 0., Nbad)
    IF (Nbad gt 0) THEN STOP,'FAIL import_check: bad ',txt[i-9],' TP=',i,bad,' -> ',data[i,bad]
  ENDFOR
  bad=WHERE(ABS(key[11,*]) ge 1., Nbad)
  IF (Nbad gt 0) THEN STOP,'FAIL import_check: bad GROWTH TP=',11,bad,' -> ',data[11,bad]
  
  ;----check location cols----
  FOR i=12,N_ELEMENTS(key[*,0])-1 DO BEGIN
    key[i,*]=FIX(key[i,*])  ;convert to integer
    bad=WHERE(key[i,*] lt 1 OR key[i,*] gt NN, Nbad, COMP=OK, NCOMP=NOK)
    IF (Nbad gt 0) THEN key[i,bad]=error
    ;OK=WHERE(key[i,*] ne error, NOK)
    IF (NOK gt 0) THEN key[i,OK]=key[i,OK]-1    ;convert from [1->NN] to [0->(NN-1)]
  ENDFOR
  
  print,'---import_check: all OK---'
  print,''
  RETURN,1
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION import_check, KEYfile, DIETfile, error, key, diets, type, groups, NN, keyhed, dietshed, $
          VERB=verb, ISOBAL=isobal
  ;COMMON globals
  
  IF (N_ELEMENTS(verb) ne 1) THEN verb=1
  IF (N_ELEMENTS(isobal) gt 0) THEN do_isobal=1 ELSE do_isobal=0
  
  ;---open files-----
  open_ecosystem_file, keyfile, error, NN, groups, area, type, key, keyhed, VERB=verb, ISOBAL=isobal
  open_diet_file, dietfile, error, NN, type, diets, dietshed, VERB=verb
  
  ;0-8  B[0]  P/B[1]  Q/B[2]  EE[3] P/Q[4]  Acc[5]  Export[6] Fishery[7]  Unassim[8]
  ;9-16: Seasonal_TP[9] Spawn_TP[10]  Growth_TP[11] Detritus_fate[12] Carcass_fate[13]  Seasonal_fate[14] Spawn_fate[15]  Growth_fate[16]
  
  ;----get some selections---
  ;OKdet=WHERE(type eq 2, Ndet)
  ;OKprod=WHERE(type eq 1, Nprod)
  ;OKcon=WHERE(type eq 0, Ncon)
  ;OKnondet=WHERE(type lt 2, Nnondet)
  
  ;---make type2----
  type2=get_type2(type, groups)
  
  ;---get NN---
  NN=N_ELEMENTS(groups)
  IF (N_ELEMENTS(key[0,*]) ne NN) THEN STOP,'Bad size key'
  IF (N_ELEMENTS(type) ne NN) THEN STOP,'Bad size type'
  IF (N_ELEMENTS(diets[0,*]) ne NN) THEN STOP,'Bad size diets'  
  
  ;----fix location cols----
  FOR i=12,N_ELEMENTS(key[*,0])-1 DO BEGIN
    key[i,*]=FIX(key[i,*])  ;convert to integer
    bad=WHERE(key[i,*] lt 1 OR key[i,*] gt NN, Nbad, COMP=OK, NCOMP=NOK)
    IF (Nbad gt 0) THEN key[i,bad]=error
    ;OK=WHERE(key[i,*] ne error, NOK)
    IF (NOK gt 0) THEN key[i,OK]=key[i,OK]-1    ;convert from [1->NN] to [0->(NN-1)]
  ENDFOR
  
  ;---check key---
  res=check_KEY(groups, type2, key, diets, error, VERB=1)
  IF (res lt 0) THEN STOP,'Bad check_KEY'
  ;stop,'asdasd'
   
  ;---replace negatives by errors----
  do_replace_KEY_negs=0
  IF (do_replace_KEY_negs) THEN BEGIN
    cols=[0,1,2,3,4,7,8,9,10,11]
    Ncols=N_ELEMENTS(cols)
    FOR i=0,Ncols-1 DO BEGIN
      bad=WHERE(key[cols[i],*] lt 0,Nbad)
      IF (Nbad gt 0) THEN STOP,'import_check - FAIL: Negative in key col(s)'
      IF (Nbad gt 0) THEN key[cols[i],bad]=error
    ENDFOR
  ENDIF
  
  ;---catch negative diets----
  bad=WHERE(diets lt 0., Nbad)
  IF (Nbad gt 0) THEN STOP,'import_check - FAIL negative diets: ',groups[bad]
  
  ;---force small and neg diets to zero----
  SMALL=1.0E-5
  bad=WHERE(diets lt 0., Nbad)
  IF (Nbad gt 0) THEN diets[bad]=0.0
  
  ;---make all diets sum to 1---
  diets=reconcile_diets_simple(diets, 0., error)    ;diets, min_diet, error
  
  ;----check isobal---
  Nisobal=N_ELEMENTS(isobal)
  IF (do_isobal eq 1) THEN BEGIN
      IF (Nisobal ne NN) THEN STOP,'Bad size isobal'
      IF (Nisobal ne NN) THEN STOP,'Bad size isobal'
      
      ;--- calling bacteria, PP and detritus "PP" for now: these need to be fixed----
      ;-- TL of cacasses (3) and consumers (0) will float --
      OKpp=WHERE(type2 eq 1 OR type2 eq -1 OR type2 eq 2, NOKpp, COMP=OK0, NCOMP=NOK0)
      IF (NOKpp lt 1 OR NOKpp gt 10) THEN STOP,'Bad number PP groups'
      IF (NOK0 le 0) THEN STOP,'No non-PP groups'
      bad=WHERE(isobal[OKpp] le 0. OR isobal[OKpp] ge 2., Nbad)
      IF (Nbad gt 0) THEN STOP,'Some bad or missing TLs for primary producers etc'
      OK1=WHERE(isobal[OK0] gt 0., NOK1)
      IF (NOK1 gt 0) THEN OKiso=OK0[OK1] $
      ELSE STOP,'No TL data to fit to??'
      print,'isobal data present and OK: ',get_lohi(isobal[OK0],error,/ALL)
  ENDIF
  
  IF (verb) THEN BEGIN
      print,'---import_check: all OK---'
      print,''
  ENDIF

RETURN,1
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
