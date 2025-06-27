;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION check_KEY, groups, type2, key, diets, error, VERB=verb
  ;0-8  B[0]  P/B[1]  Q/B[2]  EE[3] P/Q[4]  Acc[5]  Export[6] Fishery[7]  Unassim[8]
  ;9-16: Seasonal_TP[9] Spawn_TP[10]  Growth_TP[11] Detritus_fate[12] Carcass_fate[13]  Seasonal_fate[14] Spawn_fate[15]  Growth_fate[16]
  ;type2: -1=bac 0=con 1=PP 2=det 3=carcasses
  
  IF (N_ELEMENTS(verb) ne 1) THEN VERB=1
  
  ;--check key----
  Nx=N_ELEMENTS(groups)
  key=DOUBLE(key)
  siz=SIZE(key)
  IF (siz[0] ne 2 OR siz[1] ne 17 OR siz[2] ne Nx) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY - Bad size key'
    ;stop,'fdgfdsgd'
    RETURN,-1
  ENDIF
  
  ;--check types----
  IF (N_ELEMENTS(type2) ne Nx) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad size of type2'
    RETURN,-6
  ENDIF
  bad=WHERE(type2 lt -1 OR type2 gt 3,Nbad)
  IF (Nbad gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad data in type2'
    RETURN,-7
  ENDIF
  
  ;---B P/B EE: non-det groups----
  cols=[0,1,3]
  OKnondet=WHERE(type2 ne 2 AND type2 ne 3)
  FOR i=0,N_ELEMENTS(cols)-1 DO BEGIN    ;just: Biomass  P/B EE
    bad=WHERE(key[cols[i],OKnondet] lt 0., Nbad)
    IF (Nbad gt 0) THEN BEGIN
      IF (VERB) THEN print,'FAIL: check_KEY  - Bad key input (B P/B EE)'
      Btype=['B','P/B','EE']
      print,Btype[i],' ',groups[OKnondet[bad]],' -> ',key[cols[i],OKnondet[bad]]
      
      stop,'dsfds'
      RETURN,-20
    ENDIF
  ENDFOR
  
  ;---Q/B P/Q: consumers + bacteria ----
  cols=[2,4]
  OKcon=WHERE(type2 eq 0 OR type2 eq -1)
  FOR i=0,N_ELEMENTS(cols)-1 DO BEGIN    ;just: Q/B P/Q
    bad=WHERE(key[cols[i],OKcon] le 0., Nbad)
    IF (Nbad gt 0) THEN BEGIN
      IF (VERB) THEN print,'FAIL: check_KEY  - Bad key input (Q/B P/Q)'
      ;stop,'ytrtr'
      RETURN,-21
    ENDIF
  ENDFOR
  
  ;---U: consumers then bac----
  OKcon=WHERE(type2 eq 0)
  bad=WHERE(key[8,OKcon] le 0., Nbad)
  IF (Nbad gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad key input (U, con)'
    ;stop,'iuuyt'
    RETURN,-22
  ENDIF
  OKbac=WHERE(type2 eq -1)
  bad=WHERE(key[8,OKbac] lt 0., Nbad)
  IF (Nbad gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad key input (U, bac)'
    ;stop,'rreerw'
    RETURN,-23
  ENDIF
  
  ;---all groups for fishery----
  bad=WHERE(key[7,*] lt 0., Nbad)
  IF (Nbad gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad key input (F)'
    ;stop,'656543'
    RETURN,-24
  ENDIF
  
  ;bad=WHERE(key[9,0:Nx-2] lt 0 OR key[9,0:Nx-2] gt Nx-1,Nbad)
  ;IF (Nbad gt 0) THEN STOP,'Bad det fate: ',groups[bad]
  
  ;---check fates----
  cols=[12,13,14,15,16]
  bad=WHERE(key[cols,*] ne error AND (key[cols,*] lt 0 OR key[cols,*] ge Nx), Nbad)
  IF (Nbad gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - Bad fate'
    RETURN,-3
  ENDIF
  
  ;----check resp OK for CONSUMERS----
  OKcon=WHERE(type2 eq 0 OR type2 eq -1, Ncon)
  IF (Ncon le 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - No consumers?'
    RETURN,-8
  ENDIF
  
  R=key[2,OKcon]*(1.-key[8,OKcon])-key[1,OKcon]
  lo=WHERE(R lt 0., Nlo)
  IF (Nlo gt 0) THEN BEGIN
    IF (VERB) THEN print,'FAIL: check_KEY  - negative respirations: ',groups[OKcon[lo]]
    RETURN,-9
  ENDIF
  
RETURN,1
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------