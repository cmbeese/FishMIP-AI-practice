;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;   from:  C:\NIWA_case2\_Hauraki_model\IDL\trophic_level\HG_troph_lev+isotope_22.pro
;   
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION reconcile_diets_simple, in_diets, min_diet, error
  TINY=1.E-7
  
  Nx=N_ELEMENTS(in_diets[0,*])
  out_diets=in_diets
  FOR ii=0,Nx-1 DO BEGIN
    OK=WHERE(ABS(in_diets[ii,*]) ge TINY, NOK)
    
    ;---set lo to zero-----
    lo=WHERE(ABS(in_diets[ii,*]) lt TINY, Nlo)
    IF (Nlo gt 0) THEN out_diets[ii,lo]=0.
    IF (NOK gt 0) THEN BEGIN
    
      ;-----make sure within range-----
      lo=WHERE(in_diets[ii,OK] lt min_diet, Nlo)
      IF (Nlo gt 0) THEN out_diets[ii,OK[lo]]=min_diet
      hi=WHERE(in_diets[ii,OK] gt 1., Nhi)
      IF (Nhi gt 0) THEN out_diets[ii,OK[hi]]=1.
      
      ;---- get back to summing to 1 -----
      tot=TOTAL(out_diets[ii,*])
      out_diets[ii,*]=out_diets[ii,*]/tot
    ENDIF
  ENDFOR
  RETURN,out_diets
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------