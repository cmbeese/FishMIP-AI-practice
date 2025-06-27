FUNCTION get_QQ, P, Q, DD, error

  Nx=N_ELEMENTS(P)
  IF (N_ELEMENTS(Q) ne Nx) THEN STOP,'FAIL: get_QQ - bad Q'
  siz=SIZE(DD)
  IF (siz[0] ne 2 OR siz[1] ne Nx OR siz[2] ne Nx) THEN STOP,'FAIL: get_QQ - bad DD'
  
  tot=TOTAL(DD,2)
  bad=WHERE(tot lt 0. OR tot gt 1.000001,Nbad)
  IF (Nbad gt 0) THEN STOP,'Bad diet totals'
  
  out=DD
  FOR i=0,Nx-1 DO out[i,*]=Q[i]*DD[i,*]   ;by columns (preds)
  FOR i=0,Nx-1 DO out[*,i]=out[*,i]/P[i]  ;by rows (prey)
  
  tot=TOTAL(out,1)
  hi=WHERE(tot gt 1., Nhi)
  IF (Nhi gt 0) THEN FOR i=0,Nhi-1 DO out[*,hi[i]]=out[*,hi[i]]/tot[hi[i]]
  lo=WHERE(tot lt 0., Nlo)
  IF (Nlo gt 0) THEN FOR i=0,Nlo-1 DO out[*,lo[i]]=0.
  tot=TOTAL(out,1)
  SMALL=1.E-7
  bad=WHERE(tot lt 0.-SMALL OR tot gt 1.+SMALL, Nbad)
  IF (Nbad gt 0) THEN STOP,'Bad row totals'
    tot=(tot<1.)>0.
  
  RETURN,out
END