;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;
;    plot foodweb diagram
;    
;    M. Pinkerton
;    
;    revised  April 2016
;
;
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO set_globals
  COMMON globals, Gbsiz, Gypos, iter, Gxpos0, GNN, Gxhi, maxits, Gxsiz, Gysiz, Ggroups, $
    spare2, spare3, spare4, spare5, spare6
    
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION Qsimilar_part, Q, NN, error
out=MAKE_ARRAY(NN,NN,/FLOAT,value=error)
FOR i=0,NN-1 DO BEGIN
  irow=TOTAL(Q[*,i]^2)
  FOR j=i,NN-1 DO BEGIN
    jrow=TOTAL(Q[*,j]^2)
    ijrow=TOTAL(Q[*,i]*Q[*,j])
    drow=SQRT(irow*jrow)
    IF (drow gt 0.) THEN out[i,j]=ijrow/drow $
    ELSE IF (irow eq 0 AND jrow eq 0) THEN out[i,j]=1. $
    ELSE out[i,j]=0.
  ENDFOR
ENDFOR
RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION Qsimilar, Q, error

  info,Q,'Qsimilar: Q',error
  siz=SIZE(Q)
  IF (siz[0] ne 2) THEN STOP,'FAIL: Qsimilar - bad Q dims'
  NN=siz[1]
  IF (siz[2] ne NN) THEN STOP,'FAIL: Qsimilar - Q not square'
  
  Qrow=Qsimilar_part(Q, NN, error)
  Qcol=Qsimilar_part(TRANSPOSE(Q), NN, error)
  out=0.5*(Qrow+Qcol)
  
  out=TRANSPOSE(out)
  info,out,'Qsimilar: out',error
  ;stop,'fsdfsdf'
  
  RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_xlocate, Q, groups, error

  siz=SIZE(Q)
  m=siz[1]
  sim=Qsimilar(Q, error)
  
  ;---set [i,i] elements to error----
  FOR i=0,m-1 DO sim[i,i]=error
  
  OK=WHERE(sim ne error, NOK)
  
  pairdistance = -sim[OK]
  
  clusters = CLUSTER_TREE(pairdistance, linkdistance)
  PRINT, [clusters, TRANSPOSE(linkdistance)], FORMAT='(I3, I7, F10.2)'
  
  DENDROGRAM, clusters, linkdistance, outverts, outconn, LEAFNODES=leafnodes
  
  PRINT, STRTRIM(LEAFNODES, 2)
  
  print,TRANSPOSE(groups[leafnodes])
  stop,'fsdfs'
  
  
  out=leafnodes
  
  RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO Xplot_boxes, win, tit, xsiz, ysiz, xpos, ypos, bsiz, groups

  WINDOW, win, XSIZE=xsiz, YSIZE=ysiz, TITLE=tit
  PLOT, [0.,xsiz], [0.,ysiz], /NODATA, XSTYLE=5, YSTYLE=5, color=0, XMARGIN=[0,0],YMARGIN=[0,0], BACKGROUND=255
  
  FOR i=0,N_ELEMENTS(bsiz)-1 DO BEGIN
    bxlo=xpos[i] - bsiz[i]/2.
    bxhi=bxlo+bsiz[i]
    bylo=ypos[i] - bsiz[i]/2.
    byhi=bylo+bsiz[i]
    xx=[bxlo,bxhi,bxhi,bxlo,bxlo]
    yy=[bylo,bylo,byhi,byhi,bylo]
    OPLOT, xx, yy, line=0, COLOR=0, THICK=1.
    XYOUTS, xpos[i],ypos[i], groups[i], COLOR=0, CHARSIZE=1., CHARTHICK=1., ALIGNMENT=0.5 ;, FONT=0
  ENDFOR
  
  stop,'sfsdf'
  
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_tsep, x
  COMMON globals
  
  out=0.
  FOR i=0,GNN-1 DO BEGIN
  
    ;---overlap with edges of domain-----
    xlo=x[i]-Gbsiz[i]
    IF (xlo lt 0.) THEN out=out-xlo
    xhi=x[i]+Gbsiz[i]-Gxhi
    IF (xhi gt 0.) THEN out=out+xhi
    
    ;---overlap with any other boxes----
    FOR j=0,GNN-1 DO IF (j ne i) THEN BEGIN
      dx=(x[j]-x[i])^2
      dy=(Gypos[j]-Gypos[i])^2
      dd=(Gbsiz[i]+Gbsiz[j])^2 - dx-dy
      IF (dd gt 0) THEN out=out+dd
    ENDIF
  ENDFOR
  
  ;---plot?---
  do_plot=1
  IF (do_plot) THEN BEGIN
    win=iter MOD 10
    tit=STRING(win)
    Xplot_boxes, win, tit, Gxsiz, Gysiz, x, Gypos, Gbsiz, Ggroups
  ENDIF
  
  print,iter,' -> ',out
  iter=iter+1
  IF (iter gt maxits) THEN STOP,'FAIL: get_tsep - too many its'
  
  
  
  stop,'fsdf'
  RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION fit_xlocate, bsiz, ypos, xpos0, xhi, groups, xsiz,ysiz,error
  COMMON globals
  
  Gbsiz=bsiz/2.   ;half box size!!
  Gypos=ypos
  Gxpos0=xpos0
  Gxhi=xhi
  GNN=N_ELEMENTS(ypos)
  Gxsiz=xsiz
  Gysiz=ysiz
  Ggroups=groups
  
  ;===params===
  mintype='powell'
  
  ftol = 1.0e-3                     ;Define the fractional tolerance:
  maxits=3000L
  iter = 0L           ;in globals for debugging
  
  ;---starting----
  P=xpos0
  
  
  ;---now start search---
  scal=0.1*P
  ;print,'starting minimization...'
  
  ;---POWELL----
  IF (mintype eq 'powell') THEN BEGIN
    xi = xidentity(scal)
    POWELL, P, Xi, Ftol, Fmin, 'get_tsep', /DOUBLE, ITMAX=maxits
    best=P
    
    ;---AMOEBA----
  ENDIF ELSE IF (mintype eq 'amoeba') THEN BEGIN
    best = AMOEBA(Ftol, FUNCTION_NAME='get_tsep', NMAX=5000, P0=P, SCALE=scal)
    ;print,'Best=',best
    ;STOP,'aasdfasdfdf'
  ENDIF ELSE STOP,'Bad mintype=',mintype
  ;print,'end of search'
  
  
  STOP,'sfssfsdf'
  
  
  RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;  axis_type = 'x' or 'y'
;  pos0 = single value axis position in data units
;  pos01 = [lo,hi] limits of axis to plot in data units
;  tikvalues = position of tik labels to add on axis in string
;  tiklabels = tik labels to add on axis in string
;  valsiz = [size of labels to add, thickness of text, xposadj, yposadj]    (posadj moves the axis labels slightly to line up with tick marks)
;  valpos = array (or single replicated) of position of labels to add (for yaxis, starting xlocation in data units; for xaxis, starting y location)
;  tiklen = length of ticks in data units
;  col = colours [ctable,color]
;  label = axis label
;  labelsiz = [size of axis label to add, thickness]
;  labelpos = position of axis label in data units (x position for y-axis and y position for x-axis)
;  thick = thickness of axis and tick marks
;
;-------------------------------------------------------------------------------
PRO add_Maxis, axis_type, pos0, pos01, tikvalues, tiklabels, valsiz, valpos, tiklen, col, label, labelsiz, labelpos, thick, error

  ;---get type---
  IF (STRUPCASE(axis_type) eq 'X') THEN yaxis=0 $
  ELSE IF (STRUPCASE(axis_type) eq 'Y') THEN yaxis=1 $
  ELSE STOP,'FAIL: add_Maxis - bad axis type
  
  N=N_ELEMENTS(tikvalues)
  IF (N_ELEMENTS(tiklabels) ne N) THEN STOP,'FAIL: add_Maxis - wrong size of tiklabels'
  IF (N_ELEMENTS(valpos) eq N) THEN Nvalpos=valpos ELSE Nvalpos=MAKE_ARRAY(N,/FLOAT,value=valpos[0])
  IF (N_ELEMENTS(valsiz) ne 4) THEN STOP,'FAIL: add_Maxis - valsize should have 3 elements [siz, thickness, xposadj, yposadj]'
  xposadj=valsiz[2]
  yposadj=valsiz[3]
  IF (N_ELEMENTS(labelsiz) ne 2) THEN STOP,'FAIL: add_Maxis - labelsize should have 2 elements [siz, thickness]'
  
  
  ;pval=STR2NUM_ARRAY(vals,error)
  ;bad=WHERE(pval eq error, Nbad)
  ;IF (Nbad gt 0) THEN STOP,'FAIL: add_Maxis - bad values in axis labels'
  
  loadct,col[0]
  
  
  ;---add axis line----
  IF (yaxis) THEN OPLOT, [pos0,pos0], pos01, LINE=0, COLOR=col[1], THICK=thick $    ;y-axis
  ELSE OPLOT, pos01, [pos0,pos0], LINE=0, COLOR=col[1], THICK=thick                 ;x-axis
  
  ;---add ticks and axis numbers---
  FOR i=0,N-1 DO BEGIN
    IF (yaxis) THEN BEGIN   ;y-axis
      OPLOT, [pos0-tiklen,pos0], [tikvalues[i],tikvalues[i]], COLOR=col[1], THICK=thick
      XYOUTS, Nvalpos[i]+xposadj, tikvalues[i]+yposadj, tiklabels[i], COLOR=col[1], CHARSIZE=valsiz[0], CHARTHICK=valsiz[1], ALIGNMENT=0 ;, FONT=0   ;R-aligned
      
    ENDIF ELSE BEGIN    ;x-axis
      OPLOT, [tikvalues[i],tikvalues[i]], [pos0-tiklen,pos0], COLOR=col[1], THICK=thick
      XYOUTS, tikvalues[i]+xposadj, Nvalpos[i]+yposadj, tiklabels[i], COLOR=col[1], CHARSIZE=valsiz[0], CHARTHICK=valsiz[1], ALIGNMENT=0.5 ;, FONT=0    ;L-aligned
      
    ENDELSE
  ENDFOR
  
  ;---add axis labels-----
  midpos=0.5*TOTAL(pos01)
  IF (yaxis) THEN BEGIN   ;y-axis
    XYOUTS, labelpos, midpos, label, COLOR=col[1], CHARSIZE=labelsiz[0], CHARTHICK=1.5*labelsiz[1], ALIGNMENT=0.5, ORIENTATION=90.     ;, FONT=0
  ENDIF ELSE BEGIN   ; x-axis
    XYOUTS, midpos, labelpos, label, COLOR=col[1], CHARSIZE=labelsiz[0], CHARTHICK=1.5*labelsiz[1], ALIGNMENT=0.5        ;, FONT=0
  ENDELSE
  
  
  ;stop,'dfsdsdf'
  
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_ypos_TL, ylo, yhi, TL, error

  bad=WHERE(TL lt 0.5 OR TL gt 6, Nbad)
  IF (Nbad gt 0) THEN STOP,'FAIL: get_ypos_TL - Bad TL'
  
  OK14=WHERE(TL gt 0 AND TL le 4, NOK, COMPLEMENT=OK4p, NCOMPLEMENT=NOK4p)
  out=TL-TL+error
  
  ;===params===
  spacing_TLgt4=1.2    ;spacing of 4-5 (and 5-6) compared to 1-2, 2-3, 3-4
  
  ;---calculate spacing----
  hiTL=MAX(TL)
  IF (hiTL gt 4.) THEN del=(yhi-ylo)/(3.+(hiTL-4.)*spacing_TLgt4) $
  ELSE del=(yhi-ylo)/3.
  
  ;---allocate TLs---
  y0hi=ylo+del*3
  out[OK14]=INTERPOL([ylo,y0hi],[1.,4.],TL[OK14])                           ;TLs 1-4
  IF (NOK4p gt 0) THEN out[OK4p]=INTERPOL([y0hi,yhi],[4.,hiTL],TL[OK4p])    ;TLs 4+
  
  RETURN,out
END

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO plot_boxes, xpos, ypos, bsiz, groups, bthick, txtsiz_, txtthick

  Nb=N_ELEMENTS(bsiz)
  IF (N_ELEMENTS(txtsiz_) eq Nb) THEN txtsiz=txtsiz_ ELSE txtsiz=MAKE_ARRAY(Nb,/FLOAT,value=txtsiz_[0])
  
  FOR i=0,Nb-1 DO BEGIN
    bxlo=xpos[i] - bsiz[i]/2.
    bxhi=bxlo+bsiz[i]
    bylo=ypos[i] - bsiz[i]/2.
    byhi=bylo+bsiz[i]
    xx=[bxlo,bxhi,bxhi,bxlo,bxlo]
    yy=[bylo,bylo,byhi,byhi,bylo]
    OPLOT, xx, yy, line=0, COLOR=0, THICK=bthick
    
    ;---text----
    pos=STRPOS(groups[i],'-')
    IF (pos eq -1) THEN BEGIN
      ;XYOUTS, xpos[i],ypos[i], groups[i], COLOR=0, CHARSIZE=txtsiz[i], CHARTHICK=txtthick, ALIGNMENT=0.5 ;, FONT=0 $
      XYOUTS, xpos[i],ypos[i]-3.5*txtsiz[i], groups[i], COLOR=0, CHARSIZE=txtsiz[i], CHARTHICK=txtthick, ALIGNMENT=0.5 ;, FONT=0 $
    ENDIF ELSE BEGIN
      sep=STR_SEP(groups[i],'-')
      IF (N_ELEMENTS(sep) ne 2) THEN STOP,'FAIL: plot_boxes - bad label with hyphen'
      yy=ypos[i]+6.5*txtsiz[i]
      XYOUTS, xpos[i],yy, sep[0], COLOR=0, CHARSIZE=txtsiz[i], CHARTHICK=txtthick, ALIGNMENT=0.5 ;, FONT=0 $
      yy=ypos[i]-6.5*txtsiz[i]
      XYOUTS, xpos[i],yy, sep[1], COLOR=0, CHARSIZE=txtsiz[i], CHARTHICK=txtthick, ALIGNMENT=0.5 ;, FONT=0 $
      
    ENDELSE
    
  ENDFOR
  
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_xy_link, Nx, x0, y0, x1, y1, alf, error

  p=[[x0,x1],[y0,y1]]
  ;oplot, p[*,0],p[*,1],line=0, color=100, thick=2
  
  ;---rotate----
  R=SQRT((y1-y0)^2+(x1-x0)^2)
  IF (R le 0) THEN BEGIN
    RETURN, error
    STOP,'FAIL: plot_link - identical points'
  ENDIF
  sinq=(y1-y0)/R
  cosq=(x1-x0)/R
  ;q=ATAN(y1-y0, x1-x0)      ;-pi to pi
  M=[[cosq,sinq],[-sinq,cosq]]
  p=M##p
  ;stop,'dfsd'
  
  ;---shift---
  dx=-0.5*(p[0,0]+p[1,0])
  p[*,0]=p[*,0]+dx
  
  ;---quadratric---
  c=p[0,1]-alf*(p[1,0]-p[0,0])/2.
  a=2.*alf/(p[1,0]-p[0,0])
  x=p[0,0]+(p[1,0]-p[0,0])*FINDGEN(Nx)/FLOAT(Nx-1)
  y=a*x^2+c
  
  ;---transform back---
  x=x-dx      ;shift
  q=[[x],[y]]
  M=[[cosq,-sinq],[sinq,cosq]]
  q=M##q      ;rotate
  
  RETURN,TRANSPOSE(q)
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_links, Nsteps, xpos,ypos, ii0,ii1, bsiz, lims, error, xout,yout      ;lims=[lox,hix, loy,hiy]

  Nx=N_ELEMENTS(xpos)
  IF (N_ELEMENTS(ypos) ne Nx) THEN STOP,'FAIL: get_links - mismatched xpos ypos'
  IF (N_ELEMENTS(bsiz) ne Nx) THEN STOP,'FAIL: get_links - mismatched xpos bsiz'
  Nlinks=N_ELEMENTS(ii0)
  IF (N_ELEMENTS(ii1) ne Nlinks) THEN STOP,'FAIL: get_links - mismatched i0 and i1'
  
  ;===params====
  lim=[0.1,0.6,0.05]
  
  Nlim=ROUND(1+(lim[1]-lim[0])/lim[2])
  alf=lim[0]+(lim[1]-lim[0])*FINDGEN(Nlim)/FLOAT(Nlim-1)
  alf=[0.,alf,-alf]
  alf=alf[SORT(ABS(alf))]
  Nalf=N_ELEMENTS(alf)
  
  
  ;---make output---
  xout=MAKE_ARRAY(Nsteps, Nlinks, /FLOAT, value=error)
  yout=MAKE_ARRAY(Nsteps, Nlinks, /FLOAT, value=error)
  outalf=MAKE_ARRAY(Nlinks,/FLOAT,value=error)
  
  ;---work though----
  FOR ii=0,Nlinks-1 DO BEGIN
    i0=ii0[ii]
    i1=ii1[ii]
    IF (i0 ne i1) THEN BEGIN    ;link to itself
    
      ;---make diffs----
      diff=MAKE_ARRAY(Nalf,/FLOAT,value=0.)
      mindiff=MAKE_ARRAY(Nalf,/FLOAT,value=1.E10)
      outofarea=MAKE_ARRAY(Nalf,/FLOAT,value=0)
      
      ;---work through alfs---
      FOR i=0,Nalf-1 DO BEGIN
        ialf=alf[i]
        
        xy=get_xy_link(Nsteps, xpos[i0], ypos[i0], xpos[i1], ypos[i1], ialf, error)
        
        FOR j=0,Nx-1 DO IF (j ne i0 AND j ne i1) THEN BEGIN
          dx=xy[0,*]-xpos[j]
          dy=xy[1,*]-ypos[j]
          dd=SQRT(dx^2+dy^2)
          diff[i]=diff[i]+TOTAL(dd)
          mdiff=MIN(dd)-bsiz[j]   ;closest approach (negative if overlap)
          IF (mdiff lt mindiff[i]) THEN mindiff[i]=mdiff
        ENDIF
        
        ;---check out of area----lims=[lox,hix, loy,hiy]
        IF (MIN(xy[0,*]) lt lims[0] OR MAX(xy[0,*]) gt lims[1] OR MIN(xy[1,*]) lt lims[2] OR MAX(xy[1,*]) gt lims[3]) THEN $
          outofarea[i]=1
          
      ENDFOR
      ;print,[TRANSPOSE(alf),TRANSPOSE(diff),TRANSPOSE(mindiff)]
      
      ;---select best route----
      IF (mindiff[0] gt 10.) THEN BEGIN
        OK0=0
        outalf[ii]=alf[OK0]
      ENDIF ELSE BEGIN
        OK=WHERE(outofarea eq 0, NOK)
        ialf=alf[OK]
        index=SORT(mindiff[OK])
        outalf[ii]=ialf[index[NOK-1]]
      ENDELSE
      
      ;---save best route and alf----
      
      xy=get_xy_link(Nsteps, xpos[i0], ypos[i0], xpos[i1], ypos[i1], outalf[ii], error)
      xout[*,ii]=xy[0,*]
      yout[*,ii]=xy[1,*]
      
    ENDIF
  ENDFOR    ;next link
  
  ;stop,'sfd'
  RETURN,outalf
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO fix_startend, xx, yy, xpos, ypos, bsiz, Aoff, error, oxx, oyy     ;fix start

  dx=xx[1]-xx[0]
  dy=yy[1]-yy[0]
  IF (ABS(dx) ge ABS (dy)) THEN BEGIN
    IF (dx gt 0) THEN BEGIN
      OK=WHERE(xx ge xpos+bsiz/2., NOK)
      x0=xpos+bsiz/2.     + Aoff
    ENDIF ELSE BEGIN
      OK=WHERE(xx le xpos-bsiz/2., NOK)
      x0=xpos-bsiz/2.     - Aoff
    ENDELSE
    IF (NOK lt 1) THEN STOP,'sdsa'
    oxx=xx[OK]
    oyy=yy[OK]
    y0=INTERPOL(oyy[0:1],oxx[0:1],[x0])

  ENDIF ELSE BEGIN
    IF (dy gt 0) THEN BEGIN
      OK=WHERE(yy ge ypos+bsiz/2., NOK)
      y0=ypos+bsiz/2.     +Aoff
    ENDIF ELSE BEGIN
      OK=WHERE(yy le ypos-bsiz/2., NOK)
      y0=ypos-bsiz/2.     -Aoff
    ENDELSE
    oxx=xx[OK]
    oyy=yy[OK]
    x0=INTERPOL(oxx[0:1],oyy[0:1],[y0])
  ENDELSE
  oxx=[x0,oxx]
  oyy=[y0,oyy]
  
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO add_arrows, xx, yy, xpos, ypos, A, B, col, error

  N=N_ELEMENTS(xx)
  IF (N_ELEMENTS(yy) ne N) THEN STOP,'FAIL: add_arrows - bad inputs'
  
  x0=xx[N-1]
  y0=yy[N-1]
  
  dx=x0-xx[N-2]
  dy=y0-yy[N-2]
  m=SQRT(dx^2+dy^2)
  xp=x0-dx*A/m
  yp=y0-dy*A/m
  
  x1=xp+dy*B/m
  x2=xp-dy*B/m
  y1=yp-dx*B/m
  y2=yp+dx*B/m
  
  xplot=[x0,x1,x2,x0]
  yplot=[y0,y1,y2,y0]
  
  POLYFILL, xplot, yplot, color=col
  ;OPLOT, xplot, yplot, color=0, thick=1.
  
  ;stop,'sfsdf'
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO plot_links, DD, QQ, groups, xpos, ypos, bsiz, scale, lims, error

  ;---check input----
  Nx=N_ELEMENTS(groups)
  siz=SIZE(DD)
  IF (siz[0] ne 2 OR siz[1] ne Nx OR siz[2] ne Nx) THEN STOP,'FAIL: plot_links - bad DD'
  siz=SIZE(QQ)
  IF (siz[0] ne 2 OR siz[1] ne Nx OR siz[2] ne Nx) THEN STOP,'FAIL: plot_links - bad QQ'
  IF (N_ELEMENTS(bsiz) ne Nx) THEN STOP,'FAIL: plot_links - bad Bsiz'
  
  ;---graphics-----
  ctab=0    ;grey
  loadct, ctab    ;
  
  ;===params===
  Nsteps=50.
  DQlims=[2.,20.,80.]/100.   ;ignore<[0]; grey, thick1 [0]-[1]; black thick 1-3 [1]-[2]; black thick 3 >[2]
  gcols=[0.7*255., 0.]       ;greys [light grey, black]
  tthick=[1.,4.]            ;line thicknesses
  arrow_p=[0.8,2.]          ;mapping arrow size from 1,3 ->
  arrow_A=10.
  arrow_B=4.
  
  xcols=[0.,DQlims[1],1.]
  ycols=[gcols[0],gcols[1],gcols[1]]
  xthick=[0.,DQlims[1],DQlims[2],1.]
  ythick=[tthick[0],tthick[0],tthick[1],tthick[1]]
  
  aheadoff = 0.5*arrow_A*scale
  ;aheadoff = 0.
  
  ;---get links----
  OKDQ=WHERE(DD gt DQlims[0] OR QQ gt DQlims[0], NDQ)
  IF (NDQ le 0) THEN STOP,'FAIL: plot_links - no links'
  i0=FIX(OKDQ/Nx)    ;prey
  i1=OKDQ MOD Nx     ;predator
  
  alf=get_links(Nsteps, xpos,ypos, i0,i1, bsiz, lims, error, xlink, ylink)
  Nlink=N_ELEMENTS(xlink[*,0])
  
  FOR i=0,NDQ-1 DO IF (alf[i] ne error) THEN BEGIN
  
    print,i,' LINK: ',groups[i0[i]],' -> ',groups[i1[i]],' alf=',alf[i]
    
    xx_orig=REFORM(xlink[*,i],Nlink)
    yy_orig=REFORM(ylink[*,i],Nlink)  ;also checks length match
    
    ;---fix start and end---
    fix_startend, xx_orig, yy_orig, xpos[i0[i]], ypos[i0[i]], bsiz[i0[i]], 0., error, oxx, oyy    ;start
    fix_startend, REVERSE(oxx), REVERSE(oyy), xpos[i1[i]], ypos[i1[i]], bsiz[i1[i]], 0., error, xx,yy    ;end
    xx=REVERSE(xx)
    yy=REVERSE(yy)
    
    ;---get sections----
    Nxy=N_ELEMENTS(xx)-1
    index=FINDGEN(Nxy)
    v0=QQ[OKDQ[i]]    ;start is QQ
    v1=DD[OKDQ[i]]    ;end is DD
    vv=INTERPOL([v0,v1],[0.,Nxy-1.],index)
    bad=WHERE(vv lt 0. OR vv gt 1., Nbad)
    IF (Nbad gt 0) THEN STOP,'FAIL: bad vv'
    
    ;---translate to colours and thicknesses---
    colthick=MAKE_ARRAY(2,Nxy,/FLOAT,value=error)   ;colour [0-255] thick [1-3]
    colthick[0,*]=INTERPOL(ycols,xcols,vv)
    colthick[1,*]=INTERPOL(ythick,xthick,vv)
    
    ;---add arrows---
    thick=colthick[1,Nxy-1]
    thickfac=(arrow_p[1]-arrow_p[0])*(thick-tthick[0])/(tthick[1]-tthick[0]) + arrow_p[0]
    AA=arrow_A*scale*thickfac
    BB=arrow_B*scale*thickfac
    arrow_col=colthick[0,Nxy-1]
    add_arrows, xx, yy, xpos[i1[i]], ypos[i1[i]], AA, BB, arrow_col, error
    
    ;---fix last section---
    fix_startend, xx_orig, yy_orig, xpos[i0[i]], ypos[i0[i]], bsiz[i0[i]], 0., error, oxx, oyy    ;start
    fix_startend, REVERSE(oxx), REVERSE(oyy), xpos[i1[i]], ypos[i1[i]], bsiz[i1[i]], aheadoff*thickfac, error, xx,yy    ;end
    xx=REVERSE(xx)
    yy=REVERSE(yy)
    
    ;---oplot in sections----
    ;Nxy=N_ELEMENTS(xx)-1
    FOR j=0,Nxy-1 DO BEGIN
      OPLOT, xx[j:j+1], yy[j:j+1], line=0, COLOR=colthick[0,j], THICK=colthick[1,j]*scale
    ENDFOR
    
    
    
    
    ;IF (i eq 80) THEN STOP,'dads'
    
  ENDIF
  
  
  ;stop,'afdfaf'
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION plot_foodweb_diagram, groups, xpos0, TL, B, DD, QQ, in_outfile, error, NAMES=names

  ;----total size----
  NN=N_ELEMENTS(groups)
  IF (N_ELEMENTS(TL) ne NN) THEN STOP,'FAIL: plot_foodweb_diagram - mismatch size TL'
  IF (N_ELEMENTS(B) ne NN) THEN STOP,'FAIL: plot_foodweb_diagram - mismatch size B'
  IF (N_ELEMENTS(xpos0) ne NN) THEN STOP,'FAIL: plot_foodweb_diagram - mismatch size xlocate'
  siz=SIZE(DD)
  IF (siz[0] ne 2) THEN STOP,'FAIL: plot_foodweb_diagram - bad dims links'
  IF (siz[1] ne NN OR siz[2] ne NN) THEN STOP,'FAIL: plot_foodweb_diagram - mismatch size links'
  IF (N_ELEMENTS(names_) eq 1) THEN names=names_[0] ELSE names=1
  
  ;---check outfile---
  sep=STR_SEP(in_outfile[0],'.')
  IF (N_ELEMENTS(sep) ne 2) THEN STOP,'FAIL: plot_foodweb_diagram - bad outfile'
  outfile=sep[0]    ;no suffix
  osuff=sep[1]      ;"png"
  
  ;---graphics-----
  device,decomposed=0     ;sorts out colour table 60=blue; 100=cyan; 160=green; 220=red
  ctab=39
  loadct, ctab    ;Rainbow + white
  
  ;===params====
  scale=1.      ;overall size
  ;scale=5.      ;overall size
  
  aspect=0.6
  xsize=1300.
  marginx0=0.1
  marginx1=0.05
  marginy0=0.07
  marginy1=0.07

  TLexp=1.7
  
  ;---box sizes---  
  ;Bexp=0.1
  ;Bscale=[0.25,0.04]    ;[size of largest box, size of smallest box]  autoscale
  ;Bscale=[0.16,0.22,0]    ;fixed scale
  Bscale=[0.18,0.40,0]  &    marginy0=0.10
  
  
  ;---apply to size----
  xsiz=xsize*scale
  ysiz=aspect*xsiz
  
  mx0=marginx0*xsiz
  mx1=marginx1*xsiz
  my0=marginy0*ysiz/aspect
  my1=marginy1*ysiz/aspect
  
  
  
  
  xlo=mx0
  xhi=xsiz-mx1
  ylo=my0
  yhi=ysiz-my1
  lims=[0.1*mx0, xsiz-0.1*mx1, 0.1*my0, ysiz-0.1*my1]            ;lims=[lox,hix, loy,hiy]
  
  
  ;-----start plotting----
  win=0
  WINDOW, win, XSIZE=xsiz, YSIZE=ysiz, TITLE=run
  ;PLOT,[0.,1.],[0.,aspect], /NODATA,XSTYLE=5,YSTYLE=5,XMARGIN=[0,0],YMARGIN=[0,0],/YNOZERO,color=0,/NOERASE
  ;PLOT, [0.,xsiz], [0.,ysiz], /NODATA, XSTYLE=1, YSTYLE=1, color=0, XMARGIN=[0,0],YMARGIN=[0,0]
  PLOT, [0.,xsiz], [0.,ysiz], /NODATA, XSTYLE=5, YSTYLE=5, color=0, XMARGIN=[0,0],YMARGIN=[0,0], BACKGROUND=255
  
  
  ;---add y axis----
  ;yaxis = AXIS('Y', LOCATION=[xlo,0], TITLE='Trophic Level', TICKDIR=0, TEXTPOS=1, TICKVALUES=[1,2,3,4], TICKNAME=['1','2','3','4'])
  pos0=0.5*mx0
  pos01=[ylo,ysiz-0.5*my1]
  tiklabels=['1','2','3','4']
  tikvalues=get_ypos_TL(ylo, yhi, FLOAT(tiklabels), error)
  ;valsiz=[1.*scale, 1.*scale/2., 0., (yhi-ylo)*0.01]   ;[size of labels to add, thickness of text, xposadj, yposadj]
  valsiz=[1.5*scale, 1.5*scale, 0., (yhi-ylo)*0.01]   ;[size of labels to add, thickness of text, xposadj, yposadj]
  valpos=0.5*ylo
  tiklen=0.2*xlo
  col=[39,0]
  label='Trophic level'
  labelsiz=[1.5, 1.5]*scale     ;[1.2*scale, scale]      ;[size, thick]
  labelpos=0.3*my0
  thick=2.0*scale/2.   ;1.*scale/2.
  add_Maxis, 'y', pos0, pos01, tikvalues, tiklabels, valsiz, valpos, tiklen, col, label, labelsiz, labelpos, thick, error
  
  ;====boxes===
  
  ;---y-position - sort out TL scaling----
  ypos=get_ypos_TL(ylo, yhi, TL, error)
  
  ;---get box sizes----
  IF (N_ELEMENTS(Bscale) eq 2) THEN BEGIN
    Bbox=ALOG(Bscale[0]/Bscale[1])/ALOG(MAX(B)/MIN(B))
    Abox=Bscale[0]/(MAX(B)^Bbox)
    ;bsiz=bsiz/MAX(bsiz)*Bscale
    bsiz=(Abox*B^Bbox)*scale
    print,'box sizes: Abox=',Abox,' Bbox=',Bbox
  ENDIF ELSE BEGIN
    bsiz=(Bscale[0]*B^Bscale[1])*scale
  ENDELSE
  ;print,'box sizes:',bsiz
  
  
  
  bsiz_fac=(xhi-xlo) < (yhi-ylo)
  print,'bsiz_fac=',bsiz_fac
  bsiz_fac=500.
  bsiz=bsiz_fac*bsiz
  
  ;----get x position----
  xpos=xlo+(xhi-xlo)*xpos0
  ;res=fit_xlocate(bsiz, ypos, xpos, xhi, groups, xsiz, ysiz, error)
  
  ;----- group names or numbers-----
  IF (names eq 0) THEN glabels=STRCLEAN(1+INDGEN(N_ELEMENTS(groups))) $    ;numbers
  ELSE glabels=groups                                 ;group names

  ;--- add boxes ---
  bthick=1.5*scale    ;1.*scale
  ;txtsiz=1.5*scale    ;1.*scale
  ;txtthick=1.5*scale    ;1.*scale
  txtsiz=1.*scale    ;1.*scale
  txtthick=1.*scale    ;1.*scale
  
  txtsiz_all=MAKE_ARRAY(NN,/FLOAT,value=txtsiz)
  blim=percentile(bsiz,35.,error)
  OKsmall=WHERE(bsiz lt blim, Nsmall)
  txtsiz_all[OKsmall]=txtsiz_all[OKsmall]*0.7
  
  plot_boxes, xpos, ypos, bsiz, glabels, bthick, txtsiz_all, txtthick
  
  ;====add links====
  plot_links, DD, QQ, groups, xpos, ypos, bsiz, scale, lims, error
  
  
  ;====output====
  do_write=0
  IF (do_write eq 1) THEN BEGIN
      image = TVRead(/PNG, FILENAME=outfile, /NoDialog)
      print,'Image written to: ',outfile
      WDEL
  ENDIF
  
  
  ;stop,'fsdfgdf'
  
  RETURN,1
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------