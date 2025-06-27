;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
; program to plot foodweb diagrams
;
; M Pinkerton
;
; 7 Sep 2013
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION HG_get_Q_array, groups, key, diets, type, method, error

TINY=1.0E-9
SMALL=1.0E-5

  Nx=N_ELEMENTS(groups)
  OKnondet=WHERE(type lt 2, Nnondet)
  OKnoncon=WHERE(type ne 0, Nnoncon)
  OKdet=WHERE(type eq 2, Ndet)
  
  verb=0

  ;----to use---
  use=MAKE_ARRAY(Nx,/INT,value=1)
  use[OKdet]=0    ;do not use detritus
  FOR i=0,Nx-1 DO BEGIN
    pos=STRPOS(STRLOWCASE(groups[i]),'bacteria')
    IF (pos ne -1) THEN use[i]=0        ;do not use
  ENDFOR
  OKnondet_nonbac=WHERE(use eq 1)

;---make arrays-----
Bvector=key[0,*]    ;B

Qvector=key[2,*]    ;Q/B
lo=WHERE(Qvector lt 0., Nlo)
IF (Nlo gt 0) THEN Qvector[0,lo]=0.

QBvector=Bvector*Qvector    ;Q
QBvector[OKnoncon]=0.   ;zero consumption for non-consumers

;---get production---
Pvector=key[0,*]*key[1,*]
Pvector[OKdet]=0.     ;zero production for detritus

;----make g array-----
g_array=diets
lo=WHERE(diets lt 0., Nlo)
IF (Nlo gt 0) THEN g_array[lo]=0.
;help,g_array

;----make f array-----
  Nx=N_ELEMENTS(diets[*,0])
  Ny=N_ELEMENTS(diets[0,*])
  f_array=MAKE_ARRAY(Nx,Ny,/DOUBLE,value=error)
  QD=diets##QBvector

  FOR j=0,Nx-1 DO $         ;prey=j[col]   pred=i[row]
    FOR i=0,Ny-1 DO BEGIN
      top=QBvector[i]*diets[i,j]

      IF (method eq 'ecopath') THEN bot=QD[j] $
      ELSE IF (method eq 'Pbased') THEN bot=Pvector[j] $
      ELSE STOP,'Unrecognised method: >',method,'<'

      IF (bot gt 0.) THEN BEGIN
        tol=1.E-6
        IF ((top-bot)/bot gt tol) THEN BEGIN
          ;STOP,'FAIL: bot<top: predator=',groups[i],' prey=',groups[j]
          print,'WARNING: bot<top: predator=',groups[i],' prey=',groups[j]
          f_array[j,i]=1.0
        ENDIF ELSE BEGIN
          f_array[j,i]=top/bot
        ENDELSE
      ENDIF ELSE f_array[j,i]=0.
    ENDFOR
lo=WHERE(f_array lt 0.,Nlo)
IF (Nlo gt 0) THEN STOP,'Fail: some f_array negative values'

  ;---test f----
  print,'--- f_array ---'
  FOR j=0,Nx-1 DO BEGIN
    tot=TOTAL(f_array[j,*])
    diff=ABS(tot-1)
    print,j, ' ',groups[j],' -> ',diff
    IF (diff gt small AND diff lt (1.-small)) THEN BEGIN
      IF (tot lt small) THEN txt='No predators' ELSE txt='bad total'
      print,'WARNING:  f_array - ',txt,' - ',groups[j]
      ;STOP,'FAIL:  f_array - bad total: ',groups[j]
    ENDIF
  ENDFOR
  ;stop,'farray'

;---make Q array-----
  Q_array=g_array-f_array
  IF (verb) THEN BEGIN
    print,'Q_array: ',MIN(Q_array),MAX(Q_array)
    help,Q_array
  ENDIF

  ;---set diagonal to zero ???----
  FOR i=0,Nx-1 DO BEGIN
    IF (verb) THEN $
      IF (ABS(Q_array[i,i]) gt small) THEN print,i,' Q_array: ',groups[i],' -> ',Q_array[i,i]
    ;Q_array[i,i]=0.
  ENDFOR

  ;---show column sums---
  print,'--Column sums--'
  gs=TOTAL(g_array,2)   ;column sums
  fs=TOTAL(f_array,2)   ;column sums
  qs=TOTAL(Q_array,2)   ;column sums
  IF (verb) THEN FOR i=0,Nx-1 DO print,i,' col ',groups[i],' Q=',qs[i],' g=',gs[i],' f=',fs[i]

  ;---show row sums---
  print,'--Row sums--'
  gs=TOTAL(g_array,1)   ;row sums
  fs=TOTAL(f_array,1)   ;row sums
  qs=TOTAL(Q_array,1)   ;row sums
  IF (verb) THEN FOR i=0,Nx-1 DO print,i,' row ',groups[i],' Q=',qs[i],' g=',gs[i],' f=',fs[i]
  ;stop,'sdfsd'

  ;---transpose Q matrix----
  Q_array=TRANSPOSE(Q_array)    ;transpose

RETURN, Q_array
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION HG_get_M_array, groups, key, diets, type, method, error

TINY=1.0E-9
SMALL=1.0E-5
  
  Nx=N_ELEMENTS(groups)
  OKnondet=WHERE(type lt 2, Nnondet)
  OKnoncon=WHERE(type ne 0, Nnoncon)
  OKdet=WHERE(type eq 2, Ndet)
  
  verb=0
  
  ;---first get Q array----
  Q_array=HG_get_Q_array(groups, key, diets, type, method, error)

;---produce A matrix-----
ID=IDENTITY(Nx)
A=DOUBLE(ID-Q_array)
;info,A,'A',error

;-----check condition of A------
chk = COND(A, /DOUBLE)
IF (chk eq -1) THEN STOP,'Cannot invert A: chk=',chk ELSE print,'A condition check=',chk

  ;---check B---
  ;B=ID+Q_array
  ;chk = COND(B, /DOUBLE)
  ;IF (chk eq -1) THEN STOP,'Cannot invert B: chk=',chk ELSE print,'B Condition check=',chk

;----matrix inversion------
invA=INVERT(A, /DOUBLE)   ;run in DOUBLE

;----calculate M----
out=TRANSPOSE(invA)-ID
IF (verb) THEN BEGIN
  print,'M output: ',MIN(out),MAX(out)
  help,out
ENDIF

;---do test-----
test=ID
dummy=Q_array
print,'Q: dummy: ',MIN(dummy),MAX(dummy)
;NN=100
NN=10
FOR i=0L,NN DO BEGIN
  ;print,i,MAX(ABS(dummy))
  test=test+dummy
  dummy=Q_array##dummy
  ;print,i,' dummy: ',MIN(dummy),MAX(dummy)
  print,i,' test: ',MIN(test),MAX(test)
ENDFOR
test=test-ID
print,'test: ',MIN(test),MAX(test)
diff=ABS(test-out)
print,'diff: ',MIN(diff),MAX(diff)
;IF (MAX(diff) gt 1.E-9) THEN STOP,'Bad check: ',MAX(diff)
IF (MAX(diff) gt 1.E-9) THEN print,'WARNING!! Bad check: ',MAX(diff)
;stop,'fdsfsd'

;---check row and column totals----
  print,'---M matrix (out)---'
  tot=TOTAL(out,2)
  print,'row SUM: ',MIN(tot),MAX(tot)
  tot=TOTAL(out,1)
  print,'column SUM: ',MIN(tot),MAX(tot)

RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
PRO main_diagram
;COMMON globals
;set_globals

    verb=1
    error=-9999.0D
    force_overwrite=1
    
    ;====parameters=====
    
    
    ;-----starting runs-------
    ;run='HG03'
    run='HG04'
    ;run='HG07'
    
    
    ;date=''    ;present day
    ;date='1950'
    ;date='1790'
    ;date='1500'
    ;date='1000'
    ;dates=['','1950','1790','1500','1000']
    dates=['']
    
    Mrun='NoIsotope'
    ;Mrun='IsoBal'
    
    ;method='ecopath'
    ;method='Pbased'
    method='import'

;---directories---------
    basedir='C:\NIWA_case2\_Hauraki_model\'          ;Hauraki Gulf model
    indir=basedir+'dat_files\'
    ;outdir=basedir+'diagram\'
    ;outdir=basedir+'dat_files\'
    outdir=basedir+'diagram\'+run+'\'
    
    ;indir=basedir+'ISObal\'+run+'\baseline\'
    ;outdir=indir+'diagram\'
    
    
    ;---work through dates---
    FOR idate=0,N_ELEMENTS(dates)-1 DO BEGIN
        date=dates[idate]
        
        
        
        ;----make outfile---
        orun=run+'_'+Mrun

        ;outfile=outdir+run+'_diagram_01.png'    ;png      ;scale =1
        ;outfile=outdir+run+'_diagram_02.png'    ;png      ;scale = 5
        ;outfile=outdir+run+'_diagram_03.png'    ;png      ;scale = 5

        ;outfile=outdir+run+'_diagram_04.png'    ;png      ;scale = 5; Apr 2016 redrawing
        ;outfile=outdir+run+'_diagram_04_bold.png'    ;png      ;scale = 5; Apr 2016 redrawing
        ;outfile=outdir+orun+'_diagram_04_bold_nums.png'    ;png      ;scale = 5; Apr 2016 redrawing
        ;outfile=outdir+run+'_diagram_04_bold_nums_TEST.png'    ;png      ;scale = 5; Apr 2016 redrawing

        outfile=outdir+run
        IF (date eq '') THEN outfile+='_present' ELSE outfile+='_'+date
        outfile+='_0p18-0p40'
        outfile+='_diagram.png'
        
        
        ;---check----
        res=FINDFILE(outfile,COUNT=Nfnd)
        IF (Nfnd ne 0 AND NOT force_overwrite) THEN BEGIN
            print,'Skipping: ',outfile
            GOTO,next_outfile
        ENDIF


        
    
    ;---historical models---
        run0=run
        indir0=indir
        IF (date ne '') THEN BEGIN
            indir0=indir+date+'\'
            run0=run0+'_'+date
        ENDIF
        
        key_run=run0
        diet_run=run0
    
        IF (Mrun ne 'NoIsotope') THEN BEGIN
            key_run+='_'+Mrun
            diet_run+='_'+Mrun
        ENDIF
        key_run+='-key-adj'
        diet_run+='-diets-adj'
        suff='out'
      
      ;---make files-----
      keyfile=indir0+key_run+'.'+suff
      dietfile=indir0+diet_run+'.'+suff
      TLfile=indir0+key_run+'-out-trophic_levels.out'
    
        IF (date eq '' OR date eq '1950' OR date eq '1790') THEN BEGIN
              xpos0_file=indir+run+'-xpos0.txt'
        ENDIF ELSE BEGIN         
              xpos0_file=indir+run+'_1500-xpos0.txt'
        ENDELSE
   

    
    
    ;---import and check data-----
        res=import_check(keyfile, dietfile, error, key, diets, type, groups, Nx, keyhed, dietshed)
        key=DOUBLE(key)
        diets=DOUBLE(diets)
        orig_key=key
        orig_diets=diets
        OKnondet=WHERE(type lt 2, Nnondet)
        OKnoncon=WHERE(type ne 0, Nnoncon, COMPLEMENT=OKcon, NCOMPLEMENT=NOKcon)
        OKdet=WHERE(type eq 2, Ndet)
        
    ;---get TL----
        ;TL=get_trophic_levels_from_file(TLfile, groups, error)
        
        ;TLfile=indir+run+'_'+Mrun+'*.hdf'
        res=FINDFILE(TLfile,COUNT=Nfnd)
        IF (Nfnd ne 1) THEN STOP,'not found ',TLfile
        TLfile=res[0]
        ;TL=open_Mhdf(TLfile,'baseline_TL',error)
        TLdat=open_named_delimited(TLfile, tab(), xgroups, xhed, error)
        TL=REFORM(TLdat[0,*],Nx)
        
        bad=WHERE(TL eq error, Nbad)
        IF (Nbad gt 0) THEN STOP,'FAIL: getting TL'
        IF (N_ELEMENTS(TL) ne Nx) THEN STOP,'FAIL: bad size TL'
        
    
    ;--get starting xlocate---
        xpos0=open_named(xpos0_file, xgroups, xhed, error)
        IF (N_ELEMENTS(xgroups) ne Nx) THEN STOP,'Bad size xpos0 file: ',xpos0_file
        OKdiag=WHERE(xpos0 ne error, NOKdiag)
    
    ;---get groups to plot----
        TL0=TL[OKdiag]
        groups0=xgroups[OKdiag]
        B0=key[0,OKdiag]
    
        DD=diets[OKdiag,*]
        DD=DD[*,OKdiag]
    
    ;---get Q_array---
        method='Pbased'
        P=key[0,OKdiag]*key[1,OKdiag]
        Q=key[0,OKdiag]*key[2,OKdiag]
        QQ=get_QQ(P,Q,DD,error)
        
        
        ;stop,'dsasd'
    
    ;---call plotting---
        ;help,groups
        res=plot_foodweb_diagram(groups0, xpos0[OKdiag], TL0, B0, DD, QQ, outfile, error, NAMES=1)
        ;WDEL
    
    
  next_outfile:
  ENDFOR  ;next date


;---tidy up---
close,/all
STOP,'End of program'
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
