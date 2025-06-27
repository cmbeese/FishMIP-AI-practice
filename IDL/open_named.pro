function open_named, file, names, header, error, HEDSEP=hedsep, VERB=verb
    
    IF (N_ELEMENTS(verb) ne 1) THEN verb=1
    
    IF (verb) THEN print,'Open_named: ',file
    header=''
    lines=num_lines(file)-1			; number of data lines
    ;print,'lines= ',lines
    names=MAKE_ARRAY(lines,/STRING,VALUE='')		; number of data lines long
    IF (lines le 0) THEN BEGIN
    	print,'Error opening: ',file
    	RETURN,error
    ENDIF
    
    str = ''							;Define a string variable
    OPENR, lun, file, /get_lun		;Open the file
    READF, lun, str						;Read the headers: first line
    str=STRTRIM(STRCOMPRESS(str),2)
    ;header=STRUPCASE(str)
    header=str
    hedsep=STR_SEP(header,' ')
    N_columns=N_ELEMENTS(hedsep)
    data=MAKE_ARRAY(N_columns-1,lines,/FLOAT,VALUE=error)		; number of data lines long
    FOR i=0L,lines-1 DO BEGIN
    	READF, lun, str					;Read each line of data as text
       	str=STRTRIM(STRCOMPRESS(str),2)
       	dummy=STR_SEP(str, ' ')			;Separate out into variables
       	Nd=N_ELEMENTS(dummy)
    	IF (Nd ge 1) THEN names[i]=dummy[0]
    	IF (Nd eq N_columns) THEN data[*,i]=FLOAT(dummy[1:*]) $
    	ELSE IF (Nd gt 1) THEN BEGIN
    		IF (Nd gt N_columns) THEN data[*,i]=FLOAT(dummy[1:N_columns-1]) $
    		ELSE data[0:Nd-2,i]=FLOAT(dummy[1:*])
    	ENDIF
    	;print,i,' -> ',dummy
    ENDFOR
    free_lun,lun
    IF (verb) THEN print,'Data file: ',file,' columns= ',N_columns, ' lines= ',lines
    ;stop,'open named'
    
return, data
end