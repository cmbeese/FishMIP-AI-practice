FUNCTION open_named_delimited, file, delimiter, names, header, error, $
        HEDSEP=hedsep, QUIET=quiet, DOUBLE=double, FIELDS=fields, STR2NUMcheck=STR2NUMcheck
        
    IF (N_ELEMENTS(STR2NUMcheck) eq 1) THEN dos2n=STR2NUMcheck[0] ELSE dos2n=0
    IF (N_ELEMENTS(quiet) ne 1) THEN quiet=0
    IF (N_ELEMENTS(double) ne 1) THEN dub=0 ELSE dub=double[0]
    IF (dub) THEN error=DOUBLE(error) ELSE error=FLOAT(error)

    IF NOT (quiet) THEN print,'Open_named_delimited: ',file
    header=''
    lines=num_lines(file)-1			; number of data lines
    ;print,'lines= ',lines
    names=MAKE_ARRAY(lines,/STRING,VALUE='')		; number of data lines long
    IF (lines le 0) THEN BEGIN
    	IF NOT (quiet) THEN print,'Open_named_delimited: Error opening: ',file
    	RETURN,error
    ENDIF
    
    str = ''							;Define a string variable
    OPENR, lun, file, /get_lun		;Open the file
    READF, lun, str						;Read the headers: first line
    ;str=STRTRIM(STRCOMPRESS(str),2)
    ;header=STRUPCASE(str)
    header=str
    hedsep=STR_SEP(header,delimiter)
    fields=hedsep[1:*]
    N_columns=N_ELEMENTS(hedsep)
    IF (dub) THEN data=MAKE_ARRAY(N_columns-1,lines,/DOUBLE,VALUE=error) $		; number of data lines long
    ELSE data=MAKE_ARRAY(N_columns-1,lines,/FLOAT,VALUE=error)
    
    FOR i=0L,lines-1 DO BEGIN
    	READF, lun, str					;Read each line of data as text
       	;str=STRTRIM(STRCOMPRESS(str),2)
       	dummy=STR_SEP(str, delimiter)			;Separate out into variables
       	Nd=N_ELEMENTS(dummy)
    	IF (Nd ge 1) THEN names[i]=dummy[0]
    	IF (Nd eq N_columns) THEN BEGIN
    	   
    	   IF (dos2n) THEN data[*,i]=STR2NUM(dummy[1:*],error) $
    	   ELSE IF (dub) THEN data[*,i]=DOUBLE(dummy[1:*]) ELSE data[*,i]=FLOAT(dummy[1:*])

    	   
    	ENDIF ELSE IF (Nd gt 1) THEN BEGIN
    		IF (Nd gt N_columns) THEN BEGIN
    		    IF (dos2n) THEN data[*,i]=STR2NUM(dummy[1:N_columns-1],error) $
    		    ELSE IF (dub) THEN data[*,i]=DOUBLE(dummy[1:N_columns-1]) ELSE data[*,i]=FLOAT(dummy[1:N_columns-1])
    		ENDIF ELSE BEGIN
    		    IF (dos2n) THEN data[0:Nd-2,i]=STR2NUM(dummy[1:*],error) $
    		    ELSE IF (dub) THEN data[0:Nd-2,i]=DOUBLE(dummy[1:*]) ELSE data[0:Nd-2,i]=FLOAT(dummy[1:*])
    		ENDELSE
    	ENDIF
    	;print,i,' -> ',dummy
    ENDFOR
    FREE_LUN,lun
    IF NOT (quiet) THEN print,'Data file: ',file,' columns= ',N_columns, ' lines= ',lines
    ;stop,'open named'
    
RETURN, data
END