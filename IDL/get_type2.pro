;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;
;type2: -1=bac 0=con 1=PP 2=det 3=carcasses
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION get_type2, type, groups_

    groups=STRLOWCASE(groups_)
    type2=type
    N=N_ELEMENTS(groups)
    
    FOR i=0,N-1 DO BEGIN
      
        pos=STRPOS(groups[i], 'bacteria')
        IF (pos ne -1) THEN type2[i]=-1
        
        pos=STRPOS(groups[i], 'carcass')
        IF (pos ne -1) THEN type2[i]=3
      
    ENDFOR
    
    ;---checks-----
    bad=WHERE(type2 lt -1 OR type2 gt 3, Nbad)
    IF (Nbad gt 0) THEN STOP,'FAIL: get_type2 - some bad values'
    
    FOR i=-1,3 DO BEGIN
      OK=WHERE(type2 eq i, NOK)
      IF (NOK le 0) THEN BEGIN
          ;print,'FAIL: get_type2 - cannot find: ',i
          ;STOP,'asdsa'
      ENDIF
      IF (i eq 3 AND NOK gt 1) THEN STOP,'FAIL: get_type2 - too many carcasses'
    ENDFOR
    

RETURN,type2
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------