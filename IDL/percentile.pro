;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;
;   normal percentile fucntion
;   
;   "in" must be 0-100
;   
;
;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
FUNCTION percentile, in, in_pc, error
 
    ;----make out----
    pc=FLOAT(in_pc)
    out=pc-pc+error   ;same size and shape as pc filled with errors
 
	;---check OK data---
	OK=WHERE(in ne error, NOK)
	IF (NOK le 0) THEN BEGIN
		;print,'FAIL: percentile - no data'
		RETURN,out
	ENDIF
	data=in[OK]
	data=data[SORT(data)]	;ascending

	;---check pc OK----
	pcOK=WHERE(pc ge 0. AND pc le 100.,NpcOK)
	IF (NpcOK le 0) THEN BEGIN
		STOP,'FAIL: percentile - bad perceniles requested'
		info,pc,'percentile: ',error
		RETURN,out
	ENDIF

	x=pc[pcOK]/100.*FLOAT(NOK-1)
	lo=FLOOR(x)
	lo=0>lo
	dlo=data[lo]
	hi=CEIL(x)
	hi=hi<(NOK-1)
	dhi=data[hi]

	;bad=WHERE(hi-lo ne 1,Nbad)
	;IF (Nbad gt 0) THEN STOP,'FAIL: percentile'	;should be imposssible

	;out=(dlo+dhi)/2.		;average of bracketing values
	out[pcOK]=dlo+(x-FLOAT(lo))*(dhi-dlo)
	
	IF (N_ELEMENTS(out) eq 1) THEN out=out[0]    ;make scalar

;stop,'test'
RETURN,out
END
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------



