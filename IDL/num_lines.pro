function num_lines, infile, NMAX=nmax 			;gets the number of lines of infile

  IF (N_ELEMENTS(nmax) eq 1) THEN nnmax=LONG(nmax[0]) ELSE nnmax=1.E10

  counter=LONG(0)
  OPENR, lun, infile, /get_lun		;Open the infile
  str = ''							;Define a string variable
  WHILE (NOT EOF(lun) AND counter lt nnmax) DO BEGIN			;Loop until EOF is found
      READF, lun, str					;Read a line of text
   	  counter=counter+1L
  ENDWHILE
  free_lun,lun
  ;print,'lines=',counter
  return, counter		;this is the total number of data lines
  end
  
  