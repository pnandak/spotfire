%macro delsql;
   %local deletevars;
   proc sql noprint;
      select name into : deletevars separated by ' '
        from dictionary.macros
        where scope='GLOBAL' and name not like 'SYS%';
   quit;
   %symdel &deletevars;
%mend delsql;
