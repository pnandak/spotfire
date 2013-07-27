  /* Create Macro Version of BETWEEN Operator */

%macro between(var,low,high);
   %sysevalf(&var >= &low and &var <= &high)
%mend between;
