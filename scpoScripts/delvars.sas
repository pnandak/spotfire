%macro delvars;
   data temp;
     set sashelp.vmacro;
     where scope='GLOBAL';
   run;
   data _null_;
     set temp;
     call symdel(name);
   run;
%mend delvars;

