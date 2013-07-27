%macro charlist(lookupdsn, lookupvar);
   %local j;
   data _null_;
      set  &lookupdsn end=final;
      call symputx(cats("&lookupvar",_n_), quote(&lookupvar),'L');
      if final=1 then call symputx('n',_n_,'L');
   run;

   %do j= 1 %to &n;
      %let clist=&clist &&&lookupvar&j;
   %end;
%mend charlist;
