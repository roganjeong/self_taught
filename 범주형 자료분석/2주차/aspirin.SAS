data aspirin;
input group $ mi$ count;
cards;
placebo yes 189
placebo no 10845
aspirin yes 104
aspirin no 10933
;
proc freq data=aspirin order=data;
weight count;
tables group*mi/measures;
run;
