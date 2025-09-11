## Todo list.

아래와 같은 코드를 인식하지못하고있어 
Please fix it to record ZMC0042 / ERNAM / INSERT 

```
ptab-ernam = sy-uname.
ptab-erdat = sy-datum
ptab-uzeit = sy-uzeit.
ptab-aedat = sy-aedat.

append ptab_alt. clear ptab_alt.

loop at pt_alt.
at new werks.
delete from zmc0042
where matnr = ptab-matnr
and werks = ptab-werks.
endat.
endloop.

insert zmc0042 from table ptab accepting duplicate keys.
commit work.
```

## After fixing

- Git add . > git commit with specific comment > git push  



