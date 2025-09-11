## Todo list.

- Autohotkey V1 으로 작성해줘. 
- If I press F9:: , I would like to keep the same format except for date(as today), C12345678901(12length but replaced into spaces) name(korean 3 char into DH2025.KIM), and remove all string after that. and insert "SAP ID Replace" -> OKAY

- it works well. but there are various formats. now I want to remove any string after date and input DH2025.KIM SAP ID Replace.

- You can refer to the below example.   
```
As-IS
* U3 C12345678901  GSDK!@#$%^ 2025.09.10 {some text1} {some text2} 
To-be
* U3               GSDK!@#$%^ 2025.09.11 {DH2025.KIM} {SAP ID Replace} 
```


## After fixing

- Git add . > git commit with specific comment > git push  



