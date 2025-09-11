## Todo list.

- Autohotkey V1 으로 작성해줘. 
- If I press F9:: I want to remove any string after date and input DH2025.KIM SAP ID Replace.

- You can refer to the below example.   

```
As-IS
* U3 C12345678901  GSDK!@#$%^ 2025.09.10 {some text1} {some text2} 
To-be
* U3               GSDK!@#$%^ 2025.09.11 {DH2025.KIM} {SAP ID Replace} 
```
- Also it should cover N > U1 > U2 > .... 
- Also it should cover 김동현 > DH2025.KIM 
- Alsi it should cover remove rest of string after DH2025.KIM and insert SAP ID Replace 

## After fixing

- Git add . > git commit with specific comment > git push  



