# Compilador da linguagem Arith para Assembly MIPS

## Funcionalidades incluídas

**Declaração de variáveis**
> **(formalmente)** set nome_variavel = valor;\
**(exemplo)** set x = 3;

**Atribuição de valores a variáveis**
> **(formalmente)** nome_variavel = valor;\
**(exemplo)** x = 4;

print x; # impressao no stdout

x = 2 + 2; # operacoes de soma, subtracao, multiplicacao e divisao

if x>4 then (print x;) else (pass;) # construcao if-else

while x<6 do (x = x + 1;) # ciclo while

print x; # deve imprimir o valor 6

if true then (if true then (print 3;) else (pass;)) else (pass;) # construcoes if-else aninhadas

if !4<3 then (print 20;) else (pass;) # deve imprimir 20