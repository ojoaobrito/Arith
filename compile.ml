open Format
open Mips
open Ast

exception VarUndef of string (* variável não declarada *)

(* contadores que garantem que o labels "main", "else", "loop" e "exit" nunca se repetem *)
let contador_main = ref 1
let contador_if = ref 1
let contador_while = ref 1

let variables = ref (Array.make 0 ("",0)) (* as variáveis globais são arquivadas num array *)
module StrMap = Map.Make(String)

let data_variables = fun variables -> (* função auxiliar que carrega para o ficheiro as variáveis, o tipo de dados e o valor inicial *)
                                    let code = ref nop in
                                    for i=0 to ((Array.length !variables)-1) do 
                                    begin
                                        let variable = (match !variables.(i) with (a,b) -> a) in
                                        let numero = (match !variables.(i) with (a,b) -> b) in
                                        code := !code ++ ((label variable) ++ dword numero)
                                    end
                                    done;
                                    !code

let find_variable = fun variables var -> (* função auxiliar que verifica se a variável "var" já foi declarada ou não *)
                                    let final = ref false in
                                    for i=0 to (Array.length variables)-1 do 
                                    begin 
                                        if((compare (match variables.(i) with (a,b) -> a) var)==0) then final := true 
                                    end 
                                    done;
                                    !final

let compile_expr = (* função que compila expressões *) 

  let rec comprec env next = function

    | I i -> (* inteiros *)
            li t0 i ++
            push t0    

    | Var x -> (* variáveis *)
            begin
            if(find_variable !variables x) then begin
                lvw t0 x ++
                push t0
            end
            else raise (VarUndef (x))
            end

    | Op (Add, e1, e2)-> (* operação de soma *)
                        comprec env next e1 ++
                        comprec env next e2 ++
                        pop t0 ++
                        pop t1 ++
                        addr t0 t0 t1 ++
                        push t0

    | Op (Sub, e1, e2)-> (* operação de subtração *)
                        comprec env next e1 ++
                        comprec env next e2 ++
                        pop t0 ++
                        pop t1 ++
                        subr t0 t1 t0 ++
                        push t0
    | Op (Mul, e1, e2)-> (* operação de multiplicação *)
                        comprec env next e1 ++
                        comprec env next e2 ++
                        pop t0 ++
                        pop t1 ++
                        mult t0 t1 ++
                        mflo a2 ++
                        push a2
    | Op (Div, e1, e2)-> (* operação de divisão *)
                        comprec env next e1 ++
                        comprec env next e2 ++
                        pop t0 ++
                        pop t1 ++
                        div t1 t0 ++
                        mflo a2 ++
                        push a2
  in
  comprec StrMap.empty 0

(* função que compila instruções *) 
let rec compile_instr = function 

    | Dec (x, e) -> (* declaração de variáveis *)
                (match e with | I i -> variables := (Array.append !variables [|(x,i)|])); 
                nop

    | Set (x, e) -> (* atribuição de expressões a variáveis *)
                let code = ref nop in
                if(find_variable !variables x) then begin
                code := !code ++ (compile_expr e ++ pop t0 ++ svw t0 x); !code
                end
                else raise (VarUndef (x))
                !code

    | Print e -> (* imprimir uma expressão/variável *)
                compile_expr e ++
                pop t0 ++
                li v0 1 ++
                move a0 t0 ++
                syscall ++
                li v0 4 ++
                la a0 "newline" ++
                syscall

    | Cond (t, i1, i2) -> (* if-else *)
                        begin

                        match t with
                        | Neg t_in -> begin (* o compilador faz pré-processamento *)
                        
                                        match t_in with
                                        | B true -> compile_instr (Cond (B false,i1,i2))

                                        | B false -> compile_instr (Cond (B true,i1,i2))

                                        | Comp (e1,e2) -> begin
                                                                let code = ref nop in
                                                                let code2 = ref nop in
                                                                let code3 = ref nop in
                                                                let code4 = ref nop in
                                                                let code5 = ref nop in
                                                        
                                                                begin
                                                                        code := compile_expr e1 ++
                                                                        compile_expr e2 ++
                                                                        pop t0 ++
                                                                        pop t1 ++
                                                                        beq t0 t1 ("else" ^ string_of_int(!contador_if));
                                                                        
                                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                                        
                                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                                        
                                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                                        
                                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                                        contador_if := !contador_if + 1;
                                                                        contador_main := !contador_main + 1;
                                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                                end
                                                        end

                                        | Big (e1,e2) -> compile_instr (Cond (SmallEqual(e1,e2),i1,i2))

                                        | BigEqual (e1,e2) -> compile_instr (Cond (Small(e1,e2),i1,i2))

                                        | Small (e1,e2) -> compile_instr (Cond (BigEqual(e1,e2),i1,i2))

                                        | SmallEqual (e1,e2) -> compile_instr (Cond (Big(e1,e2),i1,i2))
                                end

                        | B true -> begin 
                                        let code = ref nop in 
                                        for i=0 to ((List.length i1)-1) do code := !code ++ compile_instr (List.nth i1 i) done; 
                                        !code 
                                    end

                        | B false -> begin 
                                        let code = ref nop in 
                                        for i=0 to ((List.length i2)-1) do code := !code ++ compile_instr (List.nth i2 i) done; 
                                        !code 
                                    end

                        | Comp (e1, e2) -> (* a==b *)
                                        begin
                                                let code = ref nop in
                                                let code2 = ref nop in
                                                let code3 = ref nop in
                                                let code4 = ref nop in
                                                let code5 = ref nop in
                                        
                                                begin
                                                        code := compile_expr e1 ++
                                                        compile_expr e2 ++
                                                        pop t0 ++
                                                        pop t1 ++
                                                        bgt t0 t1 ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                        
                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                        
                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                        contador_if := !contador_if + 1;
                                                        contador_main := !contador_main + 1;
                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                end
                                        end

                        | Big (e1, e2) -> (* a>b *)
                                        begin
                                                let code = ref nop in
                                                let code2 = ref nop in
                                                let code3 = ref nop in
                                                let code4 = ref nop in
                                                let code5 = ref nop in
                                        
                                                begin
                                                        code := compile_expr e1 ++
                                                        compile_expr e2 ++
                                                        pop t0 ++
                                                        pop t1 ++
                                                        ble t1 t0 ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                        
                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                        
                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                        contador_if := !contador_if + 1;
                                                        contador_main := !contador_main + 1;
                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                end
                                        end

                        | BigEqual (e1, e2) -> (* a>=b *)
                                        begin
                                                let code = ref nop in
                                                let code2 = ref nop in
                                                let code3 = ref nop in
                                                let code4 = ref nop in
                                                let code5 = ref nop in
                                        
                                                begin
                                                        code := compile_expr e1 ++
                                                        compile_expr e2 ++
                                                        pop t0 ++
                                                        pop t1 ++
                                                        blt t1 t0 ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                        
                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                        
                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                        contador_if := !contador_if + 1;
                                                        contador_main := !contador_main + 1;
                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                end
                                        end

                        | Small (e1, e2) -> (* a<b *)
                                        begin
                                                let code = ref nop in
                                                let code2 = ref nop in
                                                let code3 = ref nop in
                                                let code4 = ref nop in
                                                let code5 = ref nop in
                                        
                                                begin
                                                        code := compile_expr e1 ++
                                                        compile_expr e2 ++
                                                        pop t0 ++
                                                        pop t1 ++
                                                        bge t1 t0 ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                        
                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                        
                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                        contador_if := !contador_if + 1;
                                                        contador_main := !contador_main + 1;
                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                end
                                        end

                        | SmallEqual (e1, e2) -> (* a<=b *)
                                        begin
                                                let code = ref nop in
                                                let code2 = ref nop in
                                                let code3 = ref nop in
                                                let code4 = ref nop in
                                                let code5 = ref nop in
                                        
                                                begin
                                                        code := compile_expr e1 ++
                                                        compile_expr e2 ++
                                                        pop t0 ++
                                                        pop t1 ++
                                                        bgt t1 t0 ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i1)-1) do code2 := !code2 ++ compile_instr (List.nth i1 i) done; 
                                                        
                                                        code3 := j ("main" ^ string_of_int(!contador_main)) ++
                                                        label_jump ("else" ^ string_of_int(!contador_if));
                                                        
                                                        for i=0 to ((List.length i2)-1) do code4 := !code4 ++ compile_instr (List.nth i2 i) done; 
                                                        
                                                        code5 := label_jump ("main" ^ string_of_int(!contador_main));
                                                        contador_if := !contador_if + 1;
                                                        contador_main := !contador_main + 1;
                                                        (!code ++ !code2 ++ !code3 ++ !code4 ++ !code5)
                                                end
                                        end
                        end

    | Loop (t, i) -> (* while loop *)
                    begin
                        match t with
                        | Neg t_in -> begin (* o compilador faz pré-processamento *)
                        
                                        match t_in with
                                        | B true -> compile_instr (Loop (B false,i))

                                        | B false -> compile_instr (Loop (B true,i))

                                        | Comp (e1,e2) -> begin
                                                                let code = ref nop in
                                                                let code2 = ref nop in
                                                                let code3 = ref nop in

                                                                begin
                                                                        code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                        compile_expr e1 ++
                                                                        compile_expr e2 ++
                                                                        pop t0 ++
                                                                        pop t1 ++
                                                                        beq t0 t1 ("exit" ^ string_of_int(!contador_while));

                                                                        for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                        code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                        label_jump ("exit" ^ string_of_int(!contador_while));
                                                                        contador_while := !contador_while + 1;
                                                                        (!code ++ !code2 ++ !code3)
                                                                end
                                                        end

                                        | Big (e1,e2) -> compile_instr (Loop (SmallEqual(e1,e2),i))

                                        | BigEqual (e1,e2) -> compile_instr (Loop (Small(e1,e2),i))

                                        | Small (e1,e2) -> compile_instr (Loop (BigEqual(e1,e2),i))

                                        | SmallEqual (e1,e2) -> compile_instr (Loop (Big(e1,e2),i))
                                end
                        
                        | B true -> begin
                                                let code = label_jump ("loop" ^ string_of_int(!contador_while)) in
                                                let code2 = ref nop in
                                                let code3 = ref nop in

                                                begin
                                                        for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;
                                        
                                                        code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                        label_jump ("exit" ^ string_of_int(!contador_while));
                                                        contador_while := !contador_while + 1;
                                                        (code ++ !code2 ++ !code3)
                                                end
                                        end

                        | B false -> nop (* não fazer nada *)

                        | Comp (e1, e2) -> (* a==b *)
                                                begin
                                                        let code = ref nop in
                                                        let code2 = ref nop in
                                                        let code3 = ref nop in

                                                        begin
                                                                code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                compile_expr e1 ++
                                                                compile_expr e2 ++
                                                                pop t0 ++
                                                                pop t1 ++
                                                                bgt t0 t1 ("exit" ^ string_of_int(!contador_while));

                                                                for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                label_jump ("exit" ^ string_of_int(!contador_while));
                                                                contador_while := !contador_while + 1;
                                                                (!code ++ !code2 ++ !code3)
                                                        end
                                                end

                        | Big (e1, e2) -> (* a>b *)
                                                begin
                                                        let code = ref nop in
                                                        let code2 = ref nop in
                                                        let code3 = ref nop in

                                                        begin
                                                                code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                compile_expr e1 ++
                                                                compile_expr e2 ++
                                                                pop t0 ++
                                                                pop t1 ++
                                                                ble t1 t0 ("exit" ^ string_of_int(!contador_while));

                                                                for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                label_jump ("exit" ^ string_of_int(!contador_while));
                                                                contador_while := !contador_while + 1;
                                                                (!code ++ !code2 ++ !code3)
                                                        end
                                                end
                        
                        | BigEqual (e1, e2) -> (* a>b *)
                                                begin
                                                        let code = ref nop in
                                                        let code2 = ref nop in
                                                        let code3 = ref nop in

                                                        begin
                                                                code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                compile_expr e1 ++
                                                                compile_expr e2 ++
                                                                pop t0 ++
                                                                pop t1 ++
                                                                blt t1 t0 ("exit" ^ string_of_int(!contador_while));

                                                                for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                label_jump ("exit" ^ string_of_int(!contador_while));
                                                                contador_while := !contador_while + 1;
                                                                (!code ++ !code2 ++ !code3)
                                                        end
                                                end

                        | Small (e1, e2) -> (* a<b *)
                                                begin
                                                        let code = ref nop in
                                                        let code2 = ref nop in
                                                        let code3 = ref nop in

                                                        begin
                                                                code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                compile_expr e1 ++
                                                                compile_expr e2 ++
                                                                pop t0 ++
                                                                pop t1 ++
                                                                bge t1 t0 ("exit" ^ string_of_int(!contador_while));

                                                                for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                label_jump ("exit" ^ string_of_int(!contador_while));
                                                                contador_while := !contador_while + 1;
                                                                (!code ++ !code2 ++ !code3)
                                                        end
                                                end

                        | SmallEqual (e1, e2) -> (* a<b *)
                                                begin
                                                        let code = ref nop in
                                                        let code2 = ref nop in
                                                        let code3 = ref nop in

                                                        begin
                                                                code := label_jump ("loop" ^ string_of_int(!contador_while)) ++
                                                                compile_expr e1 ++
                                                                compile_expr e2 ++
                                                                pop t0 ++
                                                                pop t1 ++
                                                                bgt t1 t0 ("exit" ^ string_of_int(!contador_while));

                                                                for k=0 to ((List.length i)-1) do code2 := !code2 ++ compile_instr (List.nth i k) done;

                                                                code3 := j ("loop" ^ string_of_int(!contador_while)) ++
                                                                label_jump ("exit" ^ string_of_int(!contador_while));
                                                                contador_while := !contador_while + 1;
                                                                (!code ++ !code2 ++ !code3)
                                                        end
                                                end
                    end

    | Nop -> nop (* não fazer nada *)

let compile_program p ofile = (* compilar o programa "p" gerando um fichero .s com o equivalente em assembly MIPS *)

    let code = List.map compile_instr p in
    let code = List.fold_right (++) code nop in
    
    let p =
    { data = (label "newline") ++ asciiz "\n" ++ (data_variables variables);
        text = glabel "main" ++
            code ++
            li v0 10 ++ (* sair de forma limpa *)
            syscall 
    }
    in
    let f = open_out ofile in
    let fmt = formatter_of_out_channel f in
    Mips.print_program fmt p;
    fprintf fmt "@?";
    close_out f