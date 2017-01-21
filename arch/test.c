#include <stdio.h>
#include <stdlib.h>
#define DO_SHOW 1
#include "cisc.h"
#include "debug_macros.h"

int main()
{
START_MACHINE;

JUMP(CONTINUE);

#include "char.lib"
#include "io.lib"
#include "math.lib"
#include "string.lib"
#include "system.lib"
#include "scheme.lib"


#define CONST_TABLE R11
#define GLOBAL_TABLE R10

#define SOB_NIL R15
#define SOB_VOID R14
#define SOB_TRUE R13
#define SOB_FALSE R12


INIT_CONST_TABLE:
PUSH(FP);
MOV(FP, SP);
PUSH(IMM(10));
CALL(MALLOC);
DROP(1);
MOV(CONST_TABLE, R0);
MOV(INDD(CONST_TABLE, 0), IMM(T_VOID));
MOV(INDD(CONST_TABLE, 1), IMM(T_NIL));
MOV(INDD(CONST_TABLE, 2), IMM(T_BOOL));
MOV(INDD(CONST_TABLE, 3), IMM(0));
MOV(INDD(CONST_TABLE, 4), IMM(T_BOOL));
MOV(INDD(CONST_TABLE, 5), IMM(1));
MOV(INDD(CONST_TABLE, 6), IMM(T_INTEGER));
MOV(INDD(CONST_TABLE, 7), IMM(0));
MOV(INDD(CONST_TABLE, 8), IMM(T_INTEGER));
MOV(INDD(CONST_TABLE, 9), IMM(1));
POP(FP);
RETURN;


INIT_GLOBAL_TABLE:
PUSH(FP);
MOV(FP, SP);
PUSH(IMM(29));
CALL(MALLOC);
DROP(1);
MOV(GLOBAL_TABLE, R0);
JUMP(L_make_closure_g1);
L_eq:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
CMP(FPARG(2), FPARG(3));
MOV(R0, IMM(SOB_FALSE));
JUMP_NE(L_eq_exit);
MOV(R0, IMM(SOB_TRUE));
L_eq_exit:
POP(FP);
RETURN;
L_make_closure_g1:
// Create closure for L_eq
PUSH(LABEL(L_eq));
PUSH(IMM(E_EQ));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 0), IMM(R0));
JUMP(L_make_closure_g6);
L_zero:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar x 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar integer?)
MOV(R0, INDD(GLOBAL_TABLE,9));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g3);
// tc-applic
// (const 0)
MOV(R0, CONST_TABLE);
ADD(R0, 6);
PUSH(IMM(R0));
// (pvar x 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar =)
MOV(R0, INDD(GLOBAL_TABLE,21));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g5:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g4);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g5);
L_fix_stack_loop_end_g4:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
JUMP(L_if3_exit_g2);
L_if3_else_g3:
// (const #f)
MOV(R0, CONST_TABLE);
ADD(R0, 2);

L_if3_exit_g2:
POP(FP);
RETURN;
L_make_closure_g6:
// Create closure for L_zero
PUSH(LABEL(L_zero));
PUSH(IMM(E_ZERO));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 1), IMM(R0));
JUMP(L_make_closure_g9);
L_not:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// tc-applic
// (const #f)
MOV(R0, CONST_TABLE);
ADD(R0, 2);
PUSH(IMM(R0));
// (pvar x 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar eq?)
MOV(R0, INDD(GLOBAL_TABLE,0));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g8:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g7);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g8);
L_fix_stack_loop_end_g7:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;
L_make_closure_g9:
// Create closure for L_not
PUSH(LABEL(L_not));
PUSH(IMM(E_NOT));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 2), IMM(R0));
JUMP(L_make_closure_g10);
L_car:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
MOV(R1, FPARG(2));
CMP(INDD(R1, 0), T_PAIR);
JUMP_NE(L_err_invalid_param);
MOV(R0, INDD(R1, 1));
POP(FP);
RETURN;
L_make_closure_g10:
// Create closure for L_car
PUSH(LABEL(L_car));
PUSH(IMM(E_CAR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 3), IMM(R0));
JUMP(L_make_closure_g11);
L_cdr:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
MOV(R1, FPARG(2));
CMP(INDD(R1, 0), T_PAIR);
JUMP_NE(L_err_invalid_param);
MOV(R0, INDD(R1, 2));
POP(FP);
RETURN;
L_make_closure_g11:
// Create closure for L_cdr
PUSH(LABEL(L_cdr));
PUSH(IMM(E_CDR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 4), IMM(R0));
JUMP(L_make_closure_g12);
L_cons:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(3));
PUSH(FPARG(2));
CALL(MAKE_SOB_PAIR);
DROP(2);
POP(FP);
RETURN;
L_make_closure_g12:
// Create closure for L_cons
PUSH(LABEL(L_cons));
PUSH(IMM(E_CONS));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 5), IMM(R0));
JUMP(L_make_closure_g15);
L_CADR:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// tc-applic
// applic
// (pvar l 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g14:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g13);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g14);
L_fix_stack_loop_end_g13:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;
L_make_closure_g15:
// Create closure for L_CADR
PUSH(LABEL(L_CADR));
PUSH(IMM(E_CADR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 6), IMM(R0));
JUMP(L_make_closure_g16);
L_minus:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1)); // Num of params
CMP(R1, 0);
JUMP_EQ(L_err_lambda_args_count);
MOV(R0, IMM(0));
MOV(R2, IMM(FP));
SUB(R2, IMM(5));
CMP(FPARG(1), IMM(1));
JUMP_EQ(MINUS_LOOP);
MOV(R0, STACK(R2));
MOV(R0, INDD(R0, 1));
DECR(R2);
DECR(R1);
MINUS_LOOP:
CMP(R1, IMM(0));
JUMP_LE(MINUS_EXIT);
MOV(R3, STACK(R2));
SUB(R0, INDD(R3, 1));
DECR(R2);
DECR(R1);
JUMP(MINUS_LOOP);
MINUS_EXIT:
PUSH(IMM(R0));
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g16:
// Create closure for L_minus
PUSH(LABEL(L_minus));
PUSH(IMM(E_MINUS));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 7), IMM(R0));
JUMP(L_make_closure_g17);
L_plus:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1)); // Num of params
MOV(R0, IMM(0));
MOV(R2, IMM(FP));
SUB(R2, IMM(5));
PLUS_LOOP:
CMP(R1, IMM(0));
JUMP_LE(PLUS_EXIT);
MOV(R3, STACK(R2));
ADD(R0, INDD(R3, 1));
DECR(R2);
DECR(R1);
JUMP(PLUS_LOOP);
PLUS_EXIT:
PUSH(IMM(R0));
CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g17:
// Create closure for L_plus
PUSH(LABEL(L_plus));
PUSH(IMM(E_PLUS));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 8), IMM(R0));
JUMP(L_make_closure_g18);
L_pinteger:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_INTEGER);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g18:
// Create closure for L_pinteger
PUSH(LABEL(L_pinteger));
PUSH(IMM(E_PINTEGER));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 9), IMM(R0));
JUMP(L_make_closure_g19);
L_pboolean:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_BOOL);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g19:
// Create closure for L_pboolean
PUSH(LABEL(L_pboolean));
PUSH(IMM(E_PBOOLEAN));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 10), IMM(R0));
JUMP(L_make_closure_g20);
L_pchar:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_CHAR);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g20:
// Create closure for L_pchar
PUSH(LABEL(L_pchar));
PUSH(IMM(E_PCHAR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 11), IMM(R0));
JUMP(L_make_closure_g21);
L_pclosure:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_CLOSURE);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g21:
// Create closure for L_pclosure
PUSH(LABEL(L_pclosure));
PUSH(IMM(E_PCLOSURE));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 12), IMM(R0));
JUMP(L_make_closure_g22);
L_ppair:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_PAIR);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g22:
// Create closure for L_ppair
PUSH(LABEL(L_ppair));
PUSH(IMM(E_PPAIR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 13), IMM(R0));
JUMP(L_make_closure_g23);
L_psymbol:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_SYMBOL);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g23:
// Create closure for L_psymbol
PUSH(LABEL(L_psymbol));
PUSH(IMM(E_PSYMBOL));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 14), IMM(R0));
JUMP(L_make_closure_g24);
L_pstring:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_STRING);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g24:
// Create closure for L_pstring
PUSH(LABEL(L_pstring));
PUSH(IMM(E_PSTRING));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 15), IMM(R0));
JUMP(L_make_closure_g31);
L_plist:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar lst 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar pair?)
MOV(R0, INDD(GLOBAL_TABLE,13));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g26);
// tc-applic
// applic
// (pvar lst 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar list?)
MOV(R0, INDD(GLOBAL_TABLE,16));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g30:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g29);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g30);
L_fix_stack_loop_end_g29:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
JUMP(L_if3_exit_g25);
L_if3_else_g26:
// tc-applic
// (pvar lst 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g28:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g27);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g28);
L_fix_stack_loop_end_g27:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g25:
POP(FP);
RETURN;
L_make_closure_g31:
// Create closure for L_plist
PUSH(LABEL(L_plist));
PUSH(IMM(E_PLIST));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 16), IMM(R0));
JUMP(L_make_closure_g32);
L_pvector:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
PUSH(FPARG(2));
CALL(IS_SOB_VECTOR);
DROP(1);
PUSH(IMM(R0));
CALL(L_int_to_bool);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g32:
// Create closure for L_pvector
PUSH(LABEL(L_pvector));
PUSH(IMM(E_PVECTOR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 17), IMM(R0));
JUMP(L_make_closure_g35);
L_pnull:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// tc-applic
// (const ())
MOV(R0, CONST_TABLE);
ADD(R0, 1);
PUSH(IMM(R0));
// (pvar x 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar eq?)
MOV(R0, INDD(GLOBAL_TABLE,0));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g34:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g33);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g34);
L_fix_stack_loop_end_g33:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;
L_make_closure_g35:
// Create closure for L_pnull
PUSH(LABEL(L_pnull));
PUSH(IMM(E_PNULL));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 18), IMM(R0));
JUMP(L_make_closure_g36);
L_integer_to_char:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
MOV(R1, FPARG(2));
PUSH(R1);
CALL(IS_SOB_INTEGER);
DROP(1);
CMP(R0,0);
JUMP_EQ(L_err_invalid_param);
MOV(R0, INDD(R1, 1));
PUSH(IMM(R0));CALL(MAKE_SOB_CHAR);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g36:
// Create closure for L_integer_to_char
PUSH(LABEL(L_integer_to_char));
PUSH(IMM(E_INTEGER_TO_CHAR));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 19), IMM(R0));
JUMP(L_make_closure_g37);
L_char_to_integer:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
MOV(R1, FPARG(2));
PUSH(R1);
CALL(IS_SOB_CHAR);
DROP(1);
CMP(R0,0);
JUMP_EQ(L_err_invalid_param);
MOV(R0, INDD(R1, 1));
PUSH(IMM(R0));CALL(MAKE_SOB_INTEGER);
DROP(1);
POP(FP);
RETURN;
L_make_closure_g37:
// Create closure for L_char_to_integer
PUSH(LABEL(L_char_to_integer));
PUSH(IMM(E_CHAR_TO_INTEGER));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 20), IMM(R0));
JUMP(L_make_closure_g38);
L_NUM_EQ:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1)); // Num of params
CMP(R1, 0);
JUMP_EQ(L_err_lambda_args_count);
MOV(R2, IMM(FP));
SUB(R2, IMM(5));
MOV(R3, STACK(R2));
MOV(R0, IMM(SOB_TRUE));
NUM_EQ_LOOP:
CMP(R1, IMM(0));
JUMP_LE(NUM_EQ_EXIT);
MOV(R4, STACK(R2));
CMP(INDD(R4,0), T_INTEGER);
JUMP_NE(L_err_invalid_param);
CMP(INDD(R4,1), INDD(R3, 1));
JUMP_NE(NUM_EQ_NOT_EQ);
DECR(R2);
DECR(R1);
JUMP(NUM_EQ_LOOP);
NUM_EQ_NOT_EQ:
MOV(R0, IMM(SOB_FALSE));JUMP(NUM_EQ_EXIT);
NUM_EQ_EXIT:
POP(FP);
RETURN;
L_make_closure_g38:
// Create closure for L_NUM_EQ
PUSH(LABEL(L_NUM_EQ));
PUSH(IMM(E_NUM_EQ));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 21), IMM(R0));
JUMP(L_make_closure_g39);
L_GT:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(1)); // Num of params
CMP(R1, 0);
JUMP_EQ(L_err_lambda_args_count);
MOV(R2, IMM(FP));
SUB(R2, IMM(5));
MOV(R3, STACK(R2));
DECR(R2);
MOV(R0, IMM(SOB_TRUE));
GT_LOOP:
CMP(R1, IMM(1));
JUMP_LE(GT_EXIT);
MOV(R4, STACK(R2));
CMP(INDD(R4,0), T_INTEGER);
JUMP_NE(L_err_invalid_param);
CMP(INDD(R3,1), INDD(R4, 1));
JUMP_LE(GT_NOT_GT);
MOV(R3, STACK(R2));
DECR(R2);
DECR(R1);
JUMP(GT_LOOP);
GT_NOT_GT:
MOV(R0, IMM(SOB_FALSE));
JUMP(GT_EXIT);
GT_EXIT:
POP(FP);
RETURN;
L_make_closure_g39:
// Create closure for L_GT
PUSH(LABEL(L_GT));
PUSH(IMM(E_GT));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 22), IMM(R0));
JUMP(L_make_closure_g63);
L_LT:
PUSH(FP);
MOV(FP, SP);
// Lambda-opt/var body
// Init R2 with the length of the var list
MOV(R2, FPARG(1));
SUB(R2, IMM(0));
// Save the var list length in R6 for later
MOV(R6, IMM(R2));
// Create var list
MOV(R0, SOB_NIL); // Result of var list in R0
MOV(R3, IMM(FP));
SUB(R3, IMM(4));
SUB(R3, FPARG(1)); // Save increasing stack pointer in R3
L_var_list_loop_g46:
CMP(R2, 0);
JUMP_LE(L_var_list_loop_end_g45);
PUSH(IMM(R0));
PUSH(STACK(R3));
CALL(MAKE_SOB_PAIR);
DROP(2);
INCR(R3);
DECR(R2);
JUMP(L_var_list_loop_g46);
L_var_list_loop_end_g45:
// Fix the stack
CMP(R6, 0);
JUMP_EQ(L_fix_stack_empty_var_list_g42);
MOV(R1, IMM(FP));
SUB(R1, IMM(3));
SUB(R1, FPARG(1)); // R1 = bottom
MOV(R2, IMM(FP));
SUB(R2, IMM(4)); // R2 = bottom of non-optional params
L_stack_loop_g44:
CMP(R2, IMM(FP));
JUMP_GE(L_stack_fix_end_g43);
MOV(STACK(R1), STACK(R2));
INCR(R1);
INCR(R2);
JUMP(L_stack_loop_g44);
L_fix_stack_empty_var_list_g42:
// Init R3 with the loop limit (position of first optional var in original stack)
MOV(R3, IMM(FP));
SUB(R3, IMM(4));
MOV(R1, IMM(FP));
INCR(R1);
MOV(R2, IMM(FP));
L_fix_stack_empty_loop_g41:
CMP(R2, IMM(R3));
JUMP_LE(L_stack_fix_empty_end_g40);
MOV(STACK(R1), STACK(R2));
DECR(R1);
DECR(R2);
JUMP(L_fix_stack_empty_loop_g41);
L_stack_fix_empty_end_g40:
// Fix R1 to point to the new FP since the code below relies on that
MOV(R1, IMM(FP));
INCR(R1);
L_stack_fix_end_g43:
// Fix FP and SP
MOV(FP, IMM(R1));
MOV(SP, IMM(FP));
// Fix the number of params
SUB(R1, IMM(4));
MOV(STACK(R1), IMM(1));
// Write the var list
SUB(R1, IMM(1));
MOV(STACK(R1), IMM(R0)); // Put the var list
// Actual body
// if3
// or
// applic
// (pvar v 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_NE(L_or_exit_g62);
// applic
// applic
// (pvar v 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_NE(L_or_exit_g62);

L_or_exit_g62:
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g48);
// (const #t)
MOV(R0, CONST_TABLE);
ADD(R0, 4);
JUMP(L_if3_exit_g47);
L_if3_else_g48:
// tc-applic
// applic
// (pvar v 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cadr)
MOV(R0, INDD(GLOBAL_TABLE,6));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// (pvar v 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// lambda
// Allocate env list
MOV(R1, FPARG(0));
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(R2, R0);

// Copy old env
XOR(R3, R3);
MOV(R4, 1);
L_clos_copy_env_begin_g52:
CMP(R3, IMM(1));
JUMP_GE(L_clos_copy_env_exit_g53);
MOV(R5, R2);
ADD(R5, R4);
MOV(R6, R1);
ADD(R6, R3);
MOV(IND(R5), IND(R6));
INCR(R3);
INCR(R4);
JUMP(L_clos_copy_env_begin_g52);
L_clos_copy_env_exit_g53:

// Allocate current env
MOV(R3, FPARG(1)); // Number of last lambda params
PUSH(IMM(R3));
CALL(MALLOC);
DROP(1);
MOV(IND(R2), R0);
CMP(R3, IMM(0));
JUMP_NE(L_clos_params_not_empty_g56);
MOV(IND(R2), IMM(E_EMPTY));
L_clos_params_not_empty_g56:

// Copy last lambda params
XOR(R4, R4);
MOV(R5, 1);
L_clos_copy_params_begin_g54:
CMP(R4, IMM(R3));
JUMP_GE(L_clos_copy_params_exit_g55);
MOV(R6, IND(R2));
ADD(R6, R4);
MOV(R7, IMM(FP));
SUB(R7, IMM(4));
SUB(R7, IMM(R5));
MOV(IND(R6), STACK(R7));
INCR(R4);
INCR(R5);
JUMP(L_clos_copy_params_begin_g54);
L_clos_copy_params_exit_g55:

// Allocate closure object
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), T_CLOSURE);
MOV(INDD(R0, 1), IMM(R2)); // env
MOV(INDD(R0, 2), LABEL(L_clos_body_g51));
JUMP(L_clos_exit_g57);

L_clos_body_g51:
PUSH(FP);
MOV(FP, SP);
// Lambda-simple body
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// applic
// (pvar second 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
// (pvar first 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar >)
MOV(R0, INDD(GLOBAL_TABLE,22));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar not)
MOV(R0, INDD(GLOBAL_TABLE,2));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g59);
// tc-applic
// applic
// (pvar second 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
// (pvar first 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar =)
MOV(R0, INDD(GLOBAL_TABLE,21));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar not)
MOV(R0, INDD(GLOBAL_TABLE,2));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g61:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g60);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g61);
L_fix_stack_loop_end_g60:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
JUMP(L_if3_exit_g58);
L_if3_else_g59:
// (const #f)
MOV(R0, CONST_TABLE);
ADD(R0, 2);

L_if3_exit_g58:
POP(FP);
RETURN;

L_clos_exit_g57:
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g50:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g49);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g50);
L_fix_stack_loop_end_g49:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g47:
POP(FP);
RETURN;
L_make_closure_g63:
// Create closure for L_LT
PUSH(LABEL(L_LT));
PUSH(IMM(E_LT));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 23), IMM(R0));
JUMP(L_make_closure_g72);
L_map:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g65);
// (const ())
MOV(R0, CONST_TABLE);
ADD(R0, 1);
JUMP(L_if3_exit_g64);
L_if3_else_g65:
// if3
// applic
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar list?)
MOV(R0, INDD(GLOBAL_TABLE,16));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g67);
// tc-applic
// applic
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// (pvar fun 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar map)
MOV(R0, INDD(GLOBAL_TABLE,24));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// (pvar fun 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar map)
MOV(R0, INDD(GLOBAL_TABLE,24));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar cons)
MOV(R0, INDD(GLOBAL_TABLE,5));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g71:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g70);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g71);
L_fix_stack_loop_end_g70:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
JUMP(L_if3_exit_g66);
L_if3_else_g67:
// tc-applic
// applic
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// (pvar fun 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar map)
MOV(R0, INDD(GLOBAL_TABLE,24));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// applic
// (pvar lst 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (pvar fun 0)
MOV(R0, FPARG(2));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar cons)
MOV(R0, INDD(GLOBAL_TABLE,5));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g69:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g68);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g69);
L_fix_stack_loop_end_g68:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g66:

L_if3_exit_g64:
POP(FP);
RETURN;
L_make_closure_g72:
// Create closure for L_map
PUSH(LABEL(L_map));
PUSH(IMM(E_MAP));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 24), IMM(R0));
JUMP(L_make_closure_g80);
L_list:
PUSH(FP);
MOV(FP, SP);
// Lambda-opt/var body
// Init R2 with the length of the var list
MOV(R2, FPARG(1));
SUB(R2, IMM(0));
// Save the var list length in R6 for later
MOV(R6, IMM(R2));
// Create var list
MOV(R0, SOB_NIL); // Result of var list in R0
MOV(R3, IMM(FP));
SUB(R3, IMM(4));
SUB(R3, FPARG(1)); // Save increasing stack pointer in R3
L_var_list_loop_g79:
CMP(R2, 0);
JUMP_LE(L_var_list_loop_end_g78);
PUSH(IMM(R0));
PUSH(STACK(R3));
CALL(MAKE_SOB_PAIR);
DROP(2);
INCR(R3);
DECR(R2);
JUMP(L_var_list_loop_g79);
L_var_list_loop_end_g78:
// Fix the stack
CMP(R6, 0);
JUMP_EQ(L_fix_stack_empty_var_list_g75);
MOV(R1, IMM(FP));
SUB(R1, IMM(3));
SUB(R1, FPARG(1)); // R1 = bottom
MOV(R2, IMM(FP));
SUB(R2, IMM(4)); // R2 = bottom of non-optional params
L_stack_loop_g77:
CMP(R2, IMM(FP));
JUMP_GE(L_stack_fix_end_g76);
MOV(STACK(R1), STACK(R2));
INCR(R1);
INCR(R2);
JUMP(L_stack_loop_g77);
L_fix_stack_empty_var_list_g75:
// Init R3 with the loop limit (position of first optional var in original stack)
MOV(R3, IMM(FP));
SUB(R3, IMM(4));
MOV(R1, IMM(FP));
INCR(R1);
MOV(R2, IMM(FP));
L_fix_stack_empty_loop_g74:
CMP(R2, IMM(R3));
JUMP_LE(L_stack_fix_empty_end_g73);
MOV(STACK(R1), STACK(R2));
DECR(R1);
DECR(R2);
JUMP(L_fix_stack_empty_loop_g74);
L_stack_fix_empty_end_g73:
// Fix R1 to point to the new FP since the code below relies on that
MOV(R1, IMM(FP));
INCR(R1);
L_stack_fix_end_g76:
// Fix FP and SP
MOV(FP, IMM(R1));
MOV(SP, IMM(FP));
// Fix the number of params
SUB(R1, IMM(4));
MOV(STACK(R1), IMM(1));
// Write the var list
SUB(R1, IMM(1));
MOV(STACK(R1), IMM(R0)); // Put the var list
// Actual body
// (pvar v 0)
MOV(R0, FPARG(2));
POP(FP);
RETURN;
L_make_closure_g80:
// Create closure for L_list
PUSH(LABEL(L_list));
PUSH(IMM(E_LIST));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 25), IMM(R0));
JUMP(L_make_closure_g85);
L_APPEND_BINARY:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar l 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g82);
// (pvar m 1)
MOV(R0, FPARG(3));
JUMP(L_if3_exit_g81);
L_if3_else_g82:
// tc-applic
// applic
// (pvar m 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
// applic
// (pvar l 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar append-binary)
MOV(R0, INDD(GLOBAL_TABLE,26));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// (pvar l 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar cons)
MOV(R0, INDD(GLOBAL_TABLE,5));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g84:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g83);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g84);
L_fix_stack_loop_end_g83:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g81:
POP(FP);
RETURN;
L_make_closure_g85:
// Create closure for L_APPEND_BINARY
PUSH(LABEL(L_APPEND_BINARY));
PUSH(IMM(E_PRIVATE));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 26), IMM(R0));
JUMP(L_make_closure_g108);
L_LENGTH:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// tc-applic
// (const #f)
MOV(R0, CONST_TABLE);
ADD(R0, 2);
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// lambda
// Allocate env list
MOV(R1, FPARG(0));
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(R2, R0);

// Copy old env
XOR(R3, R3);
MOV(R4, 1);
L_clos_copy_env_begin_g89:
CMP(R3, IMM(1));
JUMP_GE(L_clos_copy_env_exit_g90);
MOV(R5, R2);
ADD(R5, R4);
MOV(R6, R1);
ADD(R6, R3);
MOV(IND(R5), IND(R6));
INCR(R3);
INCR(R4);
JUMP(L_clos_copy_env_begin_g89);
L_clos_copy_env_exit_g90:

// Allocate current env
MOV(R3, FPARG(1)); // Number of last lambda params
PUSH(IMM(R3));
CALL(MALLOC);
DROP(1);
MOV(IND(R2), R0);
CMP(R3, IMM(0));
JUMP_NE(L_clos_params_not_empty_g93);
MOV(IND(R2), IMM(E_EMPTY));
L_clos_params_not_empty_g93:

// Copy last lambda params
XOR(R4, R4);
MOV(R5, 1);
L_clos_copy_params_begin_g91:
CMP(R4, IMM(R3));
JUMP_GE(L_clos_copy_params_exit_g92);
MOV(R6, IND(R2));
ADD(R6, R4);
MOV(R7, IMM(FP));
SUB(R7, IMM(4));
SUB(R7, IMM(R5));
MOV(IND(R6), STACK(R7));
INCR(R4);
INCR(R5);
JUMP(L_clos_copy_params_begin_g91);
L_clos_copy_params_exit_g92:

// Allocate closure object
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), T_CLOSURE);
MOV(INDD(R0, 1), IMM(R2)); // env
MOV(INDD(R0, 2), LABEL(L_clos_body_g88));
JUMP(L_clos_exit_g94);

L_clos_body_g88:
PUSH(FP);
MOV(FP, SP);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// seq
// (set (pvar helper 0) (box (pvar helper 0)))
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0)); // Save the var before calculating val
// (box (pvar helper 0))
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
CALL(MAKE_SOB_BOX);
POP(R1);
MOV(IND(R1), R0);
// box-set
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0)); // Save the box pointer
// lambda
// Allocate env list
MOV(R1, FPARG(0));
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(R2, R0);

// Copy old env
XOR(R3, R3);
MOV(R4, 1);
L_clos_copy_env_begin_g98:
CMP(R3, IMM(2));
JUMP_GE(L_clos_copy_env_exit_g99);
MOV(R5, R2);
ADD(R5, R4);
MOV(R6, R1);
ADD(R6, R3);
MOV(IND(R5), IND(R6));
INCR(R3);
INCR(R4);
JUMP(L_clos_copy_env_begin_g98);
L_clos_copy_env_exit_g99:

// Allocate current env
MOV(R3, FPARG(1)); // Number of last lambda params
PUSH(IMM(R3));
CALL(MALLOC);
DROP(1);
MOV(IND(R2), R0);
CMP(R3, IMM(0));
JUMP_NE(L_clos_params_not_empty_g102);
MOV(IND(R2), IMM(E_EMPTY));
L_clos_params_not_empty_g102:

// Copy last lambda params
XOR(R4, R4);
MOV(R5, 1);
L_clos_copy_params_begin_g100:
CMP(R4, IMM(R3));
JUMP_GE(L_clos_copy_params_exit_g101);
MOV(R6, IND(R2));
ADD(R6, R4);
MOV(R7, IMM(FP));
SUB(R7, IMM(4));
SUB(R7, IMM(R5));
MOV(IND(R6), STACK(R7));
INCR(R4);
INCR(R5);
JUMP(L_clos_copy_params_begin_g100);
L_clos_copy_params_exit_g101:

// Allocate closure object
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), T_CLOSURE);
MOV(INDD(R0, 1), IMM(R2)); // env
MOV(INDD(R0, 2), LABEL(L_clos_body_g97));
JUMP(L_clos_exit_g103);

L_clos_body_g97:
PUSH(FP);
MOV(FP, SP);
// Lambda-simple body
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar ls 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g105);
// (pvar counter 1)
MOV(R0, FPARG(3));
JUMP(L_if3_exit_g104);
L_if3_else_g105:
// tc-applic
// applic
// (const 1)
MOV(R0, CONST_TABLE);
ADD(R0, 8);
PUSH(IMM(R0));
// (pvar counter 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar +)
MOV(R0, INDD(GLOBAL_TABLE,8));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// (pvar ls 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (box-get (bvar helper 0 0))
// (bvar helper 0 0)
MOV(R0, FPARG(0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0,1));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g107:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g106);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g107);
L_fix_stack_loop_end_g106:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g104:
POP(FP);
RETURN;

L_clos_exit_g103:
POP(R1);
MOV(INDD(R1, 1), IMM(R0));
// tc-applic
// (const 0)
MOV(R0, CONST_TABLE);
ADD(R0, 6);
PUSH(IMM(R0));
// (bvar lst 0 0)
MOV(R0, FPARG(0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0, 0));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (box-get (pvar helper 0))
// (pvar helper 0)
MOV(R0, FPARG(2));
MOV(R0, INDD(R0,1));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g96:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g95);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g96);
L_fix_stack_loop_end_g95:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;

L_clos_exit_g94:
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g87:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g86);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g87);
L_fix_stack_loop_end_g86:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;
L_make_closure_g108:
// Create closure for L_LENGTH
PUSH(LABEL(L_LENGTH));
PUSH(IMM(E_LENGTH));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 27), IMM(R0));
JUMP(L_make_closure_g131);
L_REVERSE:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// tc-applic
// (const #f)
MOV(R0, CONST_TABLE);
ADD(R0, 2);
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// lambda
// Allocate env list
MOV(R1, FPARG(0));
PUSH(IMM(2));
CALL(MALLOC);
DROP(1);
MOV(R2, R0);

// Copy old env
XOR(R3, R3);
MOV(R4, 1);
L_clos_copy_env_begin_g112:
CMP(R3, IMM(1));
JUMP_GE(L_clos_copy_env_exit_g113);
MOV(R5, R2);
ADD(R5, R4);
MOV(R6, R1);
ADD(R6, R3);
MOV(IND(R5), IND(R6));
INCR(R3);
INCR(R4);
JUMP(L_clos_copy_env_begin_g112);
L_clos_copy_env_exit_g113:

// Allocate current env
MOV(R3, FPARG(1)); // Number of last lambda params
PUSH(IMM(R3));
CALL(MALLOC);
DROP(1);
MOV(IND(R2), R0);
CMP(R3, IMM(0));
JUMP_NE(L_clos_params_not_empty_g116);
MOV(IND(R2), IMM(E_EMPTY));
L_clos_params_not_empty_g116:

// Copy last lambda params
XOR(R4, R4);
MOV(R5, 1);
L_clos_copy_params_begin_g114:
CMP(R4, IMM(R3));
JUMP_GE(L_clos_copy_params_exit_g115);
MOV(R6, IND(R2));
ADD(R6, R4);
MOV(R7, IMM(FP));
SUB(R7, IMM(4));
SUB(R7, IMM(R5));
MOV(IND(R6), STACK(R7));
INCR(R4);
INCR(R5);
JUMP(L_clos_copy_params_begin_g114);
L_clos_copy_params_exit_g115:

// Allocate closure object
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), T_CLOSURE);
MOV(INDD(R0, 1), IMM(R2)); // env
MOV(INDD(R0, 2), LABEL(L_clos_body_g111));
JUMP(L_clos_exit_g117);

L_clos_body_g111:
PUSH(FP);
MOV(FP, SP);
// Lambda-simple body
CMP(FPARG(1), IMM(1));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// seq
// (set (pvar helper 0) (box (pvar helper 0)))
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0)); // Save the var before calculating val
// (box (pvar helper 0))
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
CALL(MAKE_SOB_BOX);
POP(R1);
MOV(IND(R1), R0);
// box-set
// (pvar helper 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0)); // Save the box pointer
// lambda
// Allocate env list
MOV(R1, FPARG(0));
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(R2, R0);

// Copy old env
XOR(R3, R3);
MOV(R4, 1);
L_clos_copy_env_begin_g121:
CMP(R3, IMM(2));
JUMP_GE(L_clos_copy_env_exit_g122);
MOV(R5, R2);
ADD(R5, R4);
MOV(R6, R1);
ADD(R6, R3);
MOV(IND(R5), IND(R6));
INCR(R3);
INCR(R4);
JUMP(L_clos_copy_env_begin_g121);
L_clos_copy_env_exit_g122:

// Allocate current env
MOV(R3, FPARG(1)); // Number of last lambda params
PUSH(IMM(R3));
CALL(MALLOC);
DROP(1);
MOV(IND(R2), R0);
CMP(R3, IMM(0));
JUMP_NE(L_clos_params_not_empty_g125);
MOV(IND(R2), IMM(E_EMPTY));
L_clos_params_not_empty_g125:

// Copy last lambda params
XOR(R4, R4);
MOV(R5, 1);
L_clos_copy_params_begin_g123:
CMP(R4, IMM(R3));
JUMP_GE(L_clos_copy_params_exit_g124);
MOV(R6, IND(R2));
ADD(R6, R4);
MOV(R7, IMM(FP));
SUB(R7, IMM(4));
SUB(R7, IMM(R5));
MOV(IND(R6), STACK(R7));
INCR(R4);
INCR(R5);
JUMP(L_clos_copy_params_begin_g123);
L_clos_copy_params_exit_g124:

// Allocate closure object
PUSH(IMM(3));
CALL(MALLOC);
DROP(1);
MOV(INDD(R0, 0), T_CLOSURE);
MOV(INDD(R0, 1), IMM(R2)); // env
MOV(INDD(R0, 2), LABEL(L_clos_body_g120));
JUMP(L_clos_exit_g126);

L_clos_body_g120:
PUSH(FP);
MOV(FP, SP);
// Lambda-simple body
CMP(FPARG(1), IMM(2));
JUMP_NE(L_err_lambda_args_count);
// Actual body
// if3
// applic
// (pvar ls 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar null?)
MOV(R0, INDD(GLOBAL_TABLE,18));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
CMP(R0, IMM(SOB_FALSE));
JUMP_EQ(L_if3_else_g128);
// (pvar acc 1)
MOV(R0, FPARG(3));
JUMP(L_if3_exit_g127);
L_if3_else_g128:
// tc-applic
// applic
// (pvar acc 1)
MOV(R0, FPARG(3));
PUSH(IMM(R0));
// applic
// (pvar ls 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar car)
MOV(R0, INDD(GLOBAL_TABLE,3));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (fvar cons)
MOV(R0, INDD(GLOBAL_TABLE,5));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
// applic
// (pvar ls 0)
MOV(R0, FPARG(2));
PUSH(IMM(R0));
PUSH(IMM(1)); // Num of params
// (fvar cdr)
MOV(R0, INDD(GLOBAL_TABLE,4));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1));
CALLA(INDD(R0, 2));
DROP(1); // env
POP(R1); // num of args
DROP(IMM(R1));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (box-get (bvar helper 0 0))
// (bvar helper 0 0)
MOV(R0, FPARG(0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0,1));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g130:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g129);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g130);
L_fix_stack_loop_end_g129:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));

L_if3_exit_g127:
POP(FP);
RETURN;

L_clos_exit_g126:
POP(R1);
MOV(INDD(R1, 1), IMM(R0));
// tc-applic
// (const ())
MOV(R0, CONST_TABLE);
ADD(R0, 1);
PUSH(IMM(R0));
// (bvar lst 0 0)
MOV(R0, FPARG(0));
MOV(R0, INDD(R0, 0));
MOV(R0, INDD(R0, 0));
PUSH(IMM(R0));
PUSH(IMM(2)); // Num of params
// (box-get (pvar helper 0))
// (pvar helper 0)
MOV(R0, FPARG(2));
MOV(R0, INDD(R0,1));
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g119:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g118);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g119);
L_fix_stack_loop_end_g118:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;

L_clos_exit_g117:
CMP(INDD(R0, 0), IMM(T_CLOSURE));
JUMP_NE(L_err_cannot_apply_non_clos);
PUSH(INDD(R0, 1)); // env
PUSH(FPARG(-1)); // ret
// Save old_fp
MOV(R1, FP);
DECR(R1);
MOV(R1, STACK(R1));
// Fix the stack
MOV(R2, IMM(FP));
SUB(R2, 4);
SUB(R2, STACK(R2)); // R2 = bottom
MOV(R3, IMM(FP));
L_fix_stack_loop_g110:
CMP(R3, IMM(SP));
JUMP_GE(L_fix_stack_loop_end_g109);
MOV(STACK(R2), STACK(R3));
INCR(R2);
INCR(R3);
JUMP(L_fix_stack_loop_g110);
L_fix_stack_loop_end_g109:
MOV(SP, IMM(R2));
MOV(FP, R1);
JUMPA(INDD(R0, 2));
POP(FP);
RETURN;
L_make_closure_g131:
// Create closure for L_REVERSE
PUSH(LABEL(L_REVERSE));
PUSH(IMM(E_PRIVATE));
CALL(MAKE_SOB_CLOSURE);
DROP(2);
MOV(INDD(GLOBAL_TABLE, 28), IMM(R0));

MOV(R0, CONST_TABLE);
ADD(R0, 1);
MOV(R15, R0);
MOV(R0, CONST_TABLE);
ADD(R0, 0);
MOV(R14, R0);
MOV(R0, CONST_TABLE);
ADD(R0, 2);
MOV(R12, R0);
MOV(R0, CONST_TABLE);
ADD(R0, 4);
MOV(R13, R0);
POP(FP);
RETURN;


// Private Primitives
JUMP(L_make_closure_g0);
L_int_to_bool:
PUSH(FP);
MOV(FP, SP);
MOV(R1, FPARG(0));
MOV(R0, IMM(SOB_TRUE));
CMP(R1, IMM(1));
JUMP_EQ(INT_TO_BOOL_EXIT);
MOV(R0, IMM(SOB_FALSE));
INT_TO_BOOL_EXIT:
POP(FP);
RETURN;
L_make_closure_g0:
// Create closure for L_int_to_bool
PUSH(LABEL(L_int_to_bool));
PUSH(IMM(E_INT_TO_BOOL));
CALL(MAKE_SOB_CLOSURE);
DROP(2);


L_err_lambda_args_count:
PUSH(IMM('M')); PUSH(IMM('A')); PUSH(IMM('Y')); PUSH(IMM('E')); PUSH(IMM('R')); PUSH(IMM('-')); PUSH(IMM('E')); PUSH(IMM('X')); PUSH(IMM('C')); PUSH(IMM('E')); PUSH(IMM('P')); PUSH(IMM('T')); PUSH(IMM('I')); PUSH(IMM('O')); PUSH(IMM('N')); PUSH(IMM(':')); PUSH(IMM(' ')); PUSH(IMM('L')); PUSH(IMM('a')); PUSH(IMM('m')); PUSH(IMM('b')); PUSH(IMM('d')); PUSH(IMM('a')); PUSH(IMM(' ')); PUSH(IMM('c')); PUSH(IMM('a')); PUSH(IMM('l')); PUSH(IMM('l')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('w')); PUSH(IMM('i')); PUSH(IMM('t')); PUSH(IMM('h')); PUSH(IMM(' ')); PUSH(IMM('w')); PUSH(IMM('r')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM('g')); PUSH(IMM(' ')); PUSH(IMM('n')); PUSH(IMM('u')); PUSH(IMM('m')); PUSH(IMM('b')); PUSH(IMM('e')); PUSH(IMM('r')); PUSH(IMM(' ')); PUSH(IMM('o')); PUSH(IMM('f')); PUSH(IMM(' ')); PUSH(IMM('a')); PUSH(IMM('r')); PUSH(IMM('g')); PUSH(IMM('s')); PUSH(IMM('!')); PUSH(IMM(57));
CALL(MAKE_SOB_STRING);
DROP(58);
PUSH(IMM(R0));
CALL(WRITELN);
DROP(1)
JUMP(EXIT);
L_err_cannot_apply_non_clos:
PUSH(IMM('M')); PUSH(IMM('A')); PUSH(IMM('Y')); PUSH(IMM('E')); PUSH(IMM('R')); PUSH(IMM('-')); PUSH(IMM('E')); PUSH(IMM('X')); PUSH(IMM('C')); PUSH(IMM('E')); PUSH(IMM('P')); PUSH(IMM('T')); PUSH(IMM('I')); PUSH(IMM('O')); PUSH(IMM('N')); PUSH(IMM(':')); PUSH(IMM(' ')); PUSH(IMM('A')); PUSH(IMM('p')); PUSH(IMM('p')); PUSH(IMM('l')); PUSH(IMM('i')); PUSH(IMM('c')); PUSH(IMM(' ')); PUSH(IMM('c')); PUSH(IMM('a')); PUSH(IMM('l')); PUSH(IMM('l')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('n')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('c')); PUSH(IMM('l')); PUSH(IMM('o')); PUSH(IMM('s')); PUSH(IMM('u')); PUSH(IMM('r')); PUSH(IMM('e')); PUSH(IMM('!')); PUSH(IMM(46));
CALL(MAKE_SOB_STRING);
DROP(47);
PUSH(IMM(R0));
CALL(WRITELN);
DROP(1)
JUMP(EXIT);
L_err_define_not_fvar:
PUSH(IMM('M')); PUSH(IMM('A')); PUSH(IMM('Y')); PUSH(IMM('E')); PUSH(IMM('R')); PUSH(IMM('-')); PUSH(IMM('E')); PUSH(IMM('X')); PUSH(IMM('C')); PUSH(IMM('E')); PUSH(IMM('P')); PUSH(IMM('T')); PUSH(IMM('I')); PUSH(IMM('O')); PUSH(IMM('N')); PUSH(IMM(':')); PUSH(IMM(' ')); PUSH(IMM('D')); PUSH(IMM('e')); PUSH(IMM('f')); PUSH(IMM('i')); PUSH(IMM('n')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('c')); PUSH(IMM('a')); PUSH(IMM('l')); PUSH(IMM('l')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('n')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('f')); PUSH(IMM('v')); PUSH(IMM('a')); PUSH(IMM('r')); PUSH(IMM('!')); PUSH(IMM(44));
CALL(MAKE_SOB_STRING);
DROP(45);
PUSH(IMM(R0));
CALL(WRITELN);
DROP(1)
JUMP(EXIT);
L_err_not_in_code_gen:
PUSH(IMM('M')); PUSH(IMM('A')); PUSH(IMM('Y')); PUSH(IMM('E')); PUSH(IMM('R')); PUSH(IMM('-')); PUSH(IMM('E')); PUSH(IMM('X')); PUSH(IMM('C')); PUSH(IMM('E')); PUSH(IMM('P')); PUSH(IMM('T')); PUSH(IMM('I')); PUSH(IMM('O')); PUSH(IMM('N')); PUSH(IMM(':')); PUSH(IMM(' ')); PUSH(IMM('C')); PUSH(IMM('o')); PUSH(IMM('d')); PUSH(IMM('e')); PUSH(IMM('-')); PUSH(IMM('g')); PUSH(IMM('e')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('c')); PUSH(IMM('a')); PUSH(IMM('l')); PUSH(IMM('l')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('u')); PUSH(IMM('n')); PUSH(IMM('k')); PUSH(IMM('n')); PUSH(IMM('o')); PUSH(IMM('w')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('e')); PUSH(IMM('x')); PUSH(IMM('p')); PUSH(IMM('r')); PUSH(IMM('e')); PUSH(IMM('s')); PUSH(IMM('s')); PUSH(IMM('i')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM('!')); PUSH(IMM(55));
CALL(MAKE_SOB_STRING);
DROP(56);
PUSH(IMM(R0));
CALL(WRITELN);
DROP(1)
JUMP(EXIT);
L_err_invalid_param:
PUSH(IMM('M')); PUSH(IMM('A')); PUSH(IMM('Y')); PUSH(IMM('E')); PUSH(IMM('R')); PUSH(IMM('-')); PUSH(IMM('E')); PUSH(IMM('X')); PUSH(IMM('C')); PUSH(IMM('E')); PUSH(IMM('P')); PUSH(IMM('T')); PUSH(IMM('I')); PUSH(IMM('O')); PUSH(IMM('N')); PUSH(IMM(':')); PUSH(IMM(' ')); PUSH(IMM('F')); PUSH(IMM('u')); PUSH(IMM('n')); PUSH(IMM('c')); PUSH(IMM('t')); PUSH(IMM('i')); PUSH(IMM('o')); PUSH(IMM('n')); PUSH(IMM(' ')); PUSH(IMM('r')); PUSH(IMM('e')); PUSH(IMM('c')); PUSH(IMM('e')); PUSH(IMM('i')); PUSH(IMM('v')); PUSH(IMM('e')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('i')); PUSH(IMM('n')); PUSH(IMM('v')); PUSH(IMM('a')); PUSH(IMM('l')); PUSH(IMM('i')); PUSH(IMM('d')); PUSH(IMM(' ')); PUSH(IMM('p')); PUSH(IMM('a')); PUSH(IMM('r')); PUSH(IMM('a')); PUSH(IMM('m')); PUSH(IMM('!')); PUSH(IMM(49));
CALL(MAKE_SOB_STRING);
DROP(50);
PUSH(IMM(R0));
CALL(WRITELN);
DROP(1)
JUMP(EXIT);


WRITE_SOB_IF_NOT_VOID:
PUSH(FP);
MOV(FP, SP);
CMP(FPARG(0), IMM(SOB_VOID));
JUMP_EQ(WRITE_SOB_IF_NOT_VOID_END);
PUSH(FPARG(0));
CALL(WRITE_SOB);
DROP(1);
WRITE_SOB_IF_NOT_VOID_END:
POP(FP);
RETURN;


CONTINUE:

CREATE_FAKE_ENV:
PUSH(IMM(0));
PUSH(IMM(SOB_VOID));
PUSH(IMM(SOB_VOID));
PUSH(IMM(SOB_VOID));

CALL(INIT_CONST_TABLE);
CALL(INIT_GLOBAL_TABLE);


// (const 1)
MOV(R0, CONST_TABLE);
ADD(R0, 8);
// Print R0
PUSH(IMM(R0));
CALL(WRITE_SOB_IF_NOT_VOID);
DROP(1);
EXIT:
DROP(4); // Fake env
STOP_MACHINE;

return 0;
}
