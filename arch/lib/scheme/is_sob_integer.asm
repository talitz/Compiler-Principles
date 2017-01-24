/* scheme/is_sob_integer.asm
 * Take pointers to a Scheme object, and places in R0 either 0 or 1
 * (long, not Scheme integer objects or Scheme boolean objets),
 * depending on whether the argument is integer.
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 IS_SOB_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  CMP(IND(R0), T_INTEGER);
  JUMP_NE(L_IS_SOB_INTEGER_FALSE);
  CMP(INDD(R0, 2), 1);
  JUMP_NE(L_IS_SOB_INTEGER_FALSE);
  MOV(R0, IMM(1));
  JUMP(L_IS_SOB_INTEGER_EXIT);
 L_IS_SOB_INTEGER_FALSE:
  MOV(R0, IMM(0));
 L_IS_SOB_INTEGER_EXIT:
  POP(FP);
  RETURN;


