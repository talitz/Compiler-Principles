/* scheme/make_sob_integer.asm
 * Takes a numerator and a denumerator and puts a T_INTEGER Scheme object in R0
 * 
 * Programmer: Mayer Goldberg, 2010
 */

 MAKE_SOB_INTEGER:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_INTEGER);
  MOV(INDD(R0, 1), FPARG(1));
  MOV(INDD(R0, 2), FPARG(0));
  POP(FP);
  RETURN;
