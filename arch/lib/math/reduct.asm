// Gets Numerator, Denumerator, reducts the fraction and puts the result in R0,R1
REDUCT:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R2);
  PUSH(R3);

  MOV(R1, FPARG(0));
  MOV(R2, FPARG(1));
  PUSH(R2);
  PUSH(R1);
  CALL(GCD);
  DROP(2);
  DIV(R2, R0);
  DIV(R1, R0);

  MOV(R3, IMM(R1));
  MUL(R3, IMM(R2));
  CMP(R3, 0);
  JUMP_LT(REDUCT_MINUS);

  REDUCT_PLUS:
  PUSH(R1);
  CALL(ABS);
  DROP(1);
  MOV(R1, R0);
  PUSH(R2);
  CALL(ABS);
  DROP(1);
  JUMP(REDUCT_EXIT);

  REDUCT_MINUS:
  PUSH(R1);
  CALL(ABS);
  DROP(1);
  MOV(R1, R0);
  PUSH(R2);
  CALL(ABS);
  DROP(1);
  MOV(R2, R0);
  MOV(R0, 0);
  SUB(R0, R2);


  REDUCT_EXIT:
  POP(R3);
  POP(R2);
  POP(FP);
  RETURN;