// Gets Numerator, Denumerator and a T_INTEGER. Performs a binary sum and puts the result in R0,R1
SUM:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, FPARG(0));
  MOV(R1, FPARG(1));
  MOV(R2, FPARG(2));
  MOV(R3, INDD(R2, 2));
  MOV(R2, INDD(R2, 1));
  MUL(R0, R3);
  MUL(R2, R1);
  ADD(R2, R0);
  MUL(R1, R3);
  PUSH(R2);
  PUSH(R1);
  CALL(GCD);
  DROP(2);
  DIV(R2, R0);
  DIV(R1, R0);
  MOV(R0, R2);
  POP(R3);
  POP(R2);
  POP(FP);
  RETURN;