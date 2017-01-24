COMPARE_SOB_STRING:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);

  MOV(R1, FPARG(0));
  MOV(R2, FPARG(1));
  CMP(INDD(R1, 1), INDD(R2, 1));
  JUMP_NE(CSS_NOT_EQ);
  MOV(R0, 1);
  MOV(R3, INDD(R1, 1));
  ADD(R1, 2);
  ADD(R2, 2);

  CSS_LOOP:
  CMP(R3, 0);
  JUMP_EQ(CSS_EXIT);
  CMP(IND(R1), IND(R2));
  JUMP_NE(CSS_NOT_EQ);
  INCR(R1);
  INCR(R2);
  DECR(R3);
  JUMP(CSS_LOOP);

  CSS_NOT_EQ:
  MOV(R0, 0);

  CSS_EXIT:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;