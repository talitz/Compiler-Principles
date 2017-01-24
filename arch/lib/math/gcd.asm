// Gets 2 numbers and calculates their GCD
// gcd(a,b)
// (if (= b 0)
//    a
//    (gcd b (modulo a b)))

GCD:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  MOV(R0, FPARG(0));
  MOV(R1, FPARG(1));

  GCD_LOOP:
   CMP(R1, 0);
   JUMP_EQ(GCD_EXIT);
   MOV(R2, IMM(R0));
   REM(R2, IMM(R1));
   MOV(R0, IMM(R1));
   MOV(R1, IMM(R2));
   JUMP(GCD_LOOP);

  GCD_EXIT:
   POP(R2);
   POP(R1);
   POP(FP);
   RETURN;