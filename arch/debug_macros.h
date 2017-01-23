/* debug_macros.h
 * GDB-Like information for debugging the compiler.
 *
 * Author: Ben Eyal
 */

#define TRANS(x, res) (((x) == T_VOID) ? snprintf(res, 16, "%s", "T_VOID")     \
  : ((x) == T_NIL)     ? snprintf(res, 16, "%s", "T_NIL")                      \
  : ((x) == T_BOOL)    ? snprintf(res, 16, "%s", "T_BOOL")                     \
  : ((x) == T_CHAR)    ? snprintf(res, 16, "%s", "T_CHAR")                     \
  : ((x) == T_INTEGER) ? snprintf(res, 16, "%s", "T_INTEGER")                  \
  : ((x) == T_STRING)  ? snprintf(res, 16, "%s", "T_STRING")                   \
  : ((x) == T_SYMBOL)  ? snprintf(res, 16, "%s", "T_SYMBOL")                   \
  : ((x) == T_PAIR)    ? snprintf(res, 16, "%s", "T_PAIR")                     \
  : ((x) == T_VECTOR)  ? snprintf(res, 16, "%s", "T_VECTOR")                   \
  : ((x) == T_CLOSURE) ? snprintf(res, 16, "%s", "T_CLOSURE")                  \
  : ((x) == T_UNDEFINED) ? snprintf(res, 16, "%s", "T_UNDEFINED")              \
  : ((x) == E_EMPTY)   ? snprintf(res, 16, "%s", "E_EMPTY")                    \
  : ((x) == E_EQ)      ? snprintf(res, 16, "%s", "E_EQ")                       \
  : ((x) == E_ZERO)      ? snprintf(res, 16, "%s", "E_ZERO")                       \
  : ((x) == E_NOT)      ? snprintf(res, 16, "%s", "E_NOT")                       \
  : ((x) == E_CAR)      ? snprintf(res, 16, "%s", "E_CAR")                       \
  : ((x) == E_CDR)      ? snprintf(res, 16, "%s", "E_CDR")                       \
  : ((x) == E_CONS)      ? snprintf(res, 16, "%s", "E_CONS")                       \
  : ((x) == E_PLUS)      ? snprintf(res, 16, "%s", "E_PLUS")                       \
  : ((x) == E_MINUS)      ? snprintf(res, 16, "%s", "E_MINUS")                       \
  : ((x) == E_INTEGER)      ? snprintf(res, 16, "%s", "E_INTEGER")                       \
  : ((x) == E_INT_TO_BOOL)      ? snprintf(res, 16, "%s", "E_INT_TO_BOOL")                       \
  : ((x) == E_PINTEGER)      ? snprintf(res, 16, "%s", "E_PINTEGER")                       \
  : ((x) == E_PBOOLEAN)      ? snprintf(res, 16, "%s", "E_PBOOLEAN")                       \
  : ((x) == E_PCHAR)      ? snprintf(res, 16, "%s", "E_PCHAR")                       \
  : ((x) == E_PSYMBOL)      ? snprintf(res, 16, "%s", "E_PSYMBOL")                       \
  : ((x) == E_PVECTOR)      ? snprintf(res, 16, "%s", "E_PVECTOR")                       \
  : ((x) == E_PSTRING)      ? snprintf(res, 16, "%s", "E_PSTRING")                       \
  : ((x) == E_PNULL)      ? snprintf(res, 16, "%s", "E_PNULL")                       \
  : ((x) == E_PLIST)      ? snprintf(res, 16, "%s", "E_PLIST")                       \
  : ((x) == E_MAP)      ? snprintf(res, 16, "%s", "E_MAP")                       \
  : ((x) == E_CHAR_TO_INTEGER)      ? snprintf(res, 16, "%s", "E_CHAR_TO_INTEGER")                       \
  : ((x) == E_INTEGER_TO_CHAR)      ? snprintf(res, 16, "%s", "E_INTEGER_TO_CHAR")                       \
  : ((x) == E_LIST)      ? snprintf(res, 16, "%s", "E_LIST")                       \
  : ((x) == E_NUM_EQ)      ? snprintf(res, 16, "%s", "E_NUM_EQ")                       \
  : ((x) == E_GT)      ? snprintf(res, 16, "%s", "E_GT")              \
  : ((x) == E_LT)      ? snprintf(res, 16, "%s", "E_LT")              \
  : ((x) == E_CADR)      ? snprintf(res, 16, "%s", "E_CADR")              \
  : ((x) == E_PRIVATE)      ? snprintf(res, 16, "%s", "E_PRIVATE")              \
  : ((x) == E_APPEND)      ? snprintf(res, 16, "%s", "E_APPEND")              \
  : ((x) == E_LENGTH)      ? snprintf(res, 16, "%s", "E_LENGTH")              \
  : ((x) == E_APPLY)      ? snprintf(res, 16, "%s", "E_APPLY")              \
  : ((x) == E_BOX)      ? snprintf(res, 16, "%s", "E_BOX")              \
  : ((x) == T_BOX)      ? snprintf(res, 16, "%s", "T_BOX")              \
  : snprintf(res, 16, "%ld", (x)))

#if DO_SHOW == 0
#define INFO {}
#else
#define INFO {                                                                        \
  char type1[16];                                                                     \
  char type2[16];                                                                     \
  char type3[16];                                                                     \
  char type4[16];                                                                     \
  printf("\n");                                                                       \
  printf("----------------------------\n");                                           \
  printf("Register Info:\n");                                                         \
  printf("----------------------------\n");                                           \
  printf("FP = %-6ld SP = %ld\n\n", FP, SP);                                          \
  TRANS(R0, type1); TRANS(R1, type2);                                                 \
  printf("R0 = %-10s R1 = %s\n", type1, type2);                                       \
  TRANS(R2, type1); TRANS(R3, type2);                                                 \
  printf("R2 = %-10s R3 = %s\n", type1, type2);                                       \
  TRANS(R4, type1); TRANS(R5, type2);                                                 \
  printf("R4 = %-10s R5 = %s\n", type1, type2);                                       \
  TRANS(R6, type1); TRANS(R7, type2);                                                 \
  printf("R6 = %-10s R7 = %s\n", type1, type2);                                       \
  TRANS(R8, type1); TRANS(R9, type2);                                                 \
  printf("R8 = %-10s R9 = %s\n", type1, type2);                                       \
  TRANS(R10, type1); TRANS(R11, type2);                                               \
  printf("R10 = %-9s R11 = %s\n\n", type1, type2);                                   \
  printf("----------------------------\n");                                           \
  printf("Stack Info:\n");                                                            \
  printf("----------------------------\n");                                           \
  int i;                                                                              \
  for (i = SP; i >= 0; i--) {                                                         \
    TRANS(STACK(i), type1);                                                           \
    printf("STACK[%2d] = %s\n", i, type1);                                            \
  }                                                                                   \
  printf("\n");                                                                       \
  printf("----------------------------\n");                                           \
  printf("Memory Info:\n");                                                           \
  printf("----------------------------\n");                                           \
  for (i = 0; i <= 200; i += 4) {                                                      \
    TRANS(IND(i), type1); TRANS(IND(i + 1), type2);                                   \
    TRANS(IND(i + 2) ,type3); TRANS(IND(i + 3), type4);                               \
    printf("MEM[%d] = %-10s\t MEM[%d] = %-10s\t MEM[%d] = %-10s\t MEM[%d] = %-10s\n", \
        i, type1,                                                                     \
        i + 1, type2,                                                                 \
        i + 2, type3,                                                                 \
        i + 3, type4);                                                                \
  }                                                                                   \
  printf("\n");                                                                       \
}
#endif