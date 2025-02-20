/* Best part about compilers is naming our IR
 * I'll try make a TAC IR called WACK-IR
 * Reasons for making a TAC IR instead of straight to assembly IR
 * 1) Easier to make optimisations (for a possible extension)
 * 2) Easier to convert into SSA form
 * 3) We don't need to decide on what assembly to do yet
 * 4) Easier to work with if we want to support multiple assembly architectures
 * 5) Several smaller transformations is easier to work with than one complex
 * transformation. Plus easier to split up work assuming invariants aren't violated
 * Invariants (TBD)
 * 6) Easier debuggability - its easier to work with TAC versus the abomination that
 * is the nested AST (there's lots of different nodes which aren't easy to use)
 *
 *
 * =================== Some notes on the IR ====================
 * 1) We'll have a TAC IR (Three Address Code)
 * 2) Have high level constructs for Read/Printing/Other libc calls
 * 3) I'm not sure if I want to have high level nodes for creating pairs/arrays,
 *    but most likely since it'll be easier to reason albeit more duplication
 *    but also less chance of screwing up if we ever optimise it.
 * 4) Some of the high level nodes will convert directly into some higher level assembly AST node
 * 5) This will just make our lives easier.
 * 6) There might be some redundancies in the initial IR so uhh remove any useless ones
 * */

/* ==================== WACK-IR Structure =====================
 * program = Program(top_level*, instruction* body) // The main program
 * top_level = Function(identifier, bool global = true, identifier* params, instruction* body)
 *            | StaticConstant(identifier, type t, static_init init) - most likely not needed
 *
 * instruction = Return(val)
 *             | ZeroExtend(val src, val dst) // Might need this for Pairs - not sure
 *             | Unary(unary_operator, val src, val dst)
 *             | Binary(binary_operator, val src1, val src2, val dst)
 *             | Copy(val src, val dst)
 *             | Load(val src_ptr, val dst) // Load from memory
 *             | Store(val src, val dst_ptr) // Store to memory
 *             | AddPtr(val ptr, val index, int scale, val dst) // maybe useful for arrays/pairs
 *             but its initial purpose was pointer arithmetic which I feel like we don't need to do
 *             ie we can use the others
 *             | CopyToOffset(val src, identifier dst, int offset) - used for array initializers
 *               i.e: int[] arr = [1, 2, 3] -> CopyToOffset(1, arr, 0), CopyToOffset(2, arr, 4),
 *               CopyToOffset(3, arr, 8)
 *             | CopyFromoffset(identifier src, int offset, val dst) - used for pair accesses maybe
 *               howeva: it's a bit overkill since its meant for structs but pairs are kinda like
 *               structs - i'm going with the assumption that pairs are 16 bytes
 *             | Jump(identifier target)
 *             | JumpIfZero(val condition, identifier target)
 *             | JumpIfNotZero(val condition, identifier target)
 *             | Label(identifier)
 *             | FunCall(identifier fun_name, val* args, val dst) // I think fun calls always have
 *             a destination
 *             -- For the following im not sure if these should be vals or Identifiers
 *             | Read(val dst, type t)
 *             | Free(val src)
 *             | Exit(val src)
 *             | Print(val src, type t)
 *             | Println(val src, type t) -- this may be merged with Print
 *
 * -- wacc_specific isn't being used for now but I may find it useful to refactor and use this
 * -- for now if instruction's going to be long whats a few extra
 * wacc_specific = Read(val dst, type t)
 *              | Free(val src)
 *              | Exit(val src)
 *              | Print(val src, type t)
 *              | Println(val src, type t)
 * val = Constant(const) | Var(identifier) - this will definitely need clarification
 * unary_operator = Not | Negate | Len | Ord | Chr -- not sure if Len, Ord, Chr should be here
 * binary_operator = Mul | Div | Mod | Add | Sub | Gt | Gte | Lt | Lte | Eq | Neq | And | Or
 *
 * <const> ::= <int> | <char> | <bool> | <string> | <pair> | <array> - not sure on these yet
 *
 *
 *
 *
 *
 *
 *
 * */
