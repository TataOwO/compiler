
(** {0 Bibliothèque pour l'écriture de programmes X86-64 }

    It acts uniquely for a relatively small fragment of X86-64 
    assembler.

    @author Jean-Christophe Filliâtre (CNRS)
    @author Kim Nguyen (Université Paris Sud)
*)

(** {1 Code } *)

type 'a asm
  (** abstract type of assembly code.
      The parameter ['a] is used as a ghost type*)

type text = [ `text ] asm
  (** of the assembler code found in the text box *)

type data = [ `data ] asm
  (** the assembler code found in the data box *)

type label = string
  (** address labels are strings of characters *)
  
val nop : [> ] asm
  (** the empty instruction can be found in text or data. *)
  
val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
  (** concatenates two pieces of code (either text with text, 
      or data with data), in constant time *)

val inline: string -> [> ] asm
  (** [inline s] copies the string [s] exactly as it is into the assembler file
      assembler *)
  
type program = {
  text : text;
  data : data;
}
  (** a program consists of a text area and a data area. *)
  
val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] prints the program code [p] into the
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

(** {1 Registers } *)

type size = [`B | `W | `L | `Q]

type 'size register
  (** abstract type of registers*)

val rax: [`Q] register
val rbx: [`Q] register
val rcx: [`Q] register
val rdx: [`Q] register
val rsi: [`Q] register
val rdi: [`Q] register
val rbp: [`Q] register
val rsp: [`Q] register
val r8 : [`Q] register
val r9 : [`Q] register
val r10: [`Q] register
val r11: [`Q] register
val r12: [`Q] register
val r13: [`Q] register
val r14: [`Q] register
val r15: [`Q] register
  (** registers 64 bits *)

val eax: [`L] register
val ebx: [`L] register
val ecx: [`L] register
val edx: [`L] register
val esi: [`L] register
val edi: [`L] register
val ebp: [`L] register
val esp: [`L] register
val r8d: [`L] register
val r9d: [`L] register
val r10d: [`L] register
val r11d: [`L] register
val r12d: [`L] register
val r13d: [`L] register
val r14d: [`L] register
val r15d: [`L] register
  (** registers 32 bits *)

val ax: [`W] register
val bx: [`W] register
val cx: [`W] register
val dx: [`W] register
val si: [`W] register
val di: [`W] register
val bp: [`W] register
val sp: [`W] register
val r8w: [`W] register
val r9w: [`W] register
val r10w: [`W] register
val r11w: [`W] register
val r12w: [`W] register
val r13w: [`W] register
val r14w: [`W] register
val r15w: [`W] register
  (** registers 16 bits *)

val al: [`B] register
val bl: [`B] register
val cl: [`B] register
val dl: [`B] register
val ah: [`B] register
val bh: [`B] register
val ch: [`B] register
val dh: [`B] register
val sil: [`B] register
val dil: [`B] register
val bpl: [`B] register
val spl: [`B] register
val r8b: [`B] register
val r9b: [`B] register
val r10b: [`B] register
val r11b: [`B] register
val r12b: [`B] register
val r13b: [`B] register
val r14b: [`B] register
val r15b: [`B] register
  (** registers 8 bits *)

val register8: [`Q] register -> [`B] register
  (** access to the 8 least significant bits of a 64-bit register *)

(** {1 Operands } *)

type 'size operand
  (** The abstract type of operands *)

val imm: int -> [>] operand
  (** immediate operand $i *)
val imm32: int32 -> [>] operand
  (** immediate operand $i *)

val imm64: int64 -> [>] operand
  (** immediate operand $i *)

val reg: 'size register -> 'size operand
val (!%): 'size register -> 'size operand
  (** register *)

val ind: ?ofs:int -> ?index:'size1 register -> ?scale:int ->
  'size2 register -> [>] operand
  (** indirect operand ofs(register, index, scale) *)

val lab: label -> [>] operand
  (** label L  *)

val ilab: label -> [`Q] operand
  (** immediate label $L *)

(** {1 Instructions } *)

(** {2 Transfert } *)

val movb: [`B] operand -> [`B] operand -> text
val movw: [`W] operand -> [`W] operand -> text
val movl: [`L] operand -> [`L] operand -> text
val movq: [`Q] operand -> [`Q] operand -> text
  (**Note: not all operand combinations are allowed*)

val movsbw: [`B] operand -> [`W] register -> text
val movsbl: [`B] operand -> [`L] register -> text
val movsbq: [`B] operand -> [`Q] register -> text
val movswl: [`W] operand -> [`L] register -> text
val movswq: [`W] operand -> [`Q] register -> text
val movslq: [`L] operand -> [`Q] register -> text
  (** with sign extension *)

val movzbw: [`B] operand -> [`W] register -> text
val movzbl: [`B] operand -> [`L] register -> text
val movzbq: [`B] operand -> [`Q] register -> text
val movzwl: [`W] operand -> [`L] register -> text
val movzwq: [`W] operand -> [`Q] register -> text
  (** with zero extension *)

val movabsq: [`Q] operand -> [`Q] register -> text
  (** copies an immediate 64-bit value into a register *)

val cmove : 'size operand -> 'size operand -> text  (* =  0 *)
val cmovz : 'size operand -> 'size operand -> text  (* =  0 *)
val cmovne: 'size operand -> 'size operand -> text  (* <> 0 *)
val cmovnz: 'size operand -> 'size operand -> text  (* <> 0 *)
val cmovs : 'size operand -> 'size operand -> text  (* <  0 *)
val cmovns: 'size operand -> 'size operand -> text  (* >= 0 *)
val cmovg : 'size operand -> 'size operand -> text  (* >  signed *)
val cmovge: 'size operand -> 'size operand -> text  (* >= signed *)
val cmovl : 'size operand -> 'size operand -> text  (* <  signed *)
val cmovle: 'size operand -> 'size operand -> text  (* <= signed *)
val cmova : 'size operand -> 'size operand -> text  (* >  non signed *)
val cmovae: 'size operand -> 'size operand -> text  (* >= non signed *)
val cmovb : 'size operand -> 'size operand -> text  (* <  non signed *)
val cmovbe: 'size operand -> 'size operand -> text  (* <= non signed *)
  (** conditional copy
      (note: not all operand combinations are allowed) *)

(** {2 Arithmetic } *)

val leab: [`B] operand -> [`B] register -> text
val leaw: [`W] operand -> [`W] register -> text
val leal: [`L] operand -> [`L] register -> text
val leaq: [`Q] operand -> [`Q] register -> text

val incb: [`B] operand -> text
val incw: [`W] operand -> text
val incl: [`L] operand -> text
val incq: [`Q] operand -> text

val decb: [`B] operand -> text
val decw: [`W] operand -> text
val decl: [`L] operand -> text
val decq: [`Q] operand -> text

val negb: [`B] operand -> text
val negw: [`W] operand -> text
val negl: [`L] operand -> text
val negq: [`Q] operand -> text

val addb: [`B] operand -> [`B] operand -> text
val addw: [`W] operand -> [`W] operand -> text
val addl: [`L] operand -> [`L] operand -> text
val addq: [`Q] operand -> [`Q] operand -> text

val subb: [`B] operand -> [`B] operand -> text
val subw: [`W] operand -> [`W] operand -> text
val subl: [`L] operand -> [`L] operand -> text
val subq: [`Q] operand -> [`Q] operand -> text

val imulw: [`W] operand -> [`W] operand -> text
val imull: [`L] operand -> [`L] operand -> text
val imulq: [`Q] operand -> [`Q] operand -> text

val idivq: [`Q] operand -> text
val cqto: text

(** {2 Logic operations } *)

val notb: [`B] operand -> text
val notw: [`W] operand -> text
val notl: [`L] operand -> text
val notq: [`Q] operand -> text

val andb: [`B] operand -> [`B] operand -> text
val andw: [`W] operand -> [`W] operand -> text
val andl: [`L] operand -> [`L] operand -> text
val andq: [`Q] operand -> [`Q] operand -> text

val orb : [`B] operand -> [`B] operand -> text
val orw : [`W] operand -> [`W] operand -> text
val orl : [`L] operand -> [`L] operand -> text
val orq : [`Q] operand -> [`Q] operand -> text

val xorb: [`B] operand -> [`B] operand -> text
val xorw: [`W] operand -> [`W] operand -> text
val xorl: [`L] operand -> [`L] operand -> text
val xorq: [`Q] operand -> [`Q] operand -> text
  (** Bit manipulation operations. "AND" bitwise, 
      "OR" bitwise, "NOT" bitwise*)

(** {2 Shift } *)

val shlb: [`B] operand -> [`B] operand -> text
val shlw: [`W] operand -> [`W] operand -> text
val shll: [`L] operand -> [`L] operand -> text
val shlq: [`Q] operand -> [`Q] operand -> text
  (** note: shl is the same as sal *)

val shrb: [`B] operand -> [`B] operand -> text
val shrw: [`W] operand -> [`W] operand -> text
val shrl: [`L] operand -> [`L] operand -> text
val shrq: [`Q] operand -> [`Q] operand -> text

val sarb: [`B] operand -> [`B] operand -> text
val sarw: [`W] operand -> [`W] operand -> text
val sarl: [`L] operand -> [`L] operand -> text
val sarq: [`Q] operand -> [`Q] operand -> text

(** {2 Jump } *)

val call: label -> text
val call_star: [`Q] operand -> text
val leave: text
val ret: text
  (** function call and return *)

val jmp : label -> text
  (** inconditional jump *)

val jmp_star: [`Q] operand -> text
  (** jump to a calculated address *)

val je : label -> text  (* =  0 *)
val jz : label -> text  (* =  0 *)
val jne: label -> text  (* <> 0 *)
val jnz: label -> text  (* <> 0 *)
val js : label -> text  (* <  0 *)
val jns: label -> text  (* >= 0 *)
val jg : label -> text  (* >  signed *)
val jge: label -> text  (* >= signed *)
val jl : label -> text  (* <  signed *)
val jle: label -> text  (* <= signed *)
val ja : label -> text  (* >  non signed *)
val jae: label -> text  (* >= non signed *)
val jb : label -> text  (* <  non signed *)
val jbe: label -> text  (* <= non signed *)
  (** without conditions *)

(** {2 Conditions } *)

val cmpb: [`B] operand -> [`B] operand -> text
val cmpw: [`W] operand -> [`W] operand -> text
val cmpl: [`L] operand -> [`L] operand -> text
val cmpq: [`Q] operand -> [`Q] operand -> text

val testb: [`B] operand -> [`B] operand -> text
val testw: [`W] operand -> [`W] operand -> text
val testl: [`L] operand -> [`L] operand -> text
val testq: [`Q] operand -> [`Q] operand -> text

val sete : [`B] operand -> text  (* =  0 *)
val setne: [`B] operand -> text  (* <> 0 *)
val setz : [`B] operand -> text  (* =  0 *)
val setnz: [`B] operand -> text  (* <> 0 *)
val sets : [`B] operand -> text  (* <  0 *)
val setns: [`B] operand -> text  (* >= 0 *)
val setg : [`B] operand -> text  (* >  signed *)
val setge: [`B] operand -> text  (* >= signed *)
val setl : [`B] operand -> text  (* <  signed *)
val setle: [`B] operand -> text  (* <= signed *)
val seta : [`B] operand -> text  (* >  non signed *)
val setae: [`B] operand -> text  (* >= non signed *)
val setb : [`B] operand -> text  (* <  non signed *)
val setbe: [`B] operand -> text  (* <= non signed *)
  (** sets the operand byte to 1 or 0 depending on whether the test is true or not *)

(** {2 Manipulation de la pile} *)

val pushq : [`Q] operand -> text
  (** [pushq r] places the contents of [r] at the top of the stack.
      Reminder: %rsp points to the address of the last occupied slot *)

val popq : [`Q] register -> text
  (** [popq r] places the word at the top of the stack in [r] and pops it *)

(** {2 Divers } *)

val label : label -> [> ] asm
  (** a label. Can be found in text or data *)

val globl : label -> [> ] asm
  (** .globl declaration (for main, typically) *)

val comment : string -> [> ] asm
  (** Places a comment in the generated code. May be found in text or data *)

val aligned_call_wrapper: f:string -> newf:string -> text
  (** constructs an assembler function `newf` that calls `f` after correctly 
      aligning the stack (on a multiple of 16).
      Thus, `aligned_call "malloc" "malloc_"` adds a `malloc_` function to the 
      assembler code, which can be called anywhere in our code without worrying about alignment. *)

(** {2 Data } *)

val string : string -> data
  (** a string constant (terminated by 0) *)

val dbyte : int list -> data
val dword : int list -> data
val dint : int list -> data
val dquad : int list -> data
  (** places a list of values ​​of 1/2/4/8 bytes in the data area *)

val address: label list -> data
  (** places a list of addresses in the data zone (with .quad) *)

val space: int -> data
  (** [space n] allocates [n] bytes (value 0) in the data segment *)

