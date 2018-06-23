open Core_kernel.Std
open Bap.Std

open X86_types
open X86_asm_reg_types

type multimodereg = { v32: var; v64: var }

let gv mode { v32; v64 } = match mode with
  | X86 -> v32
  | X8664 -> v64

(* new multi-mode variable *)
let nmv n32 t32 n64 t64 = { v32=Var.create n32 t32; v64=Var.create n64 t64; }

(* registers *)
let rbp = nmv "EBP" reg32_t "RBP" reg64_t
let rsp = nmv "ESP" reg32_t "RSP" reg64_t
let rsi = nmv "ESI" reg32_t "RSI" reg64_t
let rdi = nmv "EDI" reg32_t "RDI" reg64_t
let rip = nmv "EIP" reg32_t "RIP" reg64_t (* XXX why is eip here? *)
let rax = nmv "EAX" reg32_t "RAX" reg64_t
let rbx = nmv "EBX" reg32_t "RBX" reg64_t
let rcx = nmv "ECX" reg32_t "RCX" reg64_t
let rdx = nmv "EDX" reg32_t "RDX" reg64_t
let rflags = nmv "EFLAGS" reg32_t "RFLAGS" reg64_t (* XXX why is eflags here? *)

(* segment registers let bases *)
let fs_base = nmv "FS_BASE" reg32_t "FS_BASE" reg64_t
let gs_base = nmv "GS_BASE" reg32_t "GS_BASE" reg64_t

let gdt = nmv "GDTR" reg32_t "GDTR" reg64_t
let ldt = nmv "LDTR" reg32_t "LDTR" reg64_t

let mem = nmv "mem" (Type.mem `r32 `r8) "mem" (Type.mem `r64 `r8)

(* condition flag bits *)
let cf = Var.create "CF" bool_t
let pf = Var.create "PF" bool_t
let af = Var.create "AF" bool_t
let zf = Var.create "ZF" bool_t
let sf = Var.create "SF" bool_t
let oF = Var.create "OF" bool_t
let df = Var.create "DF" bool_t

let cs = Var.create "CS" reg16_t
let ds = Var.create "DS" reg16_t
let es = Var.create "ES" reg16_t
let fs = Var.create "FS" reg16_t
let gs = Var.create "GS" reg16_t
let ss = Var.create "SS" reg16_t

let fpu_ctrl = Var.create "FPU_CONTROL" reg16_t
let mxcsr = Var.create "MXCSR" reg32_t

let ymms = Array.init 16 ~f:(fun i -> Var.create (Printf.sprintf "YMM%d" i) reg256_t)

(* floating point registers *)

let o_rax = Oreg 0
let o_rcx = Oreg 1
let o_rdx = Oreg 2
let o_rbx = Oreg 3
let o_rsp = Oreg 4
let o_rbp = Oreg 5
let o_rsi = Oreg 6
let o_rdi = Oreg 7

let o_es = Oseg 0
let o_cs = Oseg 1
let o_ss = Oseg 2
let o_ds = Oseg 3
let o_fs = Oseg 4
let o_gs = Oseg 5

(* prefix names *)
let pref_lock = 0xf0
let repnz = 0xf2
let repz = 0xf3
let hint_bnt = 0x2e
let hint_bt = 0x3e
let pref_cs = 0x2e
let pref_ss = 0x36
let pref_ds = 0x3e
let pref_es = 0x26
let pref_fs = 0x64
let pref_gs = 0x65
let pref_opsize = 0x66
let pref_addrsize = 0x67

(* Prefixes that we can usually handle automatically *)
let standard_prefs = [pref_opsize; pref_addrsize; hint_bnt; hint_bt; pref_cs; pref_ss; pref_ds; pref_es; pref_fs; pref_gs]

module type ModeVars = sig
  (** registers *)
  val rbp : var
  val rsp : var
  val rsi : var
  val rdi : var
  val rip : var
  val rax : var
  val rbx : var
  val rcx : var
  val rdx : var
  val rflags  : var

  val gdt : var
  val ldt : var

  (** segment registers let bases *)
  val fs_base : var
  val gs_base : var

  val seg_ss : var option
  val seg_es : var option
  val seg_cs : var option
  val seg_ds : var option
  val seg_fs : var option
  val seg_gs : var option

  val mem : var

  (* r8 -> r15 *)
  val r : var array
  val nums : var array
  val ymms : var array
  val of_reg : reg -> var option
end

module R32 = struct
  (** registers *)
  let rbp = rbp.v32
  let rsp = rsp.v32
  let rsi = rsi.v32
  let rdi = rdi.v32
  let rip = rip.v32
  let rax = rax.v32
  let rbx = rbx.v32
  let rcx = rcx.v32
  let rdx = rdx.v32
  let rflags = rflags.v32
  let gdt = gdt.v32
  let ldt = ldt.v32
  (** segment registers let bases *)
  let fs_base = fs_base.v32
  let gs_base = gs_base.v32

  let seg_ss = None
  let seg_es = None
  let seg_cs = None
  let seg_ds = None
  let seg_fs = Some fs_base
  let seg_gs = Some gs_base

  let mem = mem.v32

  (* No r registers in x86 32-bit mode *)
  let r = [||]

  let nums = r

  (* Only 8 YMM/XMM registers in x86 32-bit mode *)
  let ymms = Array.sub ymms 0 8

  (* mapping registers to arch variables *)
  let of_reg reg =
    let internal_reg = X86_asm_reg.decode reg in
    match internal_reg with
    | None -> None
    | Some reg ->
      (match reg with
       | `AL | `AH | `AX | `EAX -> Some rax
       | `BL | `BH | `BX | `EBX -> Some rbx
       | `CL | `CH | `CX | `ECX -> Some rcx
       | `DL | `DH | `DX | `EDX -> Some rdx
       | `SIL | `SI | `ESI -> Some rsi
       | `DIL | `DI | `EDI -> Some rdi
       | `BPL | `BP | `EBP -> Some rbp
       | `SPL | `SP | `ESP -> Some rsp
       | `EIP | `IP -> Some rip
 
       | `GS -> seg_gs
       | `FS_BASE -> Some fs_base
       | `CS -> seg_cs
       | `GS_BASE -> Some gs_base
       | `DS -> seg_ds
       | `ES -> seg_es
       | `SS -> seg_ss
       | `FS -> seg_fs

       | `XMM0 | `YMM0 -> Some ymms.(0)
       | `XMM1 | `YMM1 -> Some ymms.(1)
       | `XMM2 | `YMM2 -> Some ymms.(2)
       | `XMM3 | `YMM3 -> Some ymms.(3)
       | `XMM4 | `YMM4 -> Some ymms.(4)
       | `XMM5 | `YMM5 -> Some ymms.(5)
       | `XMM6 | `YMM6 -> Some ymms.(6)
       | `XMM7 | `YMM7 -> Some ymms.(7)
 
       (** 64-bit registers are invalid *)
       | `R8B | `R9B | `R10B | `R11B
       | `R12B | `R13B | `R14B | `R15B
       | `R8W | `R9W | `R10W | `R11W
       | `R12W | `R13W | `R14W | `R15W
       | `R8D | `R9D | `R10D | `R11D
       | `R12D | `R13D | `R14D | `R15D
       | `RAX | `RBX | `RCX | `RDX
       | `RDI | `RSI | `RBP | `RSP
       | `R8 | `R9 | `R10 | `R11
       | `R12 | `R13 | `R14 | `R15
       | `RIP
       | `XMM8 | `YMM8 | `XMM9 | `YMM9
       | `XMM10 | `YMM10 | `XMM11 | `YMM11
       | `XMM12 | `YMM12 | `XMM13 | `YMM13
       | `XMM14 | `YMM14 | `XMM15 | `YMM15
       ->
         assert false (* incorrect architecture *)
      )

end

module R64 = struct
  (** registers *)
  let rbp = rbp.v64
  let rsp = rsp.v64
  let rsi = rsi.v64
  let rdi = rdi.v64
  let rip = rip.v64
  let rax = rax.v64
  let rbx = rbx.v64
  let rcx = rcx.v64
  let rdx = rdx.v64
  let rflags = rflags.v64
  let gdt = gdt.v64
  let ldt = ldt.v64
  (** segment registers let bases *)
  let fs_base = fs_base.v64
  let gs_base = gs_base.v64

  let seg_ss = None
  let seg_es = None
  let seg_cs = None
  let seg_ds = None
  let seg_fs = Some fs_base
  let seg_gs = Some gs_base

  let mem = mem.v64

  (* r8 -> r15 *)
  let r = Array.init 8 ~f:(fun i -> Var.create (Printf.sprintf "R%d" (i+8)) reg64_t)

  let nums = r

  (* All YMM/XMM registers are available *)
  let ymms = ymms

  (* mapping registers to arch variables *)
  let of_reg reg =
    let internal_reg = X86_asm_reg.decode reg in
    match internal_reg with
    | None -> None
    | Some reg ->
      (match reg with
       | `AL | `AH | `AX | `EAX | `RAX -> Some rax
       | `BL | `BH | `BX | `EBX | `RBX -> Some rbx
       | `CL | `CH | `CX | `ECX | `RCX -> Some rcx
       | `DL | `DH | `DX | `EDX | `RDX -> Some rdx
       | `SIL | `SI | `ESI | `RSI -> Some rsi
       | `DIL | `DI | `EDI | `RDI -> Some rdi
       | `BPL | `BP | `EBP | `RBP -> Some rbp
       | `SPL | `SP | `ESP | `RSP -> Some rsp
       | `R8B | `R8W | `R8D | `R8 -> Some r.(0)
       | `R9B | `R9W | `R9D | `R9 -> Some r.(1)
       | `R10B | `R10W | `R10D | `R10 -> Some r.(2)
       | `R11B | `R11W | `R11D | `R11 -> Some r.(3)
       | `R12B | `R12W | `R12D | `R12 -> Some r.(4)
       | `R13B | `R13W | `R13D | `R13 -> Some r.(5)
       | `R14B | `R14W | `R14D | `R14 -> Some r.(6)
       | `R15B | `R15W | `R15D | `R15 -> Some r.(7)
       | `EIP | `IP | `RIP -> Some rip

       | `GS -> seg_gs
       | `FS_BASE -> Some fs_base
       | `CS -> seg_cs
       | `GS_BASE -> Some gs_base
       | `DS -> seg_ds
       | `ES -> seg_es
       | `SS -> seg_ss
       | `FS -> seg_fs

       | `XMM0 | `YMM0 -> Some ymms.(0)
       | `XMM1 | `YMM1 -> Some ymms.(1)
       | `XMM2 | `YMM2 -> Some ymms.(2)
       | `XMM3 | `YMM3 -> Some ymms.(3)
       | `XMM4 | `YMM4 -> Some ymms.(4)
       | `XMM5 | `YMM5 -> Some ymms.(5)
       | `XMM6 | `YMM6 -> Some ymms.(6)
       | `XMM7 | `YMM7 -> Some ymms.(7)
       | `XMM8 | `YMM8 -> Some ymms.(8)
       | `XMM9 | `YMM9 -> Some ymms.(9)
       | `XMM10 | `YMM10 -> Some ymms.(10)
       | `XMM11 | `YMM11 -> Some ymms.(11)
       | `XMM12 | `YMM12 -> Some ymms.(12)
       | `XMM13 | `YMM13 -> Some ymms.(13)
       | `XMM14 | `YMM14 -> Some ymms.(14)
       | `XMM15 | `YMM15 -> Some ymms.(15)
      )
 
 
end

let vars_of_mode mode =
  let module R =
    (val (match mode with
         | X86   -> (module R32)
         | X8664 -> (module R64)) : ModeVars) in
  (module R : ModeVars)
