# Pseudo-ASM bytecode

The compiler will compile a PDP file into pseudo-ASM bytecode, described by this document.  
This bytecode will be then interpreted by the VM.  

# General assembly knowledge
```x86asm
add:
    push int 1
    call +
    pop int r0
    ret r0
```
`1` is called an `immediate value`, like any other hardcoded value.  
`add:` is called a label, it's a text describing what a section of assembly does.  
Labels are used to represent functions names and `if`'s conditional branches.  

# Registers & stack

Each <ins>scope</ins> has its own 16 registers, `r0, r1, ..., r15`.  
Consider a simple program which calls a function F1, which calls another function F2.  
Before calling F1, we are in the root scope with its 16 registers.  
When going into F1, F1's `r0 ... r15` are different from root scope's `r0 ... r15`.  
If F1 sets its `r0` to 8 for instance, when going into F2, we enter a new scope and thus F2's `r0` won't be worth 8, as F2 has its own 16 registers.  
That's why we talk about <ins>scopes</ins>.  

The whole program has a stack, used when calling / returning from a function.  
When calling a function, arguments are pushed into the stack in reverse order.  
When a function needs to access its argument, it pops them from the stack in order.  
When a function returns, its return value is pushed onto the stack.  
When the caller needs to get the returned value, it pops it from the stack.  

# Branching

The VM has an internal flag BF (stands for boolean flag), used by conditional branching instructions.  
This flag is either true or false, and is unset when the VM starts until the first time it is set.  
Even if BF is used by conditional branching instructions, its raw value is not exposed to any other instructions and thus it cannot be read/set by unrelated instructions.  

# Bytecode

Each of the following instructions will be translated into bytecode, which will be interpreted by the VM.  
It means that each assembly instruction has a readable name (say "push"), but will be converted into a binary value (say 0x8).  
This binary value is called the opcode and will help the VM to identify which instruction it is reading.  
Last, this binary value will be encoded on 4 bits.  
If the VM reads an unused opcode, it will throw a runtime error.  

## Addressing mode

You'll see that some instructions can be called with different argument types (for instance, ret does).  
To let the VM know which types of arguments it must be reading, we'll add another binary value of 4 bits called the addressing mode.  
Imagine any instruction which may be called either with a register or an immediate value, an addressing mode of 0b0000 may be used to indicate a register, whereas an addressing mode of 0b0001 may be to designate an immediate value.  

Any instruction with only one combination of arguments still has an addressing mode.  
If the VM reads an unused addressing mode, it will throw a runtime error.  

Thus, if the VM read the byte 0x41, it means that it must be execute the instruction corresponding to the opcode 0x04 and the addressing mode 0x01.  

# Instructions

| Instructions family        | Instructions list      |
|----------------------------|------------------------|
| Stack manipulators         | push, pop, construct   |
| Unconditional control flow | jmp                    |
| Conditional control flow   | test, jt, jf           |
| Function control flow      | call, ret              |
| Register manipulator       | mov                    |
| VM output                  | out                    |

## Stack manipulators

As introduced above, the stack is used when dealing with functions.  
2 instructions alter the stack.  

### push

Opcode : 0x0
```x86asm
push type value     ; addressing mode : 0x0
push register       ; addressing mode : 0x1
```
This instruction pushes a value or register on top of the stack.  
When pushing a register, the type is the register's type.  
If the register is empty, the VM will throw a runtime error.  
The type must be a valid PDP type, as described in the language specification.  

Example :
```x86asm
mov r0, int 4
push r0 ; same as push int 4

push int 4
push [bool] [#t, #f]
push {uint, [float]} {4, [8, 6]}
```

### pop

Opcode : 0x1
```x86asm
pop type register   ; addressing mode = 0x0
```
This instruction pops the top value of the stack into the specified register.  
If the stack is empty, the VM will throw a runtime error.  
The type must be a valid PDP type, as described in the language specification.  

Example :
```x86asm
pop int r0
pop [bool] r1
pop {uint, [float]} r2
```

### construct

Opcode : 0x2
```x86asm
construct type n (int immediate)    ; addressing mode = 0x0
```
This instruction constructs a value of the given type from the n first top values of the stack, then pushes it on top of the same stack.  
Useful in cases like `(len [1, 2, (+ 1 4), 4, 5])` when lists or tuples are built from immediate values mixed with non-immediate values.  
If the stack has strictly less than n values, the VM will throw a runtime error.  

## Unconditional control flow

## jmp

Opcode : 0xA
```x86asm
jmp addr (int immediate)    ; addressing mode = 0x0
```
This instruction performs an unconditional jump to a relative address.  
The address is a strictly positive offset, represented by an immediate 4 bytes unsigned integer.  

## Conditional control flow

These 3 instructions allow conditional branching.  

### test

Opcode : 0x3
```x86asm
test register   ; addressing mode = 0x0
```

If the register doesn't contain a boolean value, the VM will cause a runtime error.  
This instruction sets the internal flag BF to true if the register contains true, false otherwise.  

### jt / jf

```x86asm
test r0
jt label_true   ; opcode = 0x4 // addressing mode = 0x0
jf label_false  ; opcode = 0x5 // addressing mode = 0x0

label_true:
label_false:
```

These instructions perform a conditional branching jump, depending on BF.  
If BF is unset (if no test performed before jt/jf), the VM will throw a runtime error.  
jt (<ins>j</ins>ump <ins>t</ins>rue) jumps if BF is true, otherwise does nothing.  
jf (<ins>j</ins>ump <ins>f</ins>alse) jumps if BF is false, otherwise does nothing.  

## Function control flow

These 2 instructions allow calling and returning from a procedure.  
To track the call stack, the VM has a hidden internal stack of addresses simply called CS (acronym of call stack) in this section.  

### call

Opcode : 0x6
```x86asm
call function_name  ; addressing mode = 0x0
call                ; addressing mode = 0x1
```

This instructions calls a function, it means it saves the current address in an internal and hidden stack.  
Then, if the address if specified, it jumps to the function address, otherwise it acts as the function address was at the very next opcode.  

#### Function name lookup

The function name is searched through all the functions defined in the same PDP file (and by extension its eventual imported files), and if not found searches in the builtins (standard and math library).  
If still not found, the VM will throw a runtime error.  

### ret

Opcode : 0x7
```x86asm
ret type value  ; addressing mode = 0x0
ret register    ; addressing mode = 0x1
ret             ; addressing mode = 0x2

; example
func:
    ret int 4
    ret r0
    ret
```

This instruction pushes a value / register of a certain type onto the stack (exactly the same as `push type value` or `push reg`), then pops the top address of CS and jumps to it.  
`ret` alone will cause the VM to pop the top value of the stack as a return value instead of deserializing a value or reading from a register.  
If CS is empty, the VM will throw a runtime error.  

## Register manipulator

### mov

Opcode : 0x8
```x86asm
mov register, value                            ; addressing mode = 0x0
mov register (destination), register (source)  ; addressing mode = 0x1
```

This instruction puts a value in a register, it can be either another register of the given type or an immediate value of the given type.  
If the source register is empty, the VM will throw a runtime error.  

## VM output

### out

Opcode : 0x9
```x86asm
out type immediate  ; addressing mode = 0x0
out register        ; addressing mode = 0x1
```

This instruction sends a value to the VM, which will output it.  
```scheme
4
```
Other instructions are useless to compile the above code.  
`out int 4` will send `4` as an `int` to the VM.  