Ce code en PDP :
```scheme
((lambda (x y) (+ y (factorial x))) 5 4) ; 4 + factorial(5)

(function factorial (x::integer)
    (if (eq? x 1)
        1
        (* x (factorial (- x 1)))
    )
)
```

Ressemblerait à quelque chose comme :
```x86asm
push int 5
push int 4
call __unnamed_0_int_int_ret_int
pop int r0

factorial_int_ret_int:
    pop int r0
    push int r0
    push int 1
    call eq
    pop bool r2
    test r2
    jf .not_equal
    .equal:
        mov r0, int 1
        ret int r0
    .not_equal:
        push int r0
        push int 1
        call -
        pop int r1
        push int r1
        call factorial_int_ret_int
        pop int r1
        push int r0
        push int r1
        call *
        pop int r0
        ret int r0

__unnamed_0_int_int_ret_int: ; lambda (int x, int y) = y + factorial(x)
    pop int r0
    pop int r1
    push int r0
    call factorial_int_ret_int
    pop int r0
    push int r0
    push int r1
    call *
    pop int r0
    ret int r0
```