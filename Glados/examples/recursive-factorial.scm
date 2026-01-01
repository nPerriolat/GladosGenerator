(function factorial (x::integer)
    (if (eq x 1)
        1
        (* x (factorial (- x 1)))
    )
    integer
)
(factorial 10)