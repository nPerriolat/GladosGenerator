(function collatz (n::integer)
    (if (eq n 1)
        1
        (if (eq (mod n 2) 0)
            (div n 2)
            (* n 3)
        )
    )
    integer
)