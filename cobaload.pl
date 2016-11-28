addfact(X) :-
    open(X,read,Pita),
    repeat,
    read(Pita,C),
    asserta(C),
    (at_end_of_stream(Pita)),
    close(Pita).
