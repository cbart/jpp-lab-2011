user:main(start) :-
    current_prolog_flag(argv, [ToursFile]) ->
        process(ToursFile);
	    write('Usage: tours <tours_file.txt>\n').

process(ToursFile) :-
    see(ToursFile),
    repeat,
    get_char(C),
    print(C),
    C = end_of_file,
    !,
    seen.
