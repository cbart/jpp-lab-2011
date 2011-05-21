user:runtime_entry(start) :-
    current_prolog_flag(argv, [ToursFile]) ->
        process(ToursFile)
    ;
	    write('Usage: tours <tours_file.txt>\n').

process(ToursFile) :-
    read_tours(ToursFile, Tours),
    print_tours(Tours).

read_tours(ToursFile, Tours) :-
    see(ToursFile),
    get_char(Char),
    read_tokens(Char, [], TokensList),
    split_by(TokensList, [' ', ':', '(', ',', ')', '.', '\n'], [], [], Tours),
    seen.

read_tokens(end_of_file, TokensList, TokensList).
read_tokens(Char, Acc, TokensList) :-
    get_char(AnotherChar),
    read_tokens(AnotherChar, [Char | Acc], TokensList).

split_by([], _, [], Splitted, Splitted).
split_by([], _, TokenAccList, Acc, Splitted) :-
    name(TokenAcc, TokenAccList),
    Splitted = [TokenAcc | Acc].
split_by([Token | Tokens], Splitters, TokenAcc, Acc, Splitted) :-
    member(Token, Splitters) ->
        (TokenAcc = [] ->
            split_by(Tokens, Splitters, [], Acc, Splitted)
        ;
            name(NewToken, TokenAcc),
            split_by(Tokens, Splitters, [], [NewToken | Acc], Splitted)
        )
    ;
        name(Token, [TokenNum]),
        split_by(Tokens, Splitters, [TokenNum | TokenAcc], Acc, Splitted).

print_tours([]) :- !.
print_tours([Tour|RestOfTours]) :-
    write(Tour),
    nl,
    print_tours(RestOfTours).
