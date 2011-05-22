user:runtime_entry(start) :-
    current_prolog_flag(argv, [ToursFile]) ->
        process(ToursFile)
    ;
	    write('Usage: tours <tours_file.txt>\n').

process(ToursFile) :-
    read_tours(ToursFile, Tours),
    group(Tours, KnowledgeBase),
    repl(KnowledgeBase),
    write(bye),
    nl.

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

group([], []).
group([Id, From, To, Kind, Length | Rest], [Group | RestGroupped]) :-
    Group = [Id, From, To, Kind, Length],
    group(Rest, RestGroupped).

print_tours([]) :- !.
print_tours([Tour|RestOfTours]) :-
    write(Tour),
    nl,
    print_tours(RestOfTours).

repl(KnowledgeBase) :-
    format("Tour begin: ", []),
    read(Begin),
    repl_begin(KnowledgeBase, Begin).

repl_begin(_, halt).
repl_begin(KnowledgeBase, Begin) :-
    format("\nTour end: ", []),
    read(End),
    format("\nSearching tour ~a ~~> ~a...", [Begin, End]),
    find_path(KnowledgeBase, Begin, End, Path),
    nl,
    print_path(Path),
    print_length(Path),
    nl, nl,
    repl(KnowledgeBase).

find_path(KnowledgeBase, Start, Finish, Path) :-
    is_step(KnowledgeBase, Start, Finish, Step),
    Path = [Step].
find_path(KnowledgeBase, Start, Finish, Path) :-
    is_step(KnowledgeBase, Start, Stop, Step),
    find_path(KnowledgeBase, Stop, Finish, RestPath),
    Path = [Step | RestPath].

is_step(KnowledgeBase, Start, Stop, Step) :-
    member(Step, KnowledgeBase),
    Step = [_, Start, Stop, _, _].

print_path(Path) :-
    [[_, Start, _, _, _]|_] = Path,
    write(Start),
    print_path_rest(Path).

print_path_rest([]).
print_path_rest([[Id, _, Stop, Kind, _]|Path]) :-
    format(" -(~a,~a)-> ~a", [Id, Kind, Stop]),
    print_path_rest(Path).

print_length(Path) :-
    path_length(Path, Length),
    format("\nPath length: ~d", [Length]).

path_length(Path, Length) :- path_length_tail(Path, 0, Length).

path_length_tail([], Length, Length).
path_length_tail([[_, _, _, _, StepLength] | Path], Acc, Length) :-
    path_length_tail(Path, Acc + StepLength, Length).

