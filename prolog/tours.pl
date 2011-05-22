user:runtime_entry(start) :-
    current_prolog_flag(argv, [ToursFile]),
    process(ToursFile).
user:runtime_entry(start) :-
    write('Usage: tours <tours_file.txt>\n').

process(ToursFile) :-
    read_tours(ToursFile, Tours),
    prompt(_, ' '),
    begin(Tours),
    write(bye),
    nl.

read_tours(ToursFile, Tours) :-
    see(ToursFile),
    read(Tour),
    read_tokens(Tour, [], Tours),
    seen.

read_tokens(end_of_file, Tours, Tours).
read_tokens(Token, Acc, Tours) :-
    read(AnotherToken),
    read_tokens(AnotherToken, [Token | Acc], Tours).

begin(Tours) :-
    format("Tour begin: ", []),
    read(Begin),
    nl,
    end(Tours, Begin).

end(_, halt).
end(Tours, Begin) :-
    format("Tour end: ", []),
    read(End),
    nl,
    conditions(Tours, Begin, End).

conditions(Tours, Begin, End) :-
    repeat,
    format("Conditions: ", []),
    read(Conditions),
    parse_conditions(Conditions, [], ParsedKinds, [], AllowedLengths),
    allowed_kinds(Tours, ParsedKinds, AllowedKinds),
    !,
    nl,
    path(Tours, Begin, End, AllowedKinds, AllowedLengths).

parse_conditions(nil, AllowedKinds, AllowedKinds, AllowedLengths, AllowedLengths).
parse_conditions(kind(K), AccKinds, [K | AccKinds], AllowedLengths, AllowedLengths).
parse_conditions(length(Operator, Length), AllowedKinds, AllowedKinds, AccLengths, [[Operator, Length] | AccLengths]).
parse_conditions((kind(K), Conditions), AccKinds, AllowedKinds, AccLengths, AllowedLengths) :-
    parse_conditions(Conditions, [K | AccKinds], AllowedKinds, AccLengths, AllowedLengths).
parse_conditions((length(Operator, Length), Conditions), AccKinds, AllowedKinds, AccLengths, AllowedLengths) :-
    parse_conditions(Conditions, AccKinds, AllowedKinds, [[Operator, Length] | AccLengths], AllowedLengths).

allowed_kinds(Tours, [], AllowedKinds) :-
    setof(Kind, has_kind(Tours, Kind), AllowedKinds).
allowed_kinds(_, AllowedKinds, AllowedKinds).

has_kind(Tours, Kind) :-
    member(_ : (_, _, Kind, _), Tours).

path(Tours, Begin, End, AllowedKinds, _) :-
    findall(Path, find_path(Tours, Begin, End, [], Path), Paths),
    filter_kinds(Paths, AllowedKinds, [], PathsWithAllowedKinds),
    print_paths(PathsWithAllowedKinds),
    begin(Tours).

filter_kinds([], _, PathsWithAllowedKinds, PathsWithAllowedKinds).
filter_kinds([Path | Paths], AllowedKinds, Acc, PathsWithAllowedKinds) :-
    is_of_kinds(Path, AllowedKinds),
    filter_kinds(Paths, AllowedKinds, [Path | Acc], PathsWithAllowedKinds).
filter_kinds([_ | Paths], AllowedKinds, Acc, PathsWithAllowedKinds) :-
    filter_kinds(Paths, AllowedKinds, Acc, PathsWithAllowedKinds).

is_of_kinds([], _).
is_of_kinds([_ : (_, _, Kind, _) | Paths], AllowedKinds) :-
    member(Kind, AllowedKinds),
    is_of_kinds(Paths, AllowedKinds).

find_path(Tours, Start, Finish, AlreadyVisited, Path) :-
    is_step(Tours, AlreadyVisited, Start, Finish, Step),
    Path = [Step].
find_path(Tours, Start, Finish, AlreadyVisited, Path) :-
    is_step(Tours, AlreadyVisited, Start, Stop, Step),
    find_path(Tours, Stop, Finish, [Step | AlreadyVisited], Forward),
    Path = [Step | Forward].

is_step(Tours, AlreadyVisited, Start, Stop, Step) :-
    member(Step, Tours),
    \+(member(Step, AlreadyVisited)),
    Step = _ : (Start, Stop, _, _).

print_paths([]).
print_paths([Path | Paths]) :-
    print_path(Path),
    print_length(Path),
    nl,
    nl,
    print_paths(Paths).

print_path(Path) :-
    [_ : (Start, _, _, _) | _] = Path,
    write(Start),
    print_path_rest(Path).

print_path_rest([]).
print_path_rest([Id : (_, Stop, Kind, _) | Path]) :-
    format(" -(~a,~a)-> ~a", [Id, Kind, Stop]),
    print_path_rest(Path).

print_length(Path) :-
    path_length(Path, Length),
    format("\nPath length: ~d", [Length]).

path_length(Path, Length) :- path_length_tail(Path, 0, Length).

path_length_tail([], Length, Length).
path_length_tail([_ : (_, _, _, StepLength) | Path], Acc, Length) :-
    path_length_tail(Path, Acc + StepLength, Length).
