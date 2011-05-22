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
    end_nil(Tours, Begin).

end_nil(_, halt).
end_nil(Tours, nil) :-
    end(Tours, _).
end_nil(Tours, Begin) :-
    end(Tours, Begin).

end(Tours, Begin) :-
    format("Tour end: ", []),
    read(End),
    nl,
    conditions_nil(Tours, Begin, End).

conditions_nil(Tours, Begin, nil) :-
    conditions(Tours, Begin, _).
conditions_nil(Tours, Begin, End) :-
    conditions(Tours, Begin, End).

conditions(Tours, Begin, End) :-
    repeat,
    format("Conditions: ", []),
    read(Conditions),
    valid_conditions(Conditions),
    parse_conditions(Conditions, [], ParsedKinds, [], AllowedLengths),
    allowed_kinds(Tours, ParsedKinds, AllowedKinds),
    allowed_lengths(AllowedLengths, LengthExpectation),
    !,
    nl,
    path(Tours, Begin, End, AllowedKinds, LengthExpectation).

valid_conditions(nil).
valid_conditions(kind(Kind)) :-
    valid_kind(Kind),
    !.
valid_conditions(length(Operator, Length)) :-
    valid_operator(Operator),
    valid_length(Length),
    !.
valid_conditions((Condition, Conditions)) :-
    valid_conditions(Condition),
    valid_conditions(Conditions),
    !.
valid_conditions(SomeCondition) :-
    (atom(SomeCondition) ->
        format("\n[ERROR]: Error in conditions! \"~a\" not understood.\n", [SomeCondition])
    ;
        !
    ),
    fail,
    !.

valid_kind(Kind) :-
    atom(Kind).
valid_kind(_) :-
    format("\n[ERROR]: Kind not understood!\n", []),
    fail,
    !.

valid_operator(eq).
valid_operator(lt).
valid_operator(le).
valid_operator(gt).
valid_operator(ge).
valid_operator(_) :-
    format("\n[ERROR]: Invalid operator in length condition!\n", []),
    fail.

valid_length(Length) :-
    number(Length).
valid_length(_) :-
    format("\n[ERROR]: Invalid number in length condition!\n", []),
    fail.

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

allowed_lengths([], nil).
allowed_lengths([LengthExpectation], LengthExpectation).
allowed_lengths(_, _) :-
    format("\n[ERROR]: too many length conditions!\n", []),
    fail.

path(Tours, Begin, End, AllowedKinds, LengthExpectation) :-
    findall(Path, find_path(Tours, Begin, End, [], Path), Paths),
    filter_kinds(Paths, AllowedKinds, [], PathsWithAllowedKinds),
    filter_length(PathsWithAllowedKinds, LengthExpectation, [], PathsWithAllowedKindsAndLength),
    print_paths(PathsWithAllowedKindsAndLength),
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

filter_length(Paths, nil, [], Paths).
filter_length([], _, PathsWithAllowedLength, PathsWithAllowedLength).
filter_length([Path | Paths], LengthExpectation, Acc, PathsWithAllowedLength) :-
    fulfills_length_expectation(Path, LengthExpectation),
    filter_length(Paths, LengthExpectation, [Path | Acc], PathsWithAllowedLength).
filter_length([_ | Paths], LengthExpectation, Acc, PathsWithAllowedLength) :-
    filter_length(Paths, LengthExpectation, Acc, PathsWithAllowedLength).

fulfills_length_expectation(Path, [Operator, ExpectedLength]) :-
    path_length(Path, ActualLength),
    length_operator(ActualLength, Operator, ExpectedLength).
length_operator(ActualLength, eq, ExpectedLength) :- ActualLength =:= ExpectedLength.
length_operator(ActualLength, lt, ExpectedLength) :- ActualLength < ExpectedLength.
length_operator(ActualLength, le, ExpectedLength) :- ActualLength =< ExpectedLength.
length_operator(ActualLength, gt, ExpectedLength) :- ActualLength > ExpectedLength.
length_operator(ActualLength, ge, ExpectedLength) :- ActualLength >= ExpectedLength.

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
