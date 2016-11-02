main :-
  Sr = 'Sascha',
  Mo = 'Michael',
  Th = 'Teresa',
  Ma = 'Maria',
  As = 'Asma',
  verteilung([Sr, Mo, Th, Ma, As],
    [ (('Montag', 10),     [Sr, Mo, Ma]),
      (('Montag', 12),     [Sr, Ma]),
      (('Dienstag', 16),   [Sr, Mo, Ma, Th]),
      (('Mittwoch', 10),   [Sr, Th, As]),
      (('Mittwoch', 14),   [Sr, Mo, Ma, As]),
      (('Donnerstag', 12), [Sr, Mo, Th, Ma]),
      (('Freitag', 10),    [Mo, Th, Ma]),
      (('Freitag', 12),    [Mo, Ma, As])
    ],
    Ds),
    %Einschränkungen:
    % Zum beispiel:
    % Ds = [(('Montag',_),_)|_],
  write_dates(Ds).

write_date(((Day,Time),[T1,T2,T3])) :-
  Time1 is Time+2,
  format('~w ~d-~d:~20|~w, ~w, ~w', [Day, Time, Time1, T1, T2, T3]).

write_dates([]).
write_dates([D|Ds]) :-
  write_date(D),
  nl,
  write_dates(Ds).

all_different([]).
all_different([X|Xs]) :-
  \+member(X,Xs),
  all_different(Xs).

remove_one(Xs, Xs1) :-
  member(X,Xs),
  delete(Xs, X, Xs1).

remove_some(Xs, N, Xs) :-
  length(Xs, N1),
  N1 =< N.

remove_some(Xs, N, Xs1) :-
  length(Xs, N1),
  N1 > N,
  remove_one(Xs, Xs2),
  length(Xs2, N2),
  ( N2 > N,
    remove_some(Xs2, N, Xs1)
  ; N2 =< N,
    Xs1 = Xs2).


member_of(_,[],0).
member_of(X,[Xs|Xss],N) :-
  member(X,Xs),
  member_of(X,Xss,N1),
  N is N1+1.

member_of(X,[Xs|Xss], N) :-
  \+member(X,Xs),
  member_of(X,Xss,N).

day_order('Montag', 'Dienstag').
day_order('Dienstag', 'Mittwoch').
day_order('Mittwoch', 'Donnerstag').
day_order('Donnerstag', 'Freitag').
day_order_t(D1,D2) :- day_order(D1,D2).
day_order_t(D1,D2) :-
  day_order(D1,D), day_order_t(D,D2).

dates_sorted([]).
dates_sorted([_]).
dates_sorted([Date1, Date2 | Ds]) :-
  Date1 = ((D1,T1), _),
  Date2 = ((D2,T2), _),
  ( D1 = D2, T1 < T2
  ; day_order_t(D1,D2)
  ),
  dates_sorted([Date2|Ds]).

verteilung([T1,T2,T3,T4,T5], Termine,Verteilung) :-
  member((Termin1, Tutoren1), Termine),
  member((Termin2, Tutoren2), Termine),
  member((Termin3, Tutoren3), Termine),
  member((Termin4, Tutoren4), Termine),
  member((Termin5, Tutoren5), Termine),
  all_different([Termin1, Termin2, Termin3, Termin4, Termin5]),
  length(Tutoren1, N1), N1 >= 2,
  length(Tutoren2, N2), N2 >= 2,
  length(Tutoren3, N3), N3 >= 3,
  length(Tutoren4, N4), N4 >= 3,
  length(Tutoren5, N5), N5 >= 3,
  sort(Tutoren1,Tutoren1s),
  sort(Tutoren2,Tutoren2s),
  sort(Tutoren3,Tutoren3s),
  sort(Tutoren4,Tutoren4s),
  sort(Tutoren5,Tutoren5s),
  remove_some(Tutoren1s,3,Tutoren1_1),
  remove_some(Tutoren2s,3,Tutoren2_1),
  remove_some(Tutoren3s,3,Tutoren3_1),
  remove_some(Tutoren4s,3,Tutoren4_1),
  remove_some(Tutoren5s,3,Tutoren5_1),
  Aktiv = [Tutoren1_1, Tutoren2_1, Tutoren3_1, Tutoren4_1, Tutoren5_1],
  member_of(T1, Aktiv, 3),
  member_of(T2, Aktiv, 3),
  member_of(T3, Aktiv, 3),
  member_of(T4, Aktiv, 3),
  member_of(T5, Aktiv, 3),
  Verteilung = [(Termin1, Tutoren1_1),
                (Termin2, Tutoren2_1),
                (Termin3, Tutoren3_1),
                (Termin4, Tutoren4_1),
                (Termin5, Tutoren5_1)],
  dates_sorted(Verteilung).
