:- dynamic has_motive/2, was_near_crime_scene/2, has_fingerprint_on_weapon/2.
:- dynamic eyewitness_identification/2, has_bank_transaction/2, owns_fake_identity/2.
:- dynamic has_alibi/2, suspect/1.
:- discontiguous has_motive/2, was_near_crime_scene/2, has_fingerprint_on_weapon/2.
:- discontiguous eyewitness_identification/2, has_bank_transaction/2, owns_fake_identity/2.
:- discontiguous has_alibi/2, suspect/1.

crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
has_motive(mary, assassinat).
has_motive(alice, escroquerie).

was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).

has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(mary, assassinat).

eyewitness_identification(mary, assassinat).

has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).

owns_fake_identity(sophie, escroquerie).

has_alibi(bruno, vol).

% Règles de culpabilité
is_guilty(S, C) :- has_alibi(S, C), !, fail.
is_guilty(S, vol) :- has_motive(S, vol), was_near_crime_scene(S, vol), has_fingerprint_on_weapon(S, vol).
is_guilty(S, assassinat) :- has_motive(S, assassinat), was_near_crime_scene(S, assassinat),
    (has_fingerprint_on_weapon(S, assassinat); eyewitness_identification(S, assassinat)).
is_guilty(S, escroquerie) :- has_motive(S, escroquerie), (has_bank_transaction(S, escroquerie); owns_fake_identity(S, escroquerie)).

% CLI
main :-
    repeat,
    writeln('--- Menu Enquête ---'),
    writeln('1: Tester un suspect'),
    writeln('2: Voir tous les coupables d’un crime'),
    writeln('3: Ajouter un fait'),
    writeln('4: Quitter'),
    read_line_to_string(user_input, CStr),
    number_string(C, CStr),
    ( C =:= 1 -> writeln('Suspect:'), read_line_to_string(user_input, SStr), string_to_atom(SStr, S),
        writeln('Crime (vol / assassinat / escroquerie):'), read_line_to_string(user_input, CrStr), string_to_atom(CrStr, Cr),
        (is_guilty(S, Cr) -> format('~w est coupable de ~w.~n', [S, Cr]) ; format('~w n''est pas coupable de ~w.~n', [S, Cr]))
    ; C =:= 2 -> writeln('Crime:'), read_line_to_string(user_input, CrStr), string_to_atom(CrStr, Cr), show_guilty(Cr)
    ; C =:= 3 -> add_fact
    ; C =:= 4 -> writeln('Fin de l''enquête.'), !, halt
    ; writeln('Choix invalide.')
    ),
    fail.

show_guilty(Crime) :- findall(S, is_guilty(S, Crime), L), (L=[] -> format('Aucun coupable pour ~w.~n', [Crime]); format('Coupables pour ~w : ~w~n', [Crime, L])).

add_fact :-
    writeln('Ajouter fait (ex: suspect, motif, presence, empreinte, temoin, transaction, fausse_identite, alibi)'),
    read_line_to_string(user_input, TypeStr),
    string_to_atom(TypeStr, Type),
    writeln('Nom du suspect:'), read_line_to_string(user_input, SStr), string_to_atom(SStr, S),
    writeln('Type de crime (ou vide si pas nécessaire):'), read_line_to_string(user_input, CrStr),
    (CrStr = "" -> C = none ; string_to_atom(CrStr, C)),
    assert_fact(Type, S, C),
    writeln('Fait ajouté.').

assert_fact(suspect, S, _) :- assertz(suspect(S)).
assert_fact(motif, S, C) :- assertz(has_motive(S,C)).
assert_fact(presence, S, C) :- assertz(was_near_crime_scene(S,C)).
assert_fact(empreinte, S, C) :- assertz(has_fingerprint_on_weapon(S,C)).
assert_fact(temoin, S, C) :- assertz(eyewitness_identification(S,C)).
assert_fact(transaction, S, C) :- assertz(has_bank_transaction(S,C)).
assert_fact(fausse_identite, S, C) :- assertz(owns_fake_identity(S,C)).
assert_fact(alibi, S, C) :- assertz(has_alibi(S,C)).

% GUI XPCE
:- use_module(library(pce)).

start_gui :- build_main_dialog(D), send(D, open).

build_main_dialog(D) :-
    findall(S, suspect(S), Suspects),
    findall(C, crime_type(C), Crimes),
    new(D, dialog('Enquete Judiciaire')),
    new(SuspectMenu, menu(suspect, cycle)), forall(member(S, Suspects), send(SuspectMenu, append, S)),
    send(D, append, SuspectMenu),
    new(CrimeMenu, menu(crime, cycle)), forall(member(C, Crimes), send(CrimeMenu, append, C)),
    send(D, append, CrimeMenu),
    send(D, append, button('Tester culpabilité', message(@prolog, test_guilty, D, SuspectMenu?selection, CrimeMenu?selection))),
    send(D, append, button('Tous les coupables', message(@prolog, show_all_guilty, D, CrimeMenu?selection))),
    send(D, append, button('Ajouter un fait', message(@prolog, gui_add_fact, D))),
    send(D, append, button('Rafraîchir', message(@prolog, refresh_gui, D))),
    send(D, append, button('Quitter', message(D, destroy))),
    send(D, size, size(480,220)).

refresh_gui(OldD) :- send(OldD, destroy), start_gui.

test_guilty(_, SelS, SelC) :-
    (SelS==@nil ; SelC==@nil) -> show_result('Erreur', 'Selection invalide')
    ; (is_guilty(SelS, SelC) -> format(atom(M), '~w est coupable de ~w.', [SelS, SelC]) ; format(atom(M), '~w n''est pas coupable de ~w.', [SelS, SelC])),
      show_result('Résultat', M).

show_all_guilty(_, SelC) :-
    (SelC==@nil) -> show_result('Erreur', 'Selection invalide')
    ; findall(S, is_guilty(S, SelC), L), (L=[] -> format(atom(M), 'Aucun coupable pour ~w.', [SelC]) ; format(atom(M), 'Coupables pour ~w : ~w', [SelC, L])),
      show_result('Tous les coupables', M).

show_result(Title, Msg) :-
    new(D, dialog(Title)), send(D, append, label(msg, Msg)),
    send(D, append, button(ok, message(D, destroy))), send(D, open).

gui_add_fact(ParentD) :-
    new(D, dialog('Ajouter un fait')), 
    new(TypeM, menu(type, cycle)),
    forall(member(T,[suspect,motif,presence,empreinte,temoin,transaction,fausse_identite,alibi]), send(TypeM, append, T)),
    send(D, append, TypeM),
    new(SItem, text_item(suspect, '')), send(D, append, SItem),
    new(CItem, text_item(crime, '')), send(D, append, CItem),
    send(D, append, button(ajouter, message(@prolog, do_add_fact, TypeM?selection, SItem?selection, CItem?selection, D, ParentD))),
    send(D, append, button(annuler, message(D, destroy))),
    send(D, open).

do_add_fact(Type, SVal, CVal, DClose, ParentD) :-
    (string(SVal) -> atom_string(SAtom,SVal) ; SAtom=SVal), (string(CVal) -> atom_string(CAtom,CVal) ; CAtom=CVal),
    (Type=suspect -> assertz(suspect(SAtom))
    ; Type=motif -> assertz(has_motive(SAtom,CAtom))
    ; Type=presence -> assertz(was_near_crime_scene(SAtom,CAtom))
    ; Type=empreinte -> assertz(has_fingerprint_on_weapon(SAtom,CAtom))
    ; Type=temoin -> assertz(eyewitness_identification(SAtom,CAtom))
    ; Type=transaction -> assertz(has_bank_transaction(SAtom,CAtom))
    ; Type=fausse_identite -> assertz(owns_fake_identity(SAtom,CAtom))
    ; Type=alibi -> assertz(has_alibi(SAtom,CAtom))
    ; true),
    send(DClose, destroy),
    (nonvar(ParentD) -> refresh_gui(ParentD) ; true).
