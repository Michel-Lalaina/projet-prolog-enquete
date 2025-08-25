:- use_module(library(pce)).

crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

has_motive(john, vol).
was_near_crime_scene(john, assassinat).
has_fingerprint_on_weapon(john, assassinat).

has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).

has_bank_transaction(bruno, escroquerie).

owns_fake_identity(sophie, escroquerie).

is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol).

is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    has_fingerprint_on_weapon(Suspect, assassinat).

is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie);
    has_bank_transaction(Suspect, escroquerie);
    owns_fake_identity(Suspect, escroquerie).

main :-
    new(D, dialog('Enquête Policiere')),
    findall(S, suspect(S), Suspects),
    findall(C, crime_type(C), Crimes),
    new(SuspectMenu, menu(suspect)),
    send_list(SuspectMenu, append, Suspects),
    send(D, append, SuspectMenu),
    new(CrimeMenu, menu(crime)),
    send_list(CrimeMenu, append, Crimes),
    send(D, append, CrimeMenu),
    send(D, append, button('Verifier',
        message(@prolog, check_guilt_gui, SuspectMenu?selection, CrimeMenu?selection))),
    send(D, append, button('Quitter', message(D, destroy))),
    send(D, open).

check_guilt_gui(Suspect, Crime) :-
    ( is_guilty(Suspect, Crime) ->
        format(string(M), '=> Le suspect ~w est COUPABLE de ~w', [Suspect, Crime])
    ;
        format(string(M), '=> Le suspect ~w est NON COUPABLE de ~w', [Suspect, Crime])
    ),
    new(R, dialog('Resultat')),
    send(R, append, label(result, M)),
    send(R, append, button(ok, message(R, destroy))),
    send(R, open).

