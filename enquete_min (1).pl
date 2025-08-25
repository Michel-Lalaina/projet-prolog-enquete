% 1253H_F
% Projet IA : Enquête policière

% typ de crime
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
    write('Entrer un suspect : '), nl,
    read(Suspect),
    write('Entrer un type de crime : '), nl,
    read(CrimeType),
    ( is_guilty(Suspect, CrimeType) ->
        writeln('=> Le suspect est COUPABLE')
    ;
        writeln('=> Le suspect est NON COUPABLE')
    ),
    halt.
