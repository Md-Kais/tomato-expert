% run_from_answers.pl — run expert system from a simple answers file (no prompts)
% Usage:
%   swipl -q -s run_from_answers.pl -- --answers answers.txt --top 3 --lang en
%   swipl -q -s run_from_answers.pl -- --answers answers.txt --lang bn

:- set_prolog_flag(encoding, utf8).

:- dynamic disable_plant_main/0.
:- assert(disable_plant_main).        % stop plant_expert's entrypoint()

:- use_module(library(readutil)).
:- use_module(library(lists)).

:- [plant_expert].                    % load after disabling entrypoint

:- initialization(runner_main, main).

runner_main :-
  current_prolog_flag(argv, Argv),
  parse_cli(Argv, File, TopN, Lang),
  ( Lang = bn -> set_lang(bn) ; set_lang(en) ),
  run_once(File, TopN).

parse_cli(Argv, File, TopN, Lang) :-
  ( append(_, ['--answers',File|_], Argv)
  -> true
  ; writeln('ERROR: Please pass --answers <file>'), halt(1)
  ),
  ( append(_, ['--top',TopAtom|_], Argv)
  -> atom_number(TopAtom, TopN)
  ; TopN = 3
  ),
  ( append(_, ['--lang',LangA|_], Argv)
  -> map_lang(LangA, Lang)
  ; Lang = en ).

map_lang('bn', bn) :- !.
map_lang(bn, bn) :- !.
map_lang(_, en).

run_once(AnsFile, TopN) :-
  retractall(known(_,_)),
  retractall(asked(_)),
  seed_all_facts_default(0.0),
  load_answers_file(AnsFile),
  hypotheses(H),
  sort(2, @>=, H, Sorted),
  take(TopN, Sorted, Top),
  ( get_lang(bn) -> writeln("সম্ভাব্য কারণ (Top N):\n")
                  ; writeln("Likely causes (Top N):\n") ),
  print_top(Top),
  writeln('--- Done ---').

all_antecedent_facts(Facts) :-
  findall(AList, rule(_, AList, _), ALi),
  append(ALi, Flat),
  sort(Flat, Facts).

seed_all_facts_default(CF) :-
  all_antecedent_facts(Facts),
  forall(member(F, Facts),
        ( retractall(known(F,_)),
          asserta(known(F, CF)) )).

% ---- Read file & keep internal spaces (trim ends, drop comments) ----
load_answers_file(File) :-
  setup_call_cleanup(open(File, read, In),
                     read_answers_stream(In),
                     close(In)).

read_answers_stream(In) :-
  read_line_to_string(In, L),
  ( L == end_of_file -> true
  ; clean_line(L, Str),
    ( Str == "" -> true
    ; parse_answer(Str, Term, CF),
      retractall(known(Term,_)),
      asserta(known(Term, CF))
    ),
    read_answers_stream(In)
  ).

% Keep one space between tokens; remove leading/trailing whitespace; drop trailing comments.
clean_line(In, Out) :-
  % strip anything after '#'
  ( sub_string(In, HashPos, 1, _, "#") ->
      sub_string(In, 0, HashPos, _, Pre)
    ; Pre = In
  ),
  % collapse whitespace and trim ends, but KEEP single spaces between tokens
  normalize_space(string(Trimmed), Pre),
  Out = Trimmed.

% Parse "<ans> <PrologTerm>", e.g., "y symptom(watersoaked_large_blotch)"
parse_answer(Str, Term, CF) :-
  split_string(Str, " \t", " \t", [AnsTok|Rest]),
  atomic_list_concat(Rest, ' ', TermAtom),
  atom_string(TermA, TermAtom),
  read_term_from_atom(TermA, Term, []),
  ans_to_cf(AnsTok, CF).

ans_to_cf("y", 1.0) :- !.
ans_to_cf("n", 0.0) :- !.
ans_to_cf("?", 0.5) :- !.
ans_to_cf(S, CF) :-
  number_string(CF0, S), CF0 =< 1.0, CF0 >= -1.0, !, CF = CF0.
ans_to_cf(Bad, 0.0) :-
  format('WARNING: unknown answer "~w" (use y/n/?/number). Using 0.0.~n', [Bad]).
