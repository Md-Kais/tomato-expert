% test_plant_expert.pl — unit tests for plant_expert.pl
% Run all tests:
%   swipl -q -s test_plant_expert.pl -g run_tests -t halt
%
% Requires plant_expert.pl in the same directory.

:- set_prolog_flag(encoding, utf8).
:- [plant_expert].                   % load your expert system

:- begin_tests(plant_expert).

% ---------- Helpers ----------
top1(Facts, D, CF) :-
    infer_from_facts(Facts, 1, [(D,CF)]).

approx(Expected, Actual, Tol) :-
    Diff is abs(Expected - Actual),
    Diff =< Tol.

% ---------- Core scenarios ----------

% Early blight: min(1,1,1,0.8)*0.85 = 0.68
test(early_blight_is_top) :-
    Facts = [ symptom-leaf_lesion_concentric-1.0,
              symptom-lower_leaf_first-1.0,
              weather-warm_humid_week-1.0,
              irrigation-overhead-0.8
            ],
    top1(Facts, D, CF),
    assertion(D == early_blight),
    assertion(approx(0.68, CF, 0.001)).

% Late blight: all 1.0 -> 1.0*0.90 = 0.90
test(late_blight_is_top) :-
    Facts = [ symptom-watersoaked_large_blotch-1.0,
              weather-cool_wet_week-1.0,
              spread_fast-1.0,
              setting-field-1.0
            ],
    top1(Facts, D, CF),
    assertion(D == late_blight),
    assertion(approx(0.90, CF, 0.001)).

% TSWV: all 1.0 -> 1.0*0.85 = 0.85
test(tswv_is_top) :-
    Facts = [ leaf-bronzing_necrotic_spots-1.0,
              fruit-ringspots-1.0,
              vector-thrips_seen-1.0
            ],
    top1(Facts, D, CF),
    assertion(D == tswv),
    assertion(approx(0.85, CF, 0.001)).

% Blossom end rot: physiological; all 1.0 -> 0.90
test(blossom_end_rot_is_top) :-
    Facts = [ fruit-blossom_end_sunken_black-1.0,
              moisture_fluctuation-1.0,
              calcium_issue-1.0
            ],
    top1(Facts, D, CF),
    assertion(D == blossom_end_rot),
    assertion(approx(0.90, CF, 0.001)).

% Gray mold (Botrytis) in greenhouse/high humidity
test(gray_mold_top_in_greenhouse) :-
    Facts = [ symptom-gray_fuzzy_mold-1.0,
              humidity_high-1.0,
              setting-greenhouse-1.0
            ],
    top1(Facts, D, CF),
    assertion(D == gray_mold),
    assertion(approx(0.80, CF, 0.001)).

% Root-knot nematode: 0.85
test(root_knot_top) :-
    Facts = [ root-galls-1.0,
              plant-stunted-1.0,
              wilt_hot_afternoon-1.0
            ],
    top1(Facts, D, CF),
    assertion(D == root_knot_nematode),
    assertion(approx(0.85, CF, 0.001)).

% ---------- Meta checks ----------

% Every disease in the KB should have at least one source entry
test(all_diseases_have_sources) :-
    findall(D, rule(D,_,_), Ds),
    forall(member(D, Ds), sources(D, S)),       % succeeds only if sources/2 defined
    forall(member(D, Ds),
           ( sources(D, S),
             S \= [],                            % not empty
             S = [[Title,URL]|_],
             string(Title), string(URL) )).

% Language switch sanity: we can flip bn/en
test(language_switch_bn_en) :-
    set_lang(bn), get_lang(L1), assertion(L1 == bn),
    set_lang(en), get_lang(L2), assertion(L2 == en).

:- end_tests(plant_expert).
