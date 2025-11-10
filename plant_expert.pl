% plant_expert.pl — Tomato Expert System (Backward-chaining + CF + JSON + REST + Sources + Language switch)
% RUN (interactive):  swipl -q -s plant_expert.pl -g main -t halt
% RUN (batch JSON):   swipl -q -s plant_expert.pl -- --facts facts.json
% RUN (HTTP API):     swipl -q -s plant_expert.pl -- --serve
% Example curl:       curl -X POST http://localhost:8080/infer -H "Content-Type: application/json" \
%                       -d '{"lang":"en","facts":[{"functor":"symptom","arg":"leaf_lesion_concentric","cf":1.0}], "top_n":3}'

:- set_prolog_flag(encoding, utf8).

:- dynamic known/2.
:- dynamic asked/1.           % prevent duplicate prompts
:- dynamic ui_lang/1.         % bn | en
:- dynamic disable_plant_main/0.  % runner uses this to stop auto-entrypoint

% ====================== Language selection ======================
set_lang(bn) :- retractall(ui_lang(_)), asserta(ui_lang(bn)).
set_lang(en) :- retractall(ui_lang(_)), asserta(ui_lang(en)).
get_lang(L)  :- ( ui_lang(L) -> true ; L = en ).

choose_lang_interactive :-
  ( ui_lang(_) -> true
  ; format('\nUse Bangla prompts? (y/n): '),
    read(Ans),
    ( Ans == y -> set_lang(bn) ; set_lang(en) )
  ).

normalize_lang(bn, bn) :- !.
normalize_lang('bn', bn) :- !.
normalize_lang(bangla, bn) :- !.
normalize_lang('Bangla', bn) :- !.
normalize_lang(en, en) :- !.
normalize_lang('en', en) :- !.
normalize_lang(english, en) :- !.
normalize_lang('English', en) :- !.
normalize_lang(_, en).

maybe_set_lang_from_dict(D) :-
  ( get_dict(lang, D, L0)
  -> normalize_lang(L0, L), ( L = bn -> set_lang(bn) ; set_lang(en) )
  ; true ).

% ====================== CF combine (MYCIN-style) ======================
combine_cf(A,B,R) :- A>=0, B>=0, !, R is A + B - A*B.
combine_cf(A,B,R) :- A=<0, B=<0, !, R is A + B + A*B.
combine_cf(A,B,R) :- Min is min(abs(A),abs(B)), R is (A + B) / (1 - Min).

min_list_cf([X],X).
min_list_cf([H|T],R) :- min_list_cf(T,R1), R is min(H,R1).

% ====================== Prompts (Bangla & English) ======================
pretty(Fact,Prompt) :- get_lang(bn), !, pretty_bn(Fact,Prompt).
pretty(Fact,Prompt) :- pretty_en(Fact,Prompt).

% ---- Bangla prompts ----
pretty_bn(symptom(leaf_lesion_concentric), "পাতায় concentric/bull's-eye (রিং) দাগ দেখা যাচ্ছে?").
pretty_bn(symptom(lower_leaf_first),        "দাগ/লক্ষণ প্রথমে নিচের পুরোনো পাতায় শুরু?").
pretty_bn(weather(warm_humid_week),         "গত ৭ দিনে আবহাওয়া ছিল উষ্ণ ও আর্দ্র (humid)?").
pretty_bn(irrigation(overhead),             "ওভারহেড সেচ/পাতা ভেজে এমন সেচ হয়েছে?").
pretty_bn(symptom(small_gray_centers),      "পাতায় ছোট গোল দাগ, মাঝখান ধূসর/আলগা আছে?").
pretty_bn(symptom(tiny_dark_margins),       "দাগের চারপাশে গাঢ় বর্ডার/মার্জিন আছে?").
pretty_bn(symptom(watersoaked_large_blotch),"পাতায় বড় জলসিক্ত বাদামি ব্লটচ দেখা যাচ্ছে?").
pretty_bn(weather(cool_wet_week),           "গত ৭ দিনে ঠান্ডা ও ভেজা আবহাওয়া ছিল?").
pretty_bn(spread_fast,                      "রোগ ছড়াচ্ছে খুব দ্রুত (কয়েক দিনে)?").
pretty_bn(setting(field),                   "চাষাবাদ খোলা মাঠে? (y) না গ্রিনহাউসে/হাই-টানেলে? (n)").
pretty_bn(symptom(tiny_dark_spots_halo),    "পাতায় ক্ষুদ্র গাঢ় স্পট, চারপাশে হলদে হ্যালো?").
pretty_bn(leaf_surface_greasy,              "পাতার ওপরে হালকা তেলতেলে অবস্থা?").
pretty_bn(symptom(pinpoint_black_specks),   "পাতায় সূচবিন্দু-আকারের কালো স্পেক?").
pretty_bn(symptom(halo_on_leaf),            "স্পটের চারপাশে স্পষ্ট হলদে হ্যালো?").
pretty_bn(leaf_margin_necrosis,             "পাতার কিনারা পোড়া/ব্রাউন?").
pretty_bn(stem(canker),                     "ডাঁটায় ক্যান্কার বা লম্বা ক্ষত আছে?").
pretty_bn(fruit(birds_eye_spot),            "ফলে 'bird's-eye' স্পট (সাদা কেন্দ্র, লাল রিং)?").
pretty_bn(sudden_wilt_green_plant,          "পাতা সবুজ রেখেও গাছ হঠাৎ ঢলে পড়ছে?").
pretty_bn(stem(brown_vascular),             "কাটা ডাঁটার ভিতরে বাদামি ভাস্কুলার ডিসকালারেশন/ওজ?").
pretty_bn(one_sided_yellowing,              "এক পাশে বেশি হলদে/নুইয়ে পড়া?").
pretty_bn(stem(vascular_browning),          "ডাঁটার ভাস্কুলার বাদামি (ফিউজারিয়াম-ধাঁচ)?").
pretty_bn(weather(warm_week),               "গত সপ্তাহে গরম ছিল?").
pretty_bn(symptom(v_shaped_marginal_necrosis),"পাতার কিনারা V-আকৃতির নেক্রোসিস?").
pretty_bn(cool_soil,                        "মাটি ঠান্ডা ছিল?").
pretty_bn(lower_leaf_yellowing,             "নিচের পাতাগুলো আগে হলদে?").
pretty_bn(symptom(gray_fuzzy_mold),         "পাতা/ডাঁটা/ফুলে ধূসর ফাজি ছত্রাক?").
pretty_bn(humidity_high,                    "আর্দ্রতা বেশি/ভেন্ট কম?").
pretty_bn(setting(greenhouse),              "গ্রিনহাউস/হাই-টানেল?").
pretty_bn(symptom(white_powder_upper),      "পাতার উপর সাদা 'পাউডারি' স্তর?").
pretty_bn(fruit(sunken_concentric),         "ফলে ছোট সানকেন রিং-দাগ?").
pretty_bn(rain_splash,                      "বৃষ্টি/সেচের ছিটায় বাড়ে?").
pretty_bn(fruit(near_ground),               "মাটির কাছে থাকা ফল বেশি আক্রান্ত?").
pretty_bn(root(galls),                      "মূলে গল/গাঁঠ?").
pretty_bn(plant(stunted),                   "গাছ খর্ব/বৃদ্ধি কম?").
pretty_bn(wilt_hot_afternoon,               "দুপুরে ঝুলে পড়ে?").
pretty_bn(vector(whitefly_seen),            "সাদা মাছি আছে?").
pretty_bn(leaf(curl_crumple),               "নতুন পাতায় কার্লিং/ক্রাম্পল?").
pretty_bn(plant(stunted_bushy),             "গাছ খাটো ও ঝোপালো?").
pretty_bn(leaf(mosaic_mottle),              "পাতায় মোজাইক/মটলিং?").
pretty_bn(leaf(fernleaf),                   "ফার্ন-লিফ/বিকৃত পাতা?").
pretty_bn(tool_hygiene_poor,                "টুল/হাত স্যানিটেশন খারাপ ছিল?").
pretty_bn(leaf(bronzing_necrotic_spots),    "কচি পাতায় ব্রোঞ্জিং/নেক্রোটিক স্পট?").
pretty_bn(fruit(ringspots),                 "ফলে concentric ring-spot?").
pretty_bn(vector(thrips_seen),              "থ্রিপস আছে?").
pretty_bn(rh_gt_85,                         "RH > 85% ছিল?").
pretty_bn(symptom(yellow_upper_spot),       "পাতার উপরে হলুদ স্পট?").
pretty_bn(symptom(olive_velvet_underside),  "পাতার নিচে জলপাই-রঙ ভেলভেটি স্পোর?").
pretty_bn(fruit(blossom_end_sunken_black),  "ফলের ব্লসম-এন্ডে কালচে সানকেন দাগ?").
pretty_bn(moisture_fluctuation,             "সেচে বড় ওঠানামা হয়েছে?").
pretty_bn(calcium_issue,                    "ক্যালসিয়াম সমস্যা সন্দেহ?").
pretty_bn(F,T) :- format(string(T), "~w ?", [F]).  % fallback

% ---- English prompts ----
pretty_en(symptom(leaf_lesion_concentric),  "Concentric/bull's-eye rings on leaves?").
pretty_en(symptom(lower_leaf_first),         "Do symptoms start on older lower leaves?").
pretty_en(weather(warm_humid_week),          "Were the last 7 days warm and humid?").
pretty_en(irrigation(overhead),              "Overhead irrigation / wet foliage?").
pretty_en(symptom(small_gray_centers),       "Small round leaf spots with gray centers?").
pretty_en(symptom(tiny_dark_margins),        "Do spots have dark margins?").
pretty_en(symptom(watersoaked_large_blotch), "Large water-soaked brown blotches on leaves?").
pretty_en(weather(cool_wet_week),            "Were the last 7 days cool and wet?").
pretty_en(spread_fast,                       "Is the disease spreading very quickly (few days)?").
pretty_en(setting(field),                    "Growing in open field? (y) greenhouse/high tunnel? (n)").
pretty_en(symptom(tiny_dark_spots_halo),     "Tiny dark leaf spots with yellow halos?").
pretty_en(leaf_surface_greasy,               "Leaf surface appears slightly greasy?").
pretty_en(symptom(pinpoint_black_specks),    "Pinpoint black specks on leaves?").
pretty_en(symptom(halo_on_leaf),             "Clear yellow halo around spots?").
pretty_en(leaf_margin_necrosis,              "Leaf margins scorched/brown?").
pretty_en(stem(canker),                      "Cankers / elongated lesions on stems?").
pretty_en(fruit(birds_eye_spot),             "Bird's-eye spots on fruit (white center, red ring)?").
pretty_en(sudden_wilt_green_plant,           "Plants wilt suddenly while still green?").
pretty_en(stem(brown_vascular),              "Cut stem shows brown vascular discoloration/ooze?").
pretty_en(one_sided_yellowing,               "One-sided yellowing or wilting?").
pretty_en(stem(vascular_browning),           "Vascular browning in stem (Fusarium-like)?").
pretty_en(weather(warm_week),                "Was the past week warm?").
pretty_en(symptom(v_shaped_marginal_necrosis),"V-shaped marginal necrosis on leaves?").
pretty_en(cool_soil,                         "Has the soil been cool?").
pretty_en(lower_leaf_yellowing,              "Do lower leaves yellow first?").
pretty_en(symptom(gray_fuzzy_mold),          "Gray fuzzy mold on leaves/stems/flowers?").
pretty_en(humidity_high,                     "High humidity / poor ventilation?").
pretty_en(setting(greenhouse),               "Growing in greenhouse/high tunnel?").
pretty_en(symptom(white_powder_upper),       "White powdery growth on upper leaf surface?").
pretty_en(fruit(sunken_concentric),          "Small sunken concentric lesions on fruit?").
pretty_en(rain_splash,                       "Worsens with rain splash/overhead watering?").
pretty_en(fruit(near_ground),                "Fruits close to soil most affected?").
pretty_en(root(galls),                       "Galls/knots on roots?").
pretty_en(plant(stunted),                    "Plants stunted or growing poorly?").
pretty_en(wilt_hot_afternoon,                "Wilting in hot afternoons?").
pretty_en(vector(whitefly_seen),             "Whiteflies present?").
pretty_en(leaf(curl_crumple),                "New leaves curling/crumpling?").
pretty_en(plant(stunted_bushy),              "Plants short and bushy?").
pretty_en(leaf(mosaic_mottle),               "Mosaic/mottling on leaves?").
pretty_en(leaf(fernleaf),                    "Fernlike distorted leaves?").
pretty_en(tool_hygiene_poor,                 "Tool/hand hygiene poor after grafting/pruning?").  % critical fix
pretty_en(leaf(bronzing_necrotic_spots),     "Bronzing/necrotic spots on tender leaves?").
pretty_en(fruit(ringspots),                  "Concentric ring spots on fruit?").
pretty_en(vector(thrips_seen),               "Thrips present?").
pretty_en(rh_gt_85,                          "Relative humidity > 85%?").
pretty_en(symptom(yellow_upper_spot),        "Yellow spots on upper leaf surface?").
pretty_en(symptom(olive_velvet_underside),   "Olive-velvety sporulation on leaf underside?").
pretty_en(fruit(blossom_end_sunken_black),   "Sunken black lesion at blossom end of fruit?").
pretty_en(moisture_fluctuation,              "Irrigation/water fluctuates widely?").
pretty_en(calcium_issue,                     "Possible calcium supply/uptake issue?").
pretty_en(F,T) :- format(string(T), "~w ?", [F]).  % fallback

% ====================== Ask / capture evidence ======================
ask(Fact, CF) :- known(Fact, CF), !.
ask(Fact, CF) :-
  asked(Fact), !,
  ( known(Fact, CF0) -> CF = CF0 ; CF = 0.5 ).
ask(Fact, CF) :-
  asserta(asked(Fact)),
  pretty(Fact,Prompt),
  format('\n[?] ~s (y/n/?/0..1): ', [Prompt]),
  read(Token),
  interpret(Token, CF1),
  asserta(known(Fact, CF1)),
  CF = CF1.

interpret(y, 1.0) :- !.
interpret(n, 0.0) :- !.
interpret('?', 0.5) :- !.
interpret(V, CF) :- number(V), V >= -1, V =< 1, CF is V.   % this is fine

cf_of_evidence([], []).
cf_of_evidence([F|Fs], [CF|CFs]) :- ask(F,CF), cf_of_evidence(Fs,CFs).

fire_rule(AnteFacts, RuleCF, ThisCF) :-
  cf_of_evidence(AnteFacts, CFs),
  min_list_cf(CFs, MinAnte),
  ThisCF is MinAnte * RuleCF.

% ====================== Knowledge base: Tomato problems ======================
% rule(Disease, [Facts...], RuleCF).
rule(early_blight,
 [symptom(leaf_lesion_concentric), symptom(lower_leaf_first), weather(warm_humid_week), irrigation(overhead)], 0.85).

rule(septoria_leaf_spot,
 [symptom(small_gray_centers), symptom(tiny_dark_margins), symptom(lower_leaf_first), weather(warm_humid_week)], 0.80).

rule(late_blight,
 [symptom(watersoaked_large_blotch), weather(cool_wet_week), spread_fast, setting(field)], 0.90).

rule(gray_mold,
 [symptom(gray_fuzzy_mold), humidity_high, setting(greenhouse)], 0.80).

rule(powdery_mildew,
 [symptom(white_powder_upper), humidity_high, setting(greenhouse)], 0.75).

rule(leaf_mold,
 [setting(greenhouse), rh_gt_85, symptom(yellow_upper_spot), symptom(olive_velvet_underside)], 0.85).

rule(anthracnose,
 [fruit(sunken_concentric), rain_splash, fruit(near_ground)], 0.80).

rule(bacterial_spot,
 [symptom(tiny_dark_spots_halo), leaf_surface_greasy, irrigation(overhead)], 0.75).

rule(bacterial_speck,
 [symptom(pinpoint_black_specks), symptom(halo_on_leaf), weather(cool_wet_week)], 0.70).

rule(bacterial_canker,
 [leaf_margin_necrosis, stem(canker), fruit(birds_eye_spot)], 0.85).

rule(bacterial_wilt,
 [sudden_wilt_green_plant, stem(brown_vascular)], 0.90).

rule(fusarium_wilt,
 [one_sided_yellowing, stem(vascular_browning), weather(warm_week)], 0.85).

rule(verticillium_wilt,
 [symptom(v_shaped_marginal_necrosis), cool_soil, lower_leaf_yellowing], 0.80).

rule(root_knot_nematode,
 [root(galls), plant(stunted), wilt_hot_afternoon], 0.85).

rule(tylcv,
 [vector(whitefly_seen), leaf(curl_crumple), plant(stunted_bushy)], 0.90).

rule(tomato_mosaic,
 [leaf(mosaic_mottle), leaf(fernleaf), tool_hygiene_poor], 0.75).

rule(tswv,
 [leaf(bronzing_necrotic_spots), fruit(ringspots), vector(thrips_seen)], 0.85).

rule(blossom_end_rot,
 [fruit(blossom_end_sunken_black), moisture_fluctuation, calcium_issue], 0.90).

% ====================== Management advice ======================
advice(early_blight,
 ["Rotate 2+ yrs; drip/leaf-dry; debris sanitation; resistant cultivars"],
 ["chlorothalonil","mancozeb","QoI/DMI mixes (label)"]).
advice(septoria_leaf_spot,
 ["Remove infected leaves; airflow; avoid saving seed; rotate"],
 ["chlorothalonil","mancozeb","copper (preventive)"]).
advice(late_blight,
 ["Scout & rogue; protect before cool-wet spells; 5–7 day program if alerts"],
 ["chlorothalonil/mancozeb protectants","mandipropamid/cyazofamid (where labeled)"]).
advice(gray_mold,
 ["Ventilation; keep foliage dry; bag/remove infected tissue"],
 ["labelled fungicides if severe (check label)"]).
advice(powdery_mildew,
 ["Ventilation; avoid excess N; spacing"],
 ["sulfur","potassium bicarbonate (label)"]).
advice(leaf_mold,
 ["Keep RH<85%; vent/heat; remove infected leaves"],
 ["chlorothalonil","copper (label)"]).
advice(anthracnose,
 ["Mulch; drip irrigation; harvest promptly; target fruit coverage"],
 ["chlorothalonil","mancozeb","azoxystrobin + difenoconazole (label)"]).
advice(bacterial_spot,
 ["Clean seed/transplants; avoid overhead; sanitize trays/tools"],
 ["copper (preventive)","copper+mancozeb (where labeled)"]).
advice(bacterial_speck,
 ["Avoid working when wet; disease-free seed; rotate"],
 ["copper (preventive)"]).
advice(bacterial_canker,
 ["Destroy infected plants; hot-water/clean seed; sanitize stakes"],
 ["(no curative) copper may suppress spread (label)"]).
advice(bacterial_wilt,
 ["Avoid infested fields; graft/resistant rootstock; rotate; improve drainage"],
 ["— (no curative)"]).
advice(fusarium_wilt,
 ["Use F-resistant cultivars; rotate; restrict soil movement"],
 ["— fumigation/commercial only; follow local guidance"]).
advice(verticillium_wilt,
 ["V-resistant cultivars; rotate to grains/corn; sanitize equipment"],
 ["— (no curative)"]).
advice(root_knot_nematode,
 ["VFN cultivars; rotation; solarization; organic matter"],
 ["fluensulfone (where legal/available; label)"]).
advice(tylcv,
 ["Whitefly management; remove infected plants; resistant variety"],
 ["— (no curative)"]).
advice(tomato_mosaic,
 ["Tool/hand sanitation; remove infected plants"],
 ["— (no curative)"]).
advice(tswv,
 ["Thrips IPM; remove infected plants; resistant/susceptible varieties accordingly"],
 ["— (no curative)"]).
advice(blossom_end_rot,
 ["Keep soil moisture steady; avoid over-N; balance Ca/pH"],
 ["— (physiological; not a pathogen)"]).

% ====================== Sources per disease ======================
sources(early_blight, [["UMN Early/Late blight overview","https://extension.umn.edu/disease-management/early-blight-tomato-and-potato"]]).
sources(septoria_leaf_spot, [
  ["Wisconsin Horticulture: Septoria leaf spot","https://hort.extension.wisc.edu/articles/septoria-leaf-spot/"],
  ["UW Vegetable Pathology (symptoms)","https://vegpath.plantpath.wisc.edu/diseases/tomato-septoria-leaf-spot/"]
]).
sources(late_blight, [["UMN: Late blight","https://extension.umn.edu/disease-management/late-blight"]]).
sources(gray_mold, [["UMN Gray mold of tomatoes","https://extension.umn.edu/disease-management/gray-mold-tomatoes"]]).
sources(powdery_mildew, [["USU Extension: Powdery mildew","https://extension.usu.edu/vegetableguide/tomato-pepper-eggplant/powdery-mildew"]]).
sources(leaf_mold, [["UMN: Tomato leaf mold","https://extension.umn.edu/disease-management/tomato-leaf-mold"]]).
sources(anthracnose, [["UConn IPM: Tomato anthracnose","https://ipm.cahnr.uconn.edu/tomato-anthracnose/"]]).
sources(bacterial_spot, [["UMN: Bacterial spot","https://extension.umn.edu/disease-management/bacterial-spot-tomato-and-pepper"]]).
sources(bacterial_speck, [["Wisconsin Horticulture: Bacterial speck","https://hort.extension.wisc.edu/articles/bacterial-speck-of-tomato/"]]).
sources(bacterial_canker, [["NC State: Bacterial canker of tomato","https://content.ces.ncsu.edu/bacterial-canker-of-tomato"]]).
sources(bacterial_wilt, [["NC State: Bacterial wilt of tomatoes","https://content.ces.ncsu.edu/bacterial-wilt-of-tomatoes"]]).
sources(fusarium_wilt, [["NC State: Fusarium wilt of tomato","https://content.ces.ncsu.edu/fusarium-wilt-of-tomato"]]).
sources(verticillium_wilt, [["NC State: Verticillium wilt of tomato/eggplant","https://content.ces.ncsu.edu/verticillium-wilt-of-tomato-and-eggplant"]]).
sources(root_knot_nematode, [["NC State: Root-knot nematode of tomato","https://content.ces.ncsu.edu/root-knot-nematode-of-tomato"]]).
sources(tylcv, [["NC State: Tomato yellow leaf curl virus","https://content.ces.ncsu.edu/tomato-yellow-leaf-curl-virus"]]).
sources(tomato_mosaic, [["UConn IPM: Mosaic diseases of tomatoes","https://ipm.cahnr.uconn.edu/mosaic-diseases-of-tomatoes/"]]).
sources(tswv, [["UC IPM: Tomato spotted wilt","https://ipm.ucanr.edu/agriculture/tomato/tomato-spotted-wilt/"]]).
sources(blossom_end_rot, [["OSU Extension: Blossom-end rot","https://extension.oregonstate.edu/catalog/fs-139-blossom-end-rot-tomatoes"]]).

% ====================== Inference & printing ======================
hypotheses(List) :-
  findall(D, rule(D,_,_), DsRaw),
  sort(DsRaw, Ds),
  gather(Ds, List).

gather([], []).
gather([D|T], [(D,CFbest)|Rest]) :-
  findall(CF, (rule(D,Ante,RCF), fire_rule(Ante,RCF,CF)), CFs),
  best_cf(CFs, CFbest),
  gather(T, Rest).

best_cf([], 0.0).
best_cf([H|T], R) :- foldl(combine_cf, T, H, R).

print_sources(D) :-
  ( sources(D, Pairs) ->
      writeln("  Sources:"),
      forall(member([Title,URL], Pairs), format("    - ~s — ~s~n", [Title,URL]))
    ; writeln("  Sources: (not set)")
  ).

print_top([]).
print_top([(D,CF)|T]) :-
  CF > 0.0, !,
  CFp is round(CF*100),
  format("~w — confidence ~w%~n", [D,CFp]),
  advice(D,Cult,Acts),
  format("  Cultural: ~w~n  Actives:  ~w~n", [Cult,Acts]),
  print_sources(D), nl,
  print_top(T).
print_top([_|T]) :- print_top(T).

banner :-
  ( get_lang(bn) ->
      ( writeln("টমেটো এক্সপার্ট সিস্টেম"),
        writeln("উত্তর দিন: y. / n. / ?. / বা 0.8.  প্রতিটি উত্তরের শেষে ডট দিন '.'"),
        writeln("কোনও কীটনাশক ব্যবহারের আগে স্থানীয় লেবেল/এক্সটেনশন নির্দেশনা মানুন।\n") )
    ; ( writeln("Tomato Expert System"),
        writeln("Answer with y. / n. / ?. / or a number like 0.8.  End every answer with a period '.'"),
        writeln("ALWAYS follow local labels & extension guidance for any pesticide.\n") )
  ).

main :-
  retractall(known(_,_)),
  retractall(asked(_)),
  choose_lang_interactive,
  banner,
  hypotheses(H),
  sort(2, @>=, H, Sorted),
  take(3, Sorted, Top3),
  ( get_lang(bn) -> writeln("সম্ভাব্য কারণ (Top 3):\n")
                  ; writeln("Likely causes (Top 3):\n") ),
  print_top(Top3),
  writeln("--- Done ---").

take(N,L,R) :- length(R,N), append(R,_,L), !.
take(_,L,L).

% ====================== JSON + HTTP modes ======================
:- use_module(library(http/json)).         % json_read_dict/3
:- use_module(library(http/http_json)).    % http_read_json_dict/3, reply_json_dict/1
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

as_atom(X, A) :- atom(X),   !, A = X.
as_atom(X, A) :- string(X), !, atom_string(A, X).
as_atom(X, A) :- number(X), !, atom_number(A, X).
as_atom(X, X).

set_known_fact(Term, CF) :- asserta(known(Term, CF)).

set_facts_from_list([]).
set_facts_from_list([F|Rest]) :-
  ( F = _{functor:Fun0, arg:Arg0, cf:CF} ->
      as_atom(Fun0, Fun),
      ( Arg0 == "" -> Term =.. [Fun]
      ; as_atom(Arg0, Arg), Term =.. [Fun,Arg] ),
      set_known_fact(Term, CF)
  ; F = Fun-Arg-CF ->
      Term =.. [Fun,Arg], set_known_fact(Term, CF)
  ; F =.. [Fun,Arg] ->
      Term =.. [Fun,Arg], set_known_fact(Term, 1.0)
  ; Term = F, set_known_fact(Term, 1.0)
  ),
  set_facts_from_list(Rest).

infer_from_facts(Facts, TopN, TopPairs) :-
  retractall(known(_,_)),
  retractall(asked(_)),
  set_facts_from_list(Facts),
  hypotheses(H),
  sort(2, @>=, H, Sorted),
  take(TopN, Sorted, TopPairs).

infer_json_file(File) :-
  setup_call_cleanup(open(File, read, In),
                     json_read_dict(In, Dict, [value_string_as(atom)]),
                     close(In)),
  maybe_set_lang_from_dict(Dict),
  Facts = Dict.get(facts),
  ( TopReq = Dict.get(top_n) -> TopN = TopReq ; TopN = 3 ),
  infer_from_facts(Facts, TopN, TopPairs),
  ( get_lang(bn) -> writeln("সম্ভাব্য কারণ (Top N):\n")
                  ; writeln("Likely causes (Top N):\n") ),
  forall(member((D,CF), TopPairs),
        ( CFp is round(CF*100),
          format("~w — confidence ~w%~n", [D,CFp]),
          advice(D,Cult,Acts),
          format("  Cultural: ~w~n  Actives:  ~w~n", [Cult,Acts]),
          ( sources(D,S) ->
              writeln("  Sources:"),
              forall(member([T,U],S), format("    - ~s — ~s~n", [T,U]))
            ; true ),
          nl )).

:- http_handler(root(infer), infer_handler, [method(post)]).

serve_http :-
  Port = 8080,
  http_server(http_dispatch, [port(Port)]),
  format('Server running at http://localhost:~w/infer (POST JSON)~n', [Port]).

infer_handler(Request) :-
  http_read_json_dict(Request, DictIn, [value_string_as(atom)]),
  maybe_set_lang_from_dict(DictIn),
  Facts = DictIn.get(facts),
  ( TopReq = DictIn.get(top_n) -> TopN = TopReq ; TopN = 3 ),
  infer_from_facts(Facts, TopN, TopPairs),
  maplist(pair_to_item, TopPairs, Items),
  reply_json_dict(_{diagnoses: Items}).

pair_to_item((D,CF), Item) :-
  CFp is round(CF*100),
  advice(D,Cult,Acts),
  ( sources(D,SList) -> true ; SList = [] ),
  Item = _{ disease:D,
            confidence_pct:CFp,
            cultural:Cult,
            actives:Acts,
            sources:SList }.

% ====================== Guarded entrypoint ======================
:- initialization(entrypoint, main).

entrypoint :-
  ( disable_plant_main -> true
  ; current_prolog_flag(argv, Argv),
    ( append(_,['--serve'|_], Argv) ->
        set_lang(en), serve_http
    ; append(_,['--facts',File|_], Argv) ->
        set_lang(en), infer_json_file(File)
    ; main
    )
  ).
