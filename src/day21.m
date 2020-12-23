:- module day21.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions, set.

main(!IO) :-
    io.format("===[ %s ]===\n", [s($module)], !IO),
    Ex1 = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)",
    Example1 = parse_input(Ex1),
    A1 = part1(Example1),
    E1 = 5,
    io.format("P1 test: expected %d, got %d\n", [i(E1), i(A1)], !IO),

    util.read_file_as_string("../input/day21.txt", InputString, !IO),
    Problem1 = parse_input(InputString),
    P1Answer = part1(Problem1),
    io.format("P1: got %d (expected 2410)\n", [i(P1Answer)], !IO),

    P2Test = part2(Example1),
    io.format("P2 test: expected %s, got %s\n", [s("mxmxvkd,sqjhc,fvjkl"), s(P2Test)], !IO),
    P2Answer = part2(Problem1),
    io.format("P2: got %s\n", [s(P2Answer)], !IO),
    io.print_line("=== * ===", !IO).

:- func part1(problem) = int.
part1(Foods) = SumOfNonAllergenAppearances :-
   AllergenIngredients: list(ingredient) = map.values(determine_allergens(Foods)^assigned),
   AllIngredients = condense(map((func(F) = F^ingredients), Foods)),
   NonAllergenIngredients = list.delete_elems(AllIngredients, AllergenIngredients),
    % trace [io(!IO)] (io.write_line({"AllergenIngredients", AllergenIngredients, "NonAllergenIngredients", NonAllergenIngredients}, !IO)),
    SumOfNonAllergenAppearances = length(NonAllergenIngredients).

:- func part2(problem) = string.
part2(Foods) = IngredientsSortedByAllergen :-
    AssocList = to_sorted_assoc_list(determine_allergens(Foods)^assigned),
    trace [io(!IO)] (io.write_line({"Allergens", keys(AssocList): list(string)}, !IO)),
    Ingredients = values(AssocList),
    IngredientsSortedByAllergen = string.join_list(",", Ingredients).

:- type problem == list(food).
:- type food ---> food(ingredients :: list(string), allergens:: list(string)).
:- func parse_input(string) = problem.
parse_input(Input) = Problem :-
    Lines = split_at_string("\n", strip(Input)),
    Problem = map(parse_food, Lines).

:- func parse_food(string) = food.
parse_food(Line) = food(Ingredients, Allergens) :-
    (if
        [InStr, AlStr] = split_at_string(" (contains ", Line)
     then
        Ingredients = words(strip(InStr)),
        AlCommaSepd = rstrip_pred(unify(')'), AlStr),
        Allergens = split_at_string(", ", AlCommaSepd)
     else
         unexpected($module, $pred, format("failed to split food line: %s", [s(Line)]))
    ).

:- type allergen == string.
:- type ingredient == string.
:- type round
    --->    round(
                % Maps unassigned allergens to the unassigned ingredients in the foods known to contain the allergen.
                remaining :: map(allergen, set(ingredient)),

                % Maps an allergen to the ingredient containing it.
                assigned :: map(allergen, ingredient)
            ).



:- func init_round(problem) = round.
init_round(Foods) = round(Remaining, init) :-
    Maps = map((func(F) = Result :-
        Ingredientses = from_list(F^ingredients),
        Result = foldl((func(K, A) = det_insert(A, K, Ingredientses)), F^allergens, init)
    ), Foods),
    union_list((pred(X::in, Y::in, Z::out) is det :-
        intersect(X, Y, Z)), init, Maps, Remaining).

:- func determine_allergens(problem) = round.
determine_allergens(Problem) = Result :-
    Round = init_round(Problem),
    Result = run_rounds(Round).

:- func run_rounds(round) = round.
run_rounds(Round) = Result :-
    Next = run_round(Round),
    (if
        Next = Round
     then
        Result = Next
     else
         Result = run_rounds(Next)
    ).

:- func run_round(round) = round.
run_round(Round) = Next :-
    (if
        is_empty(Round^remaining)
    then
        % trace [io(!IO)] (io.write_line({"DONE"}, !IO)),
        Next = Round
    else
        AToICount = map.map_values_only(set.count, Round^remaining),
        solutions(inverse_search(AToICount, 1), ToEliminate),
        % trace [io(!IO)] (io.write_line({"AToICount", AToICount, "ToEliminate", ToEliminate}, !IO)),
        (if
            Allergen = head(ToEliminate),
            singleton_set(Ingredient, map.lookup(Round^remaining, Allergen))
        then
            PrevAssigned = Round^assigned,
            Next^assigned = PrevAssigned^elem(Allergen) := Ingredient,
            R = delete(Round^remaining, Allergen),
            map_values_only(set.delete(Ingredient), R, Next^remaining)
            % , trace [io(!IO)] (io.write_line({"Eliminated", Allergen, Ingredient, "Remaining", map.to_assoc_list(Next^remaining): assoc_list(allergen, set(ingredient))}, !IO))
        else
            unexpected($module, $pred, "Not solvable - nothing to eliminate!")
        )
    ).

/*
list(food(ingredients :: list(string), allergens :: list(string))), known :: assoc_list(ingredient, allergen), ingredients, allergens)

Each ingredient contains zero or one allergen. Allergens aren't always marked.

mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)

allergens (3): dairy, fish, soy
ingredients (7): mxmxvkd, kfcds, sqjhc, nhms, trh, fvjkl, sbzzf

there will be 4 allergy-free foods. need to find them through some sort of process of elimination.

ROUND 1:

soy: one_of(sqjhc fvjkl) => intersection is (sqjhc fvjkl)
fish: one_of(sqjhc mxmxvkd sbzzf), one_of(mxmxvkd kfcds sqjhc nhms) => intersection is (sqjhc mxmxvkd)
dairy: one_of(mxmxvkd kfcds sqjhc nhms), one_of(trh fvjkl sbzzf mxmxvkd) => must be in the intersection! so could only be (mxmxvkd)

ROUND 2:
dairy is mxmxvkd, eliminate it from ingredient lists.

soy: one_of(sqjhc fvjkl) => intersection is still (sqjhc fvjkl)
fish: one_of(sqjhc sbzzf), one_of(kfcds sqjhc nhms) => intersection is now (sqjhc)

ROUND 3
fish is sqjhc, eliminate it

soy: one_of(fvjkl) => intersection is now (fvjkl)

RESULT
we have an assignment. now go and cross 'em all off and sum the remaining ingredients.

none of the ingredients kfcds, nhms, sbzzf, or trh can contain an allergen.

*/
