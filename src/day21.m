:- module day21.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool, char, int, list, string, util, require.
:- import_module map, assoc_list, pair, ranges.
:- import_module solutions.

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

    % util.read_file_as_string("../input/day21.txt", InputString, !IO),
    io.print_line("=== * ===", !IO).

:- func part1(problem) = int.
part1(Problem) = SumOfNonAllergenAppearances :-
    % {_Allergens, NonAllergens} = determine_allergens(Problem),
    % Sum the number of times each of the non-allergen ingredients appears in any ingredients list
    SumOfNonAllergenAppearances = -1.

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
    )
    .

:- type solution == string.

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
