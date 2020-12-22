#include "bits-and-bobs.hh"
#include <unordered_set>

using namespace std;

struct Relationship {
  vector<string> ingredients;
  vector<string> allergens;
};

vector<Relationship> parseRelationships(const vector<string> &lines) {
  vector<Relationship> relationships;
  for (const string &line : lines) {
    std::regex relRe("([a-z\\s]+)\\s\\(contains\\s([a-z,\\s]+)\\)");
    std::smatch relMatch;
    std::regex_match(line, relMatch, relRe);
    assert(!relMatch.empty());
    vector<string> ingedients{parseVectorStr(relMatch.str(1))};
    vector<string> allergensDirty{splitString(relMatch.str(2), ",")};
    vector<string> allergens;
    transform(allergensDirty.cbegin(), allergensDirty.cend(),
              back_inserter(allergens),
              [](const auto &s) { return trimWhitespace(s); });
    relationships.push_back({std::move(ingedients), std::move(allergens)});
  }
  return relationships;
}

unordered_map<string, string>
assignAllergensToIngredients(const vector<Relationship> &relationships) {
  unordered_map<string, vector<vector<string>>> ingredientsByAllergen;
  for (const auto &relationship : relationships) {
    for (const auto &allergen : relationship.allergens) {
      vector<string> ingredients = relationship.ingredients;
      std::sort(ingredients.begin(), ingredients.end());
      ingredientsByAllergen[allergen].push_back(std::move(ingredients));
    }
  }

  // assign allergens to ingredients
  size_t numAllergens = ingredientsByAllergen.size();
  unordered_map<string, string> assignedAllergens;
  while (assignedAllergens.size() < numAllergens) {
    string assignedIngredient;
    for (const auto &allergenWithIngredients : ingredientsByAllergen) {
      const string &allergen = allergenWithIngredients.first;
      const vector<vector<string>> &ingredientSets =
          allergenWithIngredients.second;
      // print(allergen);
      // printMatrix(ingredientSets);

      // run set intersections
      if (ingredientSets.size() > 1) {
        vector<string> curIntersection = ingredientSets.at(0);
        for (auto it = ingredientSets.cbegin() + 1; it != ingredientSets.cend();
             it++) {
          const auto &ingredients = *it;
          vector<string> newIntersection;
          std::set_intersection(curIntersection.cbegin(),
                                curIntersection.cend(), ingredients.cbegin(),
                                ingredients.cend(),
                                back_inserter(newIntersection));
          curIntersection = std::move(newIntersection);
        }
        // print(allergen);
        // printContainer(curIntersection);
        if (curIntersection.size() == 1) {
          assignedIngredient = curIntersection.at(0);
          assignedAllergens[allergen] = assignedIngredient;
          break;
        }
      } else {
        const vector<string> &ingredients = ingredientSets.at(0);
        if (ingredients.size() == 1) {
          assignedIngredient = ingredients.at(0);
          assignedAllergens[allergen] = assignedIngredient;
          break;
        }
      }
    }
    // remove assigned ingredient
    if (assignedIngredient.size() > 0) {
      for (auto &allergenWithIngredients : ingredientsByAllergen) {
        vector<vector<string>> &ingredientSets = allergenWithIngredients.second;
        for (vector<string> &ingredients : ingredientSets) {
          vector<string> filteredIngredients;
          std::copy_if(ingredients.cbegin(), ingredients.cend(),
                       back_inserter(filteredIngredients),
                       [&assignedIngredient](const string &s) {
                         return s != assignedIngredient;
                       });
          ingredients = filteredIngredients;
        }
      }
    }
  }
  return assignedAllergens;
}

void part1(const vector<string> &lines) {
  vector<Relationship> relationships = parseRelationships(lines);
  unordered_map<string, string> assignedAllergens =
      assignAllergensToIngredients(relationships);

  // get ingredients without allergens
  unordered_set<string> allergenFreeIngredients;
  for (const auto &relationship : relationships) {
    for_each(relationship.ingredients.cbegin(), relationship.ingredients.cend(),
             [&allergenFreeIngredients](const auto &ing) {
               allergenFreeIngredients.insert(ing);
             });
  }
  for (const auto &p : assignedAllergens) {
    allergenFreeIngredients.erase(p.second);
  }
  // count occurances of allergen free ingredients
  int count = 0;
  for (const auto &relationship : relationships) {
    count += std::accumulate(
        relationship.ingredients.cbegin(), relationship.ingredients.cend(), 0,
        [&allergenFreeIngredients](auto acc, const string &ing) {
          if (allergenFreeIngredients.find(ing) !=
              allergenFreeIngredients.cend()) {
            return acc + 1;
          }
          return acc;
        });
  }
  print(count);
}

void part2(const vector<string> &lines) {
  vector<Relationship> relationships = parseRelationships(lines);
  unordered_map<string, string> allergensToIngredients =
      assignAllergensToIngredients(relationships);
  unordered_map<string, string> ingredientsToAllgergens;
  for (const auto &p : allergensToIngredients) {
    ingredientsToAllgergens[p.second] = p.first;
  }

  vector<string> dangerousIngredients;
  transform(ingredientsToAllgergens.cbegin(), ingredientsToAllgergens.cend(),
            back_inserter(dangerousIngredients),
            [](const auto &p) { return p.first; });
  sort(dangerousIngredients.begin(), dangerousIngredients.end(),
       [&ingredientsToAllgergens](const auto &s1, const auto &s2) {
         return ingredientsToAllgergens.at(s1) < ingredientsToAllgergens.at(s2);
       });
  string dangerList =
      accumulate(dangerousIngredients.cbegin(), dangerousIngredients.cend(),
                 std::string(), [](string acc, const string &s) {
                   if (acc.size() > 0) {
                     return acc + "," + s;
                   }
                   return s;
                 });
  print(dangerList);
}

int main() {
  auto lines = readLinesFromFile("input/day21.txt");
  part1(lines);
  part2(lines);
}
