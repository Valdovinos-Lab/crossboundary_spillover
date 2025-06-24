
function [plants, nectar, animals, alphas] = expand_full(parameters)

global network_metadata

p = network_metadata.plant_qty;
a = network_metadata.animal_qty;

plants = parameters(:, 1:p);
nectar = parameters(:, p + 1 : 2 * p);
animals = parameters(:, 2 * p + 1:2 * p + a);
alphas = parameters(:,2 * p + a + 1:end);

end

