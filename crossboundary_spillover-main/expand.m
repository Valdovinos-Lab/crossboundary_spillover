function [plants, nectar, animals, alphas] = expand(parameters)

global network_metadata

p = network_metadata.plant_qty;
a = network_metadata.animal_qty;

plants = parameters(1:p, 1);
nectar = parameters(p + 1 : 2 * p, 1);
animals = parameters(2 * p + 1:2 * p + a, 1);
alphas = sparse(p, a);
alphas(network_metadata.indices) = parameters(2 * p + a + 1 : end, 1);

end

