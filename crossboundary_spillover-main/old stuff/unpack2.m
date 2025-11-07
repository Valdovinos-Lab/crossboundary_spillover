function [plants, nectar, animals, alphas] = unpack2(y, network_metadata)

n = network_metadata.plant_qty ;
m = network_metadata.animal_qty ;

plants=y(:,1:n) ;
nectar=y(:,n+1:2*n);
animals=y(:,2*n+1: 2*n+m);
alphas=y(:,2*n+m+1:end);

end
