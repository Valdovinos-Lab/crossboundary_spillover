function [data] = invasion_setup(network_data, file_name)

% Function that performs the introduction of 1 plant species
% to a network. Introduced species can be specialists (inv_type=1:4) 
% or generalists (inv_type=5:8), and they can link randomly (link_case=1), to
% generalists (link_case=2), or to specialists (link_case=3) 

[row, col] = size(network_data);
    
%%%%%%%%%%%%%% Defining degree of native plants and animals %%%%%%%%%%%%%%%

% degree of natives plants   
knative_p = sum(network_data, 2);

% degree of native animals
knative_a = sum(network_data);

%%%%%%%%%%%%%%%%%%%%% Defining degree of alien plant %%%%%%%%%%%%%%%%%%%%%%

% size of 30% of plant species in the network
third = round(length(knative_p) * 0.3);

if inv_type < 5   
    
    % specialist alien: degree equal to mean of 30% most specialist natives
    k = round(mean(knative_p(1:third)));
else    
    
    % generalist alien: degree equal to mean of 30% most generalist natives
    k = round(mean(knative_p((end - third + 1):end)));
end

%%%%%%%%%%%%%% Defining species to which alien is linked to %%%%%%%%%%%%%%%

% preallocate vector for k alien links
inv_links = zeros(1, k);
    
switch link_case
    
    % alien links randomly with any native pollinator
    case 1
        
        % randomize the order of pollinators
        rand_a = randperm(length(knative_a));
        
        % assign the first k pollinators to be linked to alien
        inv_links = rand_a(1:k);
        
    % alien links with generalist native pollinators
    case 2
         
        % sort native animals in descending degree
        [degree, indices] = sort(knative_a,'descend');
         
        % determine the k most generalist species including repeat degrees
        kmin = min(degree(1:k));
        generalists = indices(degree >= kmin);
         
        % randomly permute these generalits to determine invader links
        rand_generalists = generalists(randperm(length(generalists)));
        inv_links = rand_generalists(1:k);
             
             
     % alien links with specialist native pollinators
     case 3
        
        % sort native animals in ascending degree
        [degree, indices] = sort(knative_a,'ascend');
        
        % determine the k most specialist species including repeat degrees
        kmax = max(degree(1:k));
        specialists = indices(degree <= kmax);
        
        % randomly permute these specialists to determine invader links
        rand_specialists = specialists(randperm(length(specialists)));
        inv_links = rand_specialists(1:k);
          
end
           
%%%%%%%%%%%%%%%%%%%%%%%% Making the introduction %%%%%%%%%%%%%%%%%%%%%%%%%

% create row for alien link data
inv_mat = zeros(1, col);

% set all links in alien row to 1
inv_mat(inv_links) = 1;

% add alien row to the network data
%data = [inv_mat; network_data];
data=[network_data];
end

    
    

