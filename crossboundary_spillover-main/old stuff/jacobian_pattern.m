
function jac_mat = jacobian_pattern(network_data)

% dimensions of dataset
[p, a] = size(network_data);

% find indicies of nonzero elements
indices = find(network_data); 

% find number of nonzero elements
num_indices = length(indices);

% preallocate submatrices that build the answer
alpha_alpha = zeros(num_indices, num_indices);
a_alpha = zeros(a, num_indices);
p_alpha = zeros(p, num_indices);

% divide indices by rows to determine which animal interaction belongs to
a_interactions = idivide(uint32(indices) - 1, uint32(p)) + 1; %% tells with which pollinator the ith interaction takes place

for j = 1:a
    
    % for every animal j, determine the indices of their interactions
    j_interactions = sparse(find(a_interactions == j));
    
    % enter 1 at intersection of animal and index in a_alpha matrix
    a_alpha(j, j_interactions) = 1;
    
    % enter 1 at intersecion of index and index in alpha_alpha matrix
    alpha_alpha(j_interactions, j_interactions) = 1;
end

% make a refrence structure that assignes a counter to filled cells
count_matrix = network_data;
count_matrix(indices)=1:num_indices;

for i = 1:p
   
    % for every plant i, find the animals it does not interact with
    index = find(network_data(i,:) == 0);
    
    % copy network data into a temporary variable
    tmp = network_data;
    
    % exclude all interactions by animals that plant i doesn't interact with
    tmp(:, index) = 0;
    
    % enter 1 at intersection of plant and index in the p_alpha matrix
    p_alpha(i, count_matrix(find(tmp))) = 1 ;
end

% make matrices sparse
p_alpha = sparse(p_alpha);
a_alpha = sparse(a_alpha);
alpha_alpha = sparse(alpha_alpha);

% nectar alpha matrix is the same as the plant alpha matrix
n_alpha = p_alpha;

% compile every combination of interactions into a Jacovian matrix
% data: [plants, nectar, animals, alphas] x [plants, nectar, animals, alphas]
% dimension: [p + p + a + num_indices] x [p + p + a + num_indices]
jac_mat =[sparse(ones(p, p))         sparse(p, p)        network_data          p_alpha;
          sparse(1:p, 1:p, 1)        sparse(1:p, 1:p, 1) network_data          n_alpha;
          sparse(a, p)               network_data'       sparse(1:a, 1:a, 1)   a_alpha;
          sparse(num_indices, p)     n_alpha'            sparse(num_indices,a) alpha_alpha];
end
