function metadata = create_metadata_spillover(frG, data_versions, muAP, plant_masks, animal_masks, sim_index)

% Key differences from previous code specific to spillover:
% 1. Creates 1×3 cell array with parameter values and initial conditions for
% the three network versions, starting from the full network so then species
% with the same id keep their parameters when separated in the other two versions 
% (serpentine vs non-serpentine)
% 2. Parameter u is a plant_qty × plant_qty matrix representing inter-specific
% competition for soil resources. Elements u(i,j) are set to zero when plants i 
% and j belong to different soil types (serpentine vs non-serpentine), ensuring
% that competition only occurs between plants sharing the same soil environment.
% 3. Initial states are generated separately for each network version inside the loop
% 4. plant_masks and animal_masks are provided as inputs from RunValdovinos
% 5. DEFENSIVE PROGRAMMING: All vector subsetting operations use reshape(..., [], 1)
% to force column vectors. This prevents dimension mismatch errors when networks
% contain only one plant or animal species, where MATLAB's subsetting can return
% row vectors or scalars instead of column vectors. This does not affect results
% for multi-species networks.

% Setting a seed per simulation run that is used in the three versions
rng(sim_index);

%% Step 1: Generate parameters from FULL network
In_full = double(data_versions{1} > 0);  % Binary matrix from full network
[plant_qty_full, animal_qty_full] = size(In_full);
B_full = sparse(In_full);
vectG_full = frG * ones(1, animal_qty_full);

% Parameters of the uniform distribution
varp = 1e-1; % variance of plant parameters
vara = 0;    % variance of animal parameters
mC = 0.2; vC = vara;
mE = 0.8; vE = varp;
mb = 0.4; vb = vara;
mU = 0.06; vU = varp;
mw = 1.2; vw = varp;
mB = 0.2; vB = varp;
mG = 2; vG = vara;
mg = 0.4; vg = varp;
mphi = 0.04; vphi = varp;
mtau = 1; vtau = vara;
mepsilon = 1; vepsilon = 0;
vmA = vara; vmP = varp;

% Set mortality parameters
if muAP == 1
    mmA = 0.05; mmP = 0.001; % high pollinator mortality 
elseif muAP == 2
    mmA = 0.001; mmP = 0.02; % high plant mortality
elseif muAP == 3
    mmA = 0.001; mmP = 0.001; % low plant and animal mortality
elseif muAP == 4
    mmA = 0.03; mmP = 0.005; % high plant and animal mortality
end

% Draw parameters from uniform distribution for FULL network
c_full = uniform_rand(mC, vC, plant_qty_full, animal_qty_full) .* B_full;
b_full = uniform_rand(mb, vb, plant_qty_full, animal_qty_full) .* B_full;

e_full = uniform_rand(mE, vE, plant_qty_full, 1);

% u is now a MATRIX for inter-specific competition
u_full = uniform_rand(mU, vU, plant_qty_full, plant_qty_full);
% Zero out competition between serpentine and non-serpentine plants
serpentine_mask = plant_masks{2};  % Get serpentine mask from input
same_soil = (serpentine_mask == serpentine_mask');  % Logical matrix
u_full = u_full .* same_soil;

Beta_full = uniform_rand(mB, vB, plant_qty_full, 1);
G_full = uniform_rand(mG, vG, animal_qty_full, 1) .* vectG_full';
g_full = uniform_rand(mg, vg, plant_qty_full, 1);
mu_a_full = uniform_rand(mmA, vmA, animal_qty_full, 1);
mu_p_full = uniform_rand(mmP, vmP, plant_qty_full, 1);
w_full = uniform_rand(mw, vw, plant_qty_full, 1);
phi_full = uniform_rand(mphi, vphi, plant_qty_full, 1);
epsilon_full = uniform_rand(mepsilon, vepsilon, plant_qty_full, 1);
tau_full = uniform_rand(mtau, vtau, animal_qty_full, 1);

%% Step 2: Create metadata for all three versions
metadata = cell(1, 3);

% Initial state parameters
mz = 0.5; vz = 1e-1;

for version_idx = 1:3
    % Get current version's data
    In_version = double(data_versions{version_idx} > 0);
    [plant_qty_version, animal_qty_version] = size(In_version);
    
    % Get masks for this version
    plant_mask = plant_masks{version_idx};
    animal_mask = animal_masks{version_idx};
    
    % Subset plant-specific parameters (vectors) - FORCE COLUMN VECTORS
    e_version = reshape(e_full(plant_mask), [], 1);
    u_version = u_full(plant_mask, plant_mask);  % Subset both dimensions
    Beta_version = reshape(Beta_full(plant_mask), [], 1);
    g_version = reshape(g_full(plant_mask), [], 1);
    mu_p_version = reshape(mu_p_full(plant_mask), [], 1);
    w_version = reshape(w_full(plant_mask), [], 1);
    phi_version = reshape(phi_full(plant_mask), [], 1);
    epsilon_version = reshape(epsilon_full(plant_mask), [], 1);
    
    % Subset animal-specific parameters (vectors) - FORCE COLUMN VECTORS
    mu_a_version = reshape(mu_a_full(animal_mask), [], 1);
    tau_version = reshape(tau_full(animal_mask), [], 1);
    
    % Adjust G for this version's vectG
    vectG_version = frG * ones(1, animal_qty_version);
    G_version = reshape(G_full(animal_mask) .* vectG_version' ./ vectG_full(animal_mask)', [], 1);
    
    % Subset interaction-specific parameters (matrices)
    B_version = sparse(In_version);
    c_version = c_full(plant_mask, animal_mask) .* B_version;
    b_version = b_full(plant_mask, animal_mask) .* B_version;
    
    % Generate initial conditions for this version - FORCE COLUMN VECTORS
    yzero_version = uniform_rand(mz, vz, 2*plant_qty_version + animal_qty_version, 1);
    initial_plants_version = reshape(yzero_version(1:plant_qty_version), [], 1);
    initial_rewards_version = reshape(yzero_version(plant_qty_version+1:2*plant_qty_version), [], 1);
    initial_animals_version = reshape(yzero_version(2*plant_qty_version+1:2*plant_qty_version+animal_qty_version), [], 1);
    
    % Generate initial alphas for this version
    nz_pos_version = find(B_version);
    initial_alphas_matrix_version = B_version;
    col_sums = sum(initial_alphas_matrix_version, 1);
    col_sums(col_sums == 0) = 1;  % Avoid division by zero
    initial_alphas_matrix_version = initial_alphas_matrix_version * diag(col_sums.^(-1));
    initial_alphas_version = reshape(initial_alphas_matrix_version(nz_pos_version), [], 1);
    
    % Compute J_pattern for this version
    J_pattern_version = J_zero_pattern(In_version);
    
    % Validation checks
    assertP(isequal(size(e_version), [plant_qty_version, 1]));
    assertP(isequal(size(mu_p_version), [plant_qty_version, 1]));
    assertP(isequal(size(mu_a_version), [animal_qty_version, 1]));
    assertP(isequal(size(B_version), size(c_version)));
    assertP(isequal(size(B_version), size(b_version)));
    assertP(isequal(size(u_version), [plant_qty_version, plant_qty_version]));
    assertP(isequal(size(w_version), [plant_qty_version, 1]));
    assertP(isequal(size(Beta_version), [plant_qty_version, 1]));
    assertP(isequal(size(G_version), [animal_qty_version, 1]));
    assertP(isequal(size(g_version), [plant_qty_version, 1]));
    assertP(isequal(size(phi_version), [plant_qty_version, 1]));
    
    assertP(all(mu_p_version > 0));
    assertP(all(mu_a_version > 0));
    assertP(all(c_version(:) >= 0));
    assertP(all(b_version(:) >= 0));
    assertP(all(u_version(:) >= 0));
    assertP(all(w_version >= 0));
    assertP(all(g_version >= 0));
    assertP(all(G_version >= 0));
    assertP(all(phi_version > 0));
    
    % Create metadata structure for this version
    metadata{version_idx} = struct(...
        'plant_qty', plant_qty_version, ...
        'animal_qty', animal_qty_version, ...
        'nz_pos', nz_pos_version, ...
        'e', e_version, ...
        'mu_p', mu_p_version, ...
        'mu_a', mu_a_version, ...
        'c', c_version, ...
        'b', b_version, ...
        'u', u_version, ...
        'w', w_version, ...
        'Beta', Beta_version, ...
        'G', G_version, ...
        'g', g_version, ...
        'phi', phi_version, ...
        'tau', tau_version, ...
        'epsilon', epsilon_version, ...
        'B', B_version, ...
        'p0', initial_plants_version, ...
        'R0', initial_rewards_version, ...
        'a0', initial_animals_version, ...
        'alphas0', initial_alphas_version, ...
        'J_pattern', J_pattern_version);
end

end
