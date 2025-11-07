%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Cross-boundary spillover (Nelson et al 2026)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Runs the Valdovinos et al's (2013) model for the Spillover project
% Modifications specific to the spillover project:
% This script processes 55 empirical plant-pollinator networks and runs 
% simulations to test the effects of spillover on plant and pollinator 
% population dynamics. The simulations compare three network scenarios:
%   1. Full network: includes all plant-pollinator interactions (with spillover).
%   2. Serpentine-only network: interactions restricted to plant species 
%      that grow on serpentine soils.
%   3. Non-serpentine-only network: interactions restricted to plant species 
%      that grow on non-serpentine soils.
%
% We achieve the different treatments using masks. A mask is a logical true/false
% array used to filter or modify data. In this case:
%   a. serpentine_mask and non_serp_mask identify whether each plant sp
%      belongs to the serpentine or non-serpentine group, respectively.
%   b. These masks are applied to the rows of the network matrix to "zero out"
%      interactions for plants that don't belong to the selected group.

% KEY difference from previous code in terms of plant dynamics:
% Inter-specific competition parameter u is implemented as a matrix where
% serpentine and non-serpentine plants do not compete with each other (u(i,j)=0
% when plants i and j are from different soil types). This reflects the ecological
% reality that these plant groups occupy different soil environments.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OUTPUT STRUCTURE AND DATA RETRIEVAL GUIDE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SAVE LOCATION: ./simulation_results/[NetworkName]_all_versions.mat
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DATA STRUCTURE: {sim_index, version_index}
% - sim_index: 1 to num_simulations (e.g., 1 to 100)
% - version_index: 1=full, 2=serpentine, 3=non_serpentine
% - version_names: {'full', 'serpentine', 'non_serpentine'}
%
% STORED VARIABLES (14 cell arrays):
%
% --- POPULATION DYNAMICS ---
% P_results: Plant abundances [n_plants x 1]
% A_results: Animal abundances [n_animals x 1]
% R_results: Nectar abundances [n_plants x 1]
% Alpha_results: Preference matrix [n_plants x n_animals]
%
% --- PLANT METRICS ---
% Gamma_results: Recruitment rate [n_plants x 1]
% SeedProduced_results: Total seeds [n_plants x 1]
% sPolServ_perP_results: Pollination services per plant [n_plants x 1]
% meansigma_perP_results: Mean visit quality per plant [n_plants x 1]
% sVisits_perP_results: Visits per plant (per-capita) [n_plants x 1]
% sVisitsP_results: Visits per plant (population-level) [n_plants x 1]
%
% --- ANIMAL METRICS ---
% sN_extractj_perA_results: Resources extracted per animal [n_animals x 1]
% meansigma_perA_results: Mean visit quality per animal [n_animals x 1]
% sVisits_perA_results: Visits per animal (per-capita) [n_animals x 1]
% sVisitsA_results: Visits per animal (population-level) [n_animals x 1]
%
% --- MASKS ---
% plant_masks: {1x3 cell} Logical masks indicating which plants from the full
%              network are present in each version [n_plants_full x 1]
%              {1} = all true (full network)
%              {2} = serpentine plants only
%              {3} = non-serpentine plants only
% animal_masks: {1x3 cell} Logical masks indicating which animals from the full
%               network are present in each version [n_animals_full x 1]
%               {1} = all true (full network)
%               {2} = animals visiting serpentine plants
%               {3} = animals visiting non-serpentine plants
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HOW TO ACCESS DATA:
%
% % Load all data
% load('./simulation_results/Aikawa_2022_spring_all_versions.mat');
%
% % Access specific simulation and version
% plants_sim1_full = P_results{1, 1};           % Sim 1, full network
% plants_sim5_serp = P_results{5, 2};           % Sim 5, serpentine
% alpha_sim10_nonserp = Alpha_results{10, 3};   % Sim 10, non-serpentine
%
% % Extract all simulations for one version
% all_plants_full = P_results(:, 1);            % All sims, full network
% all_seeds_serp = SeedProduced_results(:, 2);  % All sims, serpentine
%
% % Calculate mean across simulations
% mean_plants_full = mean(cat(2, P_results{:,1}), 2);
% mean_gamma_serp = mean(cat(2, Gamma_results{:,2}), 2);
%
% % Compare versions for specific simulation
% sim = 10;
% plants_full = P_results{sim, 1};
% plants_serp = P_results{sim, 2};
% plants_nonserp = P_results{sim, 3};
%
% % Use masks to map results back to full network
% serp_plant_mask = plant_masks{2};
% serp_animal_mask = animal_masks{2};
% % Create full-sized arrays with NaN for absent species
% plants_full_size = nan(sum(plant_masks{1}), 1);
% plants_full_size(serp_plant_mask) = P_results{sim, 2};
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Model Parameters
frG = 1;
muAP = 3;

% Load input networks
network_dir = 'input_networks';
network_structs = dir(fullfile(network_dir, '*.csv'));
networks = fullfile({network_structs.folder}, {network_structs.name});

% Define non-serpentine plant species
non_serp_rows = {'ASER', 'VIVI', 'CESO', 'MEIN', 'PHAQ', 'ANAR', 'AMME', ...
                 'ERCI', 'HIIN', 'Asteraceae sp.', 'TAOF', 'MEPO', 'SEVU'};

num_simulations = 100;

% Loop through each network
for network_index = 1:length(networks)
    
    % Load network data
    tbl = readtable(networks{network_index}, 'ReadRowNames', true, 'VariableNamingRule', 'preserve');
    row_ids = tbl.Properties.RowNames;
    col_ids = tbl.Properties.VariableNames;
    network_data = table2array(tbl);

    % Create plant masks
    non_serp_mask = ismember(row_ids, non_serp_rows);
    serpentine_mask = ~non_serp_mask;
       
    % Create serpentine network: keep only serpentine plants and remove
    % pollinators with zero-sum visits.
    serpentine_data = network_data(serpentine_mask, :);
    animal_mask_serp = sum(serpentine_data, 1) > 0;
    serpentine_data = serpentine_data(:, animal_mask_serp);

    % Create non-serpentine network: keep only non-serpentine plants and remove
    % pollinators with zero-sum visits.
    non_serpentine_data = network_data(non_serp_mask, :);
    animal_mask_nonserp = sum(non_serpentine_data, 1) > 0;
    non_serpentine_data = non_serpentine_data(:, animal_mask_nonserp);
           
    % Store data versions
    data_versions = {network_data, serpentine_data, non_serpentine_data};
    version_names = {'full', 'serpentine', 'non_serpentine'};
    
    % Check which versions are empty (do this ONCE per network)
    versions_to_run = ~cellfun(@isempty, data_versions);
    
    % Store plant masks (for subsetting parameters)
    plant_masks = {
        true(size(network_data, 1), 1);  % Full: keep all plants
        serpentine_mask;                  % Serpentine: keep only serpentine
        non_serp_mask                     % Non-serpentine: keep only non-serpentine
    };
    
    % Store animal masks (for subsetting parameters)
    % These indicate which animals from the full network are present in each version
    animal_masks = {
        true(size(network_data, 2), 1);  % Full: keep all animals
        animal_mask_serp';                % Serpentine: animals that visit serpentine plants
        animal_mask_nonserp'              % Non-serpentine: animals that visit non-serp plants
    };
    
    % Extract network name for saving
    [~, network_name, ~] = fileparts(networks{network_index});

    % Preallocate results for each simulation and version
    Alpha_results = cell(num_simulations, 3);
    P_results = cell(num_simulations, 3);
    A_results = cell(num_simulations, 3);
    R_results = cell(num_simulations, 3);
    Gamma_results = cell(num_simulations, 3);
    SeedProduced_results = cell(num_simulations, 3);
    sPolServ_perP_results = cell(num_simulations, 3);
    sN_extractj_perA_results = cell(num_simulations, 3);
    meansigma_perP_results = cell(num_simulations, 3);
    sVisits_perP_results = cell(num_simulations, 3);
    sVisitsP_results = cell(num_simulations, 3);
    meansigma_perA_results = cell(num_simulations, 3);
    sVisits_perA_results = cell(num_simulations, 3);
    sVisitsA_results = cell(num_simulations, 3);

    % Report empty versions once
    for version_index = 1:3
        if ~versions_to_run(version_index)
            fprintf('Network %s, Version %d (%s): Empty network, skipping all simulations.\n', ...
                network_name, version_index, version_names{version_index});
        end
    end
    
    % Run simulations
    for sim_index = 1:num_simulations
        
        % Create metadata for all versions with correct masks
        metadata = create_metadata_spillover(frG, data_versions, muAP, ...
                                             plant_masks, animal_masks, sim_index);
                      
        % Run simulations for this version
        for version_index = 1:3
            
            % Skip if this version is empty
            if ~versions_to_run(version_index)
                continue;
            end

            % Run dynamics simulation with the correct metadata
            metadata_version = metadata{version_index};
            [plantsf, nectarf, animalsf, alphasf] = IntegrateValdovinos2013_spillover(metadata_version);
            
            % Calculate mechanistic metrics
            [Gamma, seed_produced, sPolServ_perP, sN_extractj_perA, meansigma_perP, ...
             sVisits_perP, sVisitsP, meansigma_perA, sVisits_perA, sVisitsA] = ...
             calValMechs_spillover(alphasf, plantsf, animalsf, nectarf, metadata_version);
            
            % Store results
            P_results{sim_index, version_index} = plantsf;
            A_results{sim_index, version_index} = animalsf;
            R_results{sim_index, version_index} = nectarf;
            Alpha_results{sim_index, version_index} = alphasf;
            Gamma_results{sim_index, version_index} = Gamma;
            SeedProduced_results{sim_index, version_index} = seed_produced;
            sPolServ_perP_results{sim_index, version_index} = sPolServ_perP;
            sN_extractj_perA_results{sim_index, version_index} = sN_extractj_perA;
            meansigma_perP_results{sim_index, version_index} = meansigma_perP;
            sVisits_perP_results{sim_index, version_index} = sVisits_perP;
            sVisitsP_results{sim_index, version_index} = sVisitsP;
            meansigma_perA_results{sim_index, version_index} = meansigma_perA;
            sVisits_perA_results{sim_index, version_index} = sVisits_perA;
            sVisitsA_results{sim_index, version_index} = sVisitsA;
                
        end
                
    end
    
    fprintf('%s completed\n', network_name);

    % Save all results for this network in one file
    save_path = fullfile('./simulation_results', sprintf('%s_all_versions.mat', network_name));
    save(save_path, ...
         'Alpha_results', 'P_results', 'A_results', 'R_results', ...
         'Gamma_results', 'SeedProduced_results', ...
         'sPolServ_perP_results', 'sN_extractj_perA_results', ...
         'meansigma_perP_results', 'sVisits_perP_results', 'sVisitsP_results', ...
         'meansigma_perA_results', 'sVisits_perA_results', 'sVisitsA_results', ...
         'version_names', 'plant_masks', 'animal_masks');

end

fprintf('\nAll simulations completed and results saved.\n');
fprintf('Results saved in: ./simulation_results/\n');
