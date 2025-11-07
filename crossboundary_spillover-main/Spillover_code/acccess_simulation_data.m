% HOW TO ACCESS DATA:
%
% % Load all data
%load('./simulation_results/Aikawa_2022_spring_all_versions.mat');
%
% % Access specific simulation and version
%plants_sim1_full = P_results{1, 1};           % Sim 1, full network
%plants_sim5_serp = P_results{5, 2};           % Sim 5, serpentine
%alpha_sim10_nonserp = Alpha_results{10, 3};   % Sim 10, non-serpentine
%
% % Extract all simulations for one version
%all_plants_full = P_results(:, 1);            % All sims, full network
%all_seeds_serp = SeedProduced_results(:, 2);  % All sims, serpentine
%
% % Calculate mean across simulations
%mean_plants_full = mean(cat(2, P_results{:,1}), 2);
%mean_gamma_serp = mean(cat(2, Gamma_results{:,2}), 2);
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

%%%%%%%%%%%%%%% Export simulation data for subsequent analysis in R
%%%%%%%%%%%%%%% %%%%%%%%%%%%%%%%%%%%%
%% ---------------- Setup ----------------
data_dir = './simulation_results';
matFiles = dir(fullfile(data_dir, '*_all_versions.mat'));

allPlantData  = [];
allAnimalData = [];

versionNames = {'full','serpentine','non_serpentine'};

%% ---------------- Loop  function over .mat files ----------------
for f = 1:length(matFiles)
    fileName = matFiles(f).name;
    load(fullfile(data_dir, fileName));
    [~, baseName, ~] = fileparts(fileName);  % dataset name
    
    nSim = size(P_results, 1);  % number of simulations
    
    %% ---------------- Loop over all simulations and versions ----------------
    for sim = 1:nSim
        for version = 1:3
            
            %% ---------------- Plant Outputs ----------------
            plant_mask = plant_masks{version};
            np_version = sum(plant_mask);
            
            % Preallocate with NaN
            P_vec = nan(np_version,1);
            Gamma_vec = nan(np_version,1);
            Seeds_vec = nan(np_version,1);
            PolServ_vec = nan(np_version,1);
            MeanSigmaP_vec = nan(np_version,1);
            VisitsP_percap_vec = nan(np_version,1);
            VisitsP_total_vec = nan(np_version,1);
            
            % Fill in present species
            P_vec(1:length(P_results{sim,version})) = P_results{sim,version};
            Gamma_vec(1:length(Gamma_results{sim,version})) = Gamma_results{sim,version};
            Seeds_vec(1:length(SeedProduced_results{sim,version})) = SeedProduced_results{sim,version};
            PolServ_vec(1:length(sPolServ_perP_results{sim,version})) = sPolServ_perP_results{sim,version};
            MeanSigmaP_vec(1:length(meansigma_perP_results{sim,version})) = meansigma_perP_results{sim,version};
            VisitsP_percap_vec(1:length(sVisits_perP_results{sim,version})) = sVisits_perP_results{sim,version};
            VisitsP_total_vec(1:length(sVisitsP_results{sim,version})) = sVisitsP_results{sim,version};
            
            % Creates table
            plantData = table((1:np_version)', P_vec, Gamma_vec, Seeds_vec, ...
                              PolServ_vec, MeanSigmaP_vec, VisitsP_percap_vec, VisitsP_total_vec, ...
                              'VariableNames', {'Plant_ID','P','Gamma','Seeds','PolServ','MeanSigmaP','VisitsP_percap','VisitsP_total'});
            
            % Metadata
            plantData.Plant_FullID = find(plant_mask);
            plantData.Simulation   = repmat(sim, np_version, 1);
            plantData.Version      = repmat(version, np_version, 1);
            plantData.VersionName  = repmat(string(versionNames{version}), np_version, 1);
            plantData.Dataset      = repmat(string(baseName), np_version, 1);
            
            allPlantData = [allPlantData; plantData];
            
            %% ---------------- Animal Outputs ----------------
            animal_mask = animal_masks{version};
            na_version = sum(animal_mask);
            
            % Preallocate with NaN
            A_vec = nan(na_version,1);
            N_extract_vec = nan(na_version,1);
            MeanSigmaA_vec = nan(na_version,1);
            VisitsA_percap_vec = nan(na_version,1);
            VisitsA_total_vec = nan(na_version,1);
            
            % Fill in present species
            A_vec(1:length(A_results{sim,version})) = A_results{sim,version};
            N_extract_vec(1:length(sN_extractj_perA_results{sim,version})) = sN_extractj_perA_results{sim,version};
            MeanSigmaA_vec(1:length(meansigma_perA_results{sim,version})) = meansigma_perA_results{sim,version};
            VisitsA_percap_vec(1:length(sVisits_perA_results{sim,version})) = sVisits_perA_results{sim,version};
            VisitsA_total_vec(1:length(sVisitsA_results{sim,version})) = sVisitsA_results{sim,version};
            
            % Create table
            animalData = table((1:na_version)', A_vec, N_extract_vec, MeanSigmaA_vec, VisitsA_percap_vec, VisitsA_total_vec, ...
                               'VariableNames', {'Animal_ID','A','N_extract','MeanSigmaA','VisitsA_percap','VisitsA_total'});
            
            % Metadata
            animalData.Animal_FullID = find(animal_mask);
            animalData.Simulation   = repmat(sim, na_version, 1);
            animalData.Version      = repmat(version, na_version, 1);
            animalData.VersionName  = repmat(string(versionNames{version}), na_version, 1);
            animalData.Dataset      = repmat(string(baseName), na_version, 1);
            
            allAnimalData = [allAnimalData; animalData];
        end
    end
end

%% ---------------- Save as a CSV for use in R ----------------
writetable(allPlantData, 'Plants_AllMetrics.csv'); %all plant outputs for all sites, simulations and versions
writetable(allAnimalData, 'Animals_AllMetrics.csv'); %all animal outputs for all sites, simulations and versions

disp('Export complete! Two CSV files created: Plants_AllMetrics.csv and Animals_AllMetrics.csv');
