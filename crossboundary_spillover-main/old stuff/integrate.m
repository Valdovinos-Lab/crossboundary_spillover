function [final_parameters] = integrate(file_name, initial_state, tspan)

    global J_pattern simulation extinct_level_p network_metadata 

    % determine dimensions of dataset
    [row, col] = size(network_metadata.data);
  %  final_parameters=[];
    % integrate the dynamic model
    options = odeset('JPattern', J_pattern, 'NonNegative', 1:(2 * row) + col);
    [time, parameters] = ode15s(@apply_equations, tspan, initial_state, options);

% %       % Create `u` matrix for serpentine and non-serpentine species
% %         mean_u = 0;  % Define the mean for the uniform distribution
% %         var_p = 1;   % Define the variance for the uniform distribution
% %         row = length(row_ids);  % Number of rows in the matrix
% % 
% %         % For serpentine species (rows corresponding to serpentine_mask)
% %         u_serpentine = uniform_rand(mean_u, var_p, sum(serpentine_mask), 1);
% % 
% %         % For non-serpentine species (rows corresponding to non_serp_mask)
% %         u_nonserpentine = uniform_rand(mean_u, var_p, sum(non_serp_mask), 1);
% % 
% %         % Debugging: Check the u matrices for both serpentine and non-serpentine
% %         disp('Serpentine u:'); disp(u_serpentine);
% %         disp('Non-serpentine u:'); disp(u_nonserpentine);
% % 
% %     % access final row of parameter values

   final_parameters = parameters(end,:)';
% %     % Helper function for generating uniform random variables
% % function u = uniform_rand(mean_u, var_p, row, col)
% %     u = mean_u + sqrt(var_p) * rand(row, col);  % Generate uniform distribution
% % end

end

