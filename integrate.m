function [final_parameters] = integrate(file_name, initial_state, tspan)

    global J_pattern simulation extinct_level_p network_metadata 

    % determine dimensions of dataset
    [row, col] = size(network_metadata.data);

    % integrate the dynamic model
    options = odeset('JPattern', J_pattern, 'NonNegative', 1:(2 * row) + col);
    [time, parameters] = ode15s(@apply_equations, tspan, initial_state, options);
    
    % access final row of parameter values
    final_parameters = parameters(end,:)';

end

