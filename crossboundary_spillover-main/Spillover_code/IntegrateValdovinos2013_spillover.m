%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Developer: Fernanda S. Valdovinos
% Project: Cross-boundary spillover (Nelson et al 2025)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modification 11/10/2023, Davis
% Adaptation of pior code to the specific case of the serpentine
% plant-pollinator network with goatgrass (first row)
%
% Modification 08/03/2019, Ann Arbor
% Cleaning up my codes
% Only run the dynamics, without species invasions or removals
% Runs the dynamics for only one matrix.
% Outputs the whole time-series for any variable as well as final values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [plantsf, nectarf, animalsf, alphasf]=IntegrateValdovinos2013_spillover(metadata_version)

tmax=3000;
%EUp=2e-2;
%EUa=1e-3;

% Combining all initial variables
initial_state=full([metadata_version.p0;metadata_version.R0;metadata_version.a0;metadata_version.alphas0]);
tspan = [0 tmax];

%% Integrating the dynamic model
options = odeset('JPattern', metadata_version.J_pattern,'NonNegative',1:2*metadata_version.plant_qty+metadata_version.animal_qty) ;

rhs_with_metadata = @(t, y) Valdovinos2013_rhs_spillover(t, y, metadata_version);

[~, y]=ode15s(rhs_with_metadata, tspan, initial_state, options) ;

yf = y(end,:)';

% Retriving final densities and foraging efforts
[plantsf, nectarf, animalsf, alphasf] = unpack(yf, metadata_version);

plantsf=full(plantsf);
nectarf=full(nectarf);
animalsf=full(animalsf);
alphasf=full(alphasf);

end
