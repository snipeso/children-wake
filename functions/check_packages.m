function check_packages(ListOfPackages)
% this a little script for my code that checks if all the packages are
% loaded in the computer

% EEGLAB
if contains('eeglab', ListOfPackages)
    if ~exist('eeglab', 'file')
        error('Missing EEGLAB path')
    end
elseif ~exist('topoplot', 'file')
    eeglab
end

AllFound = true;

% my packages
if contains('oscip', ListOfPackages)
    try oscip.findme; catch; warning('oscip not found'); AllFound = false; end
end

if contains('chART', ListOfPackages)
    try chART.findme; catch; warning('chART not found');AllFound = false; end
end

if contains('cycy', ListOfPackages)
    try cycy.findme; catch; warning('cycy (Matcycle) not found');AllFound = false; end
end

if ~AllFound
    error('add packages to MATLAB path')
end
clc