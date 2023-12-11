function EEG = manuallyRemoveBadComps(EEG)
% 1:Brain, 2:Muscle, 3:Eye, 4:Heart, 5:Line Noise, 6:Channel Noise, 7:Other
clc

Types = {'Brain', 'Muscle', 'Eye', 'Heart'};

KeepLooping = true;

while KeepLooping

    close all

    % if there are already bad channels, ask if all ok
    if isfield(EEG, 'manual') && any(ismember(EEG.manual, [2, 3, 4]))

        OldManual = EEG.manual;

        % plot current status
        plotComps(EEG)

        % check if user is happy with selection
        xOk = input('Is the COMPONENT selection ok? (y/n) ', "s");
        xOk = lower(xOk);

        % if yes, dont run rest of loop
        if contains(xOk, 'y') || contains(xOk, 'z') % stupid german keyboards
            break
        end

    else
        % if no manual data available, set up blank
        OldManual = nan(size(EEG.icaweights, 1), 1);
        EEG.manual = OldManual;
        plotComps(EEG)
    end


    for Indx_T = 1:numel(Types)

        AlreadyDone = find(EEG.manual==Indx_T)';
        if ~isempty(AlreadyDone)
            AlreadyDoneString = [' [', num2str(AlreadyDone(:)'), '] : '];
        else
            AlreadyDoneString = ': ';
        end

        clc
        c = 'c';
        disp("INSTRUCTIONS: 'c' to remove classification, copy&paste and add components to extend.")
        
        xComp = input([Types{Indx_T}, ' components', AlreadyDoneString]);
        
        if isempty(xComp) % keep old selection
            continue
        elseif strcmp(xComp, c)
            EEG.manual(AlreadyDone) = nan;
        elseif isnumeric(xComp)
            if any(xComp)>numel(OldManual)
                error('bad comp')
            end

            EEG.manual(AlreadyDone) = nan;
            EEG.manual(xComp) = Indx_T;
        end

    end

    % if no bad channels provided, quit loop
    if all(EEG.manual == OldManual)
        KeepLooping = false;
    end
end

% convert to badchans where it indicates 0s and 1s
EEG.reject.gcompreject = zeros(size(OldManual));
EEG.reject.gcompreject(ismember(EEG.manual, 2:4)) = 1;


end

