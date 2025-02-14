function [MatchedWaves_FH, MatchedWaves_LH] = match_sw_amplitudes(FH_Amplitudes, LH_Amplitudes, ToleranceThreshold)
arguments
    FH_Amplitudes % list of amplitdes
    LH_Amplitudes
    ToleranceThreshold = 1;
end

MatchedWaves_LH = nan(1, numel(LH_Amplitudes)); % uses nans to keep track of which ones were found (1), not found (0), or haven't been searched for yet (nan)
MatchedWaves_FH = false(1, numel(FH_Amplitudes)); % just needs to indicate which ones were found

while any(isnan(MatchedWaves_LH))
    
    % randomly select a wave from those unmatched in the last hour
    Unmatched = find(isnan(MatchedWaves_LH));
    LH_Wave_Index = Unmatched(randi(numel(Unmatched)));
    LH_Amp = LH_Amplitudes(LH_Wave_Index);

    % identify all waves in first hour within tolderance threshold
    Candidates = find(FH_Amplitudes >= LH_Amp-ToleranceThreshold & FH_Amplitudes <= LH_Amp+ToleranceThreshold);

    if isempty(Candidates) % didn't find anything matching
        MatchedWaves_LH(LH_Wave_Index) = 0;
    else

        % randomly select a wave from the first hour that matches
        FH_Wave_Index = Candidates(randi(numel(Candidates)));
        FH_Amplitudes(FH_Wave_Index) = nan; % set to nan so it doesn't get selected again
        MatchedWaves_FH(FH_Wave_Index) = 1;

        MatchedWaves_LH(LH_Wave_Index) = 1;
    end
end

MatchedWaves_LH = logical(MatchedWaves_LH);