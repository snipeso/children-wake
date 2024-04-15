function [HistogramAmplitude, HistogramQuantities] = assemble_burst_distributions(Bursts, Frequencies, MaxTimepoints)

nFrequencies = numel(Frequencies)-1;
DiscreteFrequencies = discretize([Bursts.BurstFrequency], Frequencies);
 HistogramAmplitude = nan(1, numel(Frequencies)-1);
 HistogramQuantities = HistogramAmplitude;
for FreqIdx = 1:nFrequencies
    BurstTemp = Bursts(DiscreteFrequencies==FreqIdx);
        HistogramQuantities(FreqIdx) = 100*sum([BurstTemp.DurationPoints])/MaxTimepoints;
            if numel(BurstTemp)<10
                continue
            end
    HistogramAmplitude(FreqIdx) = mean([BurstTemp.Amplitude]);
end