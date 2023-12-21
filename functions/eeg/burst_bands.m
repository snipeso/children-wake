function Bursts = burst_bands(Bursts, Bands)

Frequencies = [Bursts.BurstFrequency];
FrequencyCategories = zeros(size(Frequencies));

BandLabels = fieldnames(Bands);
for BandIdx = 1:numel(BandLabels)
    Band = Bands.(BandLabels{BandIdx});
    Indexes = Frequencies >= Band(1) & Frequencies <= Band(2);
    FrequencyCategories(Indexes) = BandIdx;
end

for BurstIdx = 1:numel(Bursts)
    Bursts(BurstIdx).NewBand = FrequencyCategories(BurstIdx);
end
