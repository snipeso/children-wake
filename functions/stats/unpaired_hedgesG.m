function Stats = unpaired_hedgesG(Data1, Data2, StatsP)

Dims1 = size(Data1);
Dims2 = size(Data2);

if numel(Dims1) == 3
    gValues = nan(Dims1(2), Dims1(3));
    CI = nan(Dims1(2), Dims1(3), 2);

    for Indx1 = 1:Dims1(2)
        for Indx2 = 1:Dims1(3)
            D1 = squeeze(Data1(:, Indx1, Indx2));
            D2 = squeeze(Data2(:, Indx1, Indx2));
            stats = mes(D2, D1, StatsP.Paired.ES, 'isDep', 0, 'nBoot', StatsP.ANOVA.nBoot);
            gValues(Indx1, Indx2) = stats.hedgesg;
            CI(Indx1, Indx2, :) = stats.hedgesgCi;
        end
    end

elseif numel(Dims1) == 2 && numel(Dims2) == 3 % D

    gValues = nan(Dims2(2), Dims2(3));
    CI = nan(Dims2(2), Dims2(3), 2);

    for Indx_S = 1:Dims2(2)
        for Indx_T = 1:Dims2(3)
            D = squeeze(Data2(:, Indx_S, Indx_T));
            BL = squeeze(Data1(:, Indx_T));

            if StatsP.ANOVA.nBoot < 100
                stats = mes(D, BL, StatsP.Paired.ES, 'isDep', 0);
            else
                stats = mes(D, BL, StatsP.Paired.ES, 'isDep', 0, 'nBoot', StatsP.ANOVA.nBoot);
            end
            gValues(Indx_S, Indx_T) = stats.hedgesg;
            CI(Indx_S, Indx_T, :) = stats.hedgesgCi;
        end
    end
elseif numel(Dims1) == 2

    gValues = nan(Dims1(2), 1);
    CI = nan(Dims1(2), 2);

    for Indx1 = 1:Dims1(2)
        D1 = squeeze(Data1(:, Indx1));
        D2 = squeeze(Data2(:, Indx1));
        stats = mes(D2, D1, StatsP.Paired.ES, 'isDep', 0, 'nBoot', StatsP.ANOVA.nBoot);
        gValues(Indx1) = stats.hedgesg;
        CI(Indx1, :) = stats.hedgesgCi;
    end
end


Stats.hedgesg = gValues;
Stats.hedgesgCI = CI;