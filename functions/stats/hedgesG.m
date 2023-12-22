function Stats = hedgesG(Data1, Data2, StatsP)
% Data1 and Data2 are P x m x n matrices resulting in m x n stats matrices
% with Hedge's g m x n matrix and confidence intervals m x n x 2. If only
% Data1 is provided, then it should be a P x m matrix, and g values will be
% calculated for every pairwise comparison

Dims1 = size(Data1);
Dims2 = size(Data2);

if nargin == 2 % if only one data matrix is provided
    gValues = nan(Dims1(2));
    CI = nan(Dims1(2), Dims1(2), 2);
    for Indx1 = 1:Dims1(2)-1
        for Indx2 = Indx1+1:Dims1(2)
            D1 = squeeze(Data1(:, Indx1));
            D2 = squeeze(Data1(:, Indx2));

            if sum(nnz(not(isnan(D2)|isnan(D1)))) > 3
                stats = mes(D2, D1, Data2.Paired.ES, 'isDep', 1, 'nBoot', Data2.ANOVA.nBoot); % bit of a hack, there's probably a nicer way to do this
                gValues(Indx1, Indx2) = stats.hedgesg;
                CI(Indx1, Indx2, :) = stats.hedgesgCi;

            else % if too few datapoints
                gValues(Indx1, Indx2) = nan;
                CI(Indx1, Indx2, :) = nan;
            end
        end
    end



elseif nargin == 3 % if two matrices are provided

    if numel(Dims1) == 3
        gValues = nan(Dims1(2), Dims1(3));
        CI = nan(Dims1(2), Dims1(3), 2);

        for Indx1 = 1:Dims1(2)
            for Indx2 = 1:Dims1(3)
                D1 = squeeze(Data1(:, Indx1, Indx2));
                D2 = squeeze(Data2(:, Indx1, Indx2));
                stats = mes(D2, D1, StatsP.Paired.ES, 'isDep', 1, 'nBoot', StatsP.ANOVA.nBoot);
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
                    stats = mes(D, BL, StatsP.Paired.ES, 'isDep', 1);
                else
                    stats = mes(D, BL, StatsP.Paired.ES, 'isDep', 1, 'nBoot', StatsP.ANOVA.nBoot);
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
            if nnz(~isnan(D1)) <2 || nnz(~isnan(D2)) <2
                gValues(Indx1) = nan;
                CI(Indx1, :) = nan;
            else
                stats = mes(D2, D1, StatsP.Paired.ES, 'isDep', 1, 'nBoot', StatsP.ANOVA.nBoot);
                gValues(Indx1) = stats.hedgesg;
                CI(Indx1, :) = stats.hedgesgCi;
            end

        end
    end
else
    error('Too few inputs')
end

Stats.hedgesg = gValues;
Stats.hedgesgCI = CI;