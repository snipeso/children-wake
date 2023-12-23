function Stats = unpaired_ttest(Data1, Data2, StatsP)

% [~, p, CI, stats] = ttest2(Data1, Data2);
[~, p, CI, stats] = ttest2(Data2, Data1);
[Sig, crit_p, ~, adj_P] = fdr_bh(p, StatsP.Alpha, StatsP.ttest.dep); % NOTE: dep is good for ERPs, since data can be negatively correlated as well
t_values = stats.tstat';

Stats.t = t_values(:);
Stats.p = p(:);
Stats.p_fdr = adj_P;
Stats.crit_p = crit_p;
Stats.sig = Sig(:);
Stats.df = stats.df(:);
Stats.N = totN(Data1, Data2);
Stats.CI = CI';

Stats.mean1 = mean(Data1, 1, 'omitnan')';
Stats.std1 = std(Data1, 0, 1, 'omitnan')';
Stats.mean2 = mean(Data2, 1, 'omitnan')';
Stats.std2 = std(Data2, 0, 1, 'omitnan')';
% Stats.cohenD = cohen_d(Data1, Data2);
Stats.cohenD = cohen_d(Data2, Data1);



stats =  unpaired_hedgesG(Data1, Data2, StatsP);
Stats.(StatsP.Paired.ES) = stats.(StatsP.Paired.ES);
Stats.([StatsP.Paired.ES, 'CI']) = stats.([StatsP.Paired.ES, 'CI']);

end

function N = totN(Data1, Data2)
N = size(Data1, 1);
end