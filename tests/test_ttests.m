clc
close all


StatParameters = struct();

StatParameters.ANOVA.ES = 'eta2';
StatParameters.ANOVA.nBoot = 2000;
StatParameters.ANOVA.pValue = 'pValueGG';
StatParameters.ttest.nBoot = 2000;
StatParameters.ttest.dep = 'pdep'; % use 'dep' for ERPs, pdep for power
StatParameters.Alpha = .05;
StatParameters.Trend = .1;
StatParameters.Paired.ES = 'hedgesg'; 

A = randn(2000, 1);
B = A+1;

% both paired and unpaired should show B > A
 StatsUnpaired = unpaired_ttest(A, B, StatParameters);
 disp(['Unpaired t: ', num2str(StatsUnpaired.t), '; cohend: ', num2str(StatsUnpaired.cohenD), '; hedges g: ', num2str(StatsUnpaired.hedgesg)])

  StatsPaired = paired_ttest(A, B, StatParameters);
   disp(['Paired t: ', num2str(StatsPaired.t), '; cohend: ', num2str(StatsPaired.cohenD), '; hedges g: ', num2str(StatsPaired.hedgesg)])
