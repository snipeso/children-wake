Visnum = nan([1 size(artndxn, 2)]);
 Visnum(vissymb=='0') = 0;
 Visnum(vissymb=='1') = 1;
 Visnum(vissymb=='2') = 2;
 Visnum(vissymb=='3') = 3;
 Visnum(vissymb=='r') = 4;

 Freqs = 0:0.25:40;
close all
N3 = Visnum==0;
figure('Units','normalized', 'Position',[ 0.0417    0.1421    0.1745    0.4546]);
plot(Freqs, squeeze(mean(ffttot(:, :, N3), 1)), 'Color', [.5 .5 .5 .3]);set(gca, 'YScale', 'log');xlim([1 40]); title('w')
N3 = Visnum==1;
figure('Units','normalized', 'Position',[ 0.0417    0.1421    0.1745    0.4546]);
plot(Freqs, squeeze(mean(ffttot(:, :, N3), 1)), 'Color', [.5 .5 .5 .3]);set(gca, 'YScale', 'log');xlim([1 40]); title('1')
N3 = Visnum==2;
figure('Units','normalized', 'Position',[ 0.0417    0.1421    0.1745    0.4546]);
plot(Freqs, squeeze(mean(ffttot(:, :, N3), 1)), 'Color', [.5 .5 .5 .3]);set(gca, 'YScale', 'log');xlim([1 40]); title('2')
N3 = Visnum==3;
figure('Units','normalized', 'Position',[ 0.0417    0.1421    0.1745    0.4546]);

plot(Freqs, squeeze(mean(ffttot(:, :, N3), 1)), 'Color', [.5 .5 .5 .3]);set(gca, 'YScale', 'log');xlim([1 40]); title('3')
N3 = Visnum==4;
figure('Units','normalized', 'Position',[ 0.0417    0.1421    0.1745    0.4546]);

plot(Freqs, squeeze(mean(ffttot(:, :, N3), 1)), 'Color', [.5 .5 .5 .3]);set(gca, 'YScale', 'log');xlim([1 40]); title('r')