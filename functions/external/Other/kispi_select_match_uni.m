function [p_ttest,p_ranksum,p_ks,matched_waves_firsthour, matched_waves_lasthour]=kispi_select_match_uni(waves_firsthour,waves_lasthour)
% from Valeria Jaramillo, 2020
%%%% test
%  waves_firsthour = wavesFH_freqamp;
%  waves_lasthour = wavesLH_freqamp;

d = 1;

rng shuffle

amp_matched_all_ndx =[];
m = 1;

matched_waves_firsthour = NaN(size(waves_lasthour,1),11);
matched_waves_lasthour = NaN(size(waves_lasthour,1),11);

for w = 1:size(waves_lasthour,1)
    %disp(num2str(w))

    amp_tomatch = waves_lasthour(w,7);
    amp_close_ndx = find(waves_firsthour(:,7)> amp_tomatch-1 & waves_firsthour(:,7)< amp_tomatch+1);
    r = rand;
    amp_r = amp_tomatch - d + 2*d*r;

    while m == 1
        if isempty(amp_close_ndx)
            matched_waves_firsthour(w,:) = NaN;
            matched_waves_lasthour(w,:) = NaN;
            m = 2;
        else

            [amp_dist amp_close_matched_ndx] = min(abs(waves_firsthour(amp_close_ndx,7) - amp_r));
            amp_matched_ndx = amp_close_ndx(amp_close_matched_ndx);
            if ismember(amp_matched_ndx,amp_matched_all_ndx)
                amp_close_ndx(amp_close_matched_ndx)=[];
            else
                matched_waves_firsthour(w,:) = waves_firsthour(amp_matched_ndx,:);
                matched_waves_lasthour(w,:) = waves_lasthour(w,:);
                amp_matched_all_ndx = [amp_matched_all_ndx amp_matched_ndx];
                m = 2;
            end
        end
    end

    m = 1;
    clear amp_dist amp_matched_ndx
end

matched_waves_firsthour_nonans = matched_waves_firsthour(~isnan(matched_waves_firsthour(:,10)),:);


if ~isempty(matched_waves_firsthour_nonans)
    [h p_ttest]=ttest2(matched_waves_firsthour(:,7),matched_waves_lasthour(:,7));
    p_ranksum=ranksum(matched_waves_firsthour(:,7),matched_waves_lasthour(:,7));
    [h,p_ks] = kstest2(matched_waves_firsthour(:,7),matched_waves_lasthour(:,7));
else
    p_ttest = NaN;
    p_ranksum = NaN;
    p_ks = NaN;
end

end





