function obj = two_step_flex_decay(dat,scale_array,os_col,ts_col,stop,ratio,cum_info,cum_info_val,info,draw,pars)
dat_copy = dat;
col = [os_col, ts_col];
if length(col) ~= length(pars) - 3
    error('The number of parameters in the decay should be one more than the number of columns entered.');
end
alpha = pars(length(col)+3);
decay_info = zeros(size(dat_copy(:,cum_info)));
for j = 1:size(dat_copy,1)
    if dat_copy(j,draw)==0
        decay_info(j) = dat_copy(j,info);
    else
        decay_info(j) = decay_info(j-1).*alpha + dat_copy(j,info);
    end
end
dat_copy(:,cum_info) = abs(decay_info);
dat_copy(:,cum_info_val) = dat_copy(:,cum_info).*dat_copy(:,ratio);
if ~isempty(scale_array)
    scale_data.max = repmat(scale_array.max,size(dat_copy,1),1);
    scale_data.min = repmat(scale_array.min,size(dat_copy,1),1);
    ind = col(scale_array.max(:,col) ~= scale_array.min(:,col));
    dat_copy(:,ind) = (dat_copy(:,ind)-scale_data.min(:,ind))./(scale_data.max(:,ind)-scale_data.min(:,ind));
end
k = pars(length(col) + 1);
b0 = pars(length(col) + 2);
p_s1 = logist(dat_copy(:,os_col) * pars(1:length(os_col)));
% ps = logist(k .* p_s1 + b0);
ps = logist(k .* (dat_copy(:,os_col) * pars(1:length(os_col))) + b0);
p_s2 = logist(dat_copy(:,ts_col) * pars((1:length(ts_col)) + length(os_col)));
p = p_s1 + (1 - p_s1) .* ps .* p_s2;
p(dat_copy(:,stop) ~= 1) = 1 - p(dat_copy(:,stop) ~= 1);
p(p == 1) = 1 - (1e-16);
p(p == 0) = 1e-16;
ll = log(p);
obj = -sum(ll);
end
