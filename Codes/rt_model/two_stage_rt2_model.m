function model_est = two_stage_rt2_model(data,rep_times,options,model_name,par_name,parfor_progress,save_csv,send_mail,pushbullet)
%two_step_model - Automatic and foolproof modeling of two-step model
id = unique(data.id);
nid = length(id);
fprintf([model_name,' starts at: ',datestr(now,31),'\r\n'])
if send_mail
    sendmail('hugh_lu@icloud.com',[model_name, ' starts'],['Task started at: ',datestr(now,31)]);
    tStart = tic;
end
if ~isempty(pushbullet)
    tic;
    pushbullet.pushNote([],[model_name, ' starts'],['Task started at: ',datestr(now,31)]);
end
spmd
    rng(1234,'combRecursive');
end
npar = 8;
lb = [-Inf -Inf 0 0 -Inf -Inf 0 0];
ub = [Inf Inf Inf Inf Inf Inf Inf Inf];
x = zeros(npar, rep_times, nid);
fval = zeros(1, rep_times, nid);
parfor i = 1:rep_times
    stream = RandStream.getGlobalStream();
    stream.Substream = i;
    for j = 1:nid
        if parfor_progress
            fprintf('%s%s%s%s\n','Epoch ',num2str(i), ' of Sub ',num2str(j));
        end
        x0 = [normrnd(0,10,2,1); abs(normrnd(0,10,2,1)); normrnd(0,10,2,1); abs(normrnd(0,10,2,1))];
        [x(:,i,j),fval(:,i,j)] = two_stage_rt2_nested(data(data.id == id(j),:),x0,[],[],[],[],lb,ub,[],options);
    end
end
[~,I] = min(fval,[],2);
ind = sub2ind(size(fval),ones(size(fval,3),1),I(:),(1:size(fval,3))');
max_ll = reshape(-fval(ind),nid,1);
x_reshaped = reshape(x, npar, []);
est_par = x_reshaped(:,ind)';
% max_ll = -fval(sub2ind(size(fval),ones(size(fval,3),1),I(:),(1:size(fval,3))'));
% est_par = reshape(x(sub2ind(size(x),repmat((1:size(x,1))',size(fval,3),1),repelem(I(:), size(x, 1)), repelem((1:size(fval,3))', size(x,1)))),size(x,1), size(x,3))';
model_est = array2table([id, est_par, max_ll],...
    'VariableNames',[{'id'},par_name,{'ll'}]);
model_est.model = repmat({model_name},nid,1);
eval([model_name,'_est = model_est;']);
save(['mr_',model_name,'_',datestr(now,30),'.mat'],[model_name,'_est'])
if save_csv
    writetable(model_est,['mr_',model_name,'_',datestr(now,30),'.csv'],'Delimiter',',','QuoteStrings',true);
end
if send_mail
    tElapsed = toc(tStart);
    sendmail('hugh_lu@icloud.com','Fitting successfully completed!',[[model_name,': ',num2str(mean(model_est.ll))] 10 ...
        ['Task finished at ', datestr(now,31)] 10 ...
        ['Time Elapsed: ', num2str(tElapsed/60), ' min']]);
end
if ~isempty(pushbullet)
    try
        pushbullet.pushNote([],'Fitting successfully completed!',[[model_name,': ',num2str(mean(model_est.ll))] 10 ...
            ['Task finished at: ', datestr(now,31)] 10 ...
            ['Time Elapsed: ', num2str(toc/60), ' min']]);
    catch
        if ~send_mail
            sendmail('hugh_lu@icloud.com','Fitting successfully completed!',[[model_name,': ',num2str(mean(model_est.ll))] 10 ...
                ['Task finished at: ', datestr(now,31)] 10 ...
                ['Time Elapsed: ', num2str(toc/60), ' min']]);
        end
    end
end
fprintf([model_name,' ends at: ',datestr(now,31),'\r\n'])
end
