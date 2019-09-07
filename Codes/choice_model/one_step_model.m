function model_est = one_step_model(data,col,stop,rep_times,options,model_name,par_name,parfor_progress,save_csv,send_mail,pushbullet)
%one_step_model - Automatic and foolproof modeling of one-step model
sub_id = unique(data.id);
n_id = length(sub_id);
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
npar_os = length(col);
lb = -Inf*ones(npar_os,1);
ub = Inf*ones(npar_os,1);
x = zeros(npar_os, rep_times, n_id);
fval = zeros(1, rep_times, n_id);
parfor i = 1:rep_times
    stream = RandStream.getGlobalStream();
    stream.Substream = i;
    for j = 1:n_id
        if parfor_progress
            fprintf('%s%s%s%s\n','Epoch ',num2str(i), ' of Sub ',num2str(sub_id(j)));
        end
        x0 = normrnd(0,5,npar_os,1);
        try
            [x(:,i,j),fval(:,i,j)] = one_step_nested(data{data.id==sub_id(j),:},col,stop,x0,[],[],[],[],lb,ub,[],options.default);
        catch
            warning('Current fmincon cannot continue. Using interior-point algorithm instead.');
            [x(:,i,j),fval(:,i,j)] = one_step_nested(data{data.id==sub_id(j),:},col,stop,x0,[],[],[],[],lb,ub,[],options.backup);
        end
    end
end
[~,I] = min(fval,[],2);
ind = sub2ind(size(fval),ones(size(fval,3),1),I(:),(1:size(fval,3))');
max_ll = reshape(-fval(ind),n_id,1);
x_reshaped = reshape(x, npar_os, []);
est_par = x_reshaped(:,ind)';
model_est = array2table([sub_id, est_par, max_ll],...
    'VariableNames',[{'id'},par_name,{'ll'}]);
model_est.model = repmat({model_name},n_id,1);
eval([model_name,'_est = model_est;']);
save(['mr_',model_name,'_',datestr(now,30),'.mat'],[model_name,'_est'])
if save_csv
    writetable(model_est,['mr_',model_name,'_',datestr(now,30),'.csv'],'Delimiter',',','QuoteStrings',true);
end
if send_mail
    tElapsed = toc(tStart);
    sendmail('hugh_lu@icloud.com','Fitting successfully completed!',[[model_name,': ',num2str(mean(model_est.ll))] 10 ...
        ['Task finished at: ', datestr(now,31)] 10 ...
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
