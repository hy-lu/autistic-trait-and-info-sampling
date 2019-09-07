folder = '../Data/';
lme_aicc = readtable([folder,'choice_rt_combined_model_aicc.csv']);
lme_bic = readtable([folder,'choice_rt_combined_model_bic.csv']); 

%% Using spm_BMS for all models
[alpha_aicc, ~, pxp_aicc, ~] = spm_BMS(-1/2.*lme_aicc{:,3:end});
table_aicc = array2table([pxp_aicc; alpha_aicc],'VariableNames',lme_aicc.Properties.VariableNames(3:end));
table_aicc.output = {'pxp';'alpha'};
writetable(table_aicc,[folder,'choice_rt_combined_model_aicc_output.csv'],'Delimiter',',','QuoteStrings',true);

[alpha_bic, ~, pxp_bic, ~] = spm_BMS(-1/2.*lme_bic{:,3:end});
table_bic = array2table([pxp_bic; alpha_bic],'VariableNames',lme_bic.Properties.VariableNames(3:end));
table_bic.output = {'pxp';'alpha'};
writetable(table_bic,[folder,'choice_rt_combined_model_bic_output.csv'],'Delimiter',',','QuoteStrings',true);

%% Using VBA toolbox
options.modelNames = lme_aicc.Properties.VariableNames(3:end);
[h_aicc, p_aicc] = VBA_groupBMC(-1/2.*lme_aicc{:,3:end}',options);
[h_bic, p_bic] = VBA_groupBMC(-1/2.*lme_bic{:,3:end}',options);
save([folder, 'choice_rt_groupBMC_aicc.mat'],'h_aicc','p_aicc');
save([folder, 'choice_rt_groupBMC_bic.mat'],'h_bic','p_bic');