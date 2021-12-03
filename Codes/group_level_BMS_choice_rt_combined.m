folder = '../Data/';
lme_aicc = readtable([folder,'choice_rt_combined_model_aicc.csv']);
lme_bic = readtable([folder,'choice_rt_combined_model_bic.csv']); 

%% Using spm_BMS for all models
[alpha_aicc, exp_r_aicc,xp_aicc, pxp_aicc, bor_aicc] = spm_BMS(-1/2.*lme_aicc{:,3:end});
table_aicc = array2table([pxp_aicc; alpha_aicc],'VariableNames',lme_aicc.Properties.VariableNames(3:end));
table_aicc.output = {'pxp';'alpha'};
writetable(table_aicc,[folder,'choice_rt_combined_model_aicc_output.csv'],'Delimiter',',','QuoteStrings',true);

[alpha_bic, exp_r_bic, xp_bic, pxp_bic, bor_bic] = spm_BMS(-1/2.*lme_bic{:,3:end});
table_bic = array2table([pxp_bic; alpha_bic],'VariableNames',lme_bic.Properties.VariableNames(3:end));
table_bic.output = {'pxp';'alpha'};
writetable(table_bic,[folder,'choice_rt_combined_model_bic_output.csv'],'Delimiter',',','QuoteStrings',true);