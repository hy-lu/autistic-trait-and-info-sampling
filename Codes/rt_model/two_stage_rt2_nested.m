function [x,fval,exitflag,output] = two_stage_rt2_nested(dat,x0,A,b,Aeq,beq,lb,ub,nonlcon,options)
[x,fval,exitflag,output] = fmincon(@two_stage_rt2_fn,x0,A,b,Aeq,beq,lb,ub,nonlcon,options);
    function obj = two_stage_rt2_fn(pars)
        % Data should have at least have 6 columns, id, lg_rt, p_s1, p_c1_over_c,
        % p_s2, p_c2_over_c.
        % There are at least 8 free parameters for each participants, a1, b1, gm1,
        % sig1, a2, b2, gm2, sig2.

        a1 = pars(1);
        b1 = pars(2);
        gm1 = pars(3);
        sig1 = pars(4);
        a2 = pars(5);
        b2 = pars(6);
        gm2 = pars(7);
        sig2 = pars(8);
        dt1 = a1.*dat.p_s1 + b1.*dat.p_s1.*(1-dat.p_s1) + gm1;
        dt2 = log(exp(a1.*(1-dat.p_s1) + b1.*dat.p_s1.*(1-dat.p_s1) + gm1) + ...
            exp(a2.*dat.p_s2 + b2.*dat.p_s2.*(1-dat.p_s2) + gm2));
        mixed_pdf = dat.p_c1_over_c .* normpdf(dat.lg_rt, dt1, sig1) + ...
            dat.p_c2_over_c .* normpdf(dat.lg_rt, dt2, sig2);
        mixed_pdf(mixed_pdf == 0) = 1e-16;
        ll = log(mixed_pdf);
        obj = -sum(ll);

    end
end
