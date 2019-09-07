function [x,fval,exitflag,output] = one_stage_rt_nested(dat,x0,A,b,Aeq,beq,lb,ub,nonlcon,options)
[x,fval,exitflag,output] = fmincon(@one_stage_rt_fn,x0,A,b,Aeq,beq,lb,ub,nonlcon,options);
    function obj = one_stage_rt_fn(pars)
        % Data should have at least have 3 columns, id, lg_rt, p_s1.
        % There are at least 4 free parameters for each participants, a1, b1, gm1,
        % sig1.
        
        a1 = pars(1);
        b1 = pars(2);
        gm1 = pars(3);
        sig1 = pars(4);
        dt1 = a1.*dat.p_s1 + b1.*dat.p_s1.*(1-dat.p_s1) + gm1;
        p = normpdf(dat.lg_rt, dt1, sig1);
        p(p == 0) = 1e-16;
        ll = log(p);
        obj = -sum(ll);
    end
end
