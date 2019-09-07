function [x,fval,exitflag,output] = one_step_nested(dat,col,stop,x0,A,b,Aeq,beq,lb,ub,nonlcon,options)
[x,fval,exitflag,output] = fmincon(@one_step_fn,x0,A,b,Aeq,beq,lb,ub,nonlcon,options);
    function [obj, grad] = one_step_fn(pars)
        p = logist(dat(:,col) * pars);
        p(dat(:,stop) ~= 1) = 1 - p(dat(:,stop) ~= 1);
        p(p == 1) = 1 - (1e-16);
        p(p == 0) = 1e-16;
        ll = log(p);
        obj = -sum(ll);
        if nargout > 1
            X = dat(:,col) * pars;
            gr = zeros(size(dat, 1), length(pars));
            temp_ns = repmat(-exp(X)./(1+exp(X)),1,length(pars)).*dat(:,col);
            temp_ns(isnan(temp_ns)) = -1;
            temp_s = repmat(1./(1 + exp(X)),1,length(pars)).*dat(:,col);
            temp_s(isnan(temp_s)) = 0;
            gr(dat(:,stop) ~= 1, :) = temp_ns(dat(:,stop) ~= 1,:);
            gr(dat(:,stop) == 1, :) = temp_s(dat(:,stop) == 1,:);
            grad = -sum(gr, 1)';
        end
    end
end
