function returns()
    compound30Years();
end

function compound30Years()
    mu = .085;
    sigma = .15;
    initial = 1000;
    
    rets = normrnd(mu, sigma, 100, 30);
    values = initial * prod(1+rets, 2);

    hist(values, 20)
    title('30-Year Compounding');
    xlabel('30-Year Value');
    ylabel('Counts');
    
    fprintf('\nInitial capital: %d', initial);
    fprintf('\nMean return: %.2f', mu);
    fprintf('\nStd Dev: %.2f', sigma);
    fprintf('\n30-Year Value Mean: %.2f', mean(values));
    fprintf('\n30-Year Value Std Dev: %.2f', mean(values));
    fprintf('\n\n');
end