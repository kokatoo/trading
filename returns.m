function returns()
    compound30Years();
end

function compound30Years()
    mu = .085;
    sigma = .15;
    initial = 1000;
    
    rets = normrnd(mu, sigma, 100, 30);
    finalReturns = initial * prod(1+rets, 2);

    hist(finalReturns, 20)
    title('30-Year Compounding');
    xlabel('30-Year Value');
    ylabel('Counts');
    
    fprintf('\nInitial amount: %d', initial);
    fprintf('\nMean return: %.2f', mu);
    fprintf('\nStd Dev: %.2f', sigma);
    fprintf('\nFinal Mean: %.2f', mean(finalReturns));
    fprintf('\nFinal Std Dev: %.2f', mean(finalReturns));
    fprintf('\n\n');
end