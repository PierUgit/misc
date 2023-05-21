isFAST = false;



if isFAST 
  a = dlmread("sums_bench_files/bench_fast_2.txt");
  ftitle = ", fast-math";
  fsuffix = "_fast";
else
  a = dlmread("sums_bench_files/bench_2.txt");
  ftitle = ""
  fsuffix = "";
endif

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest

b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
sumi = movmean(b(:,:,3).^2,33).^0.5;
sum0 = movmean(b(:,:,4).^2,33).^0.5;
sum1 = movmean(b(:,:,5).^2,33).^0.5;
psum = movmean(b(:,:,6).^2,33).^0.5;
psum10 = movmean(b(:,:,7).^2,33).^0.5;
psum100 = movmean(b(:,:,8).^2,33).^0.5;
psum1000 = movmean(b(:,:,9).^2,33,1).^0.5;
ksum = movmean(b(:,:,10).^2,33).^0.5;
ksum10 = movmean(b(:,:,11).^2,33).^0.5;
ksum100 = movmean(b(:,:,12).^2,33).^0.5;
ksum1000 = movmean(b(:,:,13).^2,33).^0.5;

set(0, 'DefaultLineLineWidth', 1);


h = figure(10)
semilogx  (nbel,b(:,1,9),'k', ...
           nbel,psum1000(:,1),'r')
axis([1 1e9 -20 20]);
xlabel('N'); ylabel("error"); %title("xxx");
legend('psum 1000 (raw)','psum 1000 (smooth RMS)', ...
       'location','northwest') 
print(['sums_bench_files/fig10' fsuffix '.png'],'-dpng','-S700,400');

h = figure(11)
semilogx  (nbel,sumi(:,1),'k', ...
           nbel,sum0(:,1),'c', ...
           nbel,sum1(:,1),'g', ...
           nbel,psum(:,1),'r', ...
           nbel,ksum(:,1),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+/- distribution" ftitle]);
legend('sumi','sum sp','sum dp','psum','ksum', ...
       'location','northwest') 
print(['sums_bench_files/fig11' fsuffix '.png'],'-dpng','-S700,400');


h = figure(12)
semilogx  (nbel,psum(:,1),'k', ...
           nbel,psum10(:,1),'b', ...
           nbel,psum100(:,1),'g', ...
           nbel,psum1000(:,1),'r')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+/- distribution" ftitle]);
legend('psum','psum 10','psum 100','psum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig12' fsuffix '.png'],'-dpng','-S700,400');


h = figure(13)
semilogx  (nbel,ksum(:,1),'k', ...
           nbel,ksum10(:,1),'b', ...
           nbel,ksum100(:,1),'g', ...
           nbel,ksum1000(:,1),'r')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+/- distribution" ftitle]);
legend('ksum','ksum 10','ksum 100','ksum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig13' fsuffix '.png'],'-dpng','-S700,400');



h = figure(14)
semilogx  (nbel,sumi(:,2),'k', ...
           nbel,sum0(:,2),'c', ...
           nbel,sum1(:,2),'g', ...
           nbel,psum(:,2),'r', ...
           nbel,ksum(:,2),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+++ distribution" ftitle]);
legend('sumi','sum sp','sum dp','psum','ksum', ...
       'location','northwest') 
print(['sums_bench_files/fig14' fsuffix '.png'],'-dpng','-S700,400');


h = figure(15)
semilogx  (nbel,psum(:,2),'k', ...
           nbel,psum10(:,2),'b', ...
           nbel,psum100(:,2),'g', ...
           nbel,psum1000(:,2),'r')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+++ distribution" ftitle]);
legend('psum','psum 10','psum 100','psum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig15' fsuffix '.png'],'-dpng','-S700,400');


h = figure(16)
semilogx  (nbel,ksum(:,2),'k', ...
           nbel,ksum10(:,2),'b', ...
           nbel,ksum100(:,2),'g', ...
           nbel,ksum1000(:,2),'r')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("Err2"); title(["+++ distribution" ftitle]);
legend('ksum','ksum 10','ksum 100','ksum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig16' fsuffix '.png'],'-dpng','-S700,400');
