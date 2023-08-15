for slowfast = 1:2




if slowfast == 2 
  a = dlmread("sums_bench_files/bench_fast_2.txt");
  ftitle = ", fast-math";
  fsuffix = "_fast";
  ff = 10;
else
  a = dlmread("sums_bench_files/bench_2.txt");
  ftitle = "";
  fsuffix = "";
  ff = 0;
endif

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest
b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
psum1000_raw = b(:,:,9);

b = sqrt(movmean(b.^2,51));

if slowfast == 2
  bfast = b;
else
  bslow = b;
endif

sumi = b(:,:,3);
sum0 = b(:,:,4);
sum1 = b(:,:,5);
psum = b(:,:,6);
psum10 = b(:,:,7);
psum100 = b(:,:,8);
psum1000 = b(:,:,9);
ksum = b(:,:,10);
ksum10 = b(:,:,11);
ksum100 = b(:,:,12);
ksum1000 = b(:,:,13);

set(0, 'DefaultLineLineWidth', 1);


h = figure(10+ff)
semilogx  (nbel,psum1000_raw(:,1),'ok','markersize',3, ...
           nbel,psum1000(:,1),'r', ...
           nbel,-psum1000(:,1),'-.r')
axis([1 1e9 -20 20]);
xlabel('N'); ylabel("error"); %title("xxx");
legend('raw error','smooth RMS', ...
       'location','northwest') 
print(['sums_bench_files/fig10' fsuffix '.png'],'-dpng','-S700,400');

h = figure(11+ff)
semilogx  (nbel,sumi(:,1),'k--','linewidth',3, ...
           nbel,sum0(:,1),'c', ...
           nbel,sum1(:,1),'g', ...
           nbel,psum(:,1),'r', ...
           nbel,ksum(:,1),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+/- distribution" ftitle]);
legend('sumi','sum sp','sum dp','psum','ksum', ...
       'location','northwest') 
print(['sums_bench_files/fig11' fsuffix '.png'],'-dpng','-S700,400');


h = figure(12+ff)
semilogx  (nbel,psum(:,1),'k','linewidth',2, ...
           nbel,psum10(:,1),'r', ...
           nbel,psum100(:,1),'g', ...
           nbel,psum1000(:,1),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+/- distribution" ftitle]);
legend('psum','psum 10','psum 100','psum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig12' fsuffix '.png'],'-dpng','-S700,400');


h = figure(13+ff)
semilogx  (nbel,ksum(:,1),'k','linewidth',2, ...
           nbel,ksum10(:,1),'r', ...
           nbel,ksum100(:,1),'g', ...
           nbel,ksum1000(:,1),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+/- distribution" ftitle]);
legend('ksum','ksum 10','ksum 100','ksum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig13' fsuffix '.png'],'-dpng','-S700,400');



h = figure(14+ff)
semilogx  (nbel,sumi(:,2),'k--','linewidth',3, ...
           nbel,sum0(:,2),'c', ...
           nbel,sum1(:,2),'g', ...
           nbel,psum(:,2),'r', ...
           nbel,ksum(:,2),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+++ distribution" ftitle]);
legend('sumi','sum sp','sum dp','psum','ksum', ...
       'location','northwest') 
print(['sums_bench_files/fig14' fsuffix '.png'],'-dpng','-S700,400');


h = figure(15+ff)
semilogx  (nbel,psum(:,2),'k','linewidth',2, ...
           nbel,psum10(:,2),'r', ...
           nbel,psum100(:,2),'g', ...
           nbel,psum1000(:,2),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+++ distribution" ftitle]);
legend('psum','psum 10','psum 100','psum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig15' fsuffix '.png'],'-dpng','-S700,400');


h = figure(16+ff)
semilogx  (nbel,ksum(:,2),'k','linewidth',2, ...
           nbel,ksum10(:,2),'r', ...
           nbel,ksum100(:,2),'g', ...
           nbel,ksum1000(:,2),'b')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title(["+++ distribution" ftitle]);
legend('ksum','ksum 10','ksum 100','ksum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig16' fsuffix '.png'],'-dpng','-S700,400');



h = figure(17+ff)
semilogx  (nbel,sumi(:,3),'k--','linewidth',3, ...
           nbel,sum0(:,3),'c', ...
           nbel,sum1(:,3),'g', ...
           nbel,psum(:,3),'r', ...
           nbel,ksum(:,3),'b')
axis([1 1e9 0 1000]);
xlabel('N'); ylabel("RMS(err2)"); title(["+0- distribution" ftitle]);
legend('sumi','sum sp','sum dp','psum','ksum', ...
       'location','northwest') 
print(['sums_bench_files/fig17' fsuffix '.png'],'-dpng','-S700,400');


h = figure(18+ff)
semilogx  (nbel,psum(:,3),'k','linewidth',2, ...
           nbel,psum10(:,3),'r', ...
           nbel,psum100(:,3),'g', ...
           nbel,psum1000(:,3),'b')
axis([1 1e9 0 1000]);
xlabel('N'); ylabel("RMS(err2)"); title(["+0- distribution" ftitle]);
legend('psum','psum 10','psum 100','psum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig18' fsuffix '.png'],'-dpng','-S700,400');


h = figure(19+ff)
semilogx  (nbel,ksum(:,3),'k','linewidth',2, ...
           nbel,ksum10(:,3),'r', ...
           nbel,ksum100(:,3),'g', ...
           nbel,ksum1000(:,3),'b')
axis([1 1e9 0 1000]);
xlabel('N'); ylabel("RMS(err2)"); title(["+0- distribution" ftitle]);
legend('ksum','ksum 10','ksum 100','ksum 1000', ...
       'location','northwest') 
print(['sums_bench_files/fig19' fsuffix '.png'],'-dpng','-S700,400');


endfor




sumi = bslow(:,:,3);
sum0     = bslow(:,:,4);
psum1000 = bslow(:,:,9);
sumifast = bfast(:,:,3);
sum0fast = bfast(:,:,4);
psum1000fast = bfast(:,:,9);

figure(31)
semilogx  (nbel,sum0(:,1)        ,'k--', ...
           nbel,sum0fast(:,1)    ,'k', ...
           nbel,psum1000(:,1)    ,'r--', ...
           nbel,psum1000fast(:,1),'r')
axis([1 1e9 0 10]);
xlabel('N'); ylabel("RMS(err2)"); title("+/- distribution");
legend('sum sp','sum sp fast','psum 1000','psum 1000 fast', ...
       'location','northeast') 
print(['sums_bench_files/fig31.png'],'-dpng','-S700,400');
