a = dlmread("sortselect_test_files/test1.txt");

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest

b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
insertselect = b(:,:,3);
heapselect   = b(:,:,4);
quickselectR = b(:,:,5);
bisection00  = b(:,:,6);
bisection10  = b(:,:,7);

set(0, 'DefaultLineLineWidth', 1);

h = figure(11)
loglog  (nbel,insertselect(:,1),'k', ...
         nbel,heapselect(:,1),  'g', ...
         nbel,quickselectR(:,1),'r', ...
         nbel,bisection00(:,1), 'b', ...
         nbel,bisection10(:,1), 'c')
axis([1 1e6 2e-9 2e-7]);
xlabel('N'); ylabel("time/N"); title("Uniform distribution");
legend('InsertionsSelect','HeapSelect','QuickSelect','BisectionSelect','BisectionSelect non-strict', ...
       'location','northeastoutside') 
print('sortselect_test_files/fig11.png','-dpng','-S700,400');

h = figure(12)
loglog  (nbel,heapselect(:,1),  'g', ...
         nbel,bisection00(:,1), 'b', ...
         nbel,bisection00(:,2), 'r', ...
         nbel,bisection00(:,3), 'm', ...
         nbel,bisection00(:,4), 'k', ...
         nbel,bisection00(:,5), 'm--')
axis([1 1e6 2e-9 2e-7]);
xlabel('N'); ylabel("time/N"); title("BisectionSelect strict vs distribution");
legend('(HeapSelect)','... Uniform','... Uniform**3','... Uniform**9','... Gaussian','... Uniform**9 bis', ...
       'location','northeastoutside') 
print('sortselect_test_files/fig12.png','-dpng','-S700,400');

h = figure(13)
loglog  (nbel,heapselect(:,1),  'g', ...
         nbel,bisection10(:,1), 'b', ...
         nbel,bisection10(:,2), 'r', ...
         nbel,bisection10(:,3), 'm', ...
         nbel,bisection10(:,4), 'k', ...
         nbel,bisection10(:,5), 'm--')
axis([1 1e6 2e-9 2e-7]);
xlabel('N'); ylabel("time/N"); title("BisectionSelect non-strict vs distribution");
legend('(HeapSelect)','... Uniform','... Uniform**3','... Uniform**9','... Gaussian','... Uniform**9 bis', ...
       'location','northeastoutside') 
print('sortselect_test_files/fig11.png','-dpng','-S700,400');





a = dlmread("sortselect_test_files/test2.txt");

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest

b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
quickselect = movmedian(b(:,:,3),31,1);
bisection00 = movmedian(b(:,:,4),31,1);
bisection10 = movmedian(b(:,:,5),31,1);
bisection01 = movmedian(b(:,:,6),31,1);
bisection11 = movmedian(b(:,:,7),31,1);

set(0, 'DefaultLineLineWidth', 1);

h = figure(21)
loglog  (nbel,bisection00(:,1),'b', ...
         nbel,bisection10(:,1),'r', ...
         nbel,bisection01(:,1),'g', ...
         nbel,bisection11(:,1),'m')
axis([1e3 1e9 2e-9 2e-8]);
xlabel('N'); ylabel("time/N"); title("Uniform distribution - Bisection Select");
legend('strict','non strict','strict + allow allocation','non strict + allow allocation') 
print('sortselect_test_files/fig11.png','-dpng','-S500,400');

h = figure(22)
loglog  (nbel,quickselect(:,1),'r', ...
         nbel,bisection00(:,1),'b')
axis([1e3 1e9 2e-9 2e-8]);
xlabel('N'); ylabel("time/N"); title("Uniform distribution");
legend('Quickselect','Bisection Select strict') 
print('sortselect_test_files/fig11.png','-dpng','-S500,400');

h = figure(23)
loglog  (nbel,bisection00(:,1),'b', ...
         nbel,bisection00(:,2),'r', ...
         nbel,bisection00(:,3),'m', ...
         nbel,bisection00(:,4),'k', ...
         nbel,bisection00(:,5),'m--')
axis([1e3 1e9 2e-9 2e-8]);
xlabel('N'); ylabel("time/N"); title("Bisection Select strict versus distribution");
legend('Uniform','Uniform**3','Uniform**9','Gaussian','uniform**9 bis', ...
       "location",'northeastoutside') 
print('sortselect_test_files/fig11.png','-dpng','-S700,400');

h = figure(24)
loglog  (nbel,quickselect(:,4),'r', ...
         nbel,bisection01(:,4),'b')
axis([1e3 1e9 2e-9 2e-8]);
xlabel('N'); ylabel("time/N"); title("Gaussian distribution");
legend('Quickselect','Bisection Select strict + allow allocation)') 
print('sortselect_test_files/fig11.png','-dpng','-S500,400');







a = dlmread("sortselect_test_files/test3.txt");

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest

b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
insertsort = b(:,:,3);
heapsort   = b(:,:,4);
quicksortR = b(:,:,5);
quicksortM = b(:,:,6);
quicksortT = b(:,:,7);

set(0, 'DefaultLineLineWidth', 1);

h = figure(31)
loglog  (nbel,insertsort(:,1),'k', ...
         nbel,heapsort(:,1),  'g', ...
         nbel,quicksortR(:,1),'r', ...
         nbel,quicksortM(:,1),'m', ...
         nbel,quicksortT(:,1),'c')
axis([1 1e6 1e-8 1e-6]);
xlabel('N'); ylabel("time/N"); title("Uniform distribution");
legend('InsertionsSort','HeapSort','QuickSort-R','QuickSort-M','QuickSort-T',"location",'southeast') 
print('sortselect_test_files/fig11.png','-dpng','-S500,400');





a = dlmread("sortselect_test_files/test4.txt");

ntest = sum(a(:,1) == 1)
np = size(a,1) / ntest

b = reshape(a,ntest,np,[]);

nbel = b(:,1,2);
insertsort = b(:,:,3);
heapsort   = b(:,:,4);
quicksortR = b(:,:,5);
quicksortM = b(:,:,6);
quicksortT = b(:,:,7);

set(0, 'DefaultLineLineWidth', 1);

h = figure(41)
loglog  (nbel,insertsort(:,1),'k', ...
         nbel,heapsort(:,1),  'g', ...
         nbel,quicksortR(:,1),'r', ...
         nbel,quicksortM(:,1),'m', ...
         nbel,quicksortT(:,1),'c')
axis([1e2 1e8 1e-8 1e-6]);
xlabel('N'); ylabel("time/N"); title("Uniform distribution");
legend('InsertionsSort','HeapSort','QuickSort-R','QuickSort-M','QuickSort-T',"location",'southeast') 
print('sortselect_test_files/fig11.png','-dpng','-S500,400');
