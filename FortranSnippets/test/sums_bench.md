Benchmark of 1D floating point summation methods
================================================

Preamble
--------

We benchmark the runtimes and accuracies of several methods of the summation of N single precision floating point values $S(n)=\sum_{i=1..N}{x_i}$:
- **sumi**: intrinsic Fortran `sum()` function (using the gfortran compiler)
- **sum_**: straight summation: simple do loop with an accumulator. The accumulator can be:
  - **sum_sp**: single precision accumulator
  - **sum_dp**: double precision accumulator
  - **sum_qp**: quadruple precision accumulator
- **psum**: [pairwise summation](https://en.wikipedia.org/wiki/Pairwise_summation). 
  - **psum_k**: variation where the intrinsic sum is used once $k$ elements or less are left in the recursion ($k=2$ for the default pairwise summation)
- **ksum**: [Kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm). 
  - **ksum_k**: variation where the different steps of the summation are performed on chunks of $k$ elements, with the $k$ results classicaly summed at the end.

The tests are performed on a x86 machine (an old Core i5 2500K from 2011), with the gfortran 12 compiler. All floating point types are IEEE-754. The single and double are native, and the quadruple is software emulated.  

Accuracy analysis
-----------------

The maximum absolute error of the sum of N elements, $S(n)=\sum_i{x_i}$ is bounded by $Err0_max(n)=\frac{\epsilon f(n)}{1-\epsilon f(n)}\sum_i{|x_i|}$, where $\epsilon$ is the machine precision (about ~$10^{-7}$ for IEEE-754 single precision), and $f(n)$ a function that depends on the summation method:
|                  | $f(n)$            | Notes |
|------------------|-------------------|-------|
| straight         | $n$               | $\epsilon$ is the one of the accumulator! |
| pairwise         | $\log_2{n}$       |       |
| Kahan            | $1$               |       |

Note the denominator that can lead to catastrophic errors when $\epsilon f(n)$ is no longer negligible wrt $1$ (say above $0.1$). In single precision it means as soon as $n$ is larger than $10^6$, and in double precision larger than $10^{14}$. The classical example is the straight summation of $x_i=1.0$ in single precision: the actual sum is equal to N for $N<=2^{24}=16777216$ (24 being the number of bits in the mantissa of a IEEE754 single precision floating point), and to 16777216 whatever $N>2^{24}$, since $2^{24}+1.0=2^{24}$.

**In the rest of the document we assume that we are in the case where the denominator can be neglected**: $Err0_{max}(n)=1-\epsilon f(n)<<1$

The maximum absolute error is then $Err0_{max}(n)=\epsilon f(n) \sum_i{|x_i|}$. This worst case happens when all the rounding errors have the same sign. In practice they behave as a random walk, with signs that can be either positive or negative. The error has then a normal distribution, and we can take the standard deviation as the average error:

$Err0(n)=\epsilon \sqrt{f(n)/3} \sum_i{|x_i|}$

What is often considered instead of the absolute error is the relative error:

$Err1(n)=\frac{Err0(n)}{S(n)}=\epsilon \sqrt{f(n)/3} . c$ 

Where $c=\frac{sum_i{|x_i|}}{\sum_i{x_i}}$ is the [condition number of the summation](https://en.wikipedia.org/wiki/Pairwise_summation#Accuracy), which is independent from the summation method.

And finally it can also be meaningful to express the error in terms of spacing between two floating points at the value of the sum. The spacing can be roughtly approximated by $\delta(x) \approx \epsilon.x$.

$Err2(n)=e.\frac{Err0(n)}{\delta(S(n))}=\sqrt{f(n)/3}.c$

$e=1$, except for straight summation with higher precision summations where $e=\frac{\epsilon}{\epsilon_{sp}}$

When taking into account all the flavors, we get the following table:
|                  | $e$         | $f(n)$       | $Err2(n)$ |
|------------------|-------------|--------------|-----------|
| sumi             |?            | ?            | ?         |
| sum_sp           |$1$          | $n$          | $\sqrt{n/3} . c$ |
| sum_dp           |$~2.10^{-9}$ | $n$          | $2.10^{-9}\sqrt{n/3} . c$ |
| sum_qp           |$~2.10^{-27}$| $n$          |$2.10^{-27}\sqrt{n/3} . c$ |
| psum             |$1$          | $\log_2{n}$  | $\sqrt{\log_2{n}/3} . c$ |
| psum_k           |$1$          | $\log_2{n}+k$| $\sqrt{(\log_2{n}+k)/3} . c$ |
| ksum             |$1$          | $1$          | $\sqrt{1/3} . c$ |
| ksum_k           |$1$          | $k$          | $\sqrt{k/3} . c$ |

A comment: in practice, the average error of **sum_dp** is excellent: $Err2(n)$ is larger than $1.0$ only for $n > 10^{18}$. No need to say that with current machine capacities such a large number of elements is totally unlikely. So where is the need of other algorithms? An answer is for instance "in case one wants to sum double precison values while keeping the double precision in the sum, and no higher precision accumulator is available". On x86 machines there exists the native "extended precision" format (80 bits), but such a format does not exist on all architectures. The compilers often propose the quadruple precision, but as of today it is most of time software emulated, hence quite slow as we will see below. 

Benchmarks
----------

The values to sum are random numbers, with two different distributions:
- **+/-** : uniform distribution in the $[-0.5 ; 0.5[$ interval. The expectation of the sum is 0, and the expected standard deviation is $\sqrt{N/12}$
- **+++** : uniform distribution in the $[1.0 ; 2.0[$ interval. The expectation of the sum is $1.5*N$, and the expected standard deviation is $\sqrt{N/12}$

The [code](../src/sums_bench.F90) is compiled either with:
- `gfortran -O3 sums_bench.F90`
- `gfortran -O3 -ffast-math -DFAST sums_bench.F90`

Runtime benchmark
-----------------

We use here only the "+/- distribution" with $N=2^{30}$ elements, the runtimes are summarized on this graph:

![figure 0.1](sums_bench_files/runtimes.png "figure 0.1")

Observations:
- the **sum_qp** runtime is clipped on the graph, it is $37.6 sec.$, and $30.8 sec.$ in fast-math: as expected, it is horribly slow.
- The intrisic **sumi** has the same runtimes than **sum_sp** : we can hence suspect it is just a straight summation with a single precision accumulator
- The fast-math compilation makes huge difference (about a 3x speed-up) for all the straight summations (except **sum_qp**). This is probably because the loop can be vectorized in these conditions. 
- **sum_dp** is about 50% slower than **sum_sp** with fast-math.
- The default **psum** and **ksum** are significantly slower than the straight summations
- **psum_k** can however get runtimes that are quite close to **sum_dp** ("only" 50% slower with fast-math), at the expanse of a slighty worse accuracy (but still very acceptable)
- **ksum_k** remains 2x to 3x slower than **sum_dp** with fast-math.

Accuracy benchmark
------------------

Unless otherwise specified, we are looking at the benchmarks with the default compilation only. We are summing random numbers for values of $n$ from $2$ to $2^30$. The distribution of the errors being following a normal distribution, the graphs can be a bit obscure (black points below), so what we will plot in the rest of the document is a moving root mean square value (representing a local estimation of the average error):

![figure 1.0](sums_bench_files/fig10.png "figure 1.0")

All graphs have a log horizontal axis. In this representation, a $f(n)=\sqrt{n}$ law looks like an exponential, and $f(n)=\sqrt{log_2(n)}$ looks like a $\sqrt{}$ curve.


# "+/-" distribution

The condition number of the sum is xxxxx. First we compare the genuine versions of the methods:

![figure 1.1](sums_bench_files/fig11.png "figure 1.1")

Observations:
- **sum_dp** and **ksum** have constantly an error that is equal to zero
- **sumi** and **sum_sp** behave exactly the same, with superimposed curves that grow exponentially. This definitely shows that the intrinsic sum is implemented with a straightforward loop and a single precision accumulator
- **psum** has an overall $\sqrt{}$ shape, as expected.

Looking now at the variants of **psum** specifically:

![figure 1.2](sums_bench_files/fig12.png "figure 1.2")

Observations:
- the error first grow fast up to the number of elements that are classical summed, then it tends to reproduce again the $\sqrt{}$ behavior.
- even **psum_1000** has an error less than 10 ($Err2=10$ means that a full significant digit is lost).

Looking now at the variants of **ksum** specifically:
- the error first grow fast up to the number of elements in the chunk, then it tends to keep a "constant" trend
- even **ksum_1000** has an error less than 10 ($Err2=10$ means that a full significant digit is lost). The main interest is that the average error is supposed to be kept constant whatever $n$ above the chunk size.


# "+++" distribution

The condition number of the sum is $1.0$. First we compare the genuine versions of the methods:

![figure 1.4](sums_bench_files/fig14.png "figure 1.4")

Observations:
- **ksum** has almost constantly an error that is equal to zero, except in a few places. However, remind that **sum_dp** is retained as the reference (true) result, while it is rather a (very good) approximation. In border cases, this can lead to "false error" attributed to the **ksum**
- **sumi** and **sum_sp** behave exactly the same, with superimposed curves that grow exponentially. This definitely shows that the intrinsic sum is implemented with a straightforward loop and a single precision accumulator
- **psum** has an overall constant error, while a $\sqrt{}$ shape is expected.

Looking now at the variants of **psum** specifically:

![figure 1.2](sums_bench_files/fig12.png "figure 1.2")

Observations:
- the error first grow fast up to the number of elements that are classical summed, then it tends to reproduce again the $\sqrt{}$ behavior.
- even **psum_1000** has an error less than 10 ($Err2=10$ means that a full significant digit is lost).

Looking now at the variants of **ksum** specifically:
- the error first grow fast up to the number of elements in the chunk, then it tends to keep a "constant" trend
- even **ksum_1000** has an error less than 10 ($Err2=10$ means that a full significant digit is lost). The main interest is that the average error is supposed to be kept constant whatever $n$ above the chunk size.


