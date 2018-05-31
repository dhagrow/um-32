import timeit
print('slice  :', timeit.timeit('a = b[:]', 'b = [0] * 1000'))
print('list   :', timeit.timeit('a = list(b)', 'b = [0] * 1000'))
print('copy   :', timeit.timeit('a = copy.copy(b)', 'import copy;b = [0] * 1000'))
print('numpy  :', timeit.timeit('a = numpy.copy(b)', 'import numpy;b = numpy.zeros(1000)'))
print('numpy_t:', timeit.timeit('a = numpy.copy(b)', 'import numpy;b = numpy.zeros(1000, dtype=numpy.uint32)'))
print('array  :', timeit.timeit('a = b[:]', 'import array;b = array.array("I", [0] * 1000)'))
