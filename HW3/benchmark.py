import time
import numpy as np
from scipy.optimize import brentq, bisect

def solve(m):
    return brentq(lambda x: m - x + 0.9*np.sin(x), 0, 2*np.pi)
    # return bisect(lambda x: m - x + 0.9*np.sin(x), 0, 2*np.pi)

mm = np.linspace(0, 2*np.pi, 50000, endpoint=False)
vsolve = np.vectorize(solve)
print("[Python] Calculating 50000 orbital positions...")
tic = time.perf_counter()
ee = vsolve(mm)
toc = time.perf_counter()

print("[Python] brentq: {:.3f}s".format(toc - tic))
