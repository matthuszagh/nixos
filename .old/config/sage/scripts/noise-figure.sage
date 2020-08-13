# noise-figure.sage
#
# Compute the equivalent noise figure for multiple noise sources connected in series.
#
# The equivalent noise factor for two noise sources in series (which can be extended indefinitely)
# is given by the equation:
# Fnet = F1 + (F2-1)/G1
#
# Noise figure is related to noise factor by:
# NF = 10log(F)

import numpy as np

n = int(input("Number of noise sources: "))
f = []
g = []

for i in range(n):
    f.append(float(input("Noise figure of source {0} (in dB): ".format(i))))
    g.append(float(input("Gain of source {0} (in dB): ".format(i))))

# Convert figures into factors for computation.
f = [10 ** (i / 10) for i in f]
g = [10 ** (i / 10) for i in g]

fnet = f[0]
gnet = g[0]
for i in range(1, n):
    fnet += (f[i] - 1) / gnet
    gnet *= g[i]

print("Net noise figure: {0:.2f} dB".format(float(10 * np.log10(fnet))))
print("Net gain: {0:.2f} dB".format(float(10 * np.log10(gnet))))
