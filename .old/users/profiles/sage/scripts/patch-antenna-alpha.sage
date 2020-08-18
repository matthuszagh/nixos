# patch-antenna.sage
#
# Compute the approximate length and width for a patch antenna to achieve a desired resonant
# frequency.
#
# This algorithm is taken from `Antenna Theory: Analysis and Design (Balanis, 4e - 2016)` page 791.
#
# ALGORITHM:
#
# Start by computing the width:
# \(W=\frac{c}{2f_r}\sqrt{\frac{2}{\epsilon_r+1}}\)
# * \(f_r\) is the resonant frequency
# * \(c\) is the speed of light in a vacuum
# * \(\epsilon_r\) is the dielectric constant of the substrate
#
# Use the width to compute the effective dielectric constant:
# \(\epsilon_{reff}=\frac{\epsilon_r+1}{2}+\frac{\epsilon_r-1}{2}(1+12h/W)^{-1/2}\)
# * \(h\) is the vertical distance between the ground plane and antenna
#
# Compute the effective length extension due to the effective dielectric:
# \(\Delta L=0.412h\frac{(\epsilon_{reff}+0.3)(W/h+0.264)}{(\epsilon_{reff}-0.258)(W/h+0.8)}
#
# Finally, compute the length:
# \(L=\frac{c}{2f_r\sqrt{\epsilon_{reff}}}-2\Delta L\)

# Constants.
# Speed of light in mm.
c = 299792458*1e3

# Get necessary user input.
er = input("Substrate dielectric constant (4.5 for OSHPark 2-layer): ")
f0 = input("Center frequency [Hz]: ")
lambda0 = c/f0
h = input("Substrate height [mm] (1.524 for OSHPark 2-layer): ")

# W = (c/(2*f0))*(sqrt(2/(er+1)))
# if (W <= h):
#     print(
#         "Width ({0}) must be greater than height ({1}) for this method to work. Exiting...".format(W, h))
#     exit()

# ereff = ((er+1)/2)+(((er-1)/2)*((1+(12*h/W)) ^ (-1/2)))
# DL = 0.412*h*(((ereff+0.3)*((W/h)+0.264))/((ereff-0.258)*((W/h)+0.8)))
L = 0.49*lambda0/sqrt(er)
W = (lambda0/2)*(((er^2+1)/2)^(-1/2))
Z = input("Desired impedance: ")
x = (L/pi)*arccos((Z/(90*(er^2/(er-1))*(L/W)^2))^(1/4))

print("L: {0:.2f}mm, W: {1:.2f}mm, probe inset: {2:.2f}mm".format(float(L), float(W), float(x)))
