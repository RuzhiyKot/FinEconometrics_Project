import pandas as pd
import matplotlib.pyplot as plt


# Открываем файл и считываем данные в DataFrame
file_path = 'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_Foreign_new_2022-2024.xlsx' #'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\SP500_closing_prices.xlsx' #'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_MOEX_2022_2024.xlsx'
data = pd.read_excel(file_path)

all = pd.concat(data, axis = 1)

corrMat = all.corr()

pallet = plt.getcmap('jet')

img = plt.imshow(corrMat, cmap = pallet, vmin = -1, vmax = 1, aspect = 'auto')

plt.colorbar(img)
