import pandas as pd
import matplotlib.pyplot as plt

# Чтение данных из Excel файла
df = pd.read_excel("C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_Foreign_new_2022-2024.xlsx", index_col=0)

# Построение графика
plt.figure(figsize=(10, 6))  # Устанавливаем размер графика

for column in df.columns[1]:
    plt.plot(df.index, df[column], label=column)

# Настройка осей и заголовков
plt.xlabel('Дата')
plt.ylabel('Цена акции')
plt.title('Цены акций различных компаний')
plt.legend()
plt.grid(True)

# Показ графика
plt.show()
