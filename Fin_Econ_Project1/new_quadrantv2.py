import networkx as nx
import pandas as pd
import numpy as np
from numba import njit, prange

# Открываем файл и считываем данные в DataFrame
file_path = 'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\SP500_closing_prices.xlsx' #'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_Foreign_new_2022-2024.xlsx' #'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\SP500_closing_prices.xlsx' #'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_MOEX_2022_2024.xlsx'
data = pd.read_excel(file_path)

# Создаем словарь для хранения данных по акциям
stock_prices = {}

# Перебираем столбцы, начиная со второго (первый столбец — это Дата)
for stock in data.columns[1:]:
    stock_prices[stock] = np.array(data[stock].tolist())

# Количество наблюдений для окна (например, 50)
window_size = 200

# Получаем все пары акций
stocks = list(stock_prices.keys())
n_stocks = len(stocks)

# Создаем словарь для хранения массивов коэффициентов корреляции
correlation_results = {}

# Функция для вычисления скользящей корреляции с помощью NumPy
@njit(parallel=True)
def rolling_correlation(prices1, prices2, window_size):
    length = len(prices1) - window_size + 1
    corrs = np.empty(length)
    
    for i in prange(length):
        window1 = prices1[i:i+window_size]
        window2 = prices2[i:i+window_size]
        
        mean1 = np.mean(window1)
        mean2 = np.mean(window2)
        
        std1 = np.std(window1)
        std2 = np.std(window2)
        
        if std1 == 0 or std2 == 0:
            corrs[i] = np.nan
        else:
            cov = np.mean((window1 - mean1) * (window2 - mean2))
            corr = cov / (std1 * std2)
            corrs[i] = corr
            
    return corrs

# Функция для преобразования коэффициента корреляции Пирсона
@njit
def transform_correlation(x):
    term = (2 * x * np.sqrt(1 - x**2) - 1) / (2 * x**2 - 1)
    if x + np.sqrt(1 - x**2) >= 0:
        return 0.5 - (np.arctan(term) / (np.pi / 2))
    else:
        return -(0.5 - (np.arctan((-2 * x * np.sqrt(1 - x**2) - 1) / (2 * x**2 - 1)) / (np.pi / 2)))

# Вычисляем скользящую корреляцию для каждой пары акций
for i in prange(n_stocks):
    print(i)
    for j in prange(i + 1, n_stocks):
        stock1 = stocks[i]
        stock2 = stocks[j]

        # Берем полные ряды данных по каждой акции
        prices1 = stock_prices[stock1]
        prices2 = stock_prices[stock2]

        # Вычисляем массив коэффициентов корреляции Пирсона
        if len(prices1) >= window_size and len(prices2) >= window_size:
            corrs = rolling_correlation(prices1, prices2, window_size)
            correlation_results[(stock1, stock2)] = corrs

quadrant_results = {}
for pair, corrs in correlation_results.items():
    transformed_corrs = [transform_correlation(x) for x in corrs]
    quadrant_results[pair] = transformed_corrs




# Пример: Получаем массив корреляций для акций i и j
'''i, j = 0, 1  # Индексы акций для примера, их можно менять
stock1 = stocks[i]
stock2 = stocks[j]

if (stock1, stock2) in correlation_results:
    print(f'Массив коэффициентов корреляции Пирсона между {stock1} и {stock2}:')
    print(correlation_results[(stock1, stock2)])
else:
    print(f'Нет данных для корреляции Пирсона между {stock1} и {stock2}')

if (stock1, stock2) in quadrant_results:
    print(f'Массив квадрантных коэффициентов корреляции между {stock1} и {stock2}:')
    print(quadrant_results[(stock1, stock2)])
else:
    print(f'Нет данных для квадрантного коэффициента корреляции между {stock1} и {stock2}')'''

# Создаем граф
G = nx.Graph()

for i in prange(n_stocks):
    for j in prange(i + 1, n_stocks):
        pair = (stocks[i], stocks[j])
        if pair in quadrant_results:
            last_value = quadrant_results[pair][-1]  # последний элемент массива
            if np.abs(last_value) < 0.1:
                G.add_edge(i, j)

# Поиск всех максимальных клик
cliques = list(nx.find_cliques(G))

# Нахождение максимального размера клики
max_clique_size = max(len(clique) for clique in cliques)

# Вывод всех кликов максимального размера
max_cliques = [clique for clique in cliques if len(clique) == max_clique_size]

print("Максимальные клики с названиями акций:")
for clique in max_cliques:
    clique_names = [stocks[node] for node in clique]  # Преобразуем индексы в названия акций
    print(clique_names)



'''import networkx as nx
import pandas as pd
import numpy as np
from numba import njit


# Открываем файл и считываем данные в DataFrame
file_path = 'C:\\Users\\verei\\Documents\\Fin_Econ_Project1\\closing_prices_MOEX_2022_2024.xlsx'
data = pd.read_excel(file_path)

# Создаем словарь для хранения данных по акциям
stock_prices = {}

# Перебираем столбцы, начиная со второго (первый столбец — это Дата)
for stock in data.columns[1:]:
    stock_prices[stock] = np.array(data[stock].tolist())

# Количество наблюдений для окна (например, 50)
window_size = 200

# Получаем все пары акций
stocks = list(stock_prices.keys())
n_stocks = len(stocks)

# Создаем словарь для хранения массивов коэффициентов корреляции
correlation_results = {}

# Функция для вычисления скользящей корреляции с помощью NumPy
@njit
def rolling_correlation(prices1, prices2, window_size):
    length = len(prices1) - window_size + 1
    corrs = np.empty(length)
    
    for i in range(length):
        window1 = prices1[i:i+window_size]
        window2 = prices2[i:i+window_size]
        
        mean1 = np.mean(window1)
        mean2 = np.mean(window2)
        
        std1 = np.std(window1)
        std2 = np.std(window2)
        
        if std1 == 0 or std2 == 0:
            corrs[i] = np.nan
        else:
            cov = np.mean((window1 - mean1) * (window2 - mean2))
            corr = cov / (std1 * std2)
            corrs[i] = corr
            
    return corrs

# Функция для преобразования коэффициента корреляции Пирсона
def transform_correlation(x):
    term = (2 * x * np.sqrt(1 - x**2) - 1) / (2 * x**2 - 1)
    if x + np.sqrt(1 - x**2) >= 0:
        return 0.5 - (np.arctan(term) / (np.pi / 2))
    else:
        return -(0.5 - (np.arctan((-2 * x * np.sqrt(1 - x**2) - 1) / (2 * x**2 - 1)) / (np.pi / 2)))


# Вычисляем скользящую корреляцию для каждой пары акций
for i in range(n_stocks):
    print(i)
    for j in range(i + 1, n_stocks):
        stock1 = stocks[i]
        stock2 = stocks[j]

        # Берем полные ряды данных по каждой акции
        prices1 = stock_prices[stock1]
        prices2 = stock_prices[stock2]

        # Вычисляем массив коэффициентов корреляции Пирсона
        if len(prices1) >= window_size and len(prices2) >= window_size:
            corrs = rolling_correlation(prices1, prices2, window_size)
            correlation_results[(stock1, stock2)] = corrs

quadrant_results = {}
for pair, corrs in correlation_results.items():
    transformed_corrs = [transform_correlation(x) for x in corrs]
    quadrant_results[pair] = transformed_corrs

# Пример: Получаем массив корреляций для акций i и j
i, j = 0, 1  # Индексы акций для примера, их можно менять
stock1 = stocks[i]
stock2 = stocks[j]

if (stock1, stock2) in correlation_results:
    print(f'Массив коэффициентов корреляции Пирсона между {stock1} и {stock2}:')
    print(correlation_results[(stock1, stock2)])
else:
    print(f'Нет данных для корреляции Пирсона между {stock1} и {stock2}')

if (stock1, stock2) in quadrant_results:
    print(f'Массив квадрантных коэффициентов корреляции между {stock1} и {stock2}:')
    print(quadrant_results[(stock1, stock2)])
else:
    print(f'Нет данных для квадрантного коэффициента корреляции между {stock1} и {stock2}')

# Создаем граф
G = nx.Graph()

for i in range(n_stocks):
    for j in range(i + 1, n_stocks):
        pair = (stocks[i], stocks[j])
        if pair in quadrant_results:
            last_value = quadrant_results[pair][-180]  # последний элемент массива
            if np.abs(last_value) < 0.1:
                G.add_edge(i, j)


# Поиск всех максимальных клик
cliques = list(nx.find_cliques(G))

# Нахождение максимального размера клики
max_clique_size = max(len(clique) for clique in cliques)

# Вывод всех кликов максимального размера
max_cliques = [clique for clique in cliques if len(clique) == max_clique_size]

print("Максимальные клики с названиями акций:")
for clique in max_cliques:
    clique_names = [stocks[node] for node in clique]  # Преобразуем индексы в названия акций
    print(clique_names)'''